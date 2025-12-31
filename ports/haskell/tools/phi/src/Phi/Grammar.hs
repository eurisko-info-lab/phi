{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- ╔═══════════════════════════════════════════════════════════════════════════╗
-- ║              Φ-GRAMMAR: Grammar Interpreter (The Brain)                   ║
-- ╠═══════════════════════════════════════════════════════════════════════════╣
-- ║  Execute grammar rules to parse strings into ASTs and render back        ║
-- ╚═══════════════════════════════════════════════════════════════════════════╝
--
-- This is the missing piece that makes `parse term "λA:*. λx:A. x"` work!
--
-- BIDIRECTIONAL GRAMMARS
-- ======================
--
-- The grammar rules work both ways:
--
--   grammar term {
--     "λ" IDENT ":" term "." term => Lam(IDENT, term, term)
--   }
--
-- PARSE:  "λx:*. x" → Lam("x", Star, Var("x"))
-- RENDER: Lam("x", Star, Var("x")) → "λx:*. x"

module Phi.Grammar
  ( -- * Grammar Interpreter (Parsing)
    GrammarInterp
  , mkGrammarInterp
  , parseSortG
  , parseAllG
    -- * Renderer (AST → String)
  , GrammarRenderer
  , mkGrammarRenderer
  , renderG
    -- * Xform Interpreter
  , XformInterp
  , mkXformInterp
  , applyXform
  , applyXformRecursive
    -- * Strategy Interpreter
  , StrategyInterp
  , mkStrategyInterp
  , normalize
  , step
    -- * Lang Runner (Main Entry Point)
  , LangRunner
  , mkLangRunner
  , parseL
  , renderL
  , transformL
  , normalizeL
  , pipelineL
    -- * Convenience
  , parse
  , render
  ) where

import Data.Char (isAlpha, isAlphaNum)
import Data.List (find, intercalate)
import Data.Maybe (mapMaybe, listToMaybe)
import qualified Data.Map.Strict as M
import Control.Monad (guard)

import Phi.Meta.Val (Val(..))
import Phi.Meta.Env (Env)
import qualified Phi.Meta.Env as Env
import Phi.Lang

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 1: Parser State and Result
-- ═══════════════════════════════════════════════════════════════════════════

-- | Parse result: either error message or (result, remaining input)
type ParseResult a = Either String (a, String)

-- | Simple parser: String → Either error (result, rest)
newtype Parser a = Parser { runParser :: String -> ParseResult a }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (a, rest) <- p s
    return (f a, rest)

instance Applicative Parser where
  pure a = Parser $ \s -> Right (a, s)
  Parser pf <*> Parser pa = Parser $ \s -> do
    (f, s') <- pf s
    (a, s'') <- pa s'
    return (f a, s'')

instance Monad Parser where
  Parser pa >>= f = Parser $ \s -> do
    (a, s') <- pa s
    runParser (f a) s'

-- | Alternative for parsers
(<|>) :: Parser a -> Parser a -> Parser a
Parser p1 <|> Parser p2 = Parser $ \s ->
  case p1 s of
    Left _ -> p2 s
    ok -> ok

-- | Fail with message
pfail :: String -> Parser a
pfail msg = Parser $ \_ -> Left msg

-- | Skip whitespace
skipWhite :: Parser ()
skipWhite = Parser $ \s -> Right ((), dropWhile (`elem` " \t\n\r") s)

-- | Parse literal string
literal :: String -> Parser String
literal lit = Parser $ \s ->
  let (ws, rest1) = span (`elem` " \t\n\r") s
      n = length lit
  in if take n rest1 == lit
     then Right (lit, drop n rest1)
     else Left $ "Expected '" ++ lit ++ "' but got '" ++ take 10 rest1 ++ "...'"

-- | Parse identifier: [a-zA-Z_][a-zA-Z0-9_]*
pIdent :: Parser String
pIdent = Parser $ \s ->
  let (ws, rest1) = span (`elem` " \t\n\r") s
  in case rest1 of
    (c:cs) | isAlpha c || c == '_' ->
      let (name, rest2) = span (\x -> isAlphaNum x || x == '_') rest1
      in Right (name, rest2)
    _ -> Left "Expected identifier"

-- | Parse integer
pInt :: Parser Int
pInt = Parser $ \s ->
  let (ws, rest1) = span (`elem` " \t\n\r") s
  in case rest1 of
    ('-':cs) -> case span (`elem` "0123456789") cs of
      ([], _) -> Left "Expected integer"
      (ds, rest2) -> Right (negate (read ds), rest2)
    _ -> case span (`elem` "0123456789") rest1 of
      ([], _) -> Left "Expected integer"
      (ds, rest2) -> Right (read ds, rest2)

-- | Parse string literal
pString :: Parser String
pString = Parser $ \s ->
  let (ws, rest1) = span (`elem` " \t\n\r") s
  in case rest1 of
    ('"':cs) ->
      let (content, rest2) = span (/= '"') cs
      in case rest2 of
        ('"':rest3) -> Right (content, rest3)
        _ -> Left "Unterminated string"
    _ -> Left "Expected string"

-- | Parse at end of input
pEof :: Parser ()
pEof = Parser $ \s ->
  let rest = dropWhile (`elem` " \t\n\r") s
  in if null rest
     then Right ((), "")
     else Left $ "Expected end of input, got: " ++ take 10 rest

-- | Optional parser
pOpt :: Parser a -> Parser (Maybe a)
pOpt p = (Just <$> p) <|> pure Nothing

-- | Zero or more
pMany :: Parser a -> Parser [a]
pMany p = pMany1 p <|> pure []

-- | One or more
pMany1 :: Parser a -> Parser [a]
pMany1 p = do
  x <- p
  xs <- pMany p
  return (x:xs)

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 2: Grammar Interpreter (Parsing)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Grammar interpreter for parsing strings according to grammar rules
data GrammarInterp = GrammarInterp
  { giSpec :: LangSpec
  }

-- | Create a grammar interpreter
mkGrammarInterp :: LangSpec -> GrammarInterp
mkGrammarInterp = GrammarInterp

-- | Get parser for a sort
parserFor :: GrammarInterp -> String -> Parser Val
parserFor gi sort = case grammarFor (giSpec gi) sort of
  Just grammar ->
    let prods = gdProductions grammar
    in if null prods
       then pfail $ "No productions for sort: " ++ sort
       else foldr1 (<|>) (map (productionParser gi) prods)
  Nothing -> pfail $ "No grammar defined for sort: " ++ sort

-- | Convert a production into a parser
productionParser :: GrammarInterp -> Production -> Parser Val
productionParser gi prod = do
  captures <- patternParser gi (prodPattern prod)
  return $ VCon (prodConstructor prod) captures

-- | Convert a grammar pattern into a parser that returns captured values
patternParser :: GrammarInterp -> GrammarPat -> Parser [Val]
patternParser gi = \case
  GPLiteral text -> do
    _ <- literal text
    return []
  
  GPRegex _ -> do
    -- Simplified: treat as identifier for now
    s <- pIdent
    return [VStr s]
  
  GPNonTerminal sort -> case sort of
    "IDENT" -> do
      s <- pIdent
      return [VStr s]
    "INT" -> do
      i <- pInt
      return [VInt i]
    "STRING" -> do
      s <- pString
      return [VStr s]
    _ -> do
      v <- parserFor gi sort
      return [v]
  
  GPSeq parts -> do
    capturesList <- mapM (patternParser gi) parts
    return (concat capturesList)
  
  GPAlt options ->
    foldr1 (<|>) (map (patternParser gi) options)
  
  GPOpt inner -> do
    mCaptures <- pOpt (patternParser gi inner)
    return $ maybe [] id mCaptures
  
  GPRep inner -> do
    capturesList <- pMany (patternParser gi inner)
    return (concat capturesList)
  
  GPRep1 inner -> do
    capturesList <- pMany1 (patternParser gi inner)
    return (concat capturesList)
  
  GPGroup inner ->
    patternParser gi inner

-- | Parse a string as a particular sort
parseSortG :: GrammarInterp -> String -> String -> Either String Val
parseSortG gi sort input = do
  (result, _) <- runParser (parserFor gi sort) input
  return result

-- | Parse a string and ensure all input is consumed
parseAllG :: GrammarInterp -> String -> String -> Either String Val
parseAllG gi sort input = do
  (result, rest) <- runParser (parserFor gi sort) input
  let remaining = dropWhile (`elem` " \t\n\r") rest
  if null remaining
    then return result
    else Left $ "Unexpected input remaining: " ++ take 20 remaining

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 3: Renderer (AST → String)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Grammar renderer for converting AST back to strings
data GrammarRenderer = GrammarRenderer
  { grSpec :: LangSpec
  }

-- | Create a grammar renderer
mkGrammarRenderer :: LangSpec -> GrammarRenderer
mkGrammarRenderer = GrammarRenderer

-- | Find the production for a constructor
findProduction :: GrammarRenderer -> String -> Maybe Production
findProduction gr constructor =
  listToMaybe $ mapMaybe (find ((== constructor) . prodConstructor) . gdProductions) 
                         (lsGrammars (grSpec gr))

-- | Render a value using the grammar
renderG :: GrammarRenderer -> Val -> String
renderG gr = \case
  VCon name args ->
    case findProduction gr name of
      Just prod -> renderProduction gr prod args
      Nothing ->
        if null args
        then name
        else name ++ "(" ++ intercalate ", " (map (renderG gr) args) ++ ")"
  
  VStr s -> s
  VInt i -> show i
  VList elems -> "[" ++ intercalate ", " (map (renderG gr) elems) ++ "]"
  VClosure _ _ _ -> "<function>"

-- | Render using a specific production
renderProduction :: GrammarRenderer -> Production -> [Val] -> String
renderProduction gr prod args =
  let (rendered, _) = renderPattern gr (prodPattern prod) args
  in rendered

-- | Render a pattern, consuming values as needed
renderPattern :: GrammarRenderer -> GrammarPat -> [Val] -> (String, [Val])
renderPattern gr = go
  where
    go (GPLiteral text) remaining = (text, remaining)
    
    go (GPRegex _) (v:remaining) = (renderG gr v, remaining)
    go (GPRegex _) [] = ("", [])
    
    go (GPNonTerminal _) (v:remaining) = (renderG gr v, remaining)
    go (GPNonTerminal _) [] = ("", [])
    
    go (GPSeq parts) remaining =
      let (strs, remaining') = foldl' goSeq ([], remaining) parts
      in (unwords (reverse strs), remaining')
      where
        goSeq (acc, rem) p =
          let (s, rem') = go p rem
          in (s:acc, rem')
    
    go (GPAlt (opt:_)) remaining = go opt remaining
    go (GPAlt []) remaining = ("", remaining)
    
    go (GPOpt inner) remaining = go inner remaining
    go (GPRep inner) remaining = go inner remaining
    go (GPRep1 inner) remaining = go inner remaining
    go (GPGroup inner) remaining = go inner remaining

    foldl' f z [] = z
    foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 4: Xform Interpreter
-- ═══════════════════════════════════════════════════════════════════════════

-- | Xform interpreter for executing transformations
data XformInterp = XformInterp
  { xiSpec :: LangSpec
  }

-- | Create an xform interpreter
mkXformInterp :: LangSpec -> XformInterp
mkXformInterp = XformInterp

-- | Match a pattern against a value, returning bindings
matchPat :: XformPat -> Val -> Maybe Env
matchPat pat val = case (pat, val) of
  (XPVar name, v) -> Just (Env.bind name v Env.empty)
  
  (XPCon conName pats, VCon vName args)
    | conName == vName && length pats == length args -> do
        envs <- zipWithM matchPat pats args
        return $ foldr Env.merge Env.empty envs
    | otherwise -> Nothing
  
  (XPWild, _) -> Just Env.empty
  
  (XPLit (VStr s1), VStr s2) | s1 == s2 -> Just Env.empty
  (XPLit (VInt i1), VInt i2) | i1 == i2 -> Just Env.empty
  (XPLit _, _) -> Nothing
  
  (XPCon _ _, _) -> Nothing

-- | Evaluate an xform body with bindings
evalBody :: XformBody -> Env -> Val
evalBody body env = case body of
  XBVar name -> 
    case Env.lookup name env of
      Just v -> v
      Nothing -> VStr ("unbound: " ++ name)
  
  XBCon conName args ->
    VCon conName (map (`evalBody` env) args)
  
  XBLit v -> v
  
  XBApp f arg ->
    case evalBody f env of
      VClosure x bodyVal closure ->
        -- For now, just substitute the argument - closures store Val not Expr
        evalBody (XBVar x) (Env.bind x (evalBody arg env) closure)
      _ -> VStr "app error"
  
  XBLet name val body' ->
    let v = evalBody val env
    in evalBody body' (Env.bind name v env)

-- | Apply an xform to a value
applyXform :: XformInterp -> String -> Val -> Either String Val
applyXform xi xformName input =
  case xformNamed (xiSpec xi) xformName of
    Just xform -> applyXformCases (xdCases xform) input
    Nothing -> Left $ "Unknown xform: " ++ xformName

-- | Try each case until one matches
applyXformCases :: [XformCase] -> Val -> Either String Val
applyXformCases [] input = Left $ "No matching case for: " ++ show input
applyXformCases (c : rest) input =
  case matchPat (xcPat c) input of
    Just env -> Right (evalBody (xcBody c) env)
    Nothing -> applyXformCases rest input

-- | Apply xform recursively (bottom-up)
applyXformRecursive :: XformInterp -> String -> Val -> Either String Val
applyXformRecursive xi xformName input = do
  -- First transform children
  transformedChildren <- case input of
    VCon name args -> do
      newArgs <- mapM (applyXformRecursive xi xformName) args
      return $ VCon name newArgs
    other -> return other
  
  -- Then try to transform this node (don't fail if no match)
  case applyXform xi xformName transformedChildren of
    Right result -> Right result
    Left _ -> Right transformedChildren  -- No rule matched, keep as-is

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 5: Strategy Interpreter
-- ═══════════════════════════════════════════════════════════════════════════

-- | Strategy interpreter for reduction strategies
data StrategyInterp = StrategyInterp
  { siXform :: XformInterp
  }

-- | Create a strategy interpreter
mkStrategyInterp :: LangSpec -> StrategyInterp
mkStrategyInterp spec = StrategyInterp (mkXformInterp spec)

-- | Normalize by repeatedly applying a strategy
normalize :: StrategyInterp -> String -> Val -> Int -> Either String Val
normalize si strategyName input maxSteps = go input 0
  where
    go current steps
      | steps >= maxSteps = Left $ "Normalization did not converge after " ++ show maxSteps ++ " steps"
      | otherwise = case applyXform (siXform si) strategyName current of
          Right next | next /= current -> go next (steps + 1)
          _ -> Right current  -- Fixed point reached

-- | Apply strategy once
step :: StrategyInterp -> String -> Val -> Either String Val
step si strategyName input = applyXform (siXform si) strategyName input

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 6: Lang Runner (Main Entry Point)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Complete language runner
data LangRunner = LangRunner
  { lrParser     :: GrammarInterp
  , lrRenderer   :: GrammarRenderer
  , lrXforms     :: XformInterp
  , lrStrategies :: StrategyInterp
  }

-- | Create a lang runner from a spec
mkLangRunner :: LangSpec -> LangRunner
mkLangRunner spec = LangRunner
  { lrParser     = mkGrammarInterp spec
  , lrRenderer   = mkGrammarRenderer spec
  , lrXforms     = mkXformInterp spec
  , lrStrategies = mkStrategyInterp spec
  }

-- | Parse a string as a sort
parseL :: LangRunner -> String -> String -> Either String Val
parseL lr = parseAllG (lrParser lr)

-- | Render a value back to string
renderL :: LangRunner -> Val -> String
renderL lr = renderG (lrRenderer lr)

-- | Apply a transformation
transformL :: LangRunner -> String -> Val -> Either String Val
transformL lr = applyXform (lrXforms lr)

-- | Normalize using a strategy
normalizeL :: LangRunner -> String -> Val -> Either String Val
normalizeL lr stratName input = normalize (lrStrategies lr) stratName input 1000

-- | Full pipeline: parse → transform → render
pipelineL :: LangRunner 
          -> String           -- ^ sort
          -> String           -- ^ input
          -> Maybe String     -- ^ xform name
          -> Maybe String     -- ^ strategy name
          -> Either String String
pipelineL lr sort input mXform mStrategy = do
  parsed <- parseL lr sort input
  transformed <- case mXform of
    Just xformName -> transformL lr xformName parsed
    Nothing -> Right parsed
  normalized <- case mStrategy of
    Just stratName -> normalizeL lr stratName transformed
    Nothing -> Right transformed
  return $ renderL lr normalized

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 7: Convenience Functions
-- ═══════════════════════════════════════════════════════════════════════════

-- | Quick parse from a spec
parse :: LangSpec -> String -> String -> Either String Val
parse spec = parseAllG (mkGrammarInterp spec)

-- | Quick render from a spec
render :: LangSpec -> Val -> String
render spec = renderG (mkGrammarRenderer spec)

-- Helper for zipWithM
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence (zipWith f xs ys)
