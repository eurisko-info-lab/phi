{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- ╔═══════════════════════════════════════════════════════════════════════════╗
-- ║              Φ-PARSER: Parse .phi specification files                     ║
-- ╠═══════════════════════════════════════════════════════════════════════════╣
-- ║  Parse language specifications written in Phi syntax into LangSpec       ║
-- ╚═══════════════════════════════════════════════════════════════════════════╝
--
-- Parses specs like:
--
--   language Phi {
--     sort Type
--     sort Term
--
--     constructor Unit : Type
--     constructor succ : Term → Term
--
--     rule BetaReduce {
--       app (lam x A body) v ↦ body[x := v]
--     }
--   }

module Phi.Parser
  ( -- * Parsing
    parseSpec
  , parseFile
    -- * Re-exports
  , LangSpec(..)
  , SortDecl(..)
  , ConDecl(..)
  , XformDecl(..)
  , RuleDecl(..)
  , GrammarDecl(..)
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (mapMaybe, listToMaybe)
import qualified Data.Map.Strict as M
import Control.Monad (guard, void)

import Phi.Lang

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 1: Parser Infrastructure
-- ═══════════════════════════════════════════════════════════════════════════

-- | Parse result
type ParseResult a = Either String (a, String)

-- | Simple recursive descent parser
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

-- | Alternative
(<|>) :: Parser a -> Parser a -> Parser a
Parser p1 <|> Parser p2 = Parser $ \s ->
  case p1 s of
    Left _ -> p2 s
    ok -> ok

-- | Fail with message
pfail :: String -> Parser a
pfail msg = Parser $ \_ -> Left msg

-- | Check predicate
pguard :: Bool -> String -> Parser ()
pguard True _ = pure ()
pguard False msg = pfail msg

-- | Skip whitespace and comments
skipWs :: Parser ()
skipWs = Parser $ \s -> Right ((), go s)
  where
    go [] = []
    go ('/':'/':rest) = go (dropWhile (/= '\n') rest)  -- Line comment
    go ('/':'*':rest) = go (dropBlock rest)             -- Block comment
    go (c:rest) | isSpace c = go rest
    go s = s
    
    dropBlock ('*':'/':rest) = rest
    dropBlock (_:rest) = dropBlock rest
    dropBlock [] = []

-- | Parse exact string
string :: String -> Parser String
string lit = Parser $ \s -> do
  let (ws, rest) = span isSpace s  -- Skip leading whitespace
      n = length lit
  if take n rest == lit
    then Right (lit, drop n rest)
    else Left $ "Expected '" ++ lit ++ "'"

-- | Parse keyword (followed by non-alphanumeric)
keyword :: String -> Parser ()
keyword kw = Parser $ \s -> do
  let rest0 = dropWhile isSpace s
      n = length kw
      prefix = take n rest0
      suffix = drop n rest0
  if prefix == kw && (null suffix || not (isAlphaNum (head suffix)))
    then Right ((), suffix)
    else Left $ "Expected keyword '" ++ kw ++ "'"

-- | Parse identifier
pIdent :: Parser String
pIdent = Parser $ \s -> do
  let rest0 = dropWhile isSpace s
  case rest0 of
    (c:cs) | isAlpha c || c == '_' ->
      let (name, rest) = span (\x -> isAlphaNum x || x == '_') rest0
      in Right (name, rest)
    _ -> Left "Expected identifier"

-- | Parse non-keyword identifier
pNonKwIdent :: Parser String
pNonKwIdent = do
  name <- pIdent
  pguard (name `notElem` keywords) $ "'" ++ name ++ "' is a keyword"
  return name
  where
    keywords = ["language", "sort", "constructor", "xform", "rule", "def", 
                "grammar", "strategy", "where", "and"]

-- | Parse integer
pInt :: Parser Int
pInt = Parser $ \s -> do
  let rest0 = dropWhile isSpace s
  case rest0 of
    ('-':cs) -> case span isDigit cs of
      ([], _) -> Left "Expected integer"
      (ds, rest) -> Right (negate (read ds), rest)
    _ -> case span isDigit rest0 of
      ([], _) -> Left "Expected integer"
      (ds, rest) -> Right (read ds, rest)

-- | Parse string literal
pString :: Parser String
pString = Parser $ \s -> do
  let rest0 = dropWhile isSpace s
  case rest0 of
    ('"':cs) ->
      let (content, rest) = span (/= '"') cs
      in case rest of
        ('"':rest') -> Right (content, rest')
        _ -> Left "Unterminated string"
    _ -> Left "Expected string"

-- | Parse symbol (consumes surrounding whitespace)
pSym :: String -> Parser ()
pSym sym = void (string sym) >> skipWs

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

-- | Separated by
pSepBy :: Parser a -> Parser sep -> Parser [a]
pSepBy p sep = pSepBy1 p sep <|> pure []

pSepBy1 :: Parser a -> Parser sep -> Parser [a]
pSepBy1 p sep = do
  x <- p
  xs <- pMany (sep >> p)
  return (x:xs)

-- | Between delimiters
pBetween :: Parser open -> Parser close -> Parser a -> Parser a
pBetween open close p = do
  _ <- open
  skipWs
  x <- p
  skipWs
  _ <- close
  return x

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 2: Grammar Parsers
-- ═══════════════════════════════════════════════════════════════════════════

-- | Parse arrow: → or ->
pArrow :: Parser ()
pArrow = pSym "→" <|> pSym "->"

-- | Parse biarrow: ⇄ or <->
pBiArrow :: Parser ()
pBiArrow = pSym "⇄" <|> pSym "<->"

-- | Parse mapsto: ↦ or |->
pMapsto :: Parser ()
pMapsto = pSym "↦" <|> pSym "|->"

-- | Parse equals
pEquals :: Parser ()
pEquals = pSym "="

-- | Parse colon
pColon :: Parser ()
pColon = pSym ":"

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 3: Type Expressions
-- ═══════════════════════════════════════════════════════════════════════════

-- | Parse type expression
pTypeExpr :: Parser TypeRef
pTypeExpr = pArrowType

pArrowType :: Parser TypeRef
pArrowType = do
  a <- pAtomType
  skipWs
  mArrow <- pOpt pArrow
  case mArrow of
    Just _ -> do
      skipWs
      b <- pArrowType
      return $ TRArrow a b
    Nothing -> return a

pAtomType :: Parser TypeRef
pAtomType = pParenType <|> pNamedType

pParenType :: Parser TypeRef
pParenType = pBetween (pSym "(") (pSym ")") pTypeExpr

pNamedType :: Parser TypeRef
pNamedType = do
  name <- pIdent
  skipWs
  return $ TRSort name

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 4: Pattern Expressions
-- ═══════════════════════════════════════════════════════════════════════════

-- | Parse pattern for rules
pPattern :: Parser XformPat
pPattern = pAppPat

pAppPat :: Parser XformPat
pAppPat = do
  atoms <- pMany1 pAtomPat
  return $ foldl1 (\f a -> XPCon "app" [f, a]) atoms

pAtomPat :: Parser XformPat
pAtomPat = pParenPat <|> pConPat <|> pVarOrCon

pParenPat :: Parser XformPat
pParenPat = pBetween (pSym "(") (pSym ")") pPattern

pConPat :: Parser XformPat
pConPat = Parser $ \s -> do
  let rest0 = dropWhile isSpace s
  case rest0 of
    (c:cs) | isAlpha c || c == '_' -> do
      let (name, rest1) = span (\x -> isAlphaNum x || x == '_') rest0
      case rest1 of
        ('(':rest2) -> runParser (pConArgs name) rest2
        _ -> Left "Not a constructor pattern"
    _ -> Left "Not a constructor pattern"
  where
    pConArgs name = do
      skipWs
      args <- pSepBy pPattern (pSym ",")
      skipWs
      pSym ")"
      return $ XPCon name args

pVarOrCon :: Parser XformPat
pVarOrCon = do
  name <- pNonKwIdent
  skipWs
  -- Uppercase multi-char names are constructors
  if length name > 1 && head name `elem` ['A'..'Z']
    then return $ XPCon name []
    else return $ XPVar name

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 5: Xform Body Expressions
-- ═══════════════════════════════════════════════════════════════════════════

-- | Parse xform body (RHS of rule)
pXformBody :: Parser XformBody
pXformBody = pBodyApp

pBodyApp :: Parser XformBody
pBodyApp = do
  atoms <- pMany1 pBodyAtom
  return $ foldl1 (\f a -> XBApp f a) atoms

pBodyAtom :: Parser XformBody
pBodyAtom = pBodyParen <|> pBodyCon <|> pBodyVarOrCon

pBodyParen :: Parser XformBody
pBodyParen = pBetween (pSym "(") (pSym ")") pXformBody

pBodyCon :: Parser XformBody
pBodyCon = Parser $ \s -> do
  let rest0 = dropWhile isSpace s
  case rest0 of
    (c:cs) | isAlpha c || c == '_' -> do
      let (name, rest1) = span (\x -> isAlphaNum x || x == '_') rest0
      case rest1 of
        ('(':rest2) -> runParser (pBodyConArgs name) rest2
        _ -> Left "Not a constructor body"
    _ -> Left "Not a constructor body"
  where
    pBodyConArgs name = do
      skipWs
      args <- pSepBy pXformBody (pSym ",")
      skipWs
      pSym ")"
      return $ XBCon name args

pBodyVarOrCon :: Parser XformBody
pBodyVarOrCon = do
  name <- pNonKwIdent
  skipWs
  if length name > 1 && head name `elem` ['A'..'Z']
    then return $ XBCon name []
    else return $ XBVar name

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 6: Declarations
-- ═══════════════════════════════════════════════════════════════════════════

-- | Parse sort declaration: sort Name
pSortDecl :: Parser SortDecl
pSortDecl = do
  keyword "sort"
  skipWs
  name <- pIdent
  skipWs
  return $ SortDecl name []

-- | Parse constructor declaration: constructor Name : Type → Type
pConDecl :: Parser ConDecl
pConDecl = do
  keyword "constructor"
  skipWs
  name <- pIdent
  skipWs
  pColon
  skipWs
  (fields, ret) <- pConType
  return $ ConDecl name fields ret

pConType :: Parser ([Field], String)
pConType = do
  types <- pSepBy1 pAtomType pArrow
  case types of
    [] -> pfail "Constructor needs return type"
    _ -> do
      let ret = case last types of
            TRSort s -> s
            _ -> "?"
          args = init types
          fields = zipWith (\i t -> Field Nothing t) [0..] args
      return (fields, ret)

-- | Parse xform declaration: xform Name : From ⇄ To
pXformDecl :: Parser XformDecl
pXformDecl = do
  keyword "xform"
  skipWs
  name <- pIdent
  skipWs
  pColon
  skipWs
  from <- pAtomType
  skipWs
  pBiArrow
  skipWs
  to <- pAtomType
  skipWs
  return $ XformDecl name from to []

-- | Parse rule declaration: rule Name { cases }
pRuleDecl :: Parser RuleDecl
pRuleDecl = do
  keyword "rule"
  skipWs
  name <- pIdent
  skipWs
  pSym "{"
  skipWs
  cases <- pMany pRuleCase
  skipWs
  pSym "}"
  return $ RuleDecl name cases

pRuleCase :: Parser XformCase
pRuleCase = do
  pat <- pPattern
  skipWs
  pMapsto
  skipWs
  body <- pXformBody
  skipWs
  return $ XformCase pat body

-- | Parse grammar declaration: grammar Name { rules }
pGrammarDecl :: Parser GrammarDecl
pGrammarDecl = do
  keyword "grammar"
  skipWs
  name <- pIdent
  skipWs
  pSym "{"
  skipWs
  prods <- pMany pProduction
  skipWs
  pSym "}"
  return $ GrammarDecl name prods

pProduction :: Parser Production
pProduction = do
  pat <- pGrammarPat
  skipWs
  pSym "=>"
  skipWs
  con <- pIdent
  skipWs
  return $ Production pat con

pGrammarPat :: Parser GrammarPat
pGrammarPat = do
  parts <- pMany1 pGrammarAtom
  return $ case parts of
    [p] -> p
    ps -> GPSeq ps

pGrammarAtom :: Parser GrammarPat
pGrammarAtom = pGrammarLit <|> pGrammarNonTerm

pGrammarLit :: Parser GrammarPat
pGrammarLit = do
  s <- pString
  skipWs
  return $ GPLiteral s

pGrammarNonTerm :: Parser GrammarPat
pGrammarNonTerm = do
  name <- pIdent
  skipWs
  mMod <- pOpt (string "*" <|> string "+")
  skipWs
  let base = GPNonTerminal name
  return $ case mMod of
    Just "*" -> GPRep base
    Just "+" -> GPRep1 base
    _ -> base

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 7: Language Specification
-- ═══════════════════════════════════════════════════════════════════════════

-- | Parse declaration
pDeclaration :: Parser (Either SortDecl (Either ConDecl (Either XformDecl (Either RuleDecl GrammarDecl))))
pDeclaration = 
  (Left <$> pSortDecl) <|>
  (Right . Left <$> pConDecl) <|>
  (Right . Right . Left <$> pXformDecl) <|>
  (Right . Right . Right . Left <$> pRuleDecl) <|>
  (Right . Right . Right . Right <$> pGrammarDecl)

-- | Parse language spec
pLangSpec :: Parser LangSpec
pLangSpec = do
  keyword "language"
  skipWs
  name <- pIdent
  skipWs
  pSym "{"
  skipWs
  decls <- pMany pDeclaration
  skipWs
  pSym "}"
  
  let sorts = [s | Left s <- decls]
      cons = [c | Right (Left c) <- decls]
      xforms = [x | Right (Right (Left x)) <- decls]
      rules = [r | Right (Right (Right (Left r))) <- decls]
      grammars = [g | Right (Right (Right (Right g))) <- decls]
  
  return $ LangSpec
    { lsName = name
    , lsSorts = sorts
    , lsCons = cons
    , lsXforms = xforms
    , lsRules = rules
    , lsGrammars = grammars
    }

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 8: Public API
-- ═══════════════════════════════════════════════════════════════════════════

-- | Parse a spec from a string
parseSpec :: String -> Either String LangSpec
parseSpec input = do
  (spec, rest) <- runParser (skipWs >> pLangSpec) input
  let remaining = dropWhile isSpace rest
  if null remaining
    then Right spec
    else Left $ "Unexpected input remaining: " ++ take 20 remaining

-- | Parse a spec file (name + content)
parseFile :: String -> String -> Either String LangSpec
parseFile _name content = parseSpec content
