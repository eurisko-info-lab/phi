{-# LANGUAGE LambdaCase #-}
-- | Syntax: Lexer and Token types
--
-- Lexer (String → [Token]) converts source code to tokens,
-- which are then parsed by the grammar interpreter.
module Phi.Syntax
  ( -- * Tokens
    Token(..)
  , renderToken
    -- * Lexer
  , Lexer(..)
  , defaultLexer
  , tokenize
    -- * Utilities
  , isIdent
  , isKeyword
  , isSymbol
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (find, isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 1: Tokens
-- ═══════════════════════════════════════════════════════════════════════════

-- | Token: The lexical units of a language.
data Token
  = TIdent String       -- ^ Identifier: foo, Bar, _x1
  | TKeyword String     -- ^ Keyword: if, then, else
  | TSymbol String      -- ^ Symbol: +, ->, ::
  | TIntLit Int         -- ^ Integer: 42, -1
  | TStrLit String      -- ^ String: "hello"
  | TOpen Char          -- ^ Opening: (, [, {
  | TClose Char         -- ^ Closing: ), ], }
  | TWhite String       -- ^ Whitespace (preserved for printing)
  | TNewline            -- ^ Line break
  | TIndent Int         -- ^ Indentation level
  | TError String       -- ^ Lexer error
  deriving (Eq, Show)

-- | Render token back to string
renderToken :: Token -> String
renderToken = \case
  TIdent n   -> n
  TKeyword w -> w
  TSymbol s  -> s
  TIntLit v  -> show v
  TStrLit v  -> "\"" ++ v ++ "\""
  TOpen b    -> [b]
  TClose b   -> [b]
  TWhite t   -> t
  TNewline   -> "\n"
  TIndent l  -> replicate (l * 2) ' '
  TError m   -> "<ERROR: " ++ m ++ ">"

-- | Check if token is identifier
isIdent :: Token -> Bool
isIdent (TIdent _) = True
isIdent _          = False

-- | Check if token is keyword
isKeyword :: Token -> Bool
isKeyword (TKeyword _) = True
isKeyword _            = False

-- | Check if token is symbol
isSymbol :: Token -> Bool
isSymbol (TSymbol _) = True
isSymbol _           = False

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 2: Lexer
-- ═══════════════════════════════════════════════════════════════════════════

-- | Lexer configuration
data Lexer = Lexer
  { lexKeywords :: Set String   -- ^ Words recognized as keywords
  , lexSymbols :: Set String    -- ^ Multi-char symbol tokens
  } deriving (Eq, Show)

-- | Default lexer with common Phi keywords and symbols
defaultLexer :: Lexer
defaultLexer = Lexer
  { lexKeywords = Set.fromList 
      ["sort", "constructor", "grammar", "xform", "rule", "language",
       "if", "then", "else", "let", "in", "match", "with", "case",
       "true", "false", "λ", "forall", "∀", "→", "⇄"]
  , lexSymbols = Set.fromList
      ["->", "=>", "<-", "::", "↦", "⇄", "→", "∀", "λ", "//", "/*", "*/",
       "<=", ">=", "==", "!=", "&&", "||", "++", "..", "|"]
  }

-- | Tokenize a string
tokenize :: Lexer -> String -> [Token]
tokenize lexer = go
  where
    keywords = lexKeywords lexer
    symbols = lexSymbols lexer
    symbolList = Set.toList symbols  -- for searching
    
    go [] = []
    go ('\n':rest) = TNewline : go rest
    
    go (c:rest) | isSpace c =
      let (ws, rest') = span (\x -> isSpace x && x /= '\n') (c:rest)
      in TWhite ws : go rest'
    
    go ('"':rest) =
      let (str, rest') = span (/= '"') rest
          rest'' = drop 1 rest'  -- skip closing quote
      in TStrLit str : go rest''
    
    go input@(c:rest) | isDigit c || (c == '-' && not (null rest) && isDigit (head rest)) =
      let (num, rest') = span isDigit (if c == '-' then rest else input)
          numStr = if c == '-' then '-' : num else num
      in TIntLit (read numStr) : go rest'
    
    go input@(c:_) | isAlpha c || c == '_' =
      let (word, rest) = span isIdentChar input
          tok = if word `Set.member` keywords then TKeyword word else TIdent word
      in tok : go rest
    
    go input@(c:rest) | c `elem` "()[]{}," =
      let tok = if c `elem` "([{" then TOpen c else TClose c
      in tok : go rest
    
    go input =
      -- Check multi-char symbols first
      case find (`isPrefixOf` input) symbolList of
        Just sym -> TSymbol sym : go (drop (length sym) input)
        Nothing  ->
          -- Single char symbol
          let (c:rest) = input
          in TSymbol [c] : go rest
    
    isIdentChar c = isAlphaNum c || c == '_' || c == '\''

-- | Tokenize with phi defaults
tokenizeDefault :: String -> [Token]
tokenizeDefault = tokenize defaultLexer
