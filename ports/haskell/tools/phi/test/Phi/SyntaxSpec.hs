module Phi.SyntaxSpec (spec) where

import Test.Hspec
import Phi.Syntax
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "Token types" $ do
    it "creates identifier token" $ do
      let t = TIdent "foo"
      case t of
        TIdent s -> s `shouldBe` "foo"
        _ -> expectationFailure "Expected TIdent"

    it "creates keyword token" $ do
      let t = TKeyword "sort"
      case t of
        TKeyword s -> s `shouldBe` "sort"
        _ -> expectationFailure "Expected TKeyword"

    it "creates symbol token" $ do
      let t = TSymbol "→"
      case t of
        TSymbol s -> s `shouldBe` "→"
        _ -> expectationFailure "Expected TSymbol"

    it "creates integer literal token" $ do
      let t = TIntLit 42
      case t of
        TIntLit n -> n `shouldBe` 42
        _ -> expectationFailure "Expected TIntLit"

    it "creates string literal token" $ do
      let t = TStrLit "hello"
      case t of
        TStrLit s -> s `shouldBe` "hello"
        _ -> expectationFailure "Expected TStrLit"

    it "creates bracket tokens" $ do
      let open = TOpen '('
          close = TClose ')'
      case open of
        TOpen c -> c `shouldBe` '('
        _ -> expectationFailure "Expected TOpen"
      case close of
        TClose c -> c `shouldBe` ')'
        _ -> expectationFailure "Expected TClose"

    it "creates whitespace tokens" $ do
      let ws = TWhite "  "
          nl = TNewline
          indent = TIndent 2
      case ws of
        TWhite s -> s `shouldBe` "  "
        _ -> expectationFailure "Expected TWhite"
      case nl of
        TNewline -> return ()
        _ -> expectationFailure "Expected TNewline"
      case indent of
        TIndent n -> n `shouldBe` 2
        _ -> expectationFailure "Expected TIndent"

    it "creates error token" $ do
      let t = TError "unexpected character"
      case t of
        TError msg -> msg `shouldContain` "unexpected"
        _ -> expectationFailure "Expected TError"

  describe "renderToken" $ do
    it "renders identifier" $ do
      renderToken (TIdent "foo") `shouldBe` "foo"

    it "renders keyword" $ do
      renderToken (TKeyword "sort") `shouldBe` "sort"

    it "renders symbol" $ do
      renderToken (TSymbol "→") `shouldBe` "→"

    it "renders integer literal" $ do
      renderToken (TIntLit 42) `shouldBe` "42"

    it "renders string literal with quotes" $ do
      renderToken (TStrLit "hello") `shouldBe` "\"hello\""

    it "renders brackets" $ do
      renderToken (TOpen '(') `shouldBe` "("
      renderToken (TClose ')') `shouldBe` ")"
      renderToken (TOpen '{') `shouldBe` "{"
      renderToken (TClose '}') `shouldBe` "}"

    it "renders whitespace" $ do
      renderToken (TWhite " ") `shouldBe` " "
      renderToken TNewline `shouldBe` "\n"

  describe "Lexer configuration" $ do
    it "default lexer has phi keywords" $ do
      Set.member "language" (lexKeywords defaultLexer) `shouldBe` True
      Set.member "sort" (lexKeywords defaultLexer) `shouldBe` True
      Set.member "constructor" (lexKeywords defaultLexer) `shouldBe` True
      Set.member "xform" (lexKeywords defaultLexer) `shouldBe` True
      Set.member "rule" (lexKeywords defaultLexer) `shouldBe` True
      Set.member "grammar" (lexKeywords defaultLexer) `shouldBe` True

    it "default lexer has phi symbols" $ do
      Set.member "->" (lexSymbols defaultLexer) `shouldBe` True
      Set.member "=>" (lexSymbols defaultLexer) `shouldBe` True
      Set.member "↦" (lexSymbols defaultLexer) `shouldBe` True
      Set.member "→" (lexSymbols defaultLexer) `shouldBe` True

  describe "tokenize" $ do
    it "tokenizes simple identifier" $ do
      let tokens = tokenize defaultLexer "foo"
          nonWs = filter (not . isWhitespace) tokens
      length nonWs `shouldBe` 1
      case head nonWs of
        TIdent s -> s `shouldBe` "foo"
        _ -> expectationFailure "Expected TIdent"

    it "tokenizes keyword" $ do
      let tokens = tokenize defaultLexer "sort"
          nonWs = filter (not . isWhitespace) tokens
      length nonWs `shouldBe` 1
      case head nonWs of
        TKeyword s -> s `shouldBe` "sort"
        _ -> expectationFailure "Expected TKeyword"

    it "tokenizes integer" $ do
      let tokens = tokenize defaultLexer "42"
          nonWs = filter (not . isWhitespace) tokens
      length nonWs `shouldBe` 1
      case head nonWs of
        TIntLit n -> n `shouldBe` 42
        _ -> expectationFailure "Expected TIntLit"

    it "tokenizes negative integer" $ do
      let tokens = tokenize defaultLexer "-123"
          nonWs = filter (not . isWhitespace) tokens
      case nonWs of
        [TIntLit n] -> n `shouldBe` (-123)
        [TSymbol "-", TIntLit n] -> n `shouldBe` 123  -- Also valid interpretation
        _ -> expectationFailure "Expected integer literal"

    it "tokenizes string literal" $ do
      let tokens = tokenize defaultLexer "\"hello\""
          nonWs = filter (not . isWhitespace) tokens
      length nonWs `shouldBe` 1
      case head nonWs of
        TStrLit s -> s `shouldBe` "hello"
        _ -> expectationFailure "Expected TStrLit"

    it "tokenizes brackets" $ do
      let tokens = tokenize defaultLexer "(x)"
          nonWs = filter (not . isWhitespace) tokens
      length nonWs `shouldBe` 3
      case nonWs of
        [TOpen '(', TIdent "x", TClose ')'] -> return ()
        _ -> expectationFailure "Expected ( x )"

    it "tokenizes arrow symbol" $ do
      let tokens = tokenize defaultLexer "A → B"
          nonWs = filter (not . isWhitespace) tokens
      length nonWs `shouldBe` 3
      case nonWs of
        [TIdent "A", TSymbol "→", TIdent "B"] -> return ()
        _ -> expectationFailure "Expected A → B"

    it "tokenizes ASCII arrow" $ do
      let tokens = tokenize defaultLexer "A -> B"
          nonWs = filter (not . isWhitespace) tokens
      case nonWs of
        [TIdent "A", TSymbol "->", TIdent "B"] -> return ()
        _ -> expectationFailure "Expected A -> B"

    it "tokenizes sort declaration" $ do
      let tokens = tokenize defaultLexer "sort Type"
          nonWs = filter (not . isWhitespace) tokens
      case nonWs of
        [TKeyword "sort", TIdent "Type"] -> return ()
        _ -> expectationFailure "Expected sort Type"

    it "tokenizes constructor declaration" $ do
      let tokens = tokenize defaultLexer "constructor Unit : Type"
          nonWs = filter (not . isWhitespace) tokens
      case nonWs of
        [TKeyword "constructor", TIdent "Unit", TSymbol ":", TIdent "Type"] -> return ()
        _ -> expectationFailure "Expected constructor Unit : Type"

    it "handles empty input" $ do
      let tokens = tokenize defaultLexer ""
      tokens `shouldBe` []

    it "handles whitespace only" $ do
      let tokens = tokenize defaultLexer "   \n\t  "
          nonWs = filter (not . isWhitespace) tokens
      nonWs `shouldBe` []

    it "preserves newlines" $ do
      let tokens = tokenize defaultLexer "a\nb"
      tokens `shouldSatisfy` any (== TNewline)

    it "tokenizes complex expression" $ do
      let tokens = tokenize defaultLexer "language Phi { sort Term }"
          nonWs = filter (not . isWhitespace) tokens
      length nonWs `shouldBe` 6
      case nonWs of
        [TKeyword "language", TIdent "Phi", TOpen '{', TKeyword "sort", TIdent "Term", TClose '}'] -> return ()
        _ -> expectationFailure "Expected language Phi { sort Term }"

-- Helper to check if token is whitespace
isWhitespace :: Token -> Bool
isWhitespace (TWhite _) = True
isWhitespace TNewline = True
isWhitespace (TIndent _) = True
isWhitespace _ = False
