module Phi.GrammarSpec (spec) where

import Test.Hspec
import Phi.Grammar
import Phi.Lang
import Phi.Meta.Val (Val(..))

spec :: Spec
spec = do
  describe "GrammarInterp" $ do
    let simpleGrammar = GrammarDecl "expr" 
          [ Production (GPNonTerminal "IDENT") "Var"
          , Production (GPSeq [GPLiteral "(", GPNonTerminal "expr", GPLiteral ")"]) "Paren"
          ]
        simpleSpec = LangSpec "Test" [] [] [] [] [simpleGrammar]
        gi = mkGrammarInterp simpleSpec

    describe "parseSortG" $ do
      it "parses identifier as Var" $ do
        case parseSortG gi "expr" "foo" of
          Right (VCon "Var" [VStr "foo"]) -> return ()
          Right v -> expectationFailure $ "Unexpected: " ++ show v
          Left e -> expectationFailure e

      it "parses parenthesized expression" $ do
        case parseSortG gi "expr" "(bar)" of
          Right (VCon "Paren" [VCon "Var" [VStr "bar"]]) -> return ()
          Right v -> expectationFailure $ "Unexpected: " ++ show v
          Left e -> expectationFailure e

    describe "parseAllG" $ do
      it "succeeds when all input consumed" $ do
        case parseAllG gi "expr" "foo" of
          Right _ -> return ()
          Left e -> expectationFailure e

      it "fails when input remains" $ do
        case parseAllG gi "expr" "foo bar" of
          Left _ -> return ()
          Right _ -> expectationFailure "Expected failure for remaining input"

  describe "GrammarRenderer" $ do
    let simpleGrammar = GrammarDecl "expr"
          [ Production (GPNonTerminal "IDENT") "Var"
          , Production (GPSeq [GPLiteral "(", GPNonTerminal "expr", GPLiteral ")"]) "Paren"
          ]
        simpleSpec = LangSpec "Test" [] [] [] [] [simpleGrammar]
        gr = mkGrammarRenderer simpleSpec

    describe "renderG" $ do
      it "renders string value" $ do
        renderG gr (VStr "hello") `shouldBe` "hello"

      it "renders int value" $ do
        renderG gr (VInt 42) `shouldBe` "42"

      it "renders list value" $ do
        renderG gr (VList [VInt 1, VInt 2]) `shouldBe` "[1, 2]"

      it "renders constructor without grammar" $ do
        renderG gr (VCon "Unknown" [VStr "x"]) `shouldBe` "Unknown(x)"

      it "renders Var using grammar" $ do
        renderG gr (VCon "Var" [VStr "foo"]) `shouldBe` "foo"

  describe "XformInterp" $ do
    let idXform = XformDecl "Id" (TRSort "A") (TRSort "A")
          [ XformCase (XPVar "x") (XBVar "x") ]
        wrapXform = XformDecl "Wrap" (TRSort "A") (TRSort "Box")
          [ XformCase (XPVar "x") (XBCon "Box" [XBVar "x"]) ]
        testSpec = LangSpec "Test" [] [] [idXform, wrapXform] [] []
        xi = mkXformInterp testSpec

    describe "applyXform" $ do
      it "applies identity transform" $ do
        case applyXform xi "Id" (VStr "hello") of
          Right (VStr "hello") -> return ()
          Right v -> expectationFailure $ "Unexpected: " ++ show v
          Left e -> expectationFailure e

      it "applies wrapping transform" $ do
        case applyXform xi "Wrap" (VInt 42) of
          Right (VCon "Box" [VInt 42]) -> return ()
          Right v -> expectationFailure $ "Unexpected: " ++ show v
          Left e -> expectationFailure e

      it "fails for unknown xform" $ do
        case applyXform xi "Unknown" (VStr "x") of
          Left _ -> return ()
          Right _ -> expectationFailure "Expected failure"

    describe "applyXformRecursive" $ do
      it "transforms nested structure" $ do
        let tree = VCon "Pair" [VInt 1, VInt 2]
        case applyXformRecursive xi "Wrap" tree of
          Right (VCon "Box" [VCon "Pair" [VCon "Box" [VInt 1], VCon "Box" [VInt 2]]]) -> return ()
          Right v -> expectationFailure $ "Unexpected: " ++ show v
          Left e -> expectationFailure e

  describe "XformPat matching" $ do
    let constPat = XformDecl "GetFirst" (TRSort "Pair") (TRSort "A")
          [ XformCase (XPCon "Pair" [XPVar "a", XPWild]) (XBVar "a") ]
        testSpec = LangSpec "Test" [] [] [constPat] [] []
        xi = mkXformInterp testSpec

    it "matches constructor pattern" $ do
      let pair = VCon "Pair" [VStr "first", VStr "second"]
      case applyXform xi "GetFirst" pair of
        Right (VStr "first") -> return ()
        Right v -> expectationFailure $ "Unexpected: " ++ show v
        Left e -> expectationFailure e

    it "wildcard matches anything" $ do
      let pair = VCon "Pair" [VInt 1, VCon "Complex" [VStr "ignored"]]
      case applyXform xi "GetFirst" pair of
        Right (VInt 1) -> return ()
        Right v -> expectationFailure $ "Unexpected: " ++ show v
        Left e -> expectationFailure e

  describe "StrategyInterp" $ do
    let succXform = XformDecl "AddOne" (TRSort "Nat") (TRSort "Nat")
          [ XformCase (XPCon "Zero" []) (XBCon "Succ" [XBCon "Zero" []])
          , XformCase (XPCon "Succ" [XPVar "n"]) (XBCon "Succ" [XBCon "Succ" [XBVar "n"]])
          ]
        testSpec = LangSpec "Test" [] [] [succXform] [] []
        si = mkStrategyInterp testSpec

    describe "step" $ do
      it "applies single step" $ do
        let zero = VCon "Zero" []
        case step si "AddOne" zero of
          Right (VCon "Succ" [VCon "Zero" []]) -> return ()
          Right v -> expectationFailure $ "Unexpected: " ++ show v
          Left e -> expectationFailure e

    describe "normalize" $ do
      it "reaches fixed point when no rule matches" $ do
        let val = VStr "unchanged"
        case normalize si "AddOne" val 100 of
          Right (VStr "unchanged") -> return ()
          Right v -> expectationFailure $ "Unexpected: " ++ show v
          Left e -> expectationFailure e

  describe "LangRunner" $ do
    let grammar = GrammarDecl "nat"
          [ Production (GPLiteral "z") "Zero"
          , Production (GPSeq [GPLiteral "s", GPNonTerminal "nat"]) "Succ"
          ]
        xform = XformDecl "Double" (TRSort "Nat") (TRSort "Nat")
          [ XformCase (XPCon "Zero" []) (XBCon "Zero" [])
          , XformCase (XPCon "Succ" [XPVar "n"]) (XBCon "Succ" [XBCon "Succ" [XBVar "n"]])
          ]
        testSpec = LangSpec "Test" [] [] [xform] [] [grammar]
        runner = mkLangRunner testSpec

    describe "parseL" $ do
      it "parses zero" $ do
        case parseL runner "nat" "z" of
          Right (VCon "Zero" []) -> return ()
          Right v -> expectationFailure $ "Unexpected: " ++ show v
          Left e -> expectationFailure e

      it "parses successor" $ do
        case parseL runner "nat" "s z" of
          Right (VCon "Succ" [VCon "Zero" []]) -> return ()
          Right v -> expectationFailure $ "Unexpected: " ++ show v
          Left e -> expectationFailure e

    describe "renderL" $ do
      it "renders zero" $ do
        renderL runner (VCon "Zero" []) `shouldBe` "z"

      it "renders successor" $ do
        renderL runner (VCon "Succ" [VCon "Zero" []]) `shouldBe` "s z"

    describe "transformL" $ do
      it "applies double transform" $ do
        let one = VCon "Succ" [VCon "Zero" []]
        case transformL runner "Double" one of
          Right (VCon "Succ" [VCon "Succ" [VCon "Zero" []]]) -> return ()
          Right v -> expectationFailure $ "Unexpected: " ++ show v
          Left e -> expectationFailure e

    describe "pipelineL" $ do
      it "parses and renders round-trip" $ do
        case pipelineL runner "nat" "s s z" Nothing Nothing of
          Right rendered -> rendered `shouldBe` "s s z"
          Left e -> expectationFailure e

  describe "Convenience functions" $ do
    let grammar = GrammarDecl "bool"
          [ Production (GPLiteral "true") "True"
          , Production (GPLiteral "false") "False"
          ]
        testSpec = LangSpec "Test" [] [] [] [] [grammar]

    describe "parse" $ do
      it "parses with convenience function" $ do
        case parse testSpec "bool" "true" of
          Right (VCon "True" []) -> return ()
          Right v -> expectationFailure $ "Unexpected: " ++ show v
          Left e -> expectationFailure e

    describe "render" $ do
      it "renders with convenience function" $ do
        render testSpec (VCon "True" []) `shouldBe` "true"
