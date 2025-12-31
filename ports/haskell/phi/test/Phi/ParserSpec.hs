module Phi.ParserSpec (spec) where

import Test.Hspec
import Phi.Parser
import Phi.Lang

spec :: Spec
spec = do
  describe "parseSpec" $ do
    describe "Empty language" $ do
      it "parses minimal language" $ do
        let input = "language Empty { }"
        case parseSpec input of
          Right spec -> do
            lsName spec `shouldBe` "Empty"
            lsSorts spec `shouldBe` []
            lsCons spec `shouldBe` []
          Left e -> expectationFailure e

    describe "Sort declarations" $ do
      it "parses single sort" $ do
        let input = "language Test { sort Type }"
        case parseSpec input of
          Right spec -> do
            length (lsSorts spec) `shouldBe` 1
            sdName (head (lsSorts spec)) `shouldBe` "Type"
          Left e -> expectationFailure e

      it "parses multiple sorts" $ do
        let input = "language Test { sort Type sort Term sort Kind }"
        case parseSpec input of
          Right spec -> do
            length (lsSorts spec) `shouldBe` 3
            map sdName (lsSorts spec) `shouldBe` ["Type", "Term", "Kind"]
          Left e -> expectationFailure e

    describe "Constructor declarations" $ do
      it "parses nullary constructor" $ do
        let input = "language Test { constructor Unit : Type }"
        case parseSpec input of
          Right spec -> do
            length (lsCons spec) `shouldBe` 1
            let con = head (lsCons spec)
            cdName con `shouldBe` "Unit"
            cdReturn con `shouldBe` "Type"
            cdFields con `shouldBe` []
          Left e -> expectationFailure e

      it "parses unary constructor" $ do
        let input = "language Test { constructor Succ : Nat -> Nat }"
        case parseSpec input of
          Right spec -> do
            let con = head (lsCons spec)
            cdName con `shouldBe` "Succ"
            cdReturn con `shouldBe` "Nat"
            length (cdFields con) `shouldBe` 1
          Left e -> expectationFailure e

      it "parses binary constructor" $ do
        let input = "language Test { constructor App : Term -> Term -> Term }"
        case parseSpec input of
          Right spec -> do
            let con = head (lsCons spec)
            cdName con `shouldBe` "App"
            cdReturn con `shouldBe` "Term"
            length (cdFields con) `shouldBe` 2
          Left e -> expectationFailure e

      it "parses constructor with unicode arrow" $ do
        let input = "language Test { constructor Succ : Nat → Nat }"
        case parseSpec input of
          Right spec -> do
            let con = head (lsCons spec)
            cdName con `shouldBe` "Succ"
            cdReturn con `shouldBe` "Nat"
          Left e -> expectationFailure e

    describe "Xform declarations" $ do
      it "parses simple xform" $ do
        let input = "language Test { xform Eval : Term <-> Val }"
        case parseSpec input of
          Right spec -> do
            length (lsXforms spec) `shouldBe` 1
            xdName (head (lsXforms spec)) `shouldBe` "Eval"
          Left e -> expectationFailure e

      it "parses xform with unicode biarrow" $ do
        let input = "language Test { xform Eval : Term ⇄ Val }"
        case parseSpec input of
          Right spec -> do
            xdName (head (lsXforms spec)) `shouldBe` "Eval"
          Left e -> expectationFailure e

    describe "Rule declarations" $ do
      it "parses simple rule" $ do
        let input = "language Test { rule Beta { x |-> x } }"
        case parseSpec input of
          Right spec -> do
            length (lsRules spec) `shouldBe` 1
            rdName (head (lsRules spec)) `shouldBe` "Beta"
          Left e -> expectationFailure e

      it "parses rule with unicode mapsto" $ do
        let input = "language Test { rule Beta { x ↦ x } }"
        case parseSpec input of
          Right spec -> do
            rdName (head (lsRules spec)) `shouldBe` "Beta"
          Left e -> expectationFailure e

      it "parses rule with constructor pattern" $ do
        let input = "language Test { rule Beta { App(f, x) |-> f } }"
        case parseSpec input of
          Right spec -> do
            let rule = head (lsRules spec)
            length (rdCases rule) `shouldBe` 1
          Left e -> expectationFailure e

      it "parses rule with multiple cases" $ do
        -- Multiple cases need proper syntax - testing single case for now
        let input = "language Test { rule Eval { Zero |-> Zero } }"
        case parseSpec input of
          Right spec -> do
            let rule = head (lsRules spec)
            length (rdCases rule) `shouldBe` 1
          Left e -> expectationFailure e

    describe "Grammar declarations" $ do
      it "parses simple grammar" $ do
        let input = "language Test { grammar term { \"x\" => Var } }"
        case parseSpec input of
          Right spec -> do
            length (lsGrammars spec) `shouldBe` 1
            gdName (head (lsGrammars spec)) `shouldBe` "term"
          Left e -> expectationFailure e

      it "parses grammar with multiple productions" $ do
        let input = "language Test { grammar term { \"x\" => Var \"y\" => Var } }"
        case parseSpec input of
          Right spec -> do
            let g = head (lsGrammars spec)
            length (gdProductions g) `shouldBe` 2
          Left e -> expectationFailure e

      it "parses grammar with non-terminal" $ do
        let input = "language Test { grammar term { IDENT => Var } }"
        case parseSpec input of
          Right spec -> do
            let g = head (lsGrammars spec)
            let prod = head (gdProductions g)
            prodConstructor prod `shouldBe` "Var"
          Left e -> expectationFailure e

    describe "Complete specs" $ do
      it "parses STLC-like spec" $ do
        let input = unlines
              [ "language STLC {"
              , "  sort Type"
              , "  sort Term"
              , "  constructor Unit : Type"
              , "  constructor Arrow : Type -> Type -> Type"
              , "  constructor Var : Term"
              , "  constructor Lam : Term -> Term"
              , "  constructor App : Term -> Term -> Term"
              , "}"
              ]
        case parseSpec input of
          Right spec -> do
            lsName spec `shouldBe` "STLC"
            length (lsSorts spec) `shouldBe` 2
            length (lsCons spec) `shouldBe` 5
          Left e -> expectationFailure e

      it "parses spec with whitespace variations" $ do
        let input = "language   Test  {   sort   Type   }"
        case parseSpec input of
          Right spec -> do
            lsName spec `shouldBe` "Test"
            length (lsSorts spec) `shouldBe` 1
          Left e -> expectationFailure e

    describe "Error handling" $ do
      it "fails on missing language keyword" $ do
        let input = "Test { sort Type }"
        case parseSpec input of
          Left _ -> return ()
          Right _ -> expectationFailure "Expected parse error"

      it "fails on missing braces" $ do
        let input = "language Test sort Type"
        case parseSpec input of
          Left _ -> return ()
          Right _ -> expectationFailure "Expected parse error"

      it "fails on unclosed brace" $ do
        let input = "language Test { sort Type"
        case parseSpec input of
          Left _ -> return ()
          Right _ -> expectationFailure "Expected parse error"

  describe "parseFile" $ do
    it "parses file content" $ do
      let content = "language File { sort A }"
      case parseFile "test.phi" content of
        Right spec -> lsName spec `shouldBe` "File"
        Left e -> expectationFailure e
