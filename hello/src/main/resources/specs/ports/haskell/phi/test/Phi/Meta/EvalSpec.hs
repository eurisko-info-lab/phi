module Phi.Meta.EvalSpec (spec) where

import Test.Hspec
import Phi.Meta.Val
import Phi.Meta.Env as Env
import Phi.Meta.Pat
import Phi.Meta.Expr
import Phi.Meta.Eval

spec :: Spec
spec = do
  describe "eval" $ do
    it "ELit evaluates to VInt" $ do
      eval (ELit 42) Env.empty `shouldBe` VInt 42

    it "EStr evaluates to VStr" $ do
      eval (EStr "hello") Env.empty `shouldBe` VStr "hello"

    it "EVar looks up in environment" $ do
      let env = Env.bind "x" (VInt 42) Env.empty
      eval (EVar "x") env `shouldBe` VInt 42

    it "ECon constructs value" $ do
      eval (ECon "Just" [ELit 1]) Env.empty `shouldBe` VCon "Just" [VInt 1]

    it "EList constructs list" $ do
      eval (EList [ELit 1, ELit 2]) Env.empty `shouldBe` VList [VInt 1, VInt 2]

    it "ELet binds value in body" $ do
      let expr = ELet "x" (ELit 42) (EVar "x")
      eval expr Env.empty `shouldBe` VInt 42

    it "ELet allows nested bindings" $ do
      let expr = ELet "x" (ELit 1) (ELet "y" (ELit 2) (ECon "Pair" [EVar "x", EVar "y"]))
      eval expr Env.empty `shouldBe` VCon "Pair" [VInt 1, VInt 2]

    it "EMatch selects correct case" $ do
      let expr = EMatch (ECon "Just" [ELit 42])
                   [ ECase (PCon "Nothing" []) (ELit 0)
                   , ECase (PCon "Just" [PVar "x"]) (EVar "x")
                   ]
      eval expr Env.empty `shouldBe` VInt 42

    it "EMatch tries cases in order" $ do
      let expr = EMatch (ELit 1)
                   [ ECase (PVar "x") (ECon "First" [EVar "x"])
                   , ECase (PVar "y") (ECon "Second" [EVar "y"])
                   ]
      eval expr Env.empty `shouldBe` VCon "First" [VInt 1]

    it "EIf evaluates correct branch" $ do
      let exprTrue = EIf (ECon "True" []) (ELit 1) (ELit 2)
      let exprFalse = EIf (ECon "False" []) (ELit 1) (ELit 2)
      eval exprTrue Env.empty `shouldBe` VInt 1
      eval exprFalse Env.empty `shouldBe` VInt 2

    it "ELam creates closure" $ do
      let expr = ELam "x" (EVar "x")
      case eval expr Env.empty of
        VClosure p _ _ -> p `shouldBe` "x"
        _ -> expectationFailure "Expected closure"

    it "EApp applies lambda" $ do
      let idFn = ELam "x" (EVar "x")
      let expr = EApp idFn (ELit 42)
      eval expr Env.empty `shouldBe` VInt 42

    it "closure captures environment" $ do
      let expr = ELet "y" (ELit 10) 
                   (EApp (ELam "x" (ECon "Pair" [EVar "x", EVar "y"])) (ELit 1))
      eval expr Env.empty `shouldBe` VCon "Pair" [VInt 1, VInt 10]
