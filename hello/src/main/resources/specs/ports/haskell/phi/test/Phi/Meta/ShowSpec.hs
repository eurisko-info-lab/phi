module Phi.Meta.ShowSpec (spec) where

import Test.Hspec
import Phi.Meta.Val
import Phi.Meta.Expr
import Phi.Meta.Show

spec :: Spec
spec = do
  describe "render" $ do
    it "renders VInt" $ do
      render (VInt 42) `shouldBe` "42"

    it "renders VStr" $ do
      render (VStr "hello") `shouldBe` "\"hello\""

    it "renders VCon without args" $ do
      render (VCon "Nothing" []) `shouldBe` "Nothing"

    it "renders VCon with args" $ do
      render (VCon "Just" [VInt 1]) `shouldBe` "Just(1)"

    it "renders VList" $ do
      render (VList [VInt 1, VInt 2]) `shouldBe` "[1, 2]"

    it "renders nested value" $ do
      let val = VCon "Pair" [VCon "Just" [VInt 1], VCon "Nothing" []]
      render val `shouldBe` "Pair(Just(1), Nothing)"

  describe "renderExpr" $ do
    it "renders EVar" $ do
      renderExpr (EVar "x") `shouldBe` "x"

    it "renders ELit" $ do
      renderExpr (ELit 42) `shouldBe` "42"

    it "renders ELam" $ do
      renderExpr (ELam "x" (EVar "x")) `shouldBe` "λx. x"

    it "renders EApp" $ do
      renderExpr (EApp (EVar "f") (EVar "x")) `shouldBe` "(f x)"

    it "renders ECon" $ do
      renderExpr (ECon "Just" [ELit 1]) `shouldBe` "Just(1)"

    it "renders ELet" $ do
      renderExpr (ELet "x" (ELit 1) (EVar "x")) `shouldBe` "let x = 1 in x"
