module Phi.Meta.MatchSpec (spec) where

import Test.Hspec
import Phi.Meta.Val
import Phi.Meta.Env as Env
import Phi.Meta.Pat
import Phi.Meta.Result
import Phi.Meta.Match

spec :: Spec
spec = do
  describe "matchWith" $ do
    it "PVar matches any value and binds it" $ do
      let result = matchWith (PVar "x") (VInt 42) Env.empty
      case result of
        ROk _ env -> Env.lookup "x" env `shouldBe` Just (VInt 42)
        RFail -> expectationFailure "Expected match to succeed"

    it "PWild matches any value without binding" $ do
      let result = matchWith PWild (VInt 42) Env.empty
      case result of
        ROk _ env -> env `shouldBe` []
        RFail -> expectationFailure "Expected match to succeed"

    it "PCon matches constructor with correct name" $ do
      let result = matchWith (PCon "Just" [PVar "x"]) (VCon "Just" [VInt 1]) Env.empty
      isOk result `shouldBe` True

    it "PCon fails on constructor name mismatch" $ do
      let result = matchWith (PCon "Just" [PVar "x"]) (VCon "Nothing" []) Env.empty
      isFail result `shouldBe` True

    it "nested patterns work correctly" $ do
      let pat = PCon "Pair" [PVar "a", PCon "Just" [PVar "b"]]
      let val = VCon "Pair" [VInt 1, VCon "Just" [VInt 2]]
      let result = matchWith pat val Env.empty
      case result of
        ROk _ env -> do
          Env.lookup "a" env `shouldBe` Just (VInt 1)
          Env.lookup "b" env `shouldBe` Just (VInt 2)
        RFail -> expectationFailure "Expected match to succeed"

    it "PList matches list with correct length" $ do
      let result = matchWith (PList [PVar "x", PVar "y"]) (VList [VInt 1, VInt 2]) Env.empty
      case result of
        ROk _ env -> do
          Env.lookup "x" env `shouldBe` Just (VInt 1)
          Env.lookup "y" env `shouldBe` Just (VInt 2)
        RFail -> expectationFailure "Expected match to succeed"

    it "PAs binds entire match" $ do
      let result = matchWith (PAs "all" (PCon "Just" [PVar "x"])) (VCon "Just" [VInt 42]) Env.empty
      case result of
        ROk _ env -> do
          Env.lookup "x" env `shouldBe` Just (VInt 42)
          Env.lookup "all" env `shouldBe` Just (VCon "Just" [VInt 42])
        RFail -> expectationFailure "Expected match to succeed"
