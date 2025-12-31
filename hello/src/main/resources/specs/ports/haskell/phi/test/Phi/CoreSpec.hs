{-# LANGUAGE LambdaCase #-}
module Phi.CoreSpec (spec) where

import Test.Hspec
import Phi.Core

spec :: Spec
spec = do
  describe "Val (Universal AST)" $ do
    it "creates constructor values" $ do
      let v = VCon "Add" [VInt 1, VInt 2]
      showVal v `shouldBe` "Add(1, 2)"
    
    it "creates string values" $ do
      let v = VStr "hello"
      showVal v `shouldBe` "\"hello\""
    
    it "creates int values" $ do
      let v = VInt 42
      showVal v `shouldBe` "42"
    
    it "creates list values" $ do
      let v = VList [VInt 1, VInt 2, VInt 3]
      showVal v `shouldBe` "[1, 2, 3]"
    
    it "creates nested constructor values" $ do
      let v = VCon "App" [VCon "Lam" [VStr "x", VCon "Var" [VStr "x"]], VInt 5]
      showVal v `shouldBe` "App(Lam(\"x\", Var(\"x\")), 5)"

  describe "Pattern Functor V" $ do
    it "projects Val to V" $ do
      let v = VCon "Test" [VInt 1]
      case vOut v of
        C name args -> do
          name `shouldBe` "Test"
          length args `shouldBe` 1
        _ -> expectationFailure "Expected C"
    
    it "embeds V to Val" $ do
      let v = C "Test" [VInt 1]
      vIn v `shouldBe` VCon "Test" [VInt 1]
    
    it "round-trips through vIn and vOut" $ do
      let original = VCon "Foo" [VInt 42, VStr "bar"]
      vIn (vOut original) `shouldBe` original

  describe "Recursion Schemes" $ do
    describe "cata (catamorphism)" $ do
      it "computes sum of all integers in tree" $ do
        let tree = VCon "Add" [VInt 1, VCon "Add" [VInt 2, VInt 3]]
            sumInts = cata $ \case
              I n -> n
              C _ args -> sum args
              L elems -> sum elems
              S _ -> 0
        sumInts tree `shouldBe` 6

      it "counts nodes in tree" $ do
        let tree = VCon "Node" [VCon "Leaf" [], VCon "Node" [VCon "Leaf" [], VCon "Leaf" []]]
            countNodes = cata $ \case
              C _ children -> 1 + sum children
              L elems -> sum elems
              _ -> 1
        countNodes tree `shouldBe` 5

      it "computes depth of tree" $ do
        let tree = VCon "Node" [VCon "Leaf" [], VCon "Node" [VCon "Leaf" []]]
            depth = cata $ \case
              C _ children -> 1 + maximum (0 : children)
              L elems -> maximum (0 : elems)
              _ -> 1
        depth tree `shouldBe` 3

    describe "para (paramorphism)" $ do
      it "has access to original subtrees" $ do
        let tree = VCon "Node" [VInt 1, VInt 2]
            countWithOriginal = para $ \case
              C _ pairs -> length pairs
              _ -> 0
        countWithOriginal tree `shouldBe` 2

  describe "Validated" $ do
    it "collects errors with invalid" $ do
      let v = invalid "error1" :: Validated String Int
      case v of
        Invalid errs -> errs `shouldBe` ["error1"]
        Valid _ -> expectationFailure "Expected Invalid"

    it "preserves valid values" $ do
      let v = valid 42 :: Validated String Int
      case v of
        Valid x -> x `shouldBe` 42
        Invalid _ -> expectationFailure "Expected Valid"

    it "converts to Either" $ do
      let v1 = valid 42 :: Validated String Int
          v2 = invalid "error" :: Validated String Int
      toEither v1 `shouldBe` Right 42
      case toEither v2 of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure "Expected Left"

    it "zips valid values" $ do
      let v1 = valid 1 :: Validated String Int
          v2 = valid 2 :: Validated String Int
      case zipV v1 v2 of
        Valid (a, b) -> (a, b) `shouldBe` (1, 2)
        Invalid _ -> expectationFailure "Expected Valid pair"

    it "combines errors when zipping invalids" $ do
      let v1 = invalid "e1" :: Validated String Int
          v2 = invalid "e2" :: Validated String Int
      case zipV v1 v2 of
        Invalid errs -> do
          errs `shouldContain` ["e1"]
          errs `shouldContain` ["e2"]
        Valid _ -> expectationFailure "Expected Invalid"

  describe "Free Monad" $ do
    it "creates pure values" $ do
      let p = Pure 42 :: Free V Int
      case p of
        Pure x -> x `shouldBe` 42
        Roll _ -> expectationFailure "Expected Pure"

    it "wraps functor layer with liftF" $ do
      let f = liftF (I 42) :: Free V Int
      case f of
        Roll layer -> case layer of
          I _ -> True `shouldBe` True
          _ -> expectationFailure "Expected I"
        Pure _ -> expectationFailure "Expected Roll"

  describe "Cofree Comonad" $ do
    it "extracts annotation" $ do
      let cf = Cofree 42 (I 0) :: Cofree V Int
      extract cf `shouldBe` 42

    it "forgets annotations" $ do
      let cf = Cofree "ignored" (C "Test" [Cofree "also-ignored" (I 1)])
          result = forget cf
      showVal result `shouldBe` "Test(1)"

  describe "Lens operations" $ do
    it "views through lens" $ do
      let pair = (1, "hello")
          fstLens = lens fst (\a (_, b) -> (a, b))
      view fstLens pair `shouldBe` 1

    it "sets through lens" $ do
      let pair = (1, "hello")
          fstLens = lens fst (\a (_, b) -> (a, b))
      set fstLens 99 pair `shouldBe` (99, "hello")

    it "modifies through lens" $ do
      let pair = (1, "hello")
          fstLens = lens fst (\a (_, b) -> (a, b))
      over fstLens (+1) pair `shouldBe` (2, "hello")

  describe "Prism operations" $ do
    it "previews matching value" $ do
      let intPrism = prism
            (\case I n -> Just n; _ -> Nothing)
            I
          val = I 42 :: V Int
      preview intPrism val `shouldBe` Just 42

    it "returns Nothing for non-matching" $ do
      let intPrism = prism
            (\case I n -> Just n; _ -> Nothing)
            I
          val = S "hello" :: V Int
      preview intPrism val `shouldBe` Nothing

    it "reviews to construct value" $ do
      let intPrism = prism
            (\case I n -> Just n; _ -> Nothing)
            I
      review intPrism 42 `shouldBe` (I 42 :: V Int)

  describe "Xform (bidirectional)" $ do
    it "creates identity xform" $ do
      let x = idX :: Xform Int Int
      xformForward x 42 `shouldBe` Just 42

    it "creates custom xform with Maybe" $ do
      let x = Xform (\n -> Just (n + 1)) (\n -> Just (n - 1)) :: Xform Int Int
      xformForward x 5 `shouldBe` Just 6
      xformBackward x 6 `shouldBe` Just 5

    it "inverts xform" $ do
      let x = Xform (\n -> Just (n + 1)) (\n -> Just (n - 1)) :: Xform Int Int
          inv = invertX x
      xformForward inv 6 `shouldBe` Just 5

  describe "Hash" $ do
    it "hashes values consistently" $ do
      let v = VCon "Test" [VInt 42]
          h1 = hashOf v
          h2 = hashOf v
      h1 `shouldBe` h2

    it "different values have different hashes" $ do
      let v1 = VCon "A" [VInt 1]
          v2 = VCon "B" [VInt 2]
      hashOf v1 `shouldNotBe` hashOf v2
