module Phi.LangSpec (spec) where

import Test.Hspec
import Phi.Lang

spec :: Spec
spec = do
  describe "LangSpec" $ do
    it "creates empty spec" $ do
      let spec = LangSpec "Test" [] [] [] [] []
      lsName spec `shouldBe` "Test"
      lsSorts spec `shouldBe` []
      lsCons spec `shouldBe` []

    it "stores sort declarations" $ do
      let sorts = [SortDecl "Type" [], SortDecl "Term" []]
          spec = LangSpec "Lang" sorts [] [] [] []
      length (lsSorts spec) `shouldBe` 2
      sdName (head (lsSorts spec)) `shouldBe` "Type"

    it "stores constructor declarations" $ do
      let cons = [ConDecl "Unit" [] "Type", ConDecl "succ" [Field Nothing (TRSort "Nat")] "Nat"]
          spec = LangSpec "Lang" [] cons [] [] []
      length (lsCons spec) `shouldBe` 2
      cdName (head (lsCons spec)) `shouldBe` "Unit"
      cdReturn (head (lsCons spec)) `shouldBe` "Type"

  describe "SortDecl" $ do
    it "creates simple sort" $ do
      let s = SortDecl "Type" []
      sdName s `shouldBe` "Type"
      sdParams s `shouldBe` []

    it "creates parameterized sort" $ do
      let s = SortDecl "List" ["A"]
      sdName s `shouldBe` "List"
      sdParams s `shouldBe` ["A"]

  describe "ConDecl" $ do
    it "creates nullary constructor" $ do
      let c = ConDecl "Unit" [] "Type"
      cdName c `shouldBe` "Unit"
      cdFields c `shouldBe` []
      cdReturn c `shouldBe` "Type"

    it "creates constructor with fields" $ do
      let fields = [Field (Just "n") (TRSort "Nat"), Field Nothing (TRSort "Term")]
          c = ConDecl "App" fields "Term"
      cdName c `shouldBe` "App"
      length (cdFields c) `shouldBe` 2
      fName (head (cdFields c)) `shouldBe` Just "n"

  describe "TypeRef" $ do
    it "represents sort reference" $ do
      let t = TRSort "Term"
      case t of
        TRSort name -> name `shouldBe` "Term"
        _ -> expectationFailure "Expected TRSort"

    it "represents arrow type" $ do
      let t = TRArrow (TRSort "A") (TRSort "B")
      case t of
        TRArrow from to -> do
          case from of
            TRSort n -> n `shouldBe` "A"
            _ -> expectationFailure "Expected TRSort"
          case to of
            TRSort n -> n `shouldBe` "B"
            _ -> expectationFailure "Expected TRSort"
        _ -> expectationFailure "Expected TRArrow"

  describe "GrammarDecl" $ do
    it "stores grammar name" $ do
      let g = GrammarDecl "term" []
      gdName g `shouldBe` "term"

    it "stores productions" $ do
      let prod = Production (GPLiteral "x") "Var"
          g = GrammarDecl "term" [prod]
      length (gdProductions g) `shouldBe` 1
      prodConstructor (head (gdProductions g)) `shouldBe` "Var"

  describe "GrammarPat" $ do
    it "creates literal pattern" $ do
      let p = GPLiteral "λ"
      case p of
        GPLiteral s -> s `shouldBe` "λ"
        _ -> expectationFailure "Expected GPLiteral"

    it "creates non-terminal pattern" $ do
      let p = GPNonTerminal "term"
      case p of
        GPNonTerminal s -> s `shouldBe` "term"
        _ -> expectationFailure "Expected GPNonTerminal"

    it "creates sequence pattern" $ do
      let p = GPSeq [GPLiteral "λ", GPNonTerminal "IDENT"]
      case p of
        GPSeq parts -> length parts `shouldBe` 2
        _ -> expectationFailure "Expected GPSeq"

    it "creates alternative pattern" $ do
      let p = GPAlt [GPLiteral "a", GPLiteral "b"]
      case p of
        GPAlt opts -> length opts `shouldBe` 2
        _ -> expectationFailure "Expected GPAlt"

    it "creates repetition patterns" $ do
      let rep = GPRep (GPNonTerminal "term")
          rep1 = GPRep1 (GPNonTerminal "term")
      case rep of
        GPRep _ -> return ()
        _ -> expectationFailure "Expected GPRep"
      case rep1 of
        GPRep1 _ -> return ()
        _ -> expectationFailure "Expected GPRep1"

  describe "XformDecl" $ do
    it "stores xform name and signature" $ do
      let x = XformDecl "Eval" (TRSort "Term") (TRSort "Val") []
      xdName x `shouldBe` "Eval"

    it "stores xform cases" $ do
      let c = XformCase (XPVar "x") (XBVar "x")
          x = XformDecl "Id" (TRSort "A") (TRSort "A") [c]
      length (xdCases x) `shouldBe` 1

  describe "XformPat" $ do
    it "creates variable pattern" $ do
      let p = XPVar "x"
      case p of
        XPVar n -> n `shouldBe` "x"
        _ -> expectationFailure "Expected XPVar"

    it "creates constructor pattern" $ do
      let p = XPCon "Lam" [XPVar "x", XPVar "body"]
      case p of
        XPCon name args -> do
          name `shouldBe` "Lam"
          length args `shouldBe` 2
        _ -> expectationFailure "Expected XPCon"

    it "creates wildcard pattern" $ do
      let p = XPWild
      case p of
        XPWild -> return ()
        _ -> expectationFailure "Expected XPWild"

  describe "XformBody" $ do
    it "creates variable reference" $ do
      let b = XBVar "x"
      case b of
        XBVar n -> n `shouldBe` "x"
        _ -> expectationFailure "Expected XBVar"

    it "creates constructor application" $ do
      let b = XBCon "Pair" [XBVar "a", XBVar "b"]
      case b of
        XBCon name args -> do
          name `shouldBe` "Pair"
          length args `shouldBe` 2
        _ -> expectationFailure "Expected XBCon"

    it "creates let binding" $ do
      let b = XBLet "x" (XBVar "y") (XBVar "x")
      case b of
        XBLet name val body -> do
          name `shouldBe` "x"
        _ -> expectationFailure "Expected XBLet"

  describe "Lookup functions" $ do
    let spec = LangSpec "Test" 
          [SortDecl "Type" [], SortDecl "Term" []]
          [ConDecl "Unit" [] "Type", ConDecl "Var" [Field Nothing (TRSort "String")] "Term"]
          [XformDecl "Eval" (TRSort "Term") (TRSort "Val") []]
          []
          [GrammarDecl "term" []]

    it "finds constructor by name" $ do
      case consFor spec "Unit" of
        Just c -> cdName c `shouldBe` "Unit"
        Nothing -> expectationFailure "Expected to find Unit"

    it "returns Nothing for unknown constructor" $ do
      consFor spec "Unknown" `shouldBe` Nothing

    it "finds grammar by name" $ do
      case grammarFor spec "term" of
        Just g -> gdName g `shouldBe` "term"
        Nothing -> expectationFailure "Expected to find term grammar"

    it "returns Nothing for unknown grammar" $ do
      grammarFor spec "unknown" `shouldBe` Nothing

    it "finds xform by name" $ do
      case xformNamed spec "Eval" of
        Just x -> xdName x `shouldBe` "Eval"
        Nothing -> expectationFailure "Expected to find Eval"

  describe "showSpec" $ do
    it "renders simple spec" $ do
      let spec = LangSpec "Test" [SortDecl "Type" []] [] [] [] []
          rendered = showSpec spec
      rendered `shouldContain` "language Test"
      rendered `shouldContain` "sort Type"
