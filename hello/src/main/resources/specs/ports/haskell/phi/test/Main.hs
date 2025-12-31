module Main where

import Test.Hspec
import qualified Phi.Meta.EvalSpec
import qualified Phi.Meta.MatchSpec
import qualified Phi.Meta.ShowSpec
import qualified Phi.CoreSpec
import qualified Phi.LangSpec
import qualified Phi.SyntaxSpec
import qualified Phi.GrammarSpec
import qualified Phi.ParserSpec

main :: IO ()
main = hspec $ do
  describe "Phi.Meta.Match" Phi.Meta.MatchSpec.spec
  describe "Phi.Meta.Eval" Phi.Meta.EvalSpec.spec
  describe "Phi.Meta.Show" Phi.Meta.ShowSpec.spec
  describe "Phi.Core" Phi.CoreSpec.spec
  describe "Phi.Lang" Phi.LangSpec.spec
  describe "Phi.Syntax" Phi.SyntaxSpec.spec
  describe "Phi.Grammar" Phi.GrammarSpec.spec
  describe "Phi.Parser" Phi.ParserSpec.spec
