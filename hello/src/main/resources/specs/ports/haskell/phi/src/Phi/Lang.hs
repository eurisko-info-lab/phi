{-# LANGUAGE LambdaCase #-}
-- | Lang: Language specification types
--
-- Parse and represent .phi language specification files.
--
-- A .phi file defines a complete language specification:
--   - Sorts (types): what kinds of things exist
--   - Constructors: how to build values of each sort
--   - Grammars: concrete syntax for parsing/printing
--   - Transforms: AST-to-AST transformations
--   - Rules: operational semantics / rewrite rules
module Phi.Lang
  ( -- * Language Specification
    LangSpec(..)
  , lsName, lsSorts, lsCons, lsXforms, lsRules, lsGrammars
  , emptySpec
  , consFor
  , grammarFor
  , xformNamed
  , showSpec
    -- * Sorts
  , SortDecl(..)
  , sdName, sdParams
    -- * Constructors
  , ConDecl(..)
  , cdName, cdFields, cdReturn
  , Field(..)
  , fName, fType
    -- * Type References
  , TypeRef(..)
  , showTypeRef
    -- * Grammars
  , GrammarDecl(..)
  , gdName, gdProductions
  , Production(..)
  , prodPattern, prodConstructor
  , GrammarPat(..)
  , showGrammarPat
    -- * Transforms
  , XformDecl(..)
  , xdName, xdFrom, xdTo, xdCases
  , XformCase(..)
  , XformPat(..)
  , XformBody(..)
    -- * Rules
  , RuleDecl(..)
  , rdName, rdCases
  , Judgment(..)
  ) where

import Data.List (intercalate, find)
import qualified Data.Map as Map
import Data.Map (Map)
import Phi.Meta.Val (Val)

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 1: Language Specification
-- ═══════════════════════════════════════════════════════════════════════════

-- | LangSpec: Complete specification of a language.
-- This is the root of a parsed .phi file.
data LangSpec = LangSpec
  { lsName :: String
  , lsSorts :: [SortDecl]
  , lsCons :: [ConDecl]
  , lsXforms :: [XformDecl]
  , lsRules :: [RuleDecl]
  , lsGrammars :: [GrammarDecl]
  } deriving (Eq, Show)

-- | Empty specification
emptySpec :: String -> LangSpec
emptySpec name = LangSpec name [] [] [] [] []

-- | Get constructor by name
consFor :: LangSpec -> String -> Maybe ConDecl
consFor spec name = find ((== name) . cdName) (lsCons spec)

-- | Get grammar rules for a sort
grammarFor :: LangSpec -> String -> Maybe GrammarDecl
grammarFor spec sort = find ((== sort) . gdName) (lsGrammars spec)

-- | Lookup a transform by name
xformNamed :: LangSpec -> String -> Maybe XformDecl
xformNamed spec name = find ((== name) . xdName) (lsXforms spec)

-- | Pretty-print the spec
showSpec :: LangSpec -> String
showSpec spec = unlines $
  ["language " ++ lsName spec ++ " {"] ++
  (if null (lsSorts spec) then [] else
    map showSortDecl (lsSorts spec)) ++
  (if null (lsCons spec) then [] else
    map showConDecl (lsCons spec)) ++
  ["}"]

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 2: Sort Declarations
-- ═══════════════════════════════════════════════════════════════════════════

-- | SortDecl: Declaration of a sort (type).
data SortDecl = SortDecl
  { sdName :: String
  , sdParams :: [String]
  } deriving (Eq, Show)

showSortDecl :: SortDecl -> String
showSortDecl (SortDecl name []) = "  sort " ++ name
showSortDecl (SortDecl name params) = 
  "  sort " ++ name ++ "[" ++ intercalate ", " params ++ "]"

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 3: Constructor Declarations
-- ═══════════════════════════════════════════════════════════════════════════

-- | ConDecl: Constructor declaration.
data ConDecl = ConDecl
  { cdName :: String
  , cdFields :: [Field]
  , cdReturn :: String
  } deriving (Eq, Show)

-- | Field in a constructor
data Field = Field
  { fName :: Maybe String
  , fType :: TypeRef
  } deriving (Eq, Show)

showConDecl :: ConDecl -> String
showConDecl (ConDecl name [] ret) = "  constructor " ++ name ++ " : " ++ ret
showConDecl (ConDecl name fields ret) = 
  "  constructor " ++ name ++ " : " ++ 
  intercalate " -> " (map showField fields) ++ " -> " ++ ret

showField :: Field -> String
showField (Field Nothing typ) = showTypeRef typ
showField (Field (Just name) typ) = "(" ++ name ++ " : " ++ showTypeRef typ ++ ")"

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 4: Type References
-- ═══════════════════════════════════════════════════════════════════════════

-- | TypeRef: Reference to a type (sort).
data TypeRef
  = TRSort String
  | TRApp String [TypeRef]
  | TRList TypeRef
  | TRArrow TypeRef TypeRef
  deriving (Eq, Show)

showTypeRef :: TypeRef -> String
showTypeRef (TRSort name) = name
showTypeRef (TRApp name args) = 
  name ++ "[" ++ intercalate ", " (map showTypeRef args) ++ "]"
showTypeRef (TRList elem) = "List[" ++ showTypeRef elem ++ "]"
showTypeRef (TRArrow a b) = showTypeRef a ++ " -> " ++ showTypeRef b

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 5: Grammar Declarations
-- ═══════════════════════════════════════════════════════════════════════════

-- | GrammarDecl: Concrete syntax for a sort.
data GrammarDecl = GrammarDecl
  { gdName :: String
  , gdProductions :: [Production]
  } deriving (Eq, Show)

-- | Production: Single grammar production.
data Production = Production
  { prodPattern :: GrammarPat
  , prodConstructor :: String
  } deriving (Eq, Show)

-- | GrammarPat: Pattern in a grammar production.
data GrammarPat
  = GPLiteral String        -- ^ "+"
  | GPRegex String          -- ^ /[0-9]+/
  | GPNonTerminal String    -- ^ Expr
  | GPSeq [GrammarPat]      -- ^ A B C
  | GPAlt [GrammarPat]      -- ^ A | B
  | GPOpt GrammarPat        -- ^ A?
  | GPRep GrammarPat        -- ^ A*
  | GPRep1 GrammarPat       -- ^ A+
  | GPGroup GrammarPat      -- ^ (A B)
  deriving (Eq, Show)

showGrammarDecl :: GrammarDecl -> String
showGrammarDecl (GrammarDecl sort prods) =
  "grammar " ++ sort ++ " {\n" ++
  unlines (map (\p -> "  " ++ showProduction p) prods) ++
  "}"

showProduction :: Production -> String
showProduction (Production pat con) = showGrammarPat pat ++ " => " ++ con

showGrammarPat :: GrammarPat -> String
showGrammarPat = \case
  GPLiteral t     -> show t
  GPRegex p       -> "/" ++ p ++ "/"
  GPNonTerminal s -> s
  GPSeq ps        -> unwords (map showGrammarPat ps)
  GPAlt os        -> intercalate " | " (map showGrammarPat os)
  GPOpt i         -> showGrammarPat i ++ "?"
  GPRep i         -> showGrammarPat i ++ "*"
  GPRep1 i        -> showGrammarPat i ++ "+"
  GPGroup i       -> "(" ++ showGrammarPat i ++ ")"

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 6: Transform Declarations
-- ═══════════════════════════════════════════════════════════════════════════

-- | XformDecl: AST transformation.
data XformDecl = XformDecl
  { xdName :: String
  , xdFrom :: TypeRef
  , xdTo :: TypeRef
  , xdCases :: [XformCase]
  } deriving (Eq, Show)

-- | XformCase: Single transformation case.
data XformCase = XformCase
  { xcPat :: XformPat
  , xcBody :: XformBody
  } deriving (Eq, Show)

-- | XformPat: Pattern in an xform case.
data XformPat
  = XPVar String                  -- ^ Variable binding
  | XPCon String [XformPat]       -- ^ Constructor pattern
  | XPWild                        -- ^ Wildcard _
  | XPLit Val                     -- ^ Literal value
  deriving (Eq, Show)

-- | XformBody: Body of an xform case.
data XformBody
  = XBVar String                  -- ^ Variable reference
  | XBCon String [XformBody]      -- ^ Constructor application
  | XBLit Val                     -- ^ Literal value
  | XBApp XformBody XformBody     -- ^ Application
  | XBLet String XformBody XformBody  -- ^ Let binding
  deriving (Eq, Show)

showXformDecl :: XformDecl -> String
showXformDecl (XformDecl name from to _cases) =
  "xform " ++ name ++ " : " ++ showTypeRef from ++ " -> " ++ showTypeRef to

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 7: Rule Declarations
-- ═══════════════════════════════════════════════════════════════════════════

-- | RuleDecl: Operational semantics rule / xform cases.
data RuleDecl = RuleDecl
  { rdName :: String
  , rdCases :: [XformCase]
  } deriving (Eq, Show)

-- | Judgment: A semantic judgment.
data Judgment = Judgment
  { judgRelation :: String
  , judgTerms :: [String]  -- Terms as strings (parsed later)
  } deriving (Eq, Show)

showJudgment :: Judgment -> String
showJudgment (Judgment rel terms) = intercalate (" " ++ rel ++ " ") terms

showRuleDecl :: RuleDecl -> String
showRuleDecl (RuleDecl name _) = "rule " ++ name ++ " { ... }"
