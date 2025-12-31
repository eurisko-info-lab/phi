{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
-- | Core: Algebraic foundation - Recursion schemes, Optics, Validation, Free/Cofree
-- 
-- The complete algebraic infrastructure for metaprogramming.
--
-- PHILOSOPHY: Everything is a tree with annotations. This unifies:
--   Zippers    = trees + locations
--   Attributes = trees + computed values
--   Versions   = trees + content hashes
--
-- All three are instances of Cofree V a where a is the annotation type.
module Phi.Core
  ( -- * Values (Universal AST)
    Val(..)
  , showVal
    -- * Pattern Functor
  , V(..)
  , vOut, vIn
    -- * Recursion Schemes
  , cata, ana, hylo, para
    -- * Free Monad
  , Free(..)
  , liftF, foldFree
    -- * Cofree Comonad  
  , Cofree(..)
  , extract, extend, duplicate
  , annotate, forget
    -- * Optics
  , Lens(..), lens, view, set, over
  , Prism(..), prism, preview, review
  , Traversal(..), toListOf, overAll
  , argL, conP, childrenT, everywhere, everywhereDown
    -- * Validated
  , Validated(..)
  , valid, invalid, zipV, sequenceV, toEither
    -- * Zipper
  , Loc(..)
  , Zipper
  , zipper, navigate, modifyZ, toVal
    -- * Xform (Bidirectional)
  , Xform(..)
  , xform, idX, composeX, invertX
    -- * Hash & Store
  , Hash, hashOf, hashValue
  , Store, newStore, putStore, getStore
  ) where

import Prelude hiding (lookup)
import Data.List (intercalate)
import Data.IORef
import qualified Data.Map as Map
import Data.Map (Map)
import Numeric (showHex)
import Data.Hashable (hash)

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 1: Values (The Universal AST)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Val: The universal value type for Phi programs.
-- Everything in Phi is represented as a Val.
data Val
  = VCon String [Val]   -- ^ Constructor: Add(x, y)
  | VStr String         -- ^ String literal
  | VInt Int            -- ^ Integer literal
  | VList [Val]         -- ^ List value
  deriving (Eq)

-- | Pretty-print a value
showVal :: Val -> String
showVal (VCon n [])   = n
showVal (VCon n args) = n ++ "(" ++ intercalate ", " (map showVal args) ++ ")"
showVal (VStr s)      = show s
showVal (VInt n)      = show n
showVal (VList [])    = "[]"
showVal (VList elems) = "[" ++ intercalate ", " (map showVal elems) ++ "]"

instance Show Val where
  show = showVal

-- | Get children for traversal
children :: Val -> [Val]
children (VCon _ args) = args
children (VList elems) = elems
children _             = []

-- | Map over immediate children
mapChildren :: (Val -> Val) -> Val -> Val
mapChildren f (VCon n args) = VCon n (map f args)
mapChildren f (VList elems) = VList (map f elems)
mapChildren _ other         = other

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 2: Pattern Functor V (One Layer of AST)
-- ═══════════════════════════════════════════════════════════════════════════

-- | V: The pattern functor - represents ONE layer of Val.
-- Conceptually: Val ≅ V Val (Val is the fixed point of V)
data V a
  = C String [a]   -- ^ Constructor node
  | S String       -- ^ String leaf
  | I Int          -- ^ Int leaf
  | L [a]          -- ^ List node
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Project: unfold Val into V Val
vOut :: Val -> V Val
vOut (VCon n args) = C n args
vOut (VStr s)      = S s
vOut (VInt i)      = I i
vOut (VList elems) = L elems

-- | Embed: fold V Val into Val
vIn :: V Val -> Val
vIn (C n args) = VCon n args
vIn (S s)      = VStr s
vIn (I i)      = VInt i
vIn (L elems)  = VList elems

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 3: Recursion Schemes
-- ═══════════════════════════════════════════════════════════════════════════

-- | Catamorphism: Generic fold (bottom-up).
-- Given an algebra (V a -> a), collapse a tree to a single value.
cata :: (V a -> a) -> Val -> a
cata alg = alg . fmap (cata alg) . vOut

-- | Anamorphism: Generic unfold (top-down).
-- Given a coalgebra (a -> V a), grow a tree from a seed.
ana :: (a -> V a) -> a -> Val
ana coalg = vIn . fmap (ana coalg) . coalg

-- | Hylomorphism: Fused unfold-then-fold.
-- Build and immediately consume without materializing intermediate tree.
hylo :: (V b -> b) -> (a -> V a) -> a -> b
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg

-- | Paramorphism: Fold with access to original subtrees.
-- Like cata but the algebra also sees the original Val at each step.
para :: (V (Val, a) -> a) -> Val -> a
para alg v = alg (fmap (\c -> (c, para alg c)) (vOut v))

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 4: Free Monad
-- ═══════════════════════════════════════════════════════════════════════════

-- | Free: The free monad over a functor f.
-- Represents computations as DATA that can be inspected/interpreted.
data Free f a
  = Pure a
  | Roll (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a)  = Pure (f a)
  fmap f (Roll fa) = Roll (fmap (fmap f) fa)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f  <*> a = fmap f a
  Roll ff <*> a = Roll (fmap (<*> a) ff)

instance Functor f => Monad (Free f) where
  Pure a  >>= f = f a
  Roll fa >>= f = Roll (fmap (>>= f) fa)

-- | Lift an effect into Free
liftF :: Functor f => f a -> Free f a
liftF fa = Roll (fmap Pure fa)

-- | Interpret Free via natural transformation
foldFree :: (Functor f, Monad m) => (forall x. f x -> m x) -> Free f a -> m a
foldFree _  (Pure a)  = return a
foldFree nt (Roll fa) = nt fa >>= foldFree nt

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 5: Cofree Comonad
-- ═══════════════════════════════════════════════════════════════════════════

-- | Cofree: A tree where EVERY node carries an annotation.
-- Unifies zippers, attributes, and versions!
data Cofree f a = Cofree { cofreeHead :: a, cofreeTail :: f (Cofree f a) }

instance Functor f => Functor (Cofree f) where
  fmap f (Cofree a t) = Cofree (f a) (fmap (fmap f) t)

-- | Extract the annotation at current node
extract :: Cofree f a -> a
extract = cofreeHead

-- | Extend: apply context-dependent function everywhere
extend :: Functor f => (Cofree f a -> b) -> Cofree f a -> Cofree f b
extend f w@(Cofree _ t) = Cofree (f w) (fmap (extend f) t)

-- | Duplicate: annotate each node with itself
duplicate :: Functor f => Cofree f a -> Cofree f (Cofree f a)
duplicate = extend id

-- | Universal attribute grammar in ONE function.
-- Computes both inherited (parent→children) and synthesized (children→parent) attrs.
annotate :: Val -> (Val -> i -> i) -> (Val -> i -> [s] -> s) -> i -> Cofree V s
annotate v inherit synth init = go v init
  where
    go val inh =
      let childInh = inherit val inh
          structure = vOut val
          annotated = case structure of
            C _ args -> map (`go` childInh) args
            L elems  -> map (`go` childInh) elems
            _        -> []
          syn = synth val childInh (map cofreeHead annotated)
          newTail = case structure of
            C n _ -> C n annotated
            L _   -> L annotated
            S s   -> S s
            I i   -> I i
      in Cofree syn newTail

-- | Strip annotations to recover original Val
forget :: Cofree V a -> Val
forget (Cofree _ t) = vIn (fmap forget t)

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 6: Optics
-- ═══════════════════════════════════════════════════════════════════════════

-- | Lens: Focus on exactly ONE part of a structure
data Lens s a = Lens { lensGet :: s -> a, lensSet :: a -> s -> s }

-- | Smart constructor for Lens
lens :: (s -> a) -> (a -> s -> s) -> Lens s a
lens = Lens

-- | Get the focused value
view :: Lens s a -> s -> a
view = lensGet

-- | Set the focused value
set :: Lens s a -> a -> s -> s
set = lensSet

-- | Modify the focused value
over :: Lens s a -> (a -> a) -> s -> s
over l f s = lensSet l (f (lensGet l s)) s

-- | Compose lenses
composeLens :: Lens a b -> Lens s a -> Lens s b
composeLens inner outer = Lens
  { lensGet = lensGet inner . lensGet outer
  , lensSet = \b s -> lensSet outer (lensSet inner b (lensGet outer s)) s
  }

-- | Prism: Focus on ONE CASE of a sum type
data Prism s a = Prism { prismPreview :: s -> Maybe a, prismReview :: a -> s }

-- | Smart constructor for Prism
prism :: (s -> Maybe a) -> (a -> s) -> Prism s a
prism = Prism

-- | Try to get the focused value (may fail)
preview :: Prism s a -> s -> Maybe a
preview = prismPreview

-- | Construct from focused value
review :: Prism s a -> a -> s
review = prismReview

-- | Modify if the prism matches
overPrism :: Prism s a -> (a -> a) -> s -> s
overPrism p f s = maybe s (prismReview p . f) (prismPreview p s)

-- | Traversal: Focus on ZERO OR MORE parts
data Traversal s a = Traversal
  { traversalGetAll :: s -> [a]
  , traversalModify :: (a -> a) -> s -> s
  }

-- | Get all focused values
toListOf :: Traversal s a -> s -> [a]
toListOf = traversalGetAll

-- | Modify all focused values
overAll :: Traversal s a -> (a -> a) -> s -> s
overAll = traversalModify

-- | Lens into nth argument of a constructor
argL :: Int -> Lens Val Val
argL n = Lens
  { lensGet = \case
      VCon _ args | n < length args -> args !! n
      v -> v
  , lensSet = \newArg -> \case
      VCon name args | n < length args -> 
        VCon name (take n args ++ [newArg] ++ drop (n + 1) args)
      s -> s
  }

-- | Prism matching constructor by name
conP :: String -> Prism Val [Val]
conP name = Prism
  { prismPreview = \case
      VCon n args | n == name -> Just args
      _ -> Nothing
  , prismReview = VCon name
  }

-- | Traversal over all immediate children
childrenT :: Traversal Val Val
childrenT = Traversal
  { traversalGetAll = children
  , traversalModify = mapChildren
  }

-- | Transform everywhere in tree (bottom-up)
everywhere :: (Val -> Val) -> Val -> Val
everywhere f v = f (mapChildren (everywhere f) v)

-- | Transform everywhere (top-down)
everywhereDown :: (Val -> Val) -> Val -> Val
everywhereDown f v = mapChildren (everywhereDown f) (f v)

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 7: Validated
-- ═══════════════════════════════════════════════════════════════════════════

-- | Validated: Like Either, but accumulates ALL errors.
data Validated e a
  = Valid a
  | Invalid [e]
  deriving (Eq, Show, Functor)

-- | Construct a valid value
valid :: a -> Validated e a
valid = Valid

-- | Construct an invalid value with one error
invalid :: e -> Validated e a
invalid e = Invalid [e]

-- | Combine validations, accumulating errors
zipV :: Validated e a -> Validated e b -> Validated e (a, b)
zipV (Valid a)    (Valid b)    = Valid (a, b)
zipV (Invalid e1) (Invalid e2) = Invalid (e1 ++ e2)
zipV (Invalid e)  _            = Invalid e
zipV _            (Invalid e)  = Invalid e

-- | Sequence a list of validations
sequenceV :: [Validated e a] -> Validated e [a]
sequenceV = foldr (\v acc -> fmap (uncurry (:)) (zipV v acc)) (Valid [])

-- | Convert to Either
toEither :: Validated e a -> Either [e] a
toEither (Valid a)   = Right a
toEither (Invalid e) = Left e

instance Applicative (Validated e) where
  pure = Valid
  Valid f    <*> Valid a    = Valid (f a)
  Invalid e1 <*> Invalid e2 = Invalid (e1 ++ e2)
  Invalid e  <*> _          = Invalid e
  _          <*> Invalid e  = Invalid e

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 8: Zipper
-- ═══════════════════════════════════════════════════════════════════════════

-- | Location in a tree: current value + path from root
data Loc = Loc { locValue :: Val, locPath :: [Int] }
  deriving (Eq, Show)

-- | Zipper: Cofree annotated with locations
type Zipper = Cofree V Loc

-- | Create a zipper from a Val
zipper :: Val -> Zipper
zipper v = go v []
  where
    go val path =
      let structure = vOut val
          annotated = case structure of
            C n args -> C n (zipWith (\c i -> go c (path ++ [i])) args [0..])
            L elems  -> L (zipWith (\c i -> go c (path ++ [i])) elems [0..])
            S s      -> S s
            I i      -> I i
      in Cofree (Loc val path) annotated

-- | Navigate to a path, returning Nothing if invalid
navigate :: [Int] -> Zipper -> Maybe Zipper
navigate []     z = Just z
navigate (i:is) z = case cofreeTail z of
  C _ args | i < length args -> navigate is (args !! i)
  L elems  | i < length elems -> navigate is (elems !! i)
  _ -> Nothing

-- | Modify the value at current focus
modifyZ :: (Val -> Val) -> Zipper -> Zipper
modifyZ f (Cofree (Loc v p) t) = Cofree (Loc (f v) p) t

-- | Convert back to Val (strips annotations)
toVal :: Zipper -> Val
toVal = forget

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 9: Xform (Bidirectional)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Xform: Bidirectional transformation A ⇄ B
data Xform a b = Xform
  { xformForward  :: a -> Maybe b
  , xformBackward :: b -> Maybe a
  }

-- | Smart constructor
xform :: (a -> Maybe b) -> (b -> Maybe a) -> Xform a b
xform = Xform

-- | Identity transform
idX :: Xform a a
idX = Xform Just Just

-- | Compose transforms
composeX :: Xform a b -> Xform b c -> Xform a c
composeX f g = Xform
  { xformForward  = xformForward f >=> xformForward g
  , xformBackward = xformBackward g >=> xformBackward f
  }
  where
    (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
    (f >=> g) a = f a >>= g

-- | Invert a transform
invertX :: Xform a b -> Xform b a
invertX x = Xform (xformBackward x) (xformForward x)

-- ═══════════════════════════════════════════════════════════════════════════
-- SECTION 10: Hash & Store
-- ═══════════════════════════════════════════════════════════════════════════

-- | Content hash
newtype Hash = Hash String
  deriving (Eq, Ord, Show)

-- | Compute hash of a Val
hashOf :: Val -> Hash
hashOf v = Hash (take 8 (showHex (abs (hash (showVal v))) ""))

-- | Get hash value as string
hashValue :: Hash -> String
hashValue (Hash s) = s

-- | Content-addressed store
data Store a = Store (IORef (Map Hash a)) (a -> Hash)

-- | Create a new store
newStore :: (a -> Hash) -> IO (Store a)
newStore hashFn = do
  ref <- newIORef Map.empty
  return (Store ref hashFn)

-- | Put a value in the store
putStore :: Store a -> a -> IO Hash
putStore (Store ref hashFn) a = do
  let h = hashFn a
  modifyIORef' ref (Map.insert h a)
  return h

-- | Get a value from the store
getStore :: Store a -> Hash -> IO (Maybe a)
getStore (Store ref _) h = Map.lookup h <$> readIORef ref
