-- | Val: Runtime values for the Phi meta-language
module Phi.Meta.Val 
  ( Val(..)
  , showVal
  ) where

-- | Runtime values
data Val
  = VInt Int                    -- ^ Integer literal
  | VStr String                 -- ^ String literal  
  | VCon String [Val]           -- ^ Constructor: name + args
  | VClosure String Expr Env    -- ^ Closure: param, body, captured env
  | VList [Val]                 -- ^ List of values
  deriving (Eq)

-- Forward declarations (will be defined in other modules)
type Expr = Val  -- Placeholder - expressions are also values in this simple model
type Env = [(String, Val)]

-- | Pretty-print a value
showVal :: Val -> String
showVal (VInt n) = show n
showVal (VStr s) = show s
showVal (VCon name []) = name
showVal (VCon name args) = name ++ "(" ++ intercalate ", " (map showVal args) ++ ")"
showVal (VClosure param _ _) = "<closure: \\" ++ param ++ " -> ...>"
showVal (VList vs) = "[" ++ intercalate ", " (map showVal vs) ++ "]"

intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

instance Show Val where
  show = showVal
