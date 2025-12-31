-- | Show: Pretty printing for values
module Phi.Meta.Show
  ( render
  , renderExpr
  ) where

import Phi.Meta.Val
import Phi.Meta.Expr
import Phi.Meta.Pat

-- | Render a value to string
render :: Val -> String
render = showVal

-- | Render an expression to string
renderExpr :: Expr -> String
renderExpr (EVar name) = name
renderExpr (ECon name []) = name
renderExpr (ECon name args) = name ++ "(" ++ intercalate ", " (map renderExpr args) ++ ")"
renderExpr (EApp f a) = "(" ++ renderExpr f ++ " " ++ renderExpr a ++ ")"
renderExpr (ELam p b) = "λ" ++ p ++ ". " ++ renderExpr b
renderExpr (ELet n v b) = "let " ++ n ++ " = " ++ renderExpr v ++ " in " ++ renderExpr b
renderExpr (EMatch s cs) = renderExpr s ++ " match { " ++ intercalate "; " (map renderExpr cs) ++ " }"
renderExpr (ECase p b) = renderPat p ++ " => " ++ renderExpr b
renderExpr (ELit n) = show n
renderExpr (EStr s) = show s
renderExpr (EList es) = "[" ++ intercalate ", " (map renderExpr es) ++ "]"
renderExpr (EIf c t e) = "if " ++ renderExpr c ++ " then " ++ renderExpr t ++ " else " ++ renderExpr e

-- | Render a pattern to string
renderPat :: Pat -> String
renderPat (PVar name) = name
renderPat PWild = "_"
renderPat (PCon name []) = name
renderPat (PCon name pats) = name ++ "(" ++ intercalate ", " (map renderPat pats) ++ ")"
renderPat (PLit s) = show s
renderPat (PList pats) = "[" ++ intercalate ", " (map renderPat pats) ++ "]"
renderPat (PAs name pat) = name ++ "@" ++ renderPat pat

intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs
