module Syntax where

data Expr 
  = EInt Int
  | EBool Bool
  | EVar String
  | ELet String Expr Expr
  | EApp Expr Expr 
  | ELam String Expr
  deriving Show 

prettyExpr ∷ Expr → String
prettyExpr = \case
  EInt     a → show a 
  EBool    a → show a 
  EVar     a → a
  ELet b v a → mconcat ["let ", b, " = ", prettyExpr v, " in ", prettyExpr a]
  EApp   a b → mconcat ["(", prettyExpr a, ") (", prettyExpr b, ")"]
  ELam   b e → mconcat ["λ", b, ".", prettyExpr e]