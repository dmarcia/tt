module Type where

data Type 
  = TInt
  | TBool
  | TVar String
  | TFun Type Type
  deriving (Show, Eq) 

prettyType ∷ Type → String
prettyType = \case
  TInt     → "Int"
  TBool    → "Bool"
  TVar v   → v
  TFun a b → prettyType a <> " → " <> prettyType b 