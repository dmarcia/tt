module Typechk where

import Prelude as P
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Map as M
import Data.Set as S 
import Data.Maybe
import Type 
import Syntax 

type Subst = Map String Type 

data Scheme = MkScheme [String] Type 

applySubst ∷ Subst → Type → Type
applySubst s = \case
  TVar v   → fromMaybe (TVar v) (M.lookup v s)
  TFun a b → TFun (applySubst s a) (applySubst s b)
  a        → a

applySubstScheme ∷ Subst → Scheme → Scheme 
applySubstScheme s (MkScheme vs t) =
  MkScheme vs $ applySubst (P.foldr M.delete s vs) t

composeSubst ∷ Subst → Subst → Subst
composeSubst s1 s2 = M.union (M.map (applySubst s1) s2) s1 

type TC = ExceptT String (State Int)

runTC ∷ TC a → Either String a
runTC tc = evalState (runExceptT tc) 0

newTVar ∷ TC Type
newTVar = do
  i ← get
  modify succ
  pure . TVar $ "t" <> show i

ftv ∷ Type → Set String
ftv = \case
  TVar v   → S.singleton v
  TFun a b → S.union (ftv a) (ftv b)
  _        → mempty

ftvScheme ∷ Scheme → Set String
ftvScheme (MkScheme vs t) = S.difference (ftv t) (S.fromList vs)

bind ∷ String → Type → TC Subst
bind v t
  | t == TVar v        = pure mempty
  | S.member v (ftv t) = throwError "infinite type"
  | otherwise          = pure $ M.singleton v t

unify ∷ Type → Type → TC Subst
unify TInt       TInt         = pure mempty 
unify TBool      TBool        = pure mempty
unify (TFun a b) (TFun a' b') = do
  s1 ← unify a a'
  s2 ← unify (applySubst s1 b) (applySubst s1 b')
  pure $ composeSubst s2 s1
unify (TVar v)    t           = bind v t
unify t           (TVar v)    = bind v t 
unify t1          t2          = throwError $ mconcat [show t1, " does not unify with " <> show t2]

type Γ = Map String Scheme

applySubstΓ ∷ Subst → Γ → Γ
applySubstΓ s = M.map (applySubstScheme s)

ftvΓ ∷ Γ → Set String
ftvΓ = foldMap ftvScheme . M.elems

generalize ∷ Γ → Type → Scheme
generalize γ t = MkScheme vs t where
  vs = S.toList $ S.difference (ftv t) (ftvΓ γ)

instantiate ∷ Scheme → TC Type
instantiate (MkScheme vs t) = do
  newTVs ← traverse (const newTVar) vs
  let s = M.fromList (zip vs newTVs)
  pure $ applySubst s t

infer ∷ Γ → Expr → TC (Subst, Type)
infer γ = \case
  EVar v → case M.lookup v γ of
    Nothing → throwError $ "unbound variable: " <> v
    Just s  → (mempty,) <$> instantiate s
  EInt  _ → pure (mempty, TInt)
  EBool _ → pure (mempty, TBool)
  EApp f a → do
    tv       ← newTVar
    (s1, t1) ← infer γ f
    (s2, t2) ← infer (applySubstΓ s1 γ) a
    s3       ← unify (applySubst s2 t1) (TFun t2 tv)
    pure (s3 `composeSubst` s2 `composeSubst` s1, applySubst s3 tv)
  ELam b a → do
    tv ← newTVar
    let γ' = M.insert b (MkScheme [] tv) γ
    (s1, t1) ← infer γ' a
    pure (s1, TFun (applySubst s1 tv) t1)
  ELet b v a → do 
    (s1, t1) ← infer γ v
    let 
      s  = MkScheme [] (applySubst s1 t1)
      γ' = M.insert b s γ 
    (s2, t2) ← infer (applySubstΓ s1 γ') a
    pure (composeSubst s2 s1, t2)

inferType' ∷ Γ → Expr → TC Type
inferType' γ a = do
  (s, t) ← infer γ a
  pure (applySubst s t)

inferType ∷ Expr → TC Type
inferType = inferType' mempty