{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Substitution
  ( Subst(..), SingleSub, empty, single, apply, compose
  ) where

import Type
import Data.List
import Data.Maybe

type SingleSub = (VarIndex, Term)
type Subst = [SingleSub]

-- Returns an empty substitution
empty :: Subst
empty = []

-- Returns a single substitution
single :: VarIndex -> Term -> Subst
single v t = [(v, t)]

-- Applies a substitution on a term
apply :: Subst -> Term -> Term
apply [] t = t
apply sub (Var v) = fromMaybe (Var v) (lookup v sub)
apply s (Comb str ts) = Comb str (map (apply s) ts)

{-
  Ziel: S ° R == S(R) (t) bzw. apply S (apply R T) == apply (compose S R) T
  S = Subst([(Var 0, Comb "test" []), (Var 5, Comb "bla" [])])
  R = Subst([(Var 1, Var 0), (Var 2, Var 5)]),
  S°R = (compose [(Var 5, Comb "bla" [])] [(Var 1, Comb "test" []), (Var 2, Var5)]) ++ [(Var 0, Comb "test" []]
  S°R = ([(Var 1, Comb "test"[]), (Var 2, Comb "bla" [])] ++ [Var 5, Comb "bla" []]) ++ [(Var 0, Comb "test" [])]
  T = Comb "append" [Var 1, Var 2, Var 5]
  R(T) = Comb "append" [Var 0, Var 5, Var 5]
  S(R(T)) = Comb "append" [Comb "test" [], Comb "bla" [], Comb "bla" []]
-}
-- Applies a substitution on a substitution
compose :: Subst -> Subst -> Subst
compose [] s = s
compose s [] = s
compose (s1:r) s2 = compose r (applySingleToSubst s1 s2) ++
                    (if elem (fst s1) (left s2) then [] else [s1])
  where
    applySingleToSubst :: SingleSub -> Subst -> Subst
    applySingleToSubst single subst = zip (left subst)
                                           (map (apply [single]) (right subst))
    left x = fst (unzip x)
    right x = snd (unzip x)

instance Pretty Subst where
  pretty s = prettyWithVars [] s
  prettyWithVars l s = "{" ++ intercalate ", " (map (prettyWithVars l) s) ++ "}"

instance Pretty SingleSub where
  pretty s = prettyWithVars [] s
  prettyWithVars l (left, right) = prettyWithVars l left ++ " -> " ++ prettyWithVars l right
