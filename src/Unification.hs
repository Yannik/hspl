module Unification
  ( ds, unify
  ) where

import Type
import Substitution

-- returns the disagreement set of two terms
ds :: Term -> Term -> Maybe (Term, Term)
ds t1 t2 | t1 == t2   = Nothing
         | otherwise  = ds' t1 t2

ds' :: Term -> Term -> Maybe (Term, Term)
ds' (Var v) t2 = Just (Var v, t2)
ds' t1 (Var v) = Just (Var v, t1) -- we want disagreement sets ordered Var->Comb for unification
ds' (Comb functor1 terms1) (Comb functor2 terms2)
    | functor1 /= functor2 ||
      length terms1 /= length terms2 = Just ((Comb functor1 terms1), (Comb functor2 terms2))
    | otherwise                      = let i = firstMismatch terms1 terms2 0
                                       in ds (terms1 !! i) (terms2 !! i)

firstMismatch :: [Term] -> [Term] -> Int -> Int
firstMismatch t1 t2 i | t1 !! i == t2 !! i = firstMismatch t1 t2 (i+1)
                      | otherwise          = i


-- Suche eine Substitution, für die gilt S(t1) = S(t2)
unify :: Term -> Term -> Maybe Subst
unify t1 t2 = unify' t1 t2 empty

unify' :: Term -> Term -> Subst -> Maybe Subst
unify' t1 t2 s = case ds (apply s t1) (apply s t2) of
                   Nothing -> Just s
                   Just (Var v, t)  | varInTerm (Var v) t -> Nothing
                                    | otherwise      -> unify' t1 t2 (compose (single v t) s)
                   _ -> Nothing


varInTerm :: Term -> Term -> Bool
varInTerm (Var a) (Var b) | a == b = True
varInTerm (Var a) (Var b) | a /= b = False
varInTerm (Var a) (Comb _ terms) = or (map (varInTerm (Var a)) terms)
