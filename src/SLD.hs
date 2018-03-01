{-# LANGUAGE FlexibleInstances #-}

module SLD
  ( SLDTree(..), sld, sgoal, sprog, mygoal, myprog, gprog, ggoal
  ) where

import Type
import Substitution
import Unification
import Data.List
import Safe.Foldable
import Data.Maybe

instance Pretty Rule where
  pretty r = prettyWithVars [] r
  prettyWithVars l (t :- []) = prettyWithVars l t ++ "."
  prettyWithVars l (t :- ts) = prettyWithVars l t ++ ":- " ++ intercalate ", " (map (prettyWithVars l) ts) ++ "."

instance Pretty Goal where
  pretty g = prettyWithVars [] g
  prettyWithVars l g = intercalate ", " (map (prettyWithVars l) g) ++ "."

instance Pretty Prog where
  pretty p = prettyWithVars [] p
  prettyWithVars l p = unlines (map (prettyWithVars l) p)

instance Pretty SLDTree where
  pretty s = prettyWithVars [] s
  prettyWithVars l (SLDTree g n) = "SLDTree " ++ prettyWithVars l g ++ unwords (map (\(s,st) -> "<=" ++ prettyWithVars l s ++ ":" ++ prettyWithVars l st ++ "=>") n)


-- type synonyms cannot be recursive :-(
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
  deriving Show


sld :: Prog -> Goal -> SLDTree
sld p g = sld' (namespaceProg (termsMaxVarIndex g) p) g Substitution.empty
  where
  sld' :: Prog -> Goal -> Subst -> SLDTree
  sld' p [] s = SLDTree [] []
  sld' p g s = SLDTree g (mapMaybe (sld'' p g s) p)
  sld'' :: Prog ->  Goal -> Subst -> Rule -> Maybe (Subst, SLDTree)
  sld'' p (hg:tg) s1 (hr :- tr) = case (unify hg hr) of
              Just s2 -> Just (s2, sld' (namespaceProg (sgMaxVarIndex s2 (hg:tg)) p) (map (apply s2) (tr ++ tg)) (compose s1 s2))
              Nothing -> Nothing -- nur wenn regel anwendbar gibt es einen neuen node

sgMaxVarIndex :: Subst -> Goal -> Int
sgMaxVarIndex s g = maximum (catMaybes [subsMaxVarIndex s, (Just (termsMaxVarIndex g))])

namespaceVariables :: Goal -> Prog -> (Goal, Prog)
namespaceVariables g p = (g, namespaceProg (termsMaxVarIndex g) p)

namespaceProg :: Int -> Prog -> Prog
namespaceProg i [h]  = [ruleIncreaseVarIndex i h]
namespaceProg i (h:t) = ruleIncreaseVarIndex i h:namespaceProg (i+ruleMaxVarIndex h) t

ruleIncreaseVarIndex :: Int -> Rule -> Rule
ruleIncreaseVarIndex i (l :- r) = termIncreaseVarIndex i l :- map (termIncreaseVarIndex i) r

termIncreaseVarIndex :: Int -> Term -> Term
termIncreaseVarIndex i (Var c) = Var (c+i)
termIncreaseVarIndex i (Comb f t) = Comb f (map (termIncreaseVarIndex i) t)

ruleMaxVarIndex :: Rule -> Int
ruleMaxVarIndex  (l :- r) = termsMaxVarIndex (l:r)

termsMaxVarIndex :: [Term] -> Int
termsMaxVarIndex terms = fromMaybe 0 (maximumMay (mapMaybe termMaxVarIndex terms))

termMaxVarIndex :: Term -> Maybe Int
termMaxVarIndex (Var v) = Just (v+1) --because 0-indexed
termMaxVarIndex (Comb _ t) = maximumMay (mapMaybe termMaxVarIndex t)

subsMaxVarIndex :: Subst -> Maybe Int
subsMaxVarIndex s = case maximumMay (fst (unzip s)) of
                       Just x -> Just (x+1)
                       Nothing -> Nothing

sgoal = [Comb "mother" [Comb "john" [], Var 0]]
srule1 = Comb "mother" [Comb "john" [], Comb "jane" []] :- []
srule2 = Comb "mother" [Comb "darleen" [], Comb "jane" []] :- []
sprog = [srule1, srule2]

mygoal = [Comb "append" [Var 0, Var 1, Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "[]" []]]]]
myrule1 = Comb "append" [Comb "[]" [], Var 0, Var 0] :- []
myrule2 = Comb "append" [Comb "." [Var 0, Var 1], Var 2, Comb "." [Var 0, Var 3]] :- [Comb "append" [Var 1, Var 2, Var 3]]
myprog = [myrule1, myrule2]

grule1 = Comb "vater" [Comb "Hans" [], Comb "Peter" []] :- []
grule2 = Comb "vater" [Comb "Peter" [], Comb "Frank" []] :- []
grule3 = Comb "grossvater" [Var 0, Var 2] :- [Comb "vater" [Var 0, Var 1], Comb "vater" [Var 1, Var 2]]
gprog = [grule1, grule2, grule3]
ggoal = [Comb "grossvater" [Comb "Hans" [], Var 0]]
