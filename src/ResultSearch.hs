module ResultSearch
  ( ds
  ) where

import SLD
import Type
import Substitution
import Unification
import Data.Maybe

type Strategy = SLDTree -> [Subst]



dfs :: Strategy
dfs (SLDTree g n) = map (extractKVMatch (goalToVarList g)) (dfs' (SLDTree g n) [])
  where
  dfs' :: SLDTree -> Subst -> [Subst]
  dfs' (SLDTree [] []) s = [s]
  dfs' (SLDTree _ []) s = []
  dfs' (SLDTree g x) s = concat (map (\(s2,st) -> dfs' st (compose s2 s)) x)

extractKVMatch :: (Eq a) => [a] -> [(a,b)] -> [(a,b)]
extractKVMatch a kvs = mapMaybe (\k -> lookupKV k kvs) a

lookupKV :: (Eq a) => a -> [(a, b)] -> Maybe (a,b)
lookupKV key ((x,y):xys)
  | key == x          =  Just (x,y)
  | otherwise         =  lookupKV key xys


goalToVarList :: Goal -> [VarIndex]
goalToVarList g = concatMap termToVarList g

termToVarList :: Term -> [VarIndex]
termToVarList (Var v) = [v]
termToVarList (Comb _ t) = concatMap termToVarList t


--bfs :: Strategy

solve :: Strategy -> Prog -> Goal -> [Subst]
solve s p g = s (sld p g)
