module ResultSearch
  ( Strategy, dfs, bfs, solve
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
  dfs' (SLDTree _ x) s = concat (map (\(s2,st) -> dfs' st (compose s2 s)) x)

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

type Queue a = [a]
type SLDQueue = Queue (Subst, SLDTree)

push :: SLDQueue -> (Subst, SLDTree) -> SLDQueue
push q x = x:q

pop :: SLDQueue -> Maybe (SLDQueue, (Subst, SLDTree))
pop q | length q > 0 = Just (init q, last q)
      | otherwise = Nothing

bfs :: Strategy
bfs (SLDTree g n) = map (extractKVMatch (goalToVarList g)) (bfs' [([], (SLDTree g n))] [])
  where
  bfs' :: SLDQueue -> [Subst] -> [Subst]
  bfs' q s = case pop q of
             -- leaf, non-EOQ
             Just (q2, (sub, SLDTree st [])) -> bfs' q2 (s ++ [sub])
             -- non-leaf
             Just (q2, (sub, SLDTree _ n)) -> bfs' (foldl (\q2 (sub2, st) -> push q2 (compose sub2 sub, st)) q2 n) s
             -- EOQ
             Nothing -> s


solve :: Strategy -> Prog -> Goal -> [Subst]
solve s p g = s (sld p g)
