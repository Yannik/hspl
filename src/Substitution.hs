import Type
import Data.List
import Data.Maybe

-- Data type for substitution
data Subst = Subst [(VarIndex, Term)]
  deriving Show

-- Returns an empty substitution
empty :: Subst
empty = Subst []

-- Returns a single substitution
single :: VarIndex -> Term -> Subst
single v t = Subst [(v, t)]

-- Applies a substitution on a term
apply :: Subst -> Term -> Term
apply (Subst ss) (Var v) =
  let ss' = unzip ss
  in (snd ss') !! (fromJust (elemIndex v (fst ss')))
    where
      fst (vs, _) = vs
      snd (_, ts) = ts
apply s (Comb str ts) = Comb str (map (apply s) ts)
apply (Subst []) t = t

testApply :: Term
testApply = apply (Subst [(0, (Comb "test" []))]) (Var 0)

{- TODO
-- Test instance
testApply :: String
testApply = apply (Subst [(0, (Comb "T1" [])), (1, (Comb "T2" [])), (2, (Comb "T3" []))])
                  (Comb "append" [ Var 0
                            , Comb "." [ Var 1
                                       , Var 2
                                       ]
                            , Comb "." [ Comb "1" []
                                       , Comb "." [ Comb "2" []
                                                  , Comb "[]" []
                                                  ]
                                       ]
                            ])
-}
