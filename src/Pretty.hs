import Type
import Data.Char (chr)

{-
------- TODO -------
 Kommata bei Aufzaehlung
 Spezielle Dartstellung fuer Listen
-}

class Pretty a where
  pretty :: a -> String

instance Pretty Term where
  pretty (Var v) = [chr (v + 65)]
  pretty (Comb functor []) = functor
  pretty (Comb functor terms) =
    functor ++ "(" ++ unwords(map pretty terms) ++ ")"

-- Test instance
test :: String
test = pretty (Comb "append" [ Var 0
                            , Comb "." [ Var 1
                                       , Var 2
                                       ]
                            , Comb "." [ Comb "1" []
                                       , Comb "." [ Comb "2" []
                                                  , Comb "[]" []
                                                  ]
                                       ]
                            ])
