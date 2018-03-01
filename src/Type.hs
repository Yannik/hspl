{-# LANGUAGE TypeSynonymInstances #-}

module Type
  ( VarIndex, Term(..), Rule(..), Prog(..), Goal(..), Pretty(..)
  ) where

import Data.Char
import Data.List


class Pretty a where
  pretty :: a -> String
  prettyWithVars :: [(VarIndex, String)] -> a -> String

-- Alias type for variables
type VarIndex = Int

-- converts 0-indexed integers to variable names.
-- e.g. 0-> "A", 1 -> "B", 26 -> "A2", 27 -> "B2"
instance Pretty VarIndex where
  pretty i = prettyWithVars [] i
  prettyWithVars l i = case lookup i l of
                        Just s -> s
                        Nothing -> if i < 26 then [chr(ord 'A' + i)]
                                             else pretty (mod i 26) ++ show (1 + (quot i 26))

-- Data type for terms
data Term = Var VarIndex | Comb String [Term]
  deriving (Show,Eq)

instance Pretty Term where
    pretty i = prettyWithVars [] i
    prettyWithVars l (Var i) = prettyWithVars l i
    -- wenn tail leere liste, nur head ausgeben
    prettyWithVars l (Comb "." [h,Comb "[]" []]) = "[" ++ prettyWithVars l h ++ "]"
    -- wenn der tail wieder eine liste ist, diese per komma appenden
    -- z.B. [2|[3|[4|A]]] == [2,3,4|A]
    prettyWithVars l (Comb "." [h,Comb "." t]) = "[" ++ prettyWithVars l h ++ "," ++ init (tail (prettyWithVars l (Comb "." t))) ++ "]"
    prettyWithVars l (Comb "." [h,t]) = "[" ++ prettyWithVars l h ++ "|" ++ prettyWithVars l t ++ "]"
    prettyWithVars l (Comb s []) = s
    prettyWithVars l (Comb s t) = s ++ "(" ++ intercalate ", " (map (prettyWithVars l) t) ++ ")"

-- Data type for program rules
data Rule = Term :- [Term]
  deriving Show

-- Data type for programs
type Prog = [Rule]

-- Data type for goals
type Goal = [Term]

-- Test instance
testPretty :: String
testPretty = pretty (Comb "append" [ Var 0
                            , Comb "." [ Var 1
                                       , Var 2
                                       ]
                            , Comb "." [ Comb "1" []
                                       , Comb "." [ Comb "2" []
                                                  , Comb "[]" []
                                                  ]
                                       ]
                            ])
