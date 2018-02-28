{-# LANGUAGE TypeSynonymInstances #-}

module Type
  ( VarIndex, Term(..), Rule(..), Prog(..), Goal(..), Pretty(..)
  ) where

import Data.Char
import Data.List


class Pretty a where
  pretty :: a -> String

-- Alias type for variables
type VarIndex = Int

-- converts 0-indexed integers to variable names.
-- e.g. 0-> "A", 1 -> "B", 26 -> "A2", 27 -> "B2"
instance Pretty VarIndex where
  pretty i = if i < 26 then [chr(ord 'A' + i)]
                        else pretty (mod i 26) ++ show (1 + (quot i 26))

-- Data type for terms
data Term = Var VarIndex | Comb String [Term]
  deriving (Show, Eq)

instance Pretty Term where
    pretty (Var i) = pretty i
    pretty (Comb "[]" []) = "[]"
    -- wenn tail leere liste, nur head ausgeben
    pretty (Comb "." [h,Comb "[]" []]) = "[" ++ pretty h ++ "]"
    -- wenn der tail wieder eine liste ist, diese per komma appenden
    -- z.B. [2|[3|[4|A]]] == [2,3,4|A]
    pretty (Comb "." [h,Comb "." t]) = "[" ++ pretty h ++ "," ++ init (tail (pretty (Comb "." t))) ++ "]"
    pretty (Comb "." [h,t]) = "[" ++ pretty h ++ "|" ++ pretty t ++ "]"
    pretty (Comb s []) = s
    pretty (Comb s t) = s ++ "(" ++ intercalate ", " (map pretty t) ++ ")"

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
