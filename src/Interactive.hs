{-# LANGUAGE ViewPatterns #-}

module Interactive (

) where

import System.Exit
import Type
import ResultSearch
import Data.List
import SLD
import Parser

main :: IO ()
main = do
  putStrLn "Welcome to Simple Prolog!"
  putStrLn "Type \":help\" for help."
  work [] dfs
  putStrLn "Bye"

work :: Prog -> Strategy -> IO ()
work p s = do
  putStr "?- "
  i <- getLine
  case i of
    ":help" -> do
      printHelp
      work p s
    ":info" -> do
      info p
      work p s
    (stripPrefix ":load " -> Just file) -> parse s
    (stripPrefix ":set " -> Just strat) -> case strat of
      "bfs" -> work p bfs
      "dfs" -> work p dfs
      otherwise -> work p s
    ":quit" -> return ()
    --goal -> parse


info :: Prog -> IO ()
info p = do
  putStrLn (unlines (nub (map getPredicate p)))

getPredicate :: Rule -> String
getPredicate (Comb s n :- _) = s ++ "/" ++ show (length n)
getPredicate _ = ""

printHelp :: IO ()
printHelp = do
  putStrLn "Commands available from the prompt:"
  putStrLn "<goal>        Solves/proves the specified goal."
  putStrLn ":help         Shows this help message."
  putStrLn ":info         Shows all available predicates."
  putStrLn ":load <file>  Loads the specified file."
  putStrLn ":quit         Exits the interactive environment."
  putStrLn ":set <strat>  Sets the specified search strategy"
  putStrLn "              where <strat> is one of 'dfs' or 'bfs'."
