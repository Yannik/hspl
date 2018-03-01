module Interactive (

) where

import System.Exit
import Type
import ResultSearch
import Data.List
import SLD

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
  if i /= ":quit" then do
    case i of
      ":help" -> printHelp
      ":info" -> info p
  --  ":load" ->
  --  ":set" ->
    work p s
  else return ()

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
