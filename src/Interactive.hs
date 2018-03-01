{-# LANGUAGE ViewPatterns #-}

module Interactive (
main
) where

import System.Exit
import Type
import ResultSearch
import Data.List
import SLD
import Parser
import Substitution

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
    (stripPrefix ":load " -> Just file) -> interpretFile p s file
    (stripPrefix ":set " -> Just strat) -> setStrategy p s strat
    ":quit" -> return ()
    ":q" -> return ()
    goal -> executeGoal p s goal

executeGoal :: Prog -> Strategy -> String -> IO ()
executeGoal p s goal = case parseWithVars goal of
                         Left e -> do
                           putStrLn ("Invalid input: " ++ e)
                           work p s
                         Right (g,l) -> do
                           outputResult l (solve s p g)
                           work p s

outputResult :: [(VarIndex, String)] -> [Subst] -> IO ()
outputResult l [] = putStrLn "Das wars!! Mehr ist hier nicht zu holen."
outputResult l (x:xs) = do
  putStr ((prettyWithVars l x) ++ " ")
  i <- getChar
  putStrLn ""
  outputResult l xs

interpretFile :: Prog -> Strategy -> String -> IO ()
interpretFile p s file = do
                          result <- parseFile file
                          case result of
                            Left e -> do
                              putStrLn ("Invalid file: " ++ e)
                              work p s
                            Right p -> work p s

setStrategy :: Prog -> Strategy -> String -> IO ()
setStrategy p s strat =
  case strat of
    "bfs" -> do
      putStrLn "Strategy set to breadth-first search."
      work p bfs
    "dfs" -> do
      putStrLn "Strategy set to depth-first search."
      work p bfs
    _ -> do
      putStrLn "Valid strategies are bfs, dfs"
      work p s


info :: Prog -> IO ()
info p = putStrLn (unlines (nub (map getPredicate p)))

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
