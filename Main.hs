module Main where

import System.IO
import System.Environment

import DFA
import Graph

main = do
    (path:_) <- getArgs
    graph <- Graph.parse <$> readFile path
    putStr $ Graph.dot graph
    -- dfa <- DFA.parse <$> readFile path
    -- putStr $ DFA.dot dfa
