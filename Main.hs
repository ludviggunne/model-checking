module Main where

import System.IO
import System.Environment

import DFA
import Graph
import Grammar
import Product

main = do
    (gpath:dpath:_) <- getArgs
    graph <- Graph.parse <$> readFile gpath
    dfa <- DFA.parse <$> readFile dpath
    let gram = graph `prod` dfa
    putStr $ gramStr gram
