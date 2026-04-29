module Main where

import System.IO
import System.Exit
import System.Environment

import Control.Monad

import DFA
import Graph
import Grammar
import Product
import Gen
import Derive

main = do
    (gpath:dpath:_) <- getArgs

    -- Parse inputs
    graph <- Graph.parse <$> readFile gpath
    dfa <- DFA.parse <$> readFile dpath

    -- Output graphviz files for inputs
    writeFile "graph.dot" $ Graph.dot graph
    writeFile "dfa.dot" $ DFA.dot $ compl dfa

    let -- Compute the cool production
        gram = graph `prod` compl dfa
        -- Compute the set of generating symbols
        gens = gen gram

    when (not $ (V startSym) `elem` gens) $ do
        -- Start symbol is not generating:
        --  the language is empty and the program is OK!
        exitSuccess

    let -- Reduce grammar to one with only generating productions
        reduced = gfilter (`elem` gens) gram
        -- Produce a counter example
        examp = derive reduced

    putStrLn $ unwords examp
    exitFailure
