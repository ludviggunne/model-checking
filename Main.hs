module Main where

import System.IO
import System.Exit
import System.Environment

import Text.Printf

import Control.Monad
import Control.Exception

import DFA
import Graph
import Grammar
import Product
import Gen
import Derive

main = do
    (gpath,dpath,opts) <- do
        args <- getArgs
        let isOpt ('-':_) = True
            isOpt _       = False
            paths         = filter (not . isOpt) args
            opts          = concat $ map (filter (/= '-')) $ filter isOpt args
        case paths of
            [gpath,dpath] -> return (gpath, dpath, opts)
            _             -> do
                putStrLn "Usage: ./check [OPTION...] GRAPH DFA"
                exitFailure

    let haveOpt = (`elem` opts)
        render  = haveOpt 'r'
        verbose = haveOpt 'v'

    -- Parse inputs
    graph <- Graph.parse <$> readFile gpath
    dfa   <- DFA.parse   <$> readFile dpath

    -- Output graphviz files for inputs
    when render $ do
        let output path content = do
            { when verbose $ putStrLn path
            ; writeFile path content }
        output (gpath <> ".dot") $ Graph.dot graph
        output (dpath <> ".dot") $ DFA.dot dfa

    let -- Compute the cool production
        gram = graph `prod` compl dfa
        -- Compute the set of generating symbols
        gens = gen gram

    -- putStr $ gstr gram

    when (not $ (V startSym) `elem` gens) $ do
        -- Start symbol is not generating: the language is empty and the program is OK!
        putStrLn "ok!"
        exitSuccess

    let -- Reduce grammar to one with only generating productions
        reduced = gfilter (`elem` gens) gram
        -- Produce a counter example
        examp = derive reduced

    putStrLn $ "bad: " <> unwords examp
    exitFailure

-- main = do
--     (path:_) <- getArgs
--     dfa@(states,start,accept,_) <- DFA.parse <$> readFile path
--     string <- words <$> getContents
--     putStrLn $ "accepting " <> (unwords accept)
--     if runDFA dfa string
--         then putStrLn "ok!"
--         else putStrLn "bad!"
