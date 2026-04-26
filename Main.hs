module Main where

import System.IO
import System.Environment

import DFA

main = do
    (path:_) <- getArgs
    dfa <- DFA.parse <$> readFile path
    putStr $ DFA.dot dfa
