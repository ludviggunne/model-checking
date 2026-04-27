module Gen where

import Data.List

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Grammar

type Gen a = State [Sym] a

gen :: Grammar -> [Sym]
gen (_,prods) = execState (impl prods) []

impl :: [Prod] -> Gen ()
impl prods = do
    update <- or <$> mapM step prods
    when update $ impl prods

-- Perform a step on a single production,
-- provided that lhs is not already marked as generating
step :: Prod -> Gen Bool
step (lhs, rhs) = do
    m <- marked (V lhs)
    if m then return False
         else stepUnmarked (lhs, rhs)

-- Perform a step on a single production
stepUnmarked :: Prod -> Gen Bool
stepUnmarked (lhs, rhs) = do
    mapM_ markTerm rhs
    all_marked <- and <$> mapM marked rhs
    if all_marked
        then return True <* mark (V lhs)
        else return False

-- Mark symbol if it's a terminal
markTerm :: Sym -> Gen ()
markTerm t@(T _) = mark t
markTerm _ = return ()

-- Mark symbol as generating
mark :: Sym -> Gen ()
mark s = modify $ nub . (s:)

-- Check if a symbol is marked as generating
marked :: Sym -> Gen Bool
marked s = gets (s `elem`)
