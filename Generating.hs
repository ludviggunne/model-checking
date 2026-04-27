module Generating where

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

step :: Prod -> Gen Bool
step (lhs, rhs) = do
    m <- marked (V lhs)
    if m then return False
         else stepUnmarked (lhs, rhs)

stepUnmarked :: Prod -> Gen Bool
stepUnmarked (lhs, rhs) = do
    mapM markTerm rhs
    all_marked <- and <$> mapM marked rhs
    if all_marked
        then mark (V lhs) >> return True
        else return False

markTerm :: Sym -> Gen ()
markTerm t@(T _) = mark t
markTerm _ = return ()

mark :: Sym -> Gen ()
mark s = modify $ nub . (s:)

marked :: Sym -> Gen Bool
marked s = gets (s `elem`)
