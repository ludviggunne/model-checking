module Generating where

import Data.List

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Grammar

type Generating a = ReaderT [Prod] (State [Sym]) a

gen :: Grammar -> [Sym]
gen (_,prods) = execState (runReaderT impl prods) []

impl :: Generating ()
impl = do
    prods <- ask
    update <- or <$> mapM step prods
    when update impl

step :: Prod -> Generating Bool
step (lhs, rhs) = do
    m <- marked (V lhs)
    if m then return False
         else stepUnmarked (lhs, rhs)

stepUnmarked :: Prod -> Generating Bool
stepUnmarked (lhs, rhs) = do
    mapM markTerm rhs
    all_marked <- and <$> mapM marked rhs
    if all_marked
        then mark (V lhs) >> return True
        else return False

markTerm :: Sym -> Generating ()
markTerm t@(T _) = mark t
markTerm _ = return ()

mark :: Sym -> Generating ()
mark s = modify $ nub . (s:)

marked :: Sym -> Generating Bool
marked s = gets (s `elem`)
