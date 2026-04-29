module Product where

import Data.List
import Data.Maybe
import Data.Foldable

import DFA
import Graph
import Grammar

startSym :: NonTerm
startSym = ("the", "start", "state")

-- Compute the product of a flow graph and a DFA
prod :: Graph -> DFA -> Grammar
prod graph dfa = (startSym, nub $ makeProds graph dfa)

makeProds :: Graph -> DFA -> [Prod]
makeProds = fold
    -- These are the five steps in the lab description
    [ fromFinalStates
    , fromTransferEdges
    , fromCallEdges
    , fromReturnNodes
    , fromTransitions ]

getEntry :: String -> [Node] -> Maybe String
getEntry meth nodes = getName <$> find pred nodes
    where
        pred (_, meth', type_) = meth' == meth && type_ == "entry"
        getName (name,_,_) = name

epsOrUndefined :: [Node] -> Edge -> Bool
epsOrUndefined _ (_,_,"eps") = True
epsOrUndefined nodes (_,_,meth) = not $ isJust $ getEntry meth nodes

-- 1
fromFinalStates :: Graph -> DFA -> [Prod]
fromFinalStates (nodes,_) (_,start,accept,_) = map make accept
    where
        make state = (startSym, [V (start,main_entry,state)])
        main_entry = fromMaybe err $ getEntry "main" nodes
        err = error "no entry for method main"

-- 2
fromTransferEdges :: Graph -> DFA -> [Prod]
fromTransferEdges (nodes,edges) (states,_,_,_) = map make pairs
    where
        pairs = [(e,s) | e <- eps_edges, s <- seqs]
        seqs = [(a,b) | a <- states, b <- states]
        eps_edges = filter (epsOrUndefined nodes) edges
        make ((src,dst,_), (a,b)) = ((a,src,b), [V (a,dst,b)])

-- 3
fromCallEdges :: Graph -> DFA -> [Prod]
fromCallEdges (nodes,edges) (states,_,_,_) = map make pairs
    where
        pairs = [(e,s) | e <- call_edges, s <- seqs]
        seqs = [(a,b,c,d) | a<-states, b<-states, c<-states, d<-states]
        call_edges = filter (not . epsOrUndefined nodes) edges
        make ((src,dst,meth), (a,b,c,d)) = (lhs,rhs)
            where
                lhs = (a,src,d)
                rhs = map V [rhs_1, rhs_2, rhs_3]
                rhs_1 = (a,meth,b)
                rhs_2 = (b,entry,c)
                rhs_3 = (c,dst,d)
                entry = fromMaybe undefined $ getEntry meth nodes

-- 4
fromReturnNodes :: Graph -> DFA -> [Prod]
fromReturnNodes (nodes,_) (states,_,_,_) = map make pairs
    where
        pairs = [(n,s) | n <- ret_nodes, s <- states]
        ret_nodes = filter ret_only nodes
        ret_only (_,_,"ret") = True
        ret_only _ = False
        make ((name,_,_), state) = ((state,name,state), [])

-- 5
fromTransitions :: Graph -> DFA -> [Prod]
fromTransitions _ (_,_,_,trans) = map make trans
    where
        make (src,dst,sym) = ((src,sym,dst), rhs sym)
        rhs "eps" = []
        rhs sym = [T sym]
