module Product where

import Data.List
import Data.Foldable

import DFA
import Graph
import Grammar

startSym :: NonTerm
startSym = ("the", "start", "state")

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

getEntry :: String -> [Node] -> String
getEntry meth nodes = unwrap $ find pred nodes
    where
        pred (_, meth', type_) = meth' == meth && type_ == "entry"
        unwrap (Just (name,_,_)) = name
        unwrap Nothing = error $ "no entry for " <> meth

epsEdge :: Edge -> Bool
epsEdge (_,_,"eps") = True
epsEdge _ = False

-- 1
fromFinalStates :: Graph -> DFA -> [Prod]
fromFinalStates (nodes,_) (_,start,accept,_) = map make accept
    where
        make state = (startSym, [V (start,main_entry,state)])
        main_entry = getEntry "main" nodes

-- 2
fromTransferEdges :: Graph -> DFA -> [Prod]
fromTransferEdges (_,edges) (states,_,_,_) = map make pairs
    where
        pairs = [(e,s) | e <- eps_edges, s <- seqs]
        seqs = [(a,b) | a <- states, b <- states]
        eps_edges = filter epsEdge edges
        make ((src,dst,_), (a,b)) = ((a,src,b), [V (a,dst,b)])

-- 3
fromCallEdges :: Graph -> DFA -> [Prod]
fromCallEdges (nodes,edges) (states,_,_,_) = map make pairs
    where
        pairs = [(e,s) | e <- call_edges, s <- seqs]
        seqs = [(a,b,c,d) | a<-states, b<-states, c<-states, d<-states]
        call_edges = filter (not . epsEdge) edges
        make ((src,dst,meth), (a,b,c,d)) = (lhs,rhs)
            where
                lhs = (a,src,d)
                rhs = [V rhs_1, V rhs_2, V rhs_3]
                rhs_1 = (a,meth,b)
                rhs_2 = (b,entry,c)
                rhs_3 = (c,dst,d)
                -- TODO: handle library methods that have no entry nodes
                entry = getEntry meth nodes

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
        make (src,dst,sym) = ((src,sym,dst), [T sym])
