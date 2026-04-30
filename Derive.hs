module Derive (derive) where

import Grammar

-- Generate a string from a grammar.
-- All productions must be generating.
derive :: Grammar -> [Term]
derive (start, prods) = impl [start] (V start)
  where
    impl :: [NonTerm]  -- Visited non-terminals (higher up in parse tree)
         -> Sym        -- Symbol to expand
         -> [Term]     -- Resulting string
    impl _ (T t) = [t]
    impl v (V lhs) = concatMap (impl (lhs:v)) rhs
      where
        (rhs:_) = filter (all (not . visited)) $ prodFn lhs

        visited (T _) = False
        visited (V n) = n `elem` v

    prodFn = foldMap mkProdFn prods

mkProdFn :: Prod -> NonTerm -> [[Sym]]
mkProdFn (lhs, rhs) = \lhs' -> if lhs == lhs' then [rhs] else []
