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
        rhs :: [Sym]
        rhs = snd $ head ok_prods

        -- Productions with rhs that doesn't have a visited non-terminal
        ok_prods :: [Prod]
        ok_prods = filter (all (not . visited) . snd) all_prods

        -- All productions with lhs matching current non-terminal
        all_prods :: [Prod]
        all_prods = filter ((== lhs) . fst) prods

        visited :: Sym -> Bool
        visited (T _) = False
        visited (V n) = n `elem` v
