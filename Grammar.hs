module Grammar where

type Term = String

type NonTerm = (String, String, String)

data Sym = T Term | V NonTerm deriving (Eq)

type Prod = (NonTerm, [Sym])

type Grammar = (NonTerm, [Prod])

showNT :: NonTerm -> String
showNT (a,b,c) = "[" <> a <> " " <> b <> " " <> c <> "]"

instance Show Sym where
    show (V v) = showNT v
    show (T t) = show t

gramStr :: Grammar -> String
gramStr (_,prods) = unlines $ map str prods
    where str (lhs,rhs) = showNT lhs <> " -> " <> (unwords $ map show rhs)

gfilter :: (Sym -> Bool) -> Grammar -> Grammar
gfilter sym_pred (start, prods) = (start, filter prod_pred prods)
    where prod_pred (lhs, rhs) = all sym_pred $ (V lhs) : rhs
