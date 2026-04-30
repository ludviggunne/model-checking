{-# LANGUAGE LambdaCase #-}

module Parser where

import Data.List

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

type Parser m a = StateT String (Writer m) a

runParser :: Monoid m => Parser m () -> String -> m
runParser p = execWriter . evalStateT p

peek :: Monoid m => Parser m Char
peek = gets head

eat :: Monoid m => Parser m ()
eat = modify tail

done :: Monoid m => Parser m Bool
done = gets null

skipOpt :: Monoid m => String -> Parser m Bool
skipOpt prefix = stripPrefix prefix <$> get >>= \case
    Nothing -> return False
    Just src -> put src >> return True

skip :: Monoid m => String -> Parser m ()
skip prefix = skipOpt prefix >>= \result ->
    when (not result) $ perror $ "expected " <> prefix

space :: Char -> Bool
space c = c == ' ' || c == '\n'

parseWhile :: Monoid m => (Char -> Bool) -> Parser m String
parseWhile pred = get >>= \case
    "" -> return ""
    (c:_) -> if pred c
        then eat >> (c:) <$> parseWhile pred
        else return ""

trim :: Monoid m => Parser m ()
trim = void $ parseWhile space

word :: Monoid m => Parser m String
word = trim >> parseWhile (not . space)

perror :: Monoid m => String -> Parser m ()
perror str = do
    left <- take 20 <$> get
    error $ str <> ": " <> left <> " <<<"
