module DFA (
    DFA (..),
    DFA_Tran (..),
    compl,
    parse,
    dot,
    runDFA,
) where

import Data.List
import Data.Maybe

import Control.Monad
import Control.Monad.Writer

import qualified Parser as P
import Parser hiding (Parser)

type DFA =
    ( [String]   -- States
    , String     -- Start state
    , [String]   -- Accepting states
    , [DFA_Tran] -- Transitions
    )

type DFA_Tran =
    ( String -- Source state
    , String -- Destination state
    , String -- Label
    )

type Parser a = P.Parser DFA a

parse :: String -> DFA
parse = dedup . runParser plines

dedup :: DFA -> DFA
dedup (q, s, f, e) = (nub q, s, nub f, e)

compl :: DFA -> DFA
compl (q, s, f, e) = (q, s, filter (not . (`elem` f)) q, e)

plines :: Parser ()
plines = do
    pline >> trim
    done <- done
    when (not done) plines

pline :: Parser ()
pline = do
    -- Parse source state
    start <- skipOpt "=>"
    (source, accept) <- pstate
    when start $ tell ([], source, [], [])
    when accept $ tell ([], "", [source], [])
    tell ([source], "", [], [])

    -- Parse label
    label <- skip "-" >> parseWhile (/= '-') <* skip "->"

    -- Parse destination state
    (dest, accept) <- pstate
    when accept $ tell ([], "", [dest], [])
    tell ([dest], "", [], [])

    -- Write transition
    tell ([], "", [], [(source, dest, label)])

pstate :: Parser (String, Bool)
pstate = do
    accept <- fmap ('(' ==) $ peek <* eat
    state <- (parseWhile $ not . (`elem` "])")) <* eat
    return (state, accept)

dot :: DFA -> String
dot (states, start, accept, trans) = unlines $
    pre <> nodes <> edges <> post
    where
        pre = [ "digraph {"
              , "\t__start\t[shape=none,label=\"\"]"
              , "\t__start\t-> \"" <> start <> "\" // start state"
              ]
        post = [ "}" ]
        nodes = (flip map) states $ \state ->
            if state `elem` accept
                then "\t\"" <> state <> "\"\t[shape=doublecircle] // accepting"
                else "\t\"" <> state <> "\"\t[shape=circle]"
        edges = (flip map) trans $ \(source, dest, label) ->
            "\t\"" <> source <> "\"\t-> \"" <> dest <> "\"\t"
                <> "[label=\"" <> label <> "\"]"
        getStates (source, dest, _) = [source, dest]

delta :: [DFA_Tran] -> String -> [String] -> String
delta _ state [] = state
delta trans state (first:rest) = delta trans new_state rest
    where
        (_,new_state,_) = fromMaybe err $ find pred trans
        pred (src,_,label) = src == state && label == first
        err = error $ "missing transition in DFA: state=" <> state <>
                      ", symbol=" <> first

runDFA :: DFA -> [String] -> Bool
runDFA (_,start,accept,trans) string = state `elem` accept
    where state = delta trans start string
