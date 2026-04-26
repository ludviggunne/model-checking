module DFA where

import Data.List

import Control.Monad
import Control.Monad.Writer

import qualified Parser as P
import Parser hiding (Parser)

type DFA =
    ( String     -- Start state
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
parse source =
    let (start, accept, trans) = runParser plines source
     in (start, nub accept, trans)

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
    when start $ tell (source, mempty, mempty)
    when accept $ tell (mempty, [source], mempty)

    -- Parse label
    label <- skip "-" >> parseWhile (/= '-') <* skip "->"

    -- Parse destination state
    (dest, accept) <- pstate
    when accept $ tell (mempty, [dest], mempty)

    -- Write transition
    tell (mempty, mempty, [(source, dest, label)])

pstate :: Parser (String, Bool)
pstate = do
    accept <- fmap ('(' ==) $ peek <* eat
    state <- (parseWhile $ not . (`elem` "])")) <* eat
    return (state, accept)

dot :: DFA -> String
dot (start, accept, trans) = unlines $
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
        states = nub $ concatMap getStates trans
        getStates (source, dest, _) = [source, dest]
