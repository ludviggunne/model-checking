module Graph where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative

import qualified Parser as P
import Parser hiding (Parser)

type Node =
    ( String -- Name
    , String -- Method
    , String -- Type
    )

type Edge =
    ( String -- Source
    , String -- Destination
    , String -- Label
    )

type Graph = ([Node], [Edge])

type Parser a = P.Parser Graph a

parse :: String -> Graph
parse = runParser plines

plines :: Parser ()
plines = do
    prefix <- word
    case prefix of
        "node" -> pnode >> plines
        "edge" -> pedge >> plines
        ""     -> return ()
        w      -> error $ "unexpected " <> w

pnode :: Parser ()
pnode = do
    name  <- word
    meth  <- pmeth
    type_ <- ptype
    let node = (name, meth, type_)
    tell ([node], [])

pmeth :: Parser String
pmeth = do
    trim
    skip "meth("
    meth <- parseWhile (/= ')')
    skip ")"
    return meth

ptype :: Parser String
ptype = do
    saved <- get
    type_ <- word
    if type_ `elem` ["ret", "entry"]
        then return type_
        else put saved >> return ""

pedge :: Parser ()
pedge = liftA3 (,,) word word word >>= \e -> tell ([], [e])

dot :: Graph -> String
dot (nodes, edges) = unlines $
    pre <> nlines <> elines <> post
    where
        pre = [ "digraph {" ]
        post = [ "}" ]
        nlines = (flip map) nodes $ \(name, _, type_) ->
            case type_ of
                "ret"   -> "\t\"" <> name <> "\"\t[shape=rect]"
                "entry" -> "\t\"" <> name <> "\"\t[shape=doublecircle]"
                _       -> "\t\"" <> name <> "\"\t[shape=circle]"
        elines = (flip map) edges $ \(source, dest, label) ->
            "\t\"" <> source <> "\" -> \"" <> dest <> "\" "
                <> "\t[label=\"" <> rename label <> "\"]"
        rename "eps" = "&epsilon;"
        rename s = s
