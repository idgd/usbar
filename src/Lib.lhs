\begin{code}
module Lib
  ( parse
  , tangle
  , weave
  ) where

import Data.Char (isSeparator)
import Data.List (isPrefixOf)
import System.FilePath (takeDirectory, (</>))

type Usbar = ([Command], [Line])
parse :: FilePath -> IO Usbar
parse a = do
  b <- readFile a
  c <- insert (line <$> lines b) a
  return $ (ordering . listing . source . comment) c
tangle :: Usbar -> String
tangle (b,a) = let z (Right (Source _ _)) = True
                   z _ = False
                   y = filter z a
                   x (Right (Source _ b)) = b
                   x _ = ""
                   w u (Order v) = head $ filter (zz v) u
                   zz b (Right (Source a _)) = a == b
                   zz b _ = False
                   yy = (x . (w y)) <$> b
               in concat yy

header :: String
header = unlines
  ["\\documentclass{book}"
  ,"\\usepackage[activate={true,nocompatibility}"
  ,",final"
  ,",tracking=true"
  ,",kerning=true"
  ,",spacing=true"
  ,",factor=1100"
  ,",stretch=10"
  ,",shrink=10"
  ,"]{microtype}"
  ,"\\microtypecontext{spacing=nonfrench}"
  ,"\\usepackage{listings}"
  ,"\\begin{document}"
  ]

weave :: Usbar -> String
weave (_, a) = header ++ weave' a
weave' :: [Line] -> String
weave' (Left a : b) = a ++ "\n" ++ weave' b
weave' (Right (Hierarch Part b) : c) = "\\part{" ++ b ++ "}\n" ++ weave' c
weave' (Right (Hierarch Chapter b) : c) = "\\chapter{" ++ b ++ "}\n" ++ weave' c
weave' (Right (Hierarch Section b) : c) = "\\section{" ++ b ++ "}\n" ++ weave' c
weave' (Right (Hierarch Subsection b) : c) = "\\subsection{" ++ b ++ "}\n" ++ weave' c
weave' (Right (Source a b) : c) = "\\begin{lstlisting}\n" ++ b ++ "\n\\end{lstlisting}\n" ++ weave' c
weave' (Right (Order a) : b) = "{\\tt" ++ a ++ "}\n" ++ weave' b
weave' (Right (Listing a b) : c) = "\\begin{verbatim}\n" ++ b ++ "\n\\end{verbatim}\n" ++ weave' c
weave' (Right (Insert a) : b) = weave' b
weave' (Right (Comment a) : b) = a ++ weave' b
weave' [] = "\\end{document}"


ordering :: [Line] -> ([Command], [Line])
ordering ((Right (Order a)):b) = let (y,z) = ordering b
                                 in (Order a : y, z)
ordering (a:b) = let (y,z) = ordering b
                 in (y, a : z)
ordering [a] = ([], [a])
ordering [] = ([], [])

listing' :: Command -> [Line] -> [Line]
listing' (Listing a b) (Left c:d) = listing' (Listing a (b ++ "\n" ++ c)) d
listing' (Listing a b) (_:c) = listing (Right (Listing a b) : c)
listing' _ _ = undefined
listing :: [Line] -> [Line]
listing ((Right (Listing a "")):b) = listing' (Listing a "") b
listing (a:b) = a : listing b
listing [a] = [a]
listing [] = []

source' :: Command -> [Line] -> [Line]
source' (Source a b) (Left c:d) = source' (Source a (b ++ "\n" ++ c)) d
source' (Source a b) (_:c) = source (Right (Source a b) : c)
source' _ _ = undefined
source :: [Line] -> [Line]
source ((Right (Source a "")):b) = source' (Source a "") b
source (a:b) = a : source b
source [a] = [a]
source [] = []

comment :: [Line] -> [Line]
comment a = comment' <$> a
comment' :: Line -> Line
comment' (Right (Comment a)) = Left a 
comment' a = a

type Line = Either String Command

data Rank = Part | Chapter | Section | Subsection
  deriving Show
type Title = String
type Content = String
data Command = Hierarch Rank Title
             | Source Title Content
             | Order Title
             | Listing Title Content
             | Insert FilePath
             | Comment Content
  deriving Show
instance Semigroup Command where
  _ <> _ = undefined

line :: String -> Line
line a = if isPrefixOf "%" a
         then Right $ command a
         else Left a

type Error = String
isInsert :: Line -> Bool
isInsert (Right (Insert _)) = True
isInsert _ = False
insertPath :: Line -> FilePath
insertPath (Right (Insert a)) = a
insertPath _ = ""
insert :: [Line] -> FilePath -> IO [Line]
insert a b =
  if any isInsert a
  then do
    let (c, d) = break isInsert a
    let z = insertPath (head d)
    let y = (takeDirectory b) </> z
    e <- readFile y 
    let f = line <$> lines e
    let g = c ++ f ++ (tail d)
    insert g b
  else return a

data Chunk = Human String | Computer String
chunks :: [Line] -> [Chunk]
chunks = undefined

strip :: String -> String
strip = dropWhile isSeparator

command :: String -> Command
command a = case a of
  ('%' : '!' : '!' : '!' : '!' : b) -> Hierarch Subsection $ strip b
  ('%' : '!' : '!' : '!' : b) -> Hierarch Section $ strip b
  ('%' : '!' : '!' : b) -> Hierarch Chapter $ strip b
  ('%' : '!' : b) -> Hierarch Part $ strip b
  ('%' : '@' : b) -> Source (strip b) ""
  ('%' : '#' : b) -> Order $ strip b
  ('%' : '$' : b) -> Listing (strip b) ""
  ('%' : '%' : b) -> Insert $ strip b
  _ -> Comment a
\end{code}
