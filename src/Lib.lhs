\begin{code}
module Lib
  ( parse
  , tangle
  , weave
  ) where

import Data.Char (isSeparator)
import Data.List (isPrefixOf)
import System.FilePath (takeDirectory, (</>))

type Title = String
type Content = String

data Rank = Part | Chapter | Section | Subsection
data Command = Hierarch Rank Title
             | Source Title Content
             | Order Title
             | Listing Title Content
             | Insert FilePath
             | Comment Content

type Line = Either String Command

type Usbar = ([Command], [Line])

parse :: FilePath -> IO Usbar
parse a = do
  b <- readFile a
  c <- insert (line <$> lines b) a
  return $ (ordering . listing . source . comment) c

tangle :: Usbar -> String
tangle (a,b) = let z (Right (Source _ _)) = True
                   z _ = False
                   y = filter z b
                   x (Right (Source _ w)) = w
                   x _ = ""
                   v u (Right (Source t _)) = u == t
                   v _ _ = False
                   s r (Order q) = head $ filter (v q) r
                   s _ _ = undefined
               in (x . (s y)) =<< a

header :: String
header = unlines
  ["\\documentclass[oneside]{book}"
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
weave' (Right (Hierarch Part b) : c) =
  "\\part{" ++ b ++ "}\n" ++ weave' c
weave' (Right (Hierarch Chapter b) : c) =
  "\\chapter{" ++ b ++ "}\n" ++ weave' c
weave' (Right (Hierarch Section b) : c) =
  "\\section{" ++ b ++ "}\n" ++ weave' c
weave' (Right (Hierarch Subsection b) : c) =
  "\\subsection{" ++ b ++ "}\n" ++ weave' c
weave' (Right (Source _ b) : c) = "\\begin{lstlisting}\n"
                               ++ b
                               ++ "\n\\end{lstlisting}\n"
                               ++ weave' c
weave' (Right (Order a) : b) =
  "\\noindent{\\tt " ++ a ++ "}\n\n" ++ weave' b
weave' (Right (Listing _ b) : c) = "\\begin{verbatim}\n"
                                ++ b
                                ++ "\n\\end{verbatim}\n"
                                ++ weave' c
weave' (Right (Insert _) : b) = weave' b
weave' (Right (Comment a) : b) = a ++ weave' b
weave' [] = "\\end{document}"

line :: String -> Line
line a = if isPrefixOf "%" a
         then Right $ command a
         else Left a

insert :: [Line] -> FilePath -> IO [Line]
insert a b = let z (Right (Insert _)) = True
                 z _ = False
                 y (Right (Insert x)) = x
                 y _ = ""
             in if any z a
             then do
               let (c, d) = break z a
               e <- readFile $ (takeDirectory b) </> y (head d)
               insert (c ++ (line <$> lines e) ++ (tail d)) b
             else return a

command :: String -> Command
command a = let z = dropWhile isSeparator
            in case a of
  ('%' : '!' : '!' : '!' : '!' : b) -> Hierarch Subsection $ z b
  ('%' : '!' : '!' : '!' : b) -> Hierarch Section $ z b
  ('%' : '!' : '!' : b) -> Hierarch Chapter $ z b
  ('%' : '!' : b) -> Hierarch Part $ z b
  ('%' : '@' : b) -> Source (z b) ""
  ('%' : '#' : b) -> Order $ z b
  ('%' : '$' : b) -> Listing (z b) ""
  ('%' : '%' : b) -> Insert $ z b
  _ -> Comment a

comment :: [Line] -> [Line]
comment a = comment' <$> a
comment' :: Line -> Line
comment' (Right (Comment a)) = Left a 
comment' a = a

source :: [Line] -> [Line]
source ((Right (Source a "")):b) = source' (Source a "") b
source (a:b) = a : source b
source [] = []
source' :: Command -> [Line] -> [Line]
source' (Source a b) (Left c:d) =
  source' (Source a (b ++ "\n" ++ c)) d
source' (Source a b) (_:c) = source (Right (Source a b) : c)
source' _ _ = undefined

listing :: [Line] -> [Line]
listing ((Right (Listing a "")):b) = listing' (Listing a "") b
listing (a:b) = a : listing b
listing [] = []
listing' :: Command -> [Line] -> [Line]
listing' (Listing a b) (Left c:d) =
  listing' (Listing a (b ++ "\n" ++ c)) d
listing' (Listing a b) (_:c) = listing (Right (Listing a b) : c)
listing' _ _ = undefined

ordering :: [Line] -> ([Command], [Line])
ordering ((Right (Order a)):b) =
  let (y,z) = ordering b
  in (Order a : y, (Right (Order a)) : z)
ordering (a:b) = let (y,z) = ordering b
                 in (y, a : z)
ordering [] = ([], [])
\end{code}
