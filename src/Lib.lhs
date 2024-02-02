\begin{code}
module Lib
  ( parse
  , tangle
  , weave
  ) where

import Data.Char
import Data.List (isPrefixOf)
import System.FilePath
\end{code}

The input is split into lines.
Each line can start with text, or a \% command.
The \% commands are:
\begin{itemize}
\item \%! :: Heirarch; each subsequent exclamation point decreases the rank of the heirarch, up to four.
\item \%\@ :: C source; these \emph{must} come in pairs, with the first having a title, and the second standing alone.
\item \%\# :: C ordering; these \emph{must} correspond with exactly one C source title, and reorder source blocks into a tangled C file.
\item \%\$ :: C listing; these follow the same rules as source, but are not inserted into a tangle.
\item \%\% :: Insert another file verbatim. Recurses, so any new files will be able to import files of their own, and so on.
\end{itemize}

\begin{code}
parse :: FilePath -> IO ()
parse a = do
  b <- readFile a
  let c = line <$> lines b
  insert c a
  return ()

type Line = Either String Command
data Rank = Part | Chapter | Section | Subsection
  deriving Show
type Title = String
type Content = String
data Command = Heirarch Rank Title
             | Source Title
             | Order Title
             | Listing Title
             | Insert FilePath
             | Comment Content
  deriving Show

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
    mapM_ print g
    insert g b
  else return a

data Chunk = Human String | Computer String
chunks :: [Line] -> [Chunk]
chunks = undefined

strip :: String -> String
strip = dropWhile isSeparator

command :: String -> Command
command a = case a of
  ('%' : '!' : '!' : '!' : '!' : b) -> Heirarch Subsection $ strip b
  ('%' : '!' : '!' : '!' : b) -> Heirarch Section $ strip b
  ('%' : '!' : '!' : b) -> Heirarch Chapter $ strip b
  ('%' : '!' : b) -> Heirarch Part $ strip b
  ('%' : '@' : b) -> Source $ strip b
  ('%' : '#' : b) -> Order $ strip b
  ('%' : '$' : b) -> Listing $ strip b
  ('%' : '%' : b) -> Insert $ strip b
  _ -> Comment a
\end{code}

\begin{code}
tangle = undefined
\end{code}

\begin{code}
weave = undefined
\end{code}
