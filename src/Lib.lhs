\begin{code}
module Lib
  ( commands
  ) where

import Data.Char
import Data.List (isPrefixOf, group)
\end{code}

Step 1: split by lines.
Step 2: find all inputs, and add them to the string.
Step 3: split by chunks.

Commands are \%!, \%\@, \%\#, \%\$, or \%\%.
\begin{itemize}
\item New section; one ! means part, two means chapter, three section, four subsection.
\item New C chunk
\item New formatted chunk
\item Insert file
\item Order C chunks
\end{itemize}

\begin{code}
data Section = Part String
             | Chapter String
             | Section String
             | Subsection String
  deriving Show
data Command = Chunk Section
             | Source String
             | Formatted String
             | Insert FilePath
             | Order String
             | Plain String
  deriving Show
type Cmd = Either String Command

commands :: String -> IO String
commands a = do
  let b = lines a
  let c = start <$> b
  print c
  return a
  where
    name :: Int -> String -> String
    name n a' = dropWhile isSeparator $ drop n a'
    start :: String -> Cmd
    start a' = if (isPrefixOf "%" a') && (length a' > 1) 
      then cmd a'
      else Right $ Plain a'
    cmd :: String -> Cmd
    cmd a' = case take 2 a' of
      "%!" -> section a' 
      "%@" -> source a' 
      "%#" -> formatted a'
      "%$" -> Right $ Insert $ drop 2 a'
      "%%" -> order a'
      "% " -> Right $ Plain a'
      _ -> Left "This is not a command"
    section :: String -> Cmd
    section a' = case length ((group a') !! 2) of
      1 -> Right $ Chunk $ Part $ name 2 a'
      2 -> Right $ Chunk $ Chapter $ name 3 a'
      3 -> Right $ Chunk $ Section $ name 4 a'
      4 -> Right $ Chunk $ Subsection $ name 5 a'
      _ -> Left "Section commands require a title."
    source :: String -> Cmd
    source a' = case name 2 a' of
      "" -> Left "C Source require a title."
      _ -> Right $ Source $ name 2 a'
    formatted :: String -> Cmd
    formatted a' = case name 2 a' of
      "" -> Left "C formatting requers a title"
      _ -> Right $ Formatted $ name 2 a'
    order :: String -> Cmd
    order a' = case name 2 a' of
      "" -> Left "Undefined ordering"
      _ -> Right $ Order $ name 2 a'
\end{code}
