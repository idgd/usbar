\begin{code}
module Main (main) where

import System.Environment (getArgs)
import System.FilePath

import Lib

main :: IO ()
main = do
  z <- getArgs
  if (length z) /= 1
  then do
    print "Please provide one .u file as an argument."
  else do
    let z' = head z
    u <- parse z' 
    let i = tangle u
    writeFile (takeBaseName z' ++ ".c") i
    let j = weave u
    writeFile (takeBaseName z' ++ ".tex") j
    return ()
\end{code}
