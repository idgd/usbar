\begin{code}
module Main (main) where

import System.Environment (getArgs)

import Usbar

main :: IO ()
main = do
  a <- getArgs
  if (length a) /= 1
  then do
    putStrLn "Please provide one and only one .u file as an argument."
  else do
    let z = head a
    tangle z
    weave z
\end{code}
