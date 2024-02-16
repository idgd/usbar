\begin{code}

module Main (main) where
import System.Environment (getArgs)
import System.FilePath ((-<.>))

import Usbar

main :: IO ()
main = do
  a <- getArgs
  if (length a) /= 1
  then do
    putStrLn "Please provide one and only one .u file as an argument."
  else do
    let z = head a
    y <- parse z
    let x = tangle y
    writeFile (z -<.> "c") x
    let w = weave y
    writeFile (z -<.> "tex") w
    return ()
\end{code}
