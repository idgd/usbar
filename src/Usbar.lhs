\documentclass{article}
\usepackage[activate={true,nocompatibility}
           ,final
           ,tracking=true
           ,kerning=true
           ,spacing=true
           ,factor=1100
           ,stretch=10
           ,shrink=10
           ]{microtype}
\microtypecontext{spacing=nonfrench}
%include polycode.fmt
\begin{document}

\begin{code}
module Usbar
  ( parse
  , tangle
  , weave
  ) where
\end{code}

\begin{code}
import Data.Char (isSeparator)
import System.FilePath (takeDirectory, (</>))
\end{code}

\begin{code}
parse :: FilePath -> IO String
parse a = readFile a >>= c' . lines
  where
    a' b = takeDirectory a </> strip b
    b' ('%':'%':b) = readFile (a' b) >>= return . lines
    b' b = return [b]
    c' b = concat <$> mapM b' b >>= \z ->
      if (any (\x -> take 2 x == "%%") z)
      then c' z
      else return $ unlines z
\end{code}

\begin{code}
strip :: String -> String
strip = dropWhile (\a -> isSeparator a || (a == '\n') || (a == '\t'))
\end{code}

\begin{code}
tangle :: String -> String
tangle = reorder . source
\end{code}

\begin{code}
source :: String -> String
source = a' . lines
  where
    a' :: [String] -> String
    a' (('%':'@':b):c) = "%@" ++ b ++ "\n" ++ b' c
    a' (('%':'#':b):c) = "%#" ++ b ++ "\n" ++ a' c
    a' (_:b) = a' b
    a' _ = []

    b' (('%':'@':_):b) = "%@\n" ++ a' b
    b' (b:c) = b ++ "\n" ++ b' c
    b' _ = error "Error: Reached end of source \
                 \file inside a source block."
\end{code}

\begin{code}
reorder :: String -> String
reorder a = c' . b' $ a' a
  where
    a' = z' 0
    z' b ('%':'@':c) = z' (b + countCodeBlock c) (skipCodeBlock c)
    z' b ('%':'#':c) = b : z' (b + 2 + countTitle c) (skipTitle c)
    z' b (_:c) = z' (b + 1) c
    z' _ [] = []

    b' b = let d = getTitle . drop 2 . snd . flip splitAt a <$> b
             in foldr y' "" d
    y' b c = x' a b ++ c
    x' ('%':'@':b) c =  if (getTitle b == c)
                        then strip $ getCodeBlock b
                        else x' (skipCodeBlock b) c
    x' (_:b) c = x' b c
    x' _ b = error "Error: Could not locate an ordering's \
                   \corresponding source block: " ++ b

    c' b = let (c,d) = splitAt (w' b 0) b
               e = x' a (getTitle . drop 2 $ d)
               f = c ++ e ++ (strip . skipTitle . drop 2 $ d)
           in if (v' f) then c' f else f
    w' ('%':'#':_) b = b
    w' (_:b) c = w' b (c + 1)
    w' _ _ = error "Error: Could not locate an ordering."
    v' ('%':'#':_) = True
    v' (_:b) = v' b
    v' _ = False
\end{code}

\begin{code}
eol :: Char -> Bool
eol = not . ((==) '\n')
\end{code}

\begin{code}
getTitle :: String -> String
getTitle = (takeWhile eol) . strip
skipTitle :: String -> String
skipTitle = (dropWhile eol) . strip
countTitle :: String -> Int
countTitle = length . takeWhile eol
\end{code}

\begin{code}
codeBlock :: String -> String
codeBlock ('%':'@':_) = ""
codeBlock (b:c) = b : codeBlock c
codeBlock _ = ""
getCodeBlock :: String -> String
getCodeBlock = codeBlock . skipTitle
skipCodeBlock :: String -> String
skipCodeBlock = a' . skipTitle
  where a' ('%':'@':b) = b
        a' (_:b) = a' b
        a' _ = ""
countCodeBlock :: String -> Int
countCodeBlock = ((+) 4) . length . codeBlock
\end{code}

\begin{code}
listingBlock :: String -> String
listingBlock ('%':'!':_) = ""
listingBlock (b:c) = b : listingBlock c
listingBlock _ = ""
getListingBlock :: String -> String
getListingBlock = listingBlock . skipTitle
skipListingBlock :: String -> String
skipListingBlock = a' . skipTitle
  where a' ('%':'!':b) = b
        a' (_:b) = a' b
        a' _ = ""
\end{code}

\begin{code}
grabLine :: String -> String
grabLine ('\n':_) = "\n"
grabLine (b:c) = b : grabLine c
grabLine [] = ""
skipLine :: String -> String
skipLine ('\n':b) = b
skipLine (_:b) = skipLine b
skipLine [] = ""
\end{code}

\begin{code}
replaceOrdering :: String -> String
replaceOrdering ('%':'#':a) = "// " ++ (strip $ replaceOrdering a)
replaceOrdering (a:b) = a : replaceOrdering b
replaceOrdering [] = ""
\end{code}

\begin{code}
weave :: String -> String
weave = concat . (fmap weaved) . loom
\end{code}

\begin{code}
data Weave = Code String String
           | Ordering String
           | Weaved String
\end{code}

\begin{code}
loom :: String -> [Weave]
loom ('%':'!':b)  =  (Code (getTitle b) (getListingBlock b))
                  :  (loom $ skipListingBlock b)
loom ('%':'@':b)  =  (Code (getTitle b) (replaceOrdering $ getCodeBlock b))
                  :  (loom $ skipCodeBlock b)
loom ('%':'#':b)  =  (Ordering $ getTitle b)
                  :  (loom $ skipTitle b)
loom (b:c) = Weaved (grabLine (b:c)) : (loom $ skipLine (b:c))
loom [] = [Weaved ""]
\end{code}

\begin{code}
weaved :: Weave -> String
weaved (Code a b) = a' ++ b ++ "\\end{lstlisting}\n"
  where
    a'  =    "\\begin{lstlisting}[language=C,tabsize=2,caption="
        ++   a
        ++   "]\n"
weaved (Ordering a) = "{\\tt " ++ a ++ "}\n"
weaved (Weaved a) = a
\end{code}

\end{document}
