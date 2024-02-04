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

Usbar is a literate programming system.
It parses combined explanations for humans, and specifications for computers, and splits them into two outputs: a document for humans to read, and a source file for computers to compile.
The act of creating a human-readable document is called \emph{weaving.}
Usbar is a Sumerian word meaning \emph{weaver.}
Creating a source file for the computer to compile is called \emph{tangling.}
The three functions this file exposes are parsing (transforming an input into a simultaneously tangle-able and weave-able data structure), tangling, and weaving.

The computer source that Usbar is focused on generating is C; the human-readable text is \LaTeX\, which can be used to generate PDFs, DVIs, or other output formats through conversion tools like Pandoc.
This project was motivated by a desire to use FRAMA-C and C with a literate programming system.
The syntax of FRAMA-C and CWEB, an existing literate programming system for C, conflict; it is impossible, to my knowledge, to use the two together.
So, over the course of a weekend, the first version of this program was created.

In this document, we'll go over the implementation of the program, and talk some about the motivation for using literate programming in general.

\begin{code}
module Lib
  ( parse
  , tangle
  , weave
  ) where
\end{code}

The external dependencies we rely on are very slim.
The only dependency that isn't in the base library is the @@System.FilePath@@ module, which gives us some very convenient features for wrangling paths for one particular command.
There is some handling of external files, and writing cross platform file handling ourselves would be difficult.

\begin{code}
import Data.Char (isSeparator)
import Data.List (isPrefixOf)
import System.FilePath (takeDirectory, (</>))
\end{code}

Our types are built for multiple stages of our work.
The first phase, where we're parsing the file into a representation that's easy to tangle \emph{and} weave, can be understood through the @Command@ and @Line@ types.
Ultimately, both end up in our @Usbar@ type, the ultimate output for tangling and weaving.
@Command@ represents some state change that is relevant for tangling, weaving, or both.
\begin{itemize}
\item A @Hierarch@ is used in weaving; this allows us to split our document into logical chunks, that are arranged in a hierarchy (hence the name) between each other. We settle on four ranks, as that seems a reasonable amount of organization for any size of program.
\item A @Source@ is used in tangling \emph{and} weaving--in tangling, it gets copied directly to the output source file. In weaving, it gets put into a \LaTeX\ macro that formats it nicely for an output document; in our case, we use the Listings package.
\item An @Order@ is used in tangling, and incidentally in weaving. For tangling, it determines the \emph{order} in which @Source@ slices are output. This is critical for literate documents. Haskell's literate system, which we're using for this document, does not support reordering--even so, we attempt to order the module for easy readability. For weaving, these are simply inserted into the source text as-is--in the future, we'd like to have them link to the source they reference.
\item A @Listing@ is like a @Source@, but it is not inserted into the tangle. It is purely something for the reader of the human document, it is not passed to the computer.
\item An @Insert@ is a command that reads an external file and inserts in verbatim into the data structure--so, if we wanted to split our very large document up into manageablely small files, this is a critical feature.
\item A @Comment@ is a simple failure case--if a command does not parse as any of the others, it's inserted verbatim into the weave. Since each command starts with the \LaTeX\ comment character, \%, it should insert as a comment, and be ignored by \LaTeX 's parser.
\end{itemize}

We also have the @Line@, which is either a plain string, or it's a command.
If it's a string, it's inserted into the weave as-is; the tangle ignores it.
The final type, the @Usbar@, combines the two--we'll see later exactly why we combine them in this way, but sufficed to say is that a certain set of commands need to be in a set \emph{order,} as a hint.

\begin{code}
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
\end{code}

The top level parsing step is extremely simple: it receives a file path, presumably to a usbar file, and it outputs the usbar data structure.
The middle steps are: insert the external files referenced by the @Insert@ commands, and then step through each of the other command types to transform them into simpler forms.
The insertion requires IO, which is unique among all the transformations--notably, the only IO operations in parsing is reading files.
Notice that the last step is @ordering@--that is intentional.
We will explore why next, in the @tangle@ function.

\begin{code}
parse :: FilePath -> IO Usbar
parse a = do
  b <- readFile a
  c <- insert (line <$> lines b) a
  return $ (ordering . listing . source . comment) c
\end{code}

Now we get to the first and simpler of the two functions: tangling.
Here we'll see why there's an extra list of commands.
Firstly, we filter out everything from the second element of the tuple, a list of lines, to exclusively the lines that are \emph{source} types.
We then filter the result to the ones that match an order command--so, a source command has a title and content, an order has a title.
When a title matches between the two, we pluck that one out and concatenate its contents into a new string.
So, if a list of sources looks like this:

\begin{enumerate}
\item @Main@: @int main(void)...@
\item @Some functions used in main@: @int foo(); char bar();...@
\item @Headers we need@: @#include <stdint.h>...@
\item @Some ugly macros@: @#define...@
\end{enumerate}

And a list of orderings that look like this:

\begin{enumerate}
\item @Headers we need@
\item @Some ugly macros@
\item @Some functions used in main@
\item @Main@
\end{enumerate}

The output will be in the order specified in the list of orderings.
Note, we use @head@ to gather the source that matches the order.
@head@ is a partial function--if the list is empty, we will error out, and the program will crash.
Right now, in this version, error handling is very light to nonexistent; if we \emph{can} crash early, we do.
This is ideal for a code-golf-ish way of thinking about these problems, but in an eventual wider release, it would be smarter to use more robust methods.
This is the plan, but for now, as a weekend project, this will do.

\begin{code}
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
\end{code}



\begin{code}
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
\end{code}

\begin{code}
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
weave' (Right (Source a b) : c) = "\\begin{lstlisting}[language=C,tabsize=2,caption=" ++ a ++ "]\n"
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

\end{document}
