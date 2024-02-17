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

Usbar is a literate programming system, designed specifically for C and \LaTeX.
While CWEB exists, its syntax is fundamentally incompatible with FRAMA-C's validation language, ACSL.
Additionally, CWEB pipes to \TeX.
While this is a perfectly capable typesetting system, \LaTeX\ has access to more capabilities, like graphing and the rich package repository on CTAN.
Because of this, I spun up a simple alternative, which should be relatively easy to rework for alternate languages, or tie into the library to build newer, more complex applications than the sample implementation included in this package.

We chose the name `Usbar' because it's the Sumerian word for `weaver.'
It seemed like an appropriate theme for something whose primary function is named `weaving,' and the use of Sumerian (the oldest written language) means I'm very unlikely to be stumbling upon intellectual property or insensitive words.
The name is a bit awkward, but it works for the purposes it's intended for.

This document can be parsed from its Literate Haskell source using Pandoc or, more preferably, lhs2\TeX.
To convert using Pandoc, use this invocation:

\begin{verbatim}
pandoc -f latex+lhs -t pdf src/Usbar.lhs -o usbar.pdf
\end{verbatim}

Alternatively, you can use a different output format, like Markdown, if you want a plaintext representation that removes the \LaTeX-isms of the source file:

\begin{verbatim}
pandoc -f latex+lhs -t commonmark src/Usbar.lhs -o usbar.md
\end{verbatim}

The more recommended path is to use lhs2\TeX, and combine that with a \LaTeX\ interpreter from the TeXLive distribution.
The invocation I use on my Linux machine is this:

\begin{verbatim}
lhs2TeX src/Usbar.lhs > usbar.tex && texi2pdf -c usbar.tex
\end{verbatim}

This produces nicer output with ligatures and similar features that pandoc's more generic conversion methods lack.
It is the method I consider canonical for this document.

The three functions this module exports are parsing, tangling, and weaving.
Parsing returns an algebraic data type, which tangle and weave consume in order to produce a final string.
So, an application which uses this library will either have to parse an input, or generate a list of the data type, Usbar, itself.

\begin{code}
module Usbar
  (  parse
  ,  tangle
  ,  weave
  ,  Usbar (..)
  )  where
\end{code}

We don't have many external dependencies, and only one that isn't included with GHC.
The file path module here is from the {\tt filepath} package, and simplifies some work with opening files.
The character module is used for its functions matching a wide class of individual characters.

\begin{code}
import Data.Char (isSeparator, isSpace)
import System.FilePath (takeDirectory, (</>))
\end{code}

Parsing is very simple: we first expand any file insert commands, then parse the finalized string into the Usbar data type.
It is at this point that I think it'd be a good idea to list what the input file format is supposed to look like.

\begin{code}
parse :: FilePath -> IO [Usbar]
parse a = expand a >>= pure . usbar
\end{code}

There are three commands in this system: {\tt \%@@}, {\tt \%\#}, and {\tt \%\%}.
The first is `generate a source block,' code that is meant to be passed to a compiler through tangling.
This is a `block' commands; that is, the command comes in a pair, and anything between the pair is interpreted as its input.
The rest are `single-line' commands: they take the rest of the line they appear on as input.

The second is an `ordering' command.
When writing the source document, we can write the source blocks in any order.
Orderings give us a way to shuffle those blocks into the order we want in the final output; they match titles with a source block, and the order they appear in the original literate source determines their output order.
Orderings can appear in the top level, outside of a code block, or inside a code block.
When they appear in the top level, these are considered the root elements of a tree-like structure.
When orderings appear in a source block, they are considered the children of whatever ordering spawned them; ultimately, the order is determined by moving down the tree depth-first.

The final command is an insert command; this reads an external file, and inserts it, verbatim, into the source that is being parsed.
We can see that command being executed below.
This is the only part of the parser that needs IO access.
We read the original, root file, and iterate over each line; when we match an insert command, we read the file pointed to in the remainder of the line.
So:

\begin{verbatim}
%% directory/filename.u
\end{verbatim}

Will read {\tt filename.u}, relative to the source file, and insert its contents wherever it's found.
We read the resulting string to see if it has any remaining insert commands, and recurse if we find them.
If we don't, we return the final string.

\begin{code}
expand :: FilePath -> IO String
expand a = readFile a >>= c' . lines
  where
    a' b = takeDirectory a </> strip b
    b' ('%':'%':b) = readFile (a' b) >>= return . lines
    b' b = return [b]
    c' b = concat <$> mapM b' b >>= \z ->
      if (any (\x -> take 2 x == "%%") z)
      then c' z
      else return $ unlines z
\end{code}

There's one utility function I'll call out.
It reads a string, and strips any leading whitespace.
This are the primary place we see the character matches come into play.

\begin{code}
strip :: String -> String
strip = dropWhile (\a -> isSeparator a || isSpace a)
\end{code}

Let's start looking at some types we'll be using next.
The first ones are simple type aliases, to clarify what we're going to be doing in the more complex algebraic data type coming up.

\begin{code}
type Title = String
type Content = String
\end{code}

This is the main data type we'll be working with going forward.
First to build it, and next to deconstruct it into its final form as a tangled or weaved source file.
These represent the four types of elements we deal with in the source literate file.
Firstly, a source block, which has a corresponding title, and a list of lines representing their content (Orderings or C).
Orderings have a title, that must match a source block in the end.
C is source code, and Weave is text meant for humans.

\begin{code}
data Usbar  =  Source Title [Usbar]
            |  Ordering Title
            |  C Content
            |  Weave Content
\end{code}

To parse it, we pass the lines through a helper function to index each one (give them a line number), and pass it to our internal parsing function.

\begin{code}
usbar :: String -> [Usbar]
usbar = a' . indexed . lines
\end{code}

The parsing function reads the first two characters of any line in order to check if they are a source block, or an ordering.
At this point, all inserts have been removed in the expansion process, so these are the only ones we must check.
When they find a source block, they check for a missing title, and error out if so, or hand it over to a helper function.
Similarly, when it finds an ordering, it will check for the same error, and prepend the correct Usbar constructor if it's clear.
If it doesn't see a command, it passes it verbatim to be weaved; these will later be ignored by the tangle.

\begin{code}
  where
    a' ((('%':'@':b),c):d) =  if strip b == ""
                              then missingTitle c
                              else b' b [] d
    a' ((('%':'#':b),c):d) =  if strip b == ""
                              then missingTitle c
                              else Ordering (strip b) : a' d
    a' (b:c) = Weave (fst b) : a' c
    a' [] = []
\end{code}

Our helper function looks first for the closing command.
If it finds it, it constructs an Usbar, and hands control back over to our first function for the remainder.
If it finds an ordering, it adds it to the list, and plunks it into the list.
It ignores empty lines, and anything else is counted as C source code.
If it reaches the end of the file without finding a closing bracket, it'll complain about missing a closing half.

\begin{code}
    b' b c ((('%':'@':d),e):f) =  if strip d /= ""
                                  then extraTitle e
                                  else Source (strip b) (reverse c) : a' f
    b' b c ((('%':'#':d),e):f) =  if strip d == ""
                                  then missingTitle e
                                  else b' b (Ordering (strip d) : c) f
    b' b c ((([]),_):d) = b' b c d
    b' b c ((d,_):e) = b' b (C d : c) e
    b' _ _ [] = noClose
\end{code}

The errors are shown below; they're very basic, crashing the program with a specific error message, including the line numbers and specific problems with the source.

\begin{code}
missingTitle :: Int -> a
missingTitle a  =   error $ "ERROR: Line "
                ++  show a
                ++  " has a missing title."
extraTitle :: Int -> a
extraTitle a  =   error $ "ERROR: Line "
              ++  show a
              ++  " has a title, when it should not."
noClose :: a
noClose  =   error $ "ERROR: Missing closing command on a "
         ++  "listing or source block."
noMatch :: String -> a
noMatch a  =   error $ "ERROR: Missing source block for ordering: "
           ++  a
\end{code}

Our indexed function is similarly simple: it zips the input and a sugary list comprehension of one to the length of the input, inclusive.

\begin{code}
indexed :: [a] -> [(a,Int)]
indexed a = zip a [1..length a]
\end{code}

Now we'll get into the meat of the operation: tangling and weaving.
Starting with tangling, we pipe the input list into a pipeline of two functions.
Let's look at those in more detail.

\begin{code}
tangle :: [Usbar] -> String
tangle a = a' a
  where
    a' = c' . b'
\end{code}

We're continuing the use of pattern matching into the rest of the program; we match on orderings, C, and nothing else.
Orderings, we hand work over to another function; otherwise, we simply recurse.
The effect of this function is to expand orderings, while removing any other type of element outside of C lines.
So, we delete weaved elements, and reorder, in one step.

\begin{code}
    b' (Ordering b:c) = d' b a ++ b' c
    b' (C b:c) = C b : b' c
    b' (_:b) = b' b
    b' [] = []
\end{code}

In the next step, we transform C into a single string by appending the disparate strings.
It's very straightforward, so we'll move forward to the final command, which reorders the source blocks.

\begin{code}
    c' (C b:c) = b ++ "\n" ++ c' c
    c' (_:b) = c' b
    c' [] = ""
\end{code}

This function matches source blocks; if we match on the input ordering title, we hand them over to b' to expand any ordering inside--otherwise, we recurse.
If we don't find a match, we error out.

\begin{code}
    d' b (Source c d:e) = if b == c then b' d else d' b e
    d' b (_:c) = d' b c
    d' b [] = noMatch b
\end{code}

And that concludes the entirety of tangling.
Weaving is considerably simpler: we transform inputs into \LaTeX, without reordering, straight through.
So we match on every data constructor in Usbar, and construct a \LaTeX\ statement based on it.
Otherwise, we're done!

\begin{code}
weave :: [Usbar] -> String
weave ((Source a b):c)  =   "\\begin{lstlisting}"
                        ++  "[language=C,tabsize=2,caption="
                        ++  a ++ "]\n" ++ weave b
                        ++  "\\end{lstlisting}\n"
                        ++  weave c
weave ((Ordering a):b) = "{\\tt " ++ a ++ "}\n" ++ weave b
weave ((C a):b) = a ++ "\n" ++ weave b
weave ((Weave a):b) = a ++ "\n" ++ weave b
weave [] = ""
\end{code}

Haskell made this process very easy and straightforward--while a few rewrites were warranted, and I cut out a lot of unecessary functionality, it ended up being a very elegant program with very few lines of code.

\end{document}
