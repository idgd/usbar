Usbar means "weaver" in Sumerian.
Ideas on how this works?

The outputs are FRAMA-C and C for tangle, \LaTeX for weave.
The inputs is a string; this string is parsed into a list of ordered chunks.
The chunks are weaved into \LaTeX, and tangled into FRAMA-C and C.
So, a chunk can be interpreted in one of two ways; the interpretation is then concatenated, output, and piped to texi2pdf or the system's CC.
So, how do we define chunks?
I think a \%! sequence at the start of the line, followed by the name of the chunk, is a good idea.
Then, we can have \%@ and \%# for chunks that will go into the C file, and chunks that will be \emph{formatted} as C, but will not be tangled.
Finally, \%$ followed by a filename will verbatim copy/paste the contents of the file into the string stream.

\begin{code}
module Main (main) where

import Lib

main :: IO ()
main = someFunc
\end{code}
