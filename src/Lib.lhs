\begin{code}
module Lib
  (
  ) where
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
data Command = Section
             | Source String
             | Formatted String
             | Insert FilePath
             | Order String
             | Plain String

commands :: String -> IO String
commands a = do
	let b = lines a
	
\end{code}
