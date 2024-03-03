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
Parsing returns a data type which tangle and weave transform into a string.
So, an application which uses this library will either have to parse an input, or generate a list of the type Usbar itself.

\begin{code}
module Usbar
  (  parse
  ,  tangle
  ,  weave
  ,  Usbar (..)
  )  where
\end{code}

We don't have many external dependencies, and only one that isn't included with GHC.
The file path module here is from the \emph{filepath} package.
The character module is used to match sets of characters.

\begin{code}
import Data.Char (isSeparator, isSpace)
import System.FilePath (takeDirectory, (</>))
\end{code}

Parsing is very simple: we first expand any file insert commands, then parse the finalized string into the Usbar data type.

\begin{code}
parse :: FilePath -> IO [Usbar]
parse = fmap usbar . expand
\end{code}

Since this document is intended to not only be a canonical source for Usbar's main functionality, but also a basic introduction to how programming is done in Haskell, I'll take a bit of extra time to break down each function and the more unique features used by each one.
Every Haskell function has two components: a type signature, and an implementation body.
The type signature is the first line:

\begin{verbatim}
parse :: FilePath -> IO [Usbar]
\end{verbatim}

The name of the function comes first, followed by the two colons, {\tt ::}, which is followed by the types involved in this function.
Each type is delimited by the arrows, {\tt ->}; the final element in the list is the output value of the function, while each preceding one is an input.
So, this function takes a {\tt FilePath} as an input, and outputs an {\tt IO [Usbar]}.
Like many languages, {\tt []} is the notation indicating a list, or array--so this function outputs an array of some sort, but it's preceded by this {\tt IO} type.

A value of the type {\tt IO} is a computation which yields some value--in this case, a list of the {\tt Usbar} type.
These values are lists of commands, interpreted by the runtime, starting at the {\tt main} function, and going down the tree until execution is done.
You may have heard the term `monad' before--they may sound scary, but they are very simple to use and understand.
Describing them succinctly is hard--there is an entire genre of blog posts dedicated just to explaining monads, so I will not explain them in much detail here.
Sufficed to say, monads are not related to {\tt IO} in any meaningful way, and many other types have a monad--you may have even used one in your own programming projects without knowing it--Haskell just happens to use the {\tt IO} monad to compose values of that type.
Let's look at the {\tt parse} function in more detail.

Instead of looking at the function as-is, though, we're going to look at a semantically equivalent version which is more readable to an audience that's been trained on imperative programming.
We'll then build backwards to the simpler version above.

\begin{verbatim}
parse a = do
	z <- expand a
	let y = usbar z
	pure y
\end{verbatim}

The name of the function is followed by the name of the input variable--the {\tt FilePath} from the type--and an equals sign.
The equals sign denotes equality--an application of the name on the left to a variable of a compatible type will expand to what's on the right, with the {\tt a} substituted for the applied value.
The body of the function is in \emph{do notation.}
Do notation is syntactic sugar, which allows us to write monadic code in a very imperative-looking way.

The first function is {\tt expand}, which is a function we define ourselves--we will look into it as our next piece of Haskell code.
This is applied to the {\tt FilePath} input; because this function performs IO, we use the leftwards-facing arrow to call it, and assign its return value to the symbol {\tt z}.
It returns an {\tt IO String}, and we will see its type signature soon.
Because we called it using a leftwards-facing arrow, in the context of this do block, the {\tt z} is a {\tt String}, not an {\tt IO String}. 
We will illustrate this difference with the next statement.

We pass the result into another function we define, {\tt usbar}, which does not perform IO.
Its return type is {\tt [Usbar]}.
Because it does not return a type matching the current do block's monad, we call it using {\tt let}.
A let statement will still execute the function, but it will not `unwrap' the monadic structure around it.
In this example, if {\tt usbar} performed IO, a let call would give us back the \emph{recipe}, not the \emph{result}.
So, if we called:

\begin{verbatim}
do
  let z = expand a
\end{verbatim}

We wouldn't have the same value as the leftwards-facing arrow call.
{\tt parse} returns an {\tt IO [Usbar]}.
When we call it using a leftwards-facing arrow, inside of an IO function using do notation, the symbol on the left side of the arrow will be of type {\tt [Usbar]} inside the do block.
When we call it using a let assignment, we'd get back a {\tt IO [Usbar]}, the \emph{recipe} to create an {\tt [Usbar]}.
So, inside do notation, we have to keep track of the types we're working with, and appropriately call functions using let or with an arrow, in order to extract their value to pass along the chain, or to get the result directly.

Let's simplify it, and then we'll take one final step and simplify it further to the version we had above.

\begin{verbatim}
parse a = expand a >>= pure . usbar
\end{verbatim}

The difference here is that we've manually desugared the do notation we used above.
When Haskell parses do notation, it first transforms it into this form, before starting on the compilation process proper.
There's a new function here, {\tt >>=}, which is colloquially called the fish.

The fish is one of the two \emph{monadic composition} functions--the other being {\tt >>}, which doesn't really have a catchy name like the first.
The fish sequentially composes two functions, taking the output of the first (left side) and applying it to the second (right side).
These functions need to output the same wrapping monad type--that is to say, a function of type {\tt a -> IO b} and {\tt b -> IO c} can be composed with the fish, but {\tt a -> IO b} and {\tt b -> [c]} cannot (lists are also a monad, but of a different type to {\tt IO}).

This is why, on the right side, we have two functions, composed with the {\tt .} function; {\tt usbar} and {\tt pure}.
{\tt pure} is a synonym for {\tt return}, but this isn't {\tt return} as you would think of it in another language--{\tt pure}/{\tt return} wrap a plain value in a monad type; in this case, it's taking the output of {\tt usbar} and wrapping it in {\tt IO}.
The {\tt .} function is the {\emph composition} function.
It takes two functions and returns one--as long as the output of the rightwards function matches the input of the leftwards one.
So, because {\tt pure} takes any type, it accepts the output of {\tt usbar}.

So, {\tt parse} is two steps: firstly, a call to {\tt expand}; then, a call to {\tt usbar}, wrapped in the IO type that {\tt parse} returns.
So how do we get to the final, simpler form?

\begin{verbatim}
parse = fmap usbar . expand
\end{verbatim}

We have a very familiar triplet of functions on the right side, but we're missing an input variable ({\tt a}), and we've been introduced to a new function {\tt fmap}.
Let's break this down step-by-step.
Firstly, the lack of an input variable: we will expand upon this later, but to put it simply for now, \emph{input variables are optional in Haskell.}
Secondly, {\tt fmap} is a \emph{functor lifting function.}
A functor is something that applies a computation to values inside some structure.
The reader may already be familiar with {\tt map}; this is a function which takes one function that operates on a certain type, and a list of that type, and applies the function to every element of that list.
{\tt fmap} is functionally identical, but it can operate on \emph{any type of structure} that defines a functor implementation.
{\tt IO} happens to include a functor implementation, so we can compose {\tt usbar} lifted over {\tt IO} onto the result of {\tt expand}.
This may be confusing for now, but it gets easier with more exposure to Haskell.

It is at this point that I think it'd be a good idea to explain what the input file format is supposed to look like.

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
\end{code}

This is another example of monadic composition, and very similar to the second simplification of our previous function.
Let's look at the right hand side.
This is another composition of two functions, {\tt lines} and {\tt c'}.
{\tt lines} is a standard library function which splits a string into a list of the component lines:

\begin{verbatim}
"abc\ndef\nghi" -> ["abc","def","ghi"]
\end{verbatim}

And {\tt c'} is a local function we define using a {\tt where} clause:

\begin{code}
  where
    a' b = takeDirectory a </> strip b
    b' ('%':'%':b) = lines <$> readFile (a' b)
    b' b = return [b]
    c' b = concat <$> mapM b' b >>= \z ->
      if any (\x -> take 2 x == "%%") z
      then c' z
      else return $ unlines z
\end{code}

The {\tt where} clause allows us to define functions which are only available to their parent function, and are not exposed in any other scope.
Here, we're using it to split out the different steps our function takes.
Let's look through them, starting with {\tt c'}, to understand what we're doing here.

\begin{verbatim}
c' b = concat <$> mapM b' b >>= \z ->
  if any (\x -> take 2 x == "%%") z
  then c' z
  else return $ unlines z
\end{verbatim}

This is a relatively more complex function than ones we've looked at before, but it'll become clear as we step through it.
It takes one input, the list of lines that {\tt lines} returns.
The {\tt <\$>} symbol is the infix version of {\tt fmap}, Haskell's structure-agnostic mapping function.
There is a {\tt fmap} defined for many structures in the standard library, from {\tt IO}, to {\tt Maybe}, or the list--in this case, we're using the list's {\tt fmap}, so we are applying the function on the left to every element of the list on the right.

The function on the left is the {\tt concat} function--this flattens a list of lists of elements to a list of elements.
Our right side is a {\tt mapM}; this is a variation of {\tt fmap} which applies functions monadically, using the fish we've seen before.
This applies the {\tt b'} function to the list of lines we received.

The output of this map is piped to a lambda function (a nameless function with one argument, {\tt z}), which is the if statement that follows.
We check to see if any lines start with the insert command, and recurse if so.
Otherwise, we return a string concatenated from the disparate lines using {\tt unlines}.
The {\tt \$} symbol is a function application function--which sounds redundant, but is very useful.
You'll see it a lot; think of it as a parentheses that goes to the end of the current statement:

\begin{verbatim}
return $ unlines z == return (unlines z)
\end{verbatim}

Let's take a look at {\tt b'}.
This is our first example of Haskell's pattern matching syntax.
In the same way that we can define piecewise functions in basic algebra, in Haskell, we can choose what to execute based on what the input is, without using an if or case statement like in many other languages.
In this case, we're matching on a string, so we split it into the individual characters we want to see.
If we find them, we read the resulting file, and split the result into lines.
Otherwise, we simply wrap the result in the necessary types, and return that.

\begin{verbatim}
b' ('%':'%':b) = readFile (a' b) >>= return . lines
b' b = return [b]
\end{verbatim}

We call an {\tt a'} in {\tt b'}, so as a final deep dive into the {\tt expand} function, let's look at that.
This is a much simpler function: we want to take the directory of the input file path, and append the filename listed in the {\tt \%\%} command.
This gives us the file that the command points to, without forcing the file to specify a global URI for the file it wants to insert.

\begin{verbatim}
a' b = takeDirectory a </> strip b
\end{verbatim}

After this, there's one utility function I'll call out.
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

This is an algebraic data type declaration: Haskell's main selling point is its strong type system, and this is one very good example.
We call it an `algebraic' data type because we can run operations on the type that transform it in very mathematical ways.
As an example, the $\mid$ symbol is equivalent to addition--each new element in a list of $\mid$ is a new, exclusive possibility for that type.
Types that use this are called `sum types,' because they are the sum of all the different possible constructors (the elements on the right of the equals sign).
We will be using pattern matching on this type extensively coming up.

\begin{code}
data Usbar  =  Source Title [Usbar]
            |  Ordering Title
            |  C Content
            |  Weave Content
\end{code}

To parse it, we pass the lines through a helper function to index each one (give them a line number), and pass it to our internal parsing function.

This function is written in a point-free style; that is, the input variables aren't explicitly written out.
We have seen this before.
Writing functions like this is somewhat controversial, but the semantics of Haskell permit it, so I use it here as demonstration.
To understand what it's doing, imagine the right side of the body in parenthesis, applied to a string.
{\tt lines} splits it into a list of lines, the {\tt indexed} function adds line numbers to them, and {\tt a'} does the special magic that turns it into a list of our algebraic data type, {\tt Usbar}.

\begin{code}
usbar :: String -> [Usbar]
usbar = a' . indexed . lines
\end{code}

The parsing function reads the first two characters of any line in order to check if they are a source block, or an ordering.
At this point, all inserts have been removed in the expansion process, so these are the only ones we must check.
When they find a source block, they check for a missing title, and error out if so, or hand it over to a helper function.
Similarly, when it finds an ordering, it will check for the same error, and prepend the correct Usbar constructor if it's clear.
If it doesn't see a command, it passes it verbatim to be weaved; these will later be ignored by the tangle.

If the pattern match is a bit confusing, don't worry--it's just verbose, not inherently complex.
Just like with our string match, it's matching on a string at the innermost level: the {\tt '\%':'\#':b} statement matches a string that starts with {\tt "\%\#"}, and ends with any value, represented by {\tt b}.
After that, we are matching on what's called a tuple--think of it as a fixed-length list, like a vector.
In code, you'd write a tuple like {\tt (a,b,c,d)}, same as in the pattern match.
In this case, the tuple is of the string value in the first slot, and the line number in the second.
This is what our indexed function does--it takes a list, and transforms it into a list of tuples, with the original value on the left, and the line number on the right.

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
    b' b c (([],_):d) = b' b c d
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
Orderings, we hand work over to another function, with the ordering's title added; otherwise, we simply recurse.
This should match a source block somewhere in the original file, so we send the original file ({\tt a}) in along with the title we're looking for.
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
The data type is ideal for this kind of work; {\tt Source} is a title and list of other {\tt Usbar} constructors.
So, we have a function which consumes {\tt Usbar} in a list, and if it finds an ordering, it asks for the corresponding {\tt Source}, and replaces it with the {\tt Source}'s contents--\emph{which are expanded in the same way as the top-level list.}
This allows a one-step recursion that expands a tree and orders it by traversing it depth-first, expanding each node it encounters along the way.

\begin{code}
    d' b (Source c d:e) = if b == c then b' d else d' b e
    d' b (_:c) = d' b c
    d' b [] = noMatch b
\end{code}

And that concludes the entirety of tangling.
Weaving is considerably simpler: we transform inputs into \LaTeX, without reordering, straight through.
So we match on every data constructor in Usbar, and construct a \LaTeX\ statement based on it.
Otherwise, we're done!
When matching a Source specifically, we want to handle the orderings slightly differently than in the top level--we want them to appear as comments pointing to the ordered listing, so the reader can follow along.
So, when encountering a source, we add a bunch of options (mostly prettification for C's operators) to the listing environment, and hand off to a local function to handle the inner section.

\begin{code}
weave :: [Usbar] -> String
weave ((Source a b):c)  =   "\\begin{lstlisting}"
                        ++  "[ language=C"
                        ++  ", tabsize=2"
                        ++  ", columns=fullflexible"
                        ++  ", breaklines=true"
                        ++  ", literate="
                        ++  "{=}{${\\mathrel"
                            ++  "{\\vcenter"
                            ++  "{\\baselineskip0.5ex"
                            ++  "\\lineskiplimit0pt"
                            ++  "\\hbox{\\scriptsize.}"
                            ++  "\\hbox{\\scriptsize.}}}=}$"
                            ++  "\\hspace{0.6ex}}{1}" -- prettier :=
                        ++  "{==}{$\\equiv$\\hspace{0.7ex}}{2}"
                        ++  "{!}{$\\lnot$}{2}"
                        ++  "{!=}{$\\neq$}{2}"
                        ++  "{&&}{$\\land$}{2}"
                        ++  "{||}{$\\lor$}{2}"
                        ++  "{>=}{$\\geq$}{2}"
                        ++  "{<=}{$\\leq$}{2}"
                        ++  "{+}{$+$}{2}"
                        ++  "{++}{$\\succ$}{2}"
                        ++  "{-}{$-$}{2}"
                        ++  "{--}{$\\prec$}{2}"
                        ++  "{NULL}{$\\O$}{1}"
                        ++  "{->}{$\\rightarrow$}{2}"
                        ++  ", caption="
                        ++  a ++ "]\n" ++ a' b
                        ++  "\\end{lstlisting}\n"
                        ++  weave c
  where
    a' ((Ordering d):e) = "// Listing:\t" ++ d ++ "\n" ++ a' e
    a' ((C d):e) = d ++ "\n" ++ a' e
    a' (_:d) = a' d
    a' [] = ""
weave ((Ordering a):b)  =   "\\noindent\\emph{Ordering:}\t"
                        ++  a
                        ++  "\\\\ \n"
                        ++  weave b
weave ((C a):b) = a ++ "\n" ++ weave b
weave ((Weave a):b) = a ++ "\n" ++ weave b
weave [] = ""
\end{code}

Haskell made this process very straightforward.
While a few rewrites were warranted, it ended up being an elegant program.n

\end{document}
