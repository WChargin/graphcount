\clearpage
\section{The main program}

First, we import the things we've created so far,
as well as some standard libraries.

\begin{code}
module Main where

import Liese
import CAS

import Math.Algebra.Group.PermutationGroup
    (_S, Permutation, (.^), fromList)
import Math.Algebra.Group.SchreierSims (elts)
import Data.List (elemIndices)
\end{code}
Note that |_S| generates the symmetric group of a given order,
and |(.^)| applies a permutation to an element (like |x .^ perm|).
\begin{haskellnote}[On capitalization]
Capitalization is semantically important in Haskell:
only data and type constructors may start with an uppercase letter.
(The compiler enforces this.)
Because |_S| is not a data constructor
(it's not a variant of any |data| type; it's just a convenient function),
this library prefixes it with an underscore to avoid this constraint.
\end{haskellnote}

Also, |elemIndices :: a -> [a] -> [Int]|;
combined with its name,
that type signature should be sufficient for you to determine what it does.

We'll start by defining a few type aliases for our own use,
since we talk about edges on the graph a lot.
\begin{code}
type Edge = (Int, Int)
type Edges = [Edge]
\end{code}
This is the first difference from the Mathematica implementation:
we explicitly use \emph{pairs} of integers to represent edges
instead of an arbitrary array of integers |[Int]|.
This way, the type system ensures that values like |(3)| and |(5, 6, 7)|
cannot possibly be called edges.

Next, this particular problem is all about $S_7$,
but we might as well make that arbitrary.
\begin{code}
groupNumber :: Int
groupNumber = 7
\end{code}

The Mathematica code used a @Subsets@ function to generate the sorted edges.
I~think it's simpler, though, to just use a two-dimensional list comprehension.
This also avoids confusion due to the fact that
we want the edges to be sorted,
but the name sub\emph{sets} seems to imply that that need not be the case
(even though it probably is).
\begin{code}
sortedEdges :: Edges
sortedEdges = [(a, b) | a <- [1..groupNumber], b <- [a + 1..groupNumber]]
\end{code}

We can use the built-in |_S| function
to get the generating elements for the symmetric group of our order.
\begin{code}
group :: [Permutation Int]
group = _S groupNumber
\end{code}

Next comes the |applyPermutationToEdges| function.
This is actually built in to the @PermutationGroup@ module
as the |(-^)| operator.
However, we implement it just to show that there's no magic going on.
\begin{code}
applyPermutationToEdges :: Permutation Int -> Edges
applyPermutationToEdges perm = map applyEdge sortedEdges
  where
    applyEdge (a, b) =
        let     (a', b') = (a .^ perm, b .^ perm)
        in      if a' < b'
                    then    (a', b')
                    else    (b', a')
\end{code}
\begin{haskellnote}[Destructuring]
Note that |applyPermutationToEdges| is a function of just one argument,
but we're defining it by writing |applyEdge (a, b) = ...|.
Here, the |(a, b)| refers not to two different variables,
but to one variable;
we're expecting that this variable is a two-element tuple
and extracting its elements as |a| and |b|.
That is, we could equivalently have written
\begin{spec}
applyPermutationToEdges :: Permutation Int -> Edges
applyPermutationToEdges perm = map applyEdge sortedEdges
  where
    applyEdge edge =
        let     a   = fst edge
                b   = snd edge
        in      if a' < b'
                        then    (a', b')
                        else    (b', a')
\end{spec}
where |fst| and |snd| are built-in functions
that get the first and second elements of a pair.

This technique (the shorter version) is called \emph{destructuring}.
It's a special case of \emph{pattern matching}, which is very powerful.
\end{haskellnote}

If we wanted this to be a bit more general, we could write
\begin{spec}
import Data.List (sort)

applyPermutationToEdges :: Permutation Int -> Edges
applyPermutationToEdges perm = map applyEdge sortedEdges
  where
    applyEdge (a, b) =
        let     [a', b'] = sort [a .^ perm, b .^ perm]
        in      (a', b')
\end{spec}
to allow this to be more easily extended to arbitrary ``edge-like'' things.
But we won't do that because we don't care about that case.

Then, finally, we have |getPermutationFromEdgeList|:
\begin{code}
getPermutationFromEdgeList :: Permutation Int -> Edges -> Permutation Int
getPermutationFromEdgeList perm edges =
    let     permutedEdges       = applyPermutationToEdges perm
            findPositions edge  = edge `elemIndices` edges
    in      fromList $ concatMap findPositions permutedEdges
\end{code}
Note that |concatMap = concat . map :: (a -> [b]) -> [a] -> [b]|.

\begin{note}
Stop and think about that type signature.
It tells you a lot.
Can you tell exactly what it will do just from the type?
Can you implement it?
\end{note}

For the finale, we create our group and get the results!
\begin{code}
g = [getPermutationFromEdgeList perm sortedEdges | perm <- elts group]

main = do
    print $ length $ elts g             -- |5040|
    let zgExpression = getZG g 21 2
    let env = const (Just 1)            -- let $x_i = 1 \quad \forall i$
    print $ eval env zgExpression       -- |Right 1044|
\end{code}
