\clearpage
\section{Dr.~Liese's code}

Dr.~Liese provided some Mathematica code.
Let's write it in Haskell.

\begin{code}
module Liese where
\end{code}

First off, we'll need to pull in the @PermutationGroup@ module.
This provides functions for, e.g.,
converting permutations to cycle decompositions and vice versa,
finding the order of a permutation group, etc.
By importing this, we save ourselves from having to rewrite all that
(and of course this functionality is built into Mathematica).
\begin{code}
import Math.Algebra.Group.PermutationGroup
    (Permutation, toCycles, elts)
\end{code}
The parenthesized import list indicates that we only want those functions;
this makes it easy to tell what comes from where when reading the code.
We're importing the |Permutation| type, and two functions.
The |toCycles| function takes a |Permutation| to its cycle decomposition.
The |elts| function is like Mathematica's @GroupElements@ function:
its type is |[Permutation] -> [Permutation]|,
and it finds the closure of a permutation group given its generators.

Of course, we also want our own CAS.
\begin{code}
import CAS
\end{code}

\subsection{|cycleOrders|}

We start by replicating the ``cycle type'' function
from the Mathematica code;
this function, given a permutation group and its order,
finds the length of each of the disjoint cycles.
\begin{code}
cycleOrders :: Ord a => Permutation a -> Int -> [Int]
cycleOrders grp n = nontrivials ++ trivials
  where
    cycles          = toCycles grp
    numTrivials     = n - length (concat cycles)
    trivials        = replicate numTrivials 1
    nontrivials     = map length cycles
\end{code}

Note that |toCycles| is a function from that library we imported,
whose type signature is |toCycles :: Permutation a -> [[a]]|.

Also, |replicate :: Int -> a -> [a]| is a built-in function;
it's defined such that, e.g., |replicate 3 'x' = ['x', 'x', 'x']|.

So, for example, the following should hold
(up to order of the resulting list, which we don't really care about):
\begin{spec}
import Math.Algebra.Group.PermutationGroup (fromCycles)
cycleOrders (fromCycles [[2, 3, 1], [4, 5]]) 7 == [3, 2, 1, 1]
\end{spec}

\subsection{|getP|}

Using our mini-CAS, we can also implement the |getP| function,
where we want |getP n k| to equal $x_1^n + \dotsb + x_k^n$.
Personally, I~think that this is a lot clearer than the original Mathematica code:
we're creating an expression that's a |sum| of
a bunch of powers of $x_i$ to the $n$th power
for various $i \in \{ 1, \dotsc, k \}$ and constant $n$.
\begin{code}
getP :: Int -> Int -> Expression Int
getP n k = NOp sum [BinOp (^) (xsub i) (Constant n) | i <- [1..k]]
  where
    xsub i = Literal $ Var 'x' `Sub` i
\end{code}

(The |^| operator, written @^@, is exponentiation.)

\begin{haskellnote}[List comprehensions]
The expression in brackets is a \emph{list comprehension}.
Here are two simple examples:
\begin{spec}
firstFiveSquares    = [x * x | x <- [1..5]]
trianglePoints      = [(x, y) | x <- [1..10], y <- [1..x]]
\end{spec}
The format is made to resemble set-builder notation.
\end{haskellnote}

\subsection{|powerSym|}

\begin{code}
powerSym :: [Int] -> Int -> Expression Int
powerSym lambda k = NOp product [getP li k | li <- lambda]
\end{code}

\subsection{|getZG|}

\begin{code}
getZG grp n k = MonOp (`div` length elements) $ NOp sum terms
  where
    elements        = elts grp
    terms           = map term elements
    term element    = powerSym (cycleOrders element n) k
\end{code}
