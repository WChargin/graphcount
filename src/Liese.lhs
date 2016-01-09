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
\end{code}

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

Note that |toCycles| is a built-in function from that library we imported,
whose type signature is |toCycles :: Permutation a -> [[a]]|.

So, for example, the following should hold
(up to order of the resulting list, which we don't really care about):
\begin{spec}
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

\subsection{|powerSym|}

And, also, that |powerSym| thing.
This one is created by applying |getP| with a fixed |k|
to each element of |lambda|,
and then multiplying all the results together.
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
