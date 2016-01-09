\documentclass{article}

%include polycode.fmt
%format phi = "\varphi"

\usepackage{amsmath}

\begin{document}

First off, we'll need to pull in the @PermutationGroup@ module.
This provides functions for, e.g.,
converting permutations to cycle decompositions and vice versa,
finding the order of a permutation group, etc.
By importing this, we save ourselves from having to rewrite all that
(and of course this functionality is built into Mathematica).
\begin{code}
import Math.Algebra.Group.PermutationGroup
\end{code}

We start by replicating the ``cycle type'' function
from the Mathematica code;
this function, given a permutation group and its order,
finds the length of each of the disjoint cycles.
\begin{code}
cycleOrders :: Ord a => Permutation a -> Int -> [Int]
cycleOrders grp n = nontrivials ++ trivials
  where
    cycles = toCycles grp
    numTrivials = n - length (concat cycles)
    trivials = replicate numTrivials 1
    nontrivials = map length cycles
\end{code}

So, for example, the following should hold
(up to order of the resulting list, which we don't really care about):
\begin{spec}
cycleOrders (fromCycles [[2, 3, 1], [4, 5]]) 7 == [3, 2, 1, 1]
\end{spec}

\section{The CAS}
%include src/CAS.lhs

\end{document}

\begin{code}
main = print 123
\end{code}
