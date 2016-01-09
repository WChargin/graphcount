\documentclass{article}

%include polycode.fmt
%format phi = "\varphi"

\usepackage{amsmath}

\begin{document}

%include src/CAS.lhs

%include src/Liese.lhs

\section{The main program}

\begin{code}
module Main where
import Math.Algebra.Group.PermutationGroup (_S, Permutation, (.^), fromList, p, elts)
import Data.List (elemIndices)
import Liese
import CAS

type Edge = (Int, Int)
type Edges = [Edge]

sortedEdges :: Edges
sortedEdges = [(a, b) | a <- [1..7], b <- [a + 1..7]]

symgroup :: [Permutation Int]
symgroup = _S 7

-- Apply a given permutation to the fixed list |sortedEdges|.
applyPermutationToEdges :: Permutation Int -> Edges
applyPermutationToEdges perm = map applyEdge sortedEdges
  where
    applyEdge (a, b) =
        let     (a', b') = (a .^ perm, b .^ perm)
        in      if a' < b'
                    then (a', b')
                    else (b', a')

getPermutationFromEdgeList :: Permutation Int -> Edges -> Permutation Int
getPermutationFromEdgeList perm es =
    let     permutedEdges    = applyPermutationToEdges perm
            findPositions x  = elemIndices x es
    in      fromList $ concatMap findPositions permutedEdges

g = [getPermutationFromEdgeList e sortedEdges | e <- elts symgroup]

main = do
    print $ length $ elts g             -- 5040
    let zgExpression = getZG g 21 2
    let env = const (Just 1)
    print $ eval env zgExpression       -- Right 1044
\end{code}

\end{document}
