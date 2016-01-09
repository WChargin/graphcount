\documentclass{article}

%include polycode.fmt
%format phi = "\varphi"

\title{Exploring graphs in Haskell}
\author{William Chargin}
\date{9~January 2016}

\usepackage{amsmath}

\begin{document}

\maketitle

This program ports a specific Mathematica notebook to Haskell.
In doing so, we hope to write a more easily understandable program.

The code comprises three modules.
In |CAS|, we implement a \emph{very} basic CAS
so that we can write and evaluate symbolic expressions.
In |Liese|, we port some code provide by Dr.~Liese
as a preamble to the problem.
Finally, in |Main|, we present the solution to the problem.

The reader is not assumed to have any Haskell knowledge.
We'll see how this goes\ldots

%include src/CAS.lhs
%include src/Liese.lhs
%include src/Main.lhs

\end{document}
