\documentclass{article}

%include polycode.fmt
%format alpha   = "\alpha"
%format beta    = "\beta"
%format gamma   = "\gamma"
%format alpha_1 = "\alpha_1"
%format beta_1  = "\beta_1"
%format alpha_2 = "\alpha_2"
%format beta_2  = "\beta_2"
%format phi     = "\phi"
%format psi     = "\psi"

\long\def\ignore#1{}

\title{Some discrete math in Haskell}
\author{William Chargin}
\date{9~January 2016}

\usepackage{amsmath}
\usepackage{tikz-cd}
\usetikzlibrary{arrows}
\tikzset{
  commutative diagrams/.cd, 
  arrow style=tikz, 
  diagrams={>=stealth}
}

\usepackage{zi4}
\usepackage{xcolor}
\definecolor{haskellpurple}{HTML}{5c4e84}
\usepackage{mdframed}
\newmdenv[
  skipabove=7pt,
  skipbelow=7pt,
  rightline=false,
  leftline=true,
  topline=false,
  bottomline=false,
  backgroundcolor=haskellpurple!15,
  linecolor=haskellpurple,
  innerleftmargin=5pt,
  innerrightmargin=5pt,
  innertopmargin=5pt,
  innerbottommargin=5pt,
  leftmargin=0cm,
  rightmargin=0cm,
  linewidth=4pt,
]{haskellbox}
\newenvironment{haskellnote}[1][]{
  \begin{haskellbox}\sffamily\textbf{#1}\\
}{
  \end{haskellbox}
}

\newmdenv[
  skipabove=7pt,
  skipbelow=7pt,
  rightline=false,
  leftline=true,
  topline=false,
  bottomline=false,
  backgroundcolor=cyan!15,
  linecolor=cyan,
  innerleftmargin=5pt,
  innerrightmargin=5pt,
  innertopmargin=5pt,
  innerbottommargin=5pt,
  leftmargin=0cm,
  rightmargin=0cm,
  linewidth=4pt,
]{note}

\newmdenv[
  skipabove=7pt,
  skipbelow=7pt,
  rightline=false,
  leftline=true,
  topline=false,
  bottomline=false,
  backgroundcolor=orange!15,
  linecolor=orange,
  innerleftmargin=5pt,
  innerrightmargin=5pt,
  innertopmargin=5pt,
  innerbottommargin=5pt,
  leftmargin=0cm,
  rightmargin=0cm,
  linewidth=4pt,
]{exerciseBox}
\newenvironment{exercise}%
    {\begin{exerciseBox}\textbf{Exercise.}\quad\ignorespaces}%
    {\end{exerciseBox}}

\begin{document}

\maketitle

\subsection*{Introduction}

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

\subsection*{A quick note on notation}

This document is written in Literate Haskell;
the \LaTeX{} source for the document is also the Haskell source!
(The Haskell compiler knows to ignore
anything outside a @\begin{code} \end{code}@ environment
if it detects \LaTeX{} input.)

Usually, Literate Haskell does quite a good job with the formatting.
However, it does rather poorly with infix operator sections.
Thus, we explicitly explain the following notations:
\begin{itemize}
  \item
    the expression |(2 /)| is called an \emph{left operator section},
    and represents a function that will divide |2| by its argument,
    so it is the same as |\x -> 2 / x|;
  \item
    the expression |(/ 2)| is called an \emph{right operator section},
    and represents a function that will divide its argument by |2|,
    so it is the same as |\x -> x / 2|;
  \item
    the expression |x `div` y| is entered @x `div` y@,
    and is the same as |div x y|;
    that is, it treats |div| as an infix operator
    and applies it to two operands;
  \item
    the expression |(`div` 3)| is written @(`div` 3)@,
    and it is a \emph{right infix operator section},
    which does exactly what you think it would do
    (it is equivalent to |\x -> x `div` 3|).
\end{itemize}
The only really confusing one of these is the last,
where the lack of space between the trailing @`@ and the @3@
can make these expressions difficult to read.
Sorry about that!

%include doc/haskell.lhs
%include src/CAS.lhs
%include src/Liese.lhs
%include src/Main.lhs
%include doc/monads.lhs

\end{document}
