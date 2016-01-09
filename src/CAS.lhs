We now need to digress a bit to develop a mini-CAS.
Really, all we need is some basic symbolic expression handling,
which is not too difficult to write.
\begin{code}
module CAS where
\end{code}

We'll start by defining a data type for symbols.
For our purposes, it'll be sufficient to have plain variables (e.g., $x$)
and symbols with subscripts (e.g., $x_3$).
So we're defining a data type with two variants:
\begin{code}
data Symbol = Var Char
            | Symbol `Sub` Int
            deriving (Eq, Show)
\end{code}
For example, we might use a variable |Var 'x'|,
or its subscript form |Var 'x' `Sub` 3|.
Note that, because the |Sub| constructor accepts any symbol,
we can take subscripts of subscripts:
e.g., |Var 'x' `Sub` 3 `Sub` 6| is valid.

The first |deriving| declaration is important:
it says that we have a total equivalence relation on |Symbol|,
so that the operation |Var 'x' == Var 'x'| is well-defined.
(The compiler automatically figures out the equivalence relation
and implements it for us.)
The second one just lets us display |Symbol|s for debugging or output.
