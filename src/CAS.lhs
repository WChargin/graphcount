\section{The CAS}

We'll start with a digression: we need to develop a mini-CAS.
Really, all we need is some basic symbolic expression handling,
which is not too difficult to write.
\begin{code}
module CAS where
\end{code}

We'll need these utilities later on; pay them no heed\ldots
\begin{code}
import Control.Monad (liftM, liftM2)
\end{code}

\subsection{Symbols}

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

\subsection{Expressions}

Next, we need to define some basic arithmetic expressions.
For generality's sake, we'll let these be over an arbitrary set of scalars;
thus, instead of just describing an |Expression|,
we'll describe, say, an |Expression Int|.

As before, we'll start by creating a type with a few variants:
\begin{code}
data Expression a =
\end{code}
(note that $a$ is the type of scalars).

Now, one variant that we certainly want to add is a constant term,
because any scalar can act as an expression.
So we add the following variant:
\begin{code}
                    Constant a
\end{code}

Another important core term type is a symbol as an expression;
e.g., the first term in the expression $x + 3$.
So we add this variant:
\begin{code}
                  | Literal Symbol
\end{code}

We could go ahead and add variants specifically for sums, products, etc.
But we're going to need a bunch of them,
so we might as well define them for arbitrary
unary, binary, and $n$-ary operations:
\begin{code}
                  | MonOp (a -> a) (Expression a)
                  | BinOp (a -> a -> a) (Expression a) (Expression a)
                  | NOp ([a] -> a) [Expression a]
\end{code}

These three variants work as follows.
The |MonOp| variant requires two parameters:
a function of type $a \to a$ (where $a$ is the set of scalars),
and an expression to which to apply that function.
So, we might write something like this:
\begin{spec}
double :: Int -> Int
double x = 2 * x

baseExpr :: Expression Int
baseExpr = Literal (Var 'x')

doubledExpr :: Expression Int
doubledExpr = MonOp double baseExpr
\end{spec}

Similarly, |BinOp| takes a binary function and two expressions.
So we could easily write |sumExpr = BinOp (+) exprA exprB|.

The case of |NOp| is slightly more interesting,
because it takes a function whose input is a list of scalars
and whose output is a single scalar,
and also takes a list of expressions.
So, for example, because |sum :: [Int] -> Int| is a built-in function,
we can write |NOp sum [exprA, exprB, exprC, exprD]|.
This is clearly generalizable to |NOp product|, etc.

\subsection{Environments}

To evaluate an expresion, we'll need to keep track of the variable values.
We'll force the user to provide an |Environment|,
which is just a partial function from symbols to scalars:
\begin{code}
type Environment a = Symbol -> Maybe a
\end{code}

Note that we're using |type| to define a type alias;
that is, anywhere we write |Environment a|,
we could just as well write |Symbol -> Maybe a|.

Also note that the codomain of an |Environment a| is a |Maybe a|;
this represents a value that could be absent.
For example, if we only know the values for |Var 'x'| and |Var 'y'|,
then we could write an environment as follows:
\begin{spec}
myEnvironment :: Environment Int
myEnvironment (Var 'x') = Just 12
myEnvironment (Var 'y') = Just 33
myEnvironment _         = Nothing
\end{spec}
This indicates a piecewise-defined function
where we've specified values for |Var 'x'| and |Var 'y'|
and let everything else be absent (|Nothing|).

Of course, we expect the user to define all the variables we need,
so if we ever actually come across a |Nothing| while evaluating,
we should complain loudly.

We can also create a helper function
to convert a list of bindings to an environment function.
That is, we'd like to be able to write
\begin{spec}
myEnvironment' = environmentFromList [ (Var 'x', 12), (Var 'y', 33) ]
\end{spec}
and have it behave the same as |myEnvironment|.

We can take advantage of the built-in |lookup| function,
which works as follows:
\begin{spec}
lookup 10 [(10, "dog"), (30, "cat")] === Just "dog"
lookup 20 [(10, "dog"), (30, "cat")] === Nothing
\end{spec}
So our definition might be
\begin{spec}
environmentFromList' :: [(Symbol, a)] -> Environment a
environmentFromList' table = \symbol -> lookup symbol table
\end{spec}
But this is an unnecessary use of a lambda function;
remember that |Environment a == Symbol -> Maybe a|,
so we can actually write
\begin{spec}
environmentFromList'' :: [(Symbol, a)] -> Environment a
environmentFromList'' table symbol = lookup symbol table
\end{spec}
There's one more simplification we can make.
We can now see that this function literally is |lookup|
except that its arguments are flipped.
The built-in |flip| combinator,
whose type is |flip :: (a -> b -> c) -> (b -> a -> c)|
and whose implementation is |flip f x y = f y x|,
lets us write the final version:
\begin{code}
environmentFromList :: [(Symbol, a)] -> Environment a
environmentFromList = flip lookup
\end{code}

\subsection*{Evaluation}

We now have all the pieces necessary to write the evaluator.
First, the type signature:
\begin{code}
eval :: Environment a -> Expression a -> Either String a
\end{code}
The first two components should be no surprise:
we created the |Environment|s to be used in the evaluator,
and we obviously need an expression to evaluate.

The return type may be new to you.
It's an |Either String a|,
which can either be an error message (of type |String|)
or the scalar result (of type |a|).
Here are two values of that type:
\begin{spec}
valueError :: Either String Int
valueError = Left "you forgot the flux capacitor"

valueSuccess :: Either String Int
valueSuccess = Right 71
\end{spec}
(You can remember which is which because |Right| is the right answer!)

Now we can implement the evaluator in a piecewise manner.
First, we address constant terms.
When we evaluate a constant term,
we don't care about the environment,
and the computation will always succeed.
So we can just write the rule
\begin{code}
eval _ (Constant n) = Right n
\end{code}
where the |_| indicates that we discard the first argument.

The next variant of an |Expression| is a |Literal|, like |Literal (Var 'x')|.
Here, we'll need to look something up in the environment:
\begin{code}
eval env (Literal x) = case env x of
    Just val    -> Right val
    Nothing     -> Left $ "no such symbol: " ++ show x
\end{code}
Recall that the environment is just a function,
so we apply that function to the variable symbol
and then inspect the result.
If we got a successful value, that's the return value,
so we wrap it up in a |Right| and we're done.
Otherwise, the user failed to provide a value,
so we return an error indicating which variable was missing.
Here, we use the |show| function,
which converts any |Show|able value into a string
so that we can add it to our error message.

All the remaining variants are interesting
because they involve sub-expressions.
For example, let's think about how we'd evaluate |BinOp (+) lhs rhs|:
\begin{enumerate}
  \item
    Evaluate |lhs| as a sub-expression.
  \item
    If the evaluation failed, return the error immediately.
    Otherwise, get the resulting value; call it |va|.
  \item
    Evaluate |rhs| as a sub-expression.
  \item
    If the evaluation failed, return the error immediately.
    Otherwise, get the resulting value; call it |vb|.
  \item
    Compute |va + vb|, and wrap it up in a |Right|.
\end{enumerate}
This sounds like a lot of complicated logic,
so it may surprise you that the answer is a one-liner:
\begin{code}
eval env (BinOp op a b) = liftM2 op (eval env a) (eval env b)
\end{code}
It's clear that |eval env a| and |eval env b|
correspond to steps~1 and~3 in our plan above.
The magical |liftM2| function is the ``glue''
that carries out the rest of the steps.
In particular, this function says:
\begin{quote}
I see that your function returns an |Either String a|,
so I'll treat it as a computation that can either fail or succeed.
You gave me a binary operator---|op|---that applies to the scalars,
and two results of sub-expressions,
either of which may be an error.
I'll try to apply them if I can;
otherwise, I'll return the first error.
\end{quote}
In other words, |liftM2| does \emph{exactly} what we want!

(The real beauty of |liftM2| is that it's more general than this!
In fact, it can work in any \emph{monad}; see later for a crash course.)

Similarly, of course, |liftM| works on unary functions:
\begin{code}
eval env (MonOp op a) = liftM op (eval env a)
\end{code}

We need a slightly different kind of glue for the |NOp| case.
Here's what we'll write:
\begin{code}
eval env (NOp op as) = liftM op $ mapM (eval env) as
\end{code}

Let's look at the right-most sub-expression first: |mapM (eval env) as|.

First of all, |eval env| is the result of
partially applying |eval| in the current environment;
that is, |eval env :: Expression a -> Either String a|
is a function that will evaluate any expression with a fixed environment.

Then we call |mapM|.
Here, |mapM| has type
\begin{spec}
mapM :: (Expression a -> Either String a)
     -> [Expression a]
     -> Either String [a]
\end{spec}
so it can either have a single error message or a list of results.
The call to |mapM| will apply this function to each of the |as|;
if any of them fails, it'll return that error message,
and otherwise it will return a list of all the results.
So, all together, the type of this sub-expression is
\begin{spec}
mapM (eval env) as :: Either String [a]
\end{spec}

Finally, we have this function |op :: [a] -> a|,
provided by the user,
and we have an |Either String [a]| that we'd like to apply it to.
Again, |liftM| is helpful:
it ``lifts'' our normal function into an |Either|-like function,
so |liftM op :: Either String [a] -> Either String a|.
Then we can apply this directly to the result from |mapM|
and we have the result of evaluation!

If all of this monadic stuff makes little sense to you yet,
don't worry.
There's a lot of type-juggling,
and moving into and out of different levels of abstraction
(|Either|s, in this case).
It takes a while to get used to,
but is extremely powerful once you're comfortable.

Anyway, this completes the definition for our evaluator,
and that's all we need for our mini-CAS!

\subsection{Example}

Let's write $(x_1 + x_2 + x_3)^2 + x_3^4$.

\begin{spec}
xsub :: Int -> Symbol
xsub i = Var 'x' `Sub` i

xsubs = map (Literal . xsub) [1, 2, 3]
-- Note: |xsub :: Int -> Symbol|
-- and |Literal :: Symbol -> Expression a|,
-- so |Literal . xsub :: Int -> Expression a|
-- is a function that generates expressions over any scalar set.

squaredQuantity = MonOp (^ 2) $ NOp sum xsubs

x3tothe4 = MonOp (^ 4) (Literal $ xsub 3)

expr = BinOp (+) squaredQuantity x3tothe4
\end{spec}

Then we can evaluate it with some variable assignments:
\begin{spec}
env = environmentFromList [(xsub 1, 10), (xsub 2, 20), (xsub 3, 30)]
result = eval env expr  -- comes out to |Right 813600|
\end{spec}

Note that we can write nice composable units like |xsub|
for building up larger expressions,
and that we can use existing Haskell functions like |map| to do the same.
