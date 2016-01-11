\clearpage
\appendix
\section{Monads}

Okay, here we go.
This will not be comprehensive, but perhaps it will be useful.

I have to introduce a new colored box for this section:
\begin{exercise}
This is an exercise.
Do it before proceeding.
\end{exercise}
(The answers will often immediately follow the exercises.)

\bigskip

The core unit of computation is the function.
But, to some degree, functions represent an idealized view of the world.
In many cases, we want our computations to have some more structure.
For example, the following facts are all true:
\begin{itemize}
  \item
    Some computations may sometimes fail to return a value;
    e.g., looking for an item in a list if it's not actually there.
  \item
    Some computations may sometimes return a helpful error message
    instead of the expected result;
    e.g., parsing a file that may be corrupt.
  \item
    Some computations may be nondeterministic;
    e.g., rolling a die.
  \item
    Some computations may be nondeterministic and with non-uniform probability;
    e.g., rolling two dice and computing their sum.
  \item
    Some computations may require reading from some global configuration file
    that would be awkward or annoying to pass to every function;
    e.g., running a physics simulation with lots of parameters.
  \item
    Some computations may want to perform I/O;
    e.g., writing to a file or getting user input.
  \item
    Some computations may want to update some persistent internal state;
    e.g., evaluating a computer program
    in a language that allows mutation of variables.
\end{itemize}
Monads are%
\footnote{%
    Well, this is one way to view them, anyway, and it works for right now.
}
an abstraction over the notion of computation itself,
and provide a way to easily express and compose
computations that have structures like these and others.

\subsection{A motivating example: genealogy}

Suppose we have a family tree.
In a family tree, each person has two biological parents:
a mother and a father.

However, this being the Real World,
we may sometimes lack genealogical data for one or more of the parents.
Suppose we have functions like this:
\begin{spec}
getMother :: Person -> Maybe Person
getFather :: Person -> Maybe Person
\end{spec}
Recall that the |Maybe| type indicates a possible absence of a value.
So maybe |getMother isaac == Just sarah|,
but |getFather isaac == Nothing|
don't know who Isaac's father was.

Now suppose we want to create the function
|getPaternalGrandfather :: Person -> Maybe Person|.
If we didn't have all this |Maybe| business,
we could just write |getPaternalGrandfather = getFather . getFather|.
But instead we have to write
\begin{spec}
getPaternalGrandfather person =
    let maybeFather = getFather person in
        case maybeFather of
            Just father     -> getFather father
            Nothing         -> Nothing
\end{spec}

\begin{exercise}
What if we wanted to find the father's father's father---%
that is, the paternal great-grandfather?
Can you write |getPPGreatgrandfather :: Person -> Maybe Person|?
\end{exercise}

We might write
\begin{spec}
getPPGreatgrandfather person =
    let maybeFather = getFather person in
        case maybeFather of
            Just father     ->
                let maybeGrandfather = getFather father in
                    case maybeGrandfather of
                        Just grandfather    -> getFather grandfather
                        Nothing             -> Nothing
            Nothing         -> Nothing
\end{spec}
Clearly, this gets worse when we want to get, say, a great$^n$-grandmother.
This is hardly elegant.

We'll try to come up with a clean solution for this and similar problems.

\subsubsection{Digression: |fmap|}

We'll start by noting the following:
For any type |a|, there is a type |Maybe a|,
and for any function |f :: a -> b|,
there's a function that |fmap f :: Maybe a -> Maybe b|.
In particular,
\begin{spec}
fmap :: (a -> b) -> (Maybe a -> Maybe b)
fmap f Nothing      = Nothing
fmap f (Just x)     = Just (f x)
\end{spec}
For example:
\begin{spec}
double          = (*) 2
doubleMaybe     = fmap double

doubleMaybe (Just 10)   == Just 20
doubleMaybe Nothing     == Nothing
\end{spec}
As you can see, the |fmap| function
lets us use functions \emph{inside} a |Maybe|.

\subsubsection{A helper function: |join|}

Let's write a function:
\begin{spec}
join :: Maybe (Maybe a) -> Maybe a
join (Just (Just x))    = Just x
join (Just Nothing)     = Nothing
join Nothing            = Nothing
\end{spec}

From this, we can see that |join (Just (Just 10)) = Just 10|
and the |join| of anything with a |Nothing| anywhere is |Nothing|.
In a sense, |join| ``squashes out'' one level of |Just|s
and only gets the value at the very end.

Why is this useful?
Well, it may not look like it,
but we now have all the tools to construct |getPaternalGrandfather|!

\subsubsection{Take two}

We can now do two things:
\begin{itemize}
  \item
    We can take a normal function
    and slap a |Maybe| onto its domain and codomain using |fmap|.
  \item
    We can squash a |Just (Just a)| into a |Just a| using |join|.
\end{itemize}

\begin{exercise}
Can you use this to write |getPaternalGrandfather|?
Think about the types:
what do you have?
what do you want?
what can you do?
\end{exercise}
(If this sounds remarkably like a proof strategy, by the way,
then you'll love the Curry--Howard correspondence!)
\smallskip

Well, |getFather :: Person -> Maybe Person|\ldots

\ldots so |fmap getFather :: Maybe Person -> Maybe (Maybe Person)|\ldots

\ldots so |fmap getFather . getFather :: Person -> Maybe (Maybe Person)|\ldots

\ldots so |join . fmap getFather . getFather :: Person -> Maybe Person|\ldots

and all of a sudden, we have a function of type |Person -> Maybe Person|
that suspiciously contains two uses of |getFather|.

Is this function |getPaternalGrandfather|?
You bet!

\begin{note}
You're supposed to pause and think between paragraphs, by the way.
Go back and read that again.
\end{note}

\subsubsection{A thousand words}

Maybe this helps:
\begin{equation*}
    \begin{tikzcd}
        |Person| \rar{|getFather|}
            & |Maybe Person| \rar[bend left]{|fmap getFather|}
                & |Maybe Maybe Person| \lar[bend left]{|join|}
        \end{tikzcd}
\end{equation*}
We can iterate that cycle as many times as we like.
If we ever hit a |Nothing|,
the |join| will make sure that we keep getting |Nothing|s.

Note that this diagram is most certainly \emph{not} commutative!
If it were, then we would need |join . fmap getFather|
(or at its least restriction to the range of |getFather|)
to be the identity map,
which would render it rather useless.

\subsection*{What just happened?}

Was that ``monads''?
Well, kind of.
The |Maybe| type constructor is \emph{a} monad.
The real power of monads is that there are many things that behave similarly.
Let's look at another one!

\subsection{Another example: nondeterminism}

Suppose we want to model computations that could return zero or more values
instead of just one.
Suppose further that we don't care about the probabilities,
or we assume they're all uniform;
we just want to know which are possible.

How might we model this?
Well, we can just return a list of all the possibilities!

\begin{spec}
rollDie :: [Int]
rollDie = [1..6]
\end{spec}

We're now treating lists as models of nondeterministic computations.
You probably want to see a function, so here you go:
\begin{spec}
-- |rollDn|: e.g., roll a D20
rollDn :: Int -> [Int]
rollDn n = [1..n]
\end{spec}

We've seen that all the hard stuff happens when we try to compose these things.
So let's do that.
Suppose we have all the dice from~D1 to~D$n$.
Let's roll a D$n$, and then roll another die of the number we just got.
So if we get a~14 when we roll the~D$n$, the next die will roll will be the~D$n$.

Of course, by ``roll,''
we mean ``define a computation of nondeterministic result,''
where the ``result'' we care about is, say,
the value of the last die, with repetition
(so if there are $k$ ways to get it, we include it $k$ times).

\subsubsection{The bad way}

As before, we'll start with the brute-force way:
\begin{spec}
-- Given a maximum die number $n$, roll two dice as described above.
rollTwice :: Int -> [Int]
rollTwice n =
    -- Find all the values we could get for the second die\ldots
    let     secondDieTypes      = rollDn n

    -- \ldots then, for each one of them, roll that die\ldots
            secondDieValues     = map rollDn secondDieTypes

    -- \ldots and, finally, flatten the results.
    in      concat secondDieValues
\end{spec}

Now, though, what if we wanted to add one more iteration?
\begin{spec}
-- Given a maximum die number $n$, roll three dice like described above.
rollThrice :: Int -> [Int]
rollThrice n =
    let     secondDieTypes      = rollDn n
            secondDieValues     = map rollDn secondDieTypes
            flatSecondDies      = concat flatSecondDies
            thirdDieTypes       = flatSecondDies
            thirdDieValues      = map rollDn thirdDieTypes
    in      concat thirdDieValues
\end{spec}
To be honest, this isn't \emph{awful}---%
at least it's not nested like the |Maybe| example was---%
but it sure isn't great.
For example, isn't obvious how to define |rollKTimes k n|.

Hey, here's an idea---|fmap| and |join| were useful with |Maybe|.
How about with lists?

\subsubsection{|fmap| for lists}

When we introduced |fmap|,
we said that it let us ``use functions inside a |Maybe|.''
That is, we had |fmap :: (a -> b) -> (Maybe a -> Maybe b)|.

Now we have lists, so we want to ``use functions inside a list,''
and we probably want |fmap :: (a -> b) -> ([a] -> [b])|.

But we already have that!
In fact, we've just used it!
It's |map|!

\begin{spec}
fmap :: (a -> b) -> ([a] -> [b])
fmap = map
\end{spec}

\subsubsection{|join| for lists}

When we introduced |join|,
we said that it let us ``squash out'' one level of |Just|s,
and we had |join :: Maybe (Maybe a) -> Maybe a|.

So we probably want |join :: [[a]] -> [a]|,
and we want to squash out a level of lists
without losing any information
(e.g., we don't want to just say
``always take the first list and discard the rest'').

Again, this function already exists, and we've already used it!
This time, it's |concat|, which flattens one level of lists!

\begin{spec}
join :: [[a]] -> [a]
join = concat
\end{spec}

\subsubsection{Rewriting with |fmap| and |join|}

Let's try it again.

\begin{spec}
rollTwice :: Int -> [Int]
rollTwice n = join $ fmap rollDn (rollDn n)
\end{spec}

\begin{spec}
rollThrice :: Int -> [Int]
rollThrice n = join $ fmap rollDn $ join $ fmap rollDn (rollDn n)
\end{spec}

Well, at least it looks a bit nicer.
That last line---with the repeated |join $ fmap rollDn|---%
\ignore$
is just \emph{begging} to be improved further,
and indeed we will.
But we'll need a few more tools to do that.

Here's that diagram again:
\begin{equation*}
    \begin{tikzcd}
        |Int| \rar{|rollDn|}
            & |[Int]| \rar[bend left]{|fmap rollDn|}
                & |[[Int]]| \lar[bend left]{|join|}
    \end{tikzcd}
\end{equation*}

\subsection{A new friend: |bind|, or |(>>=)|}

I'm about to introduce something ``new'' that's not really new at all.
Here it goes:
\begin{spec}
bind x f    = join $ fmap f x
x >>= f     = bind x f              -- or just |(>>=) = bind|
\end{spec}
\ignore$

Do you see that this just gives a name to the pattern we've been using?
\begin{spec}
getPaternalGrandfather p = getFather p >>= getFather
getPPGreatgrandfather p = getFather p >>= getFather >>= getFather
rollTwice n = rollDn n >>= rollDn
rollThrice n = rollDn n >>= rollDn >>= rollDn
\end{spec}

\subsubsection{What does |bind| do?}

Even though we have the definition above,
|bind| is not very helpful to us if we had no intuition.
Let's add it to our catalog of monadic functions:
\begin{itemize}
  \item
    |join| squashes one level of a monad (like |Maybe| or lists);
  \item
    |fmap| pulls a function into a monad;
  \item
    {
      \bfseries
      |(>>=)| does an action that might introduce ``too much'' monad-ness
      {\normalfont (e.g., too many levels of |Just|s or nested lists)}
      and then squashes out the extra stuff,
    }
    so, as a result, you don't end up any more or less monad-y
    than how you started.
\end{itemize}

\subsubsection{Why do we care?}

At the very least, the rewritten versions
of |getPPGreatgrandfather| and |rollThrice|
are nicer to look at!
This also lets us not have to visit that second-level list in our diagram:
\begin{equation*}
    \begin{tikzcd}
        |Int| \arrow{r}{|rollDn|}
            & |[Int]| \arrow[loop right]{}{|(>>= rollDn)|}
    \end{tikzcd}
\end{equation*}

More importantly, it now becomes clearer
how we would develop the following functions,
which we spoke of earlier:
\begin{spec}
getNthPaternalGrandfather :: Int -> Person -> Maybe Person
rollKTimes :: Int -> Int -> [Int]
\end{spec}

We can do so as follows:
\begin{spec}
-- Get someone's $n$th pater-$n$-nal great$^{n-2}$-grandfather.
-- That is, if $n = 0$, the result is |Just| the person;
-- if $n = 0$, it is the same as the result of |getFather|; etc.
getNthPaternalGrandfather :: Int -> Person -> Maybe Person
getNthPaternalGrandfather 0 p   = Just p
getNthPaternalGrandfather n p   = previous >>= getFather
  where
    previous = getNthPaternalGrandfather (n - 1) p
\end{spec}

Alternatively, we could have done the following,
which is perhaps more reminiscent of the diagram above:
\begin{spec}
getNthPaternalGrandfather' :: Int -> Person -> Maybe Person
getNthPaternalGrandfather' n p  = iterate (>>= getFather) p !! n
\end{spec}
Note that |iterate f x = [x, f x, f (f x), ...]|,
and |(!!)| is the list index operator
(e.g., |["a", "b", "c", "d", "e"] !! 3 = "d"|),
so this constructs an (infinite) list of all the fathers
and takes the $n$th one.
\begin{haskellnote}[Infinity and laziness]
Infinite lists are totally kosher in Haskell,
because Haskell is a \emph{lazily evaluated language}.
Basically, this means that the Haskell runtime never actually does anything
until you actually need the result \emph{right now}
(e.g., to print to the screen or write to a file).

So when you write |iterate f x|, or even just |[1..]|,
the runtime says, ``sure, I can do that'';
it's only when you actually try to use that value
that anything is computed.

In this case, the list indexing operator
only needs to traverse the first $n$~elements of the list.
So the runtime keeps advancing through the elements of the list,
\emph{generating them as it needs to},
until it finds the one you're looking for,
at which point it stops.

(In fact, it doesn't even evaluate the other elements of the list
if it doesn't have to!
In this case, it does, because each element depends on the previous,
but you can run, e.g., |[error "bad", "good"] !! 1| and get |"good"|
because the error is never encountered.)
\end{haskellnote}

\subsubsection{What is |bind|'s type?}

In the examples above, I conveniently omitted the type signature for |bind|.
Is it |bind :: [a] -> (a -> [b]) -> [b]|,
or |bind :: Maybe a -> (a -> Maybe b) -> Maybe b|?

The answer is---both!
Let's not cover this too deeply right now,
but suffice it to say that if |m| is a monad,
then we can use |bind :: m a -> (a -> m b) -> m b|.
In general, we write
\begin{spec}
(>>=) :: Monad m => m a -> (a -> m b) -> m b
\end{spec}

Examples of monads are |Maybe| and $[\,\cdot\,]$ (lists).

If this is confusing and you don't care, ignore it.

\subsection{Monads in the CAS}

Here's one final example
before we go back to more generalized abstract nonsense.
The monad we're using in our CAS---the place this all started---%
isn't either |Maybe| or $[\,\cdot\,]$.

The monad we're using is |Either String|.
This represents a computation that might succeed,
or might return an error message that's a |String|.

\begin{haskellnote}[Monads need exactly one type parameter]
Note that we said |Maybe| is a monad,
but we always talk about a |Maybe Int| or a |Maybe Person|.
We said that $[\,\cdot\,]$ is a monad,
but we always talk about |[Int]| or |[String]|.
That's because |Maybe| and $[\,\cdot\,]$ each require one type parameter
to form a concrete type that can take on values.
We say that |Maybe| and $[\,\cdot\,]$ have kind |* -> *|.

By constrast, with |Either|, we talk about things like |Either String Int|.
That is, |Either| is not a type, nor is |Either String|;
we need \emph{two} type parameters.
So |Either| has kind |* -> * -> *|.

However, just as we can partially apply functions---like |double = (*) 2|---%
we can partially apply type constructors.
So |Either :: * -> * -> *|;
we can apply it once to get |Either String :: * -> *|,
and again to get |Either String Int :: *|.

Thus, |Either String| is a type constructor that requires one type parameter,
so it is eligible to be a monad, if it satisfies certain properties---%
in fact, it does.
\end{haskellnote}

\subsubsection{|fmap| and |join|}

\begin{exercise}
  \begin{enumerate}
    \item
      What should the types of |fmap| and |join| be now?
    \item
      How should we implement |fmap| and |join|
      to be consistent with the intuition we've developed,
      while losing as little information as possible?
  \end{enumerate}
\end{exercise}

Did you try it?
(The answers will appear on the next page.)

\clearpage
For the first exercise, you should probably have found the types
\begin{itemize}
  \item
    |fmap :: (a -> b) -> (Either String a -> Either String b)|, and
  \item
    |join :: Either String (Either String a) -> Either String a|.
\end{itemize}
You can find this by just replacing |Maybe| with |Either String|
in the types for |fmap| and |join| in the |Maybe| monad.

For the second question, |fmap| should have been rather straightforward,
using pattern matching:
\begin{spec}
fmap :: (a -> b) -> (Either String a -> Either String b)
fmap f (Right v)    = Right (f v)
fmap f leftVal      = leftVal
\end{spec}
\begin{haskellnote}[Variable patterns match any value]
Note that the |leftVal| in the |fmap f leftVal| pattern
isn't directly related at all to the |Left| variant of the type.
We could have written |fmap f x = x| for that branch
and it would have done the same thing.
That is, |leftVal| matches any value.

However, if the value were a |Right|,
then it would already have matched on the previous pattern.
This, combined with the fact that
we know the |Either| data type has exactly two variants,
proves that |leftVal| really must be a |Left|.

We could also have written |fmap f (Left e) = Left e|,
but there's no need to destructure and then restructure the value.
\end{haskellnote}
For |join|, you could have explicitly covered all three cases:
\begin{spec}
join :: Either String (Either String a) -> Either String a
join (Right (Right v))  = Right v
join (Right (Left e))   = Left e
join (Left e)           = Left e
\end{spec}
\ldots or you could have consolidated that a bit:
\begin{spec}
join :: Either String (Either String a) -> Either String a
join (Right either)     = either
join (Left e)           = Left e
\end{spec}
\begin{haskellnote}[Values can't change types]
You may be wondering why I wrote |join (Left e) = Left e| in the last line
when I~had just said that that's not necessary!
The issue is as follows.

Suppose we had written |join leftVal = leftVal|.
Then we know that |leftVal| must be an |Either String (Either String a)|.
But the result of the function must be just an |Either String a|,
of which |leftVal| is not a member.

In other words, the |Left| in the |(Left e)| in the pattern
refers to the data constructor for the |Either String (Either String a)| type,
but the |Left| on the right-hand side
is the data constructor for the |Either String a| type.
We don't have a value of type |Either String a|,
so we need to take the |String| from the |Either String (Either String a)|
and use that to build our result.
\end{haskellnote}

\subsection{The missing piece: |unit|, or |return|}

There's one more factor common to all monads
(indeed, by requirement).
I've postponed it because it's not particularly interesting,
but we'll need it to proceed further, so here goes.

It's called |unit|.
Here's how it's implemented for our three example monads:
\begin{spec}
unit :: a -> Maybe a
unit x = Just x
-- or just |unit = Just|

unit :: a -> [a]
unit x = [x]

unit :: a -> Either String a
unit x = Right x
-- or just |unit = Right|
\end{spec}

Do you see what it does?
It takes a normal value and wraps it up in a monad
in the simplest way possible.

That's it.

The only sticky point is in the name.
The Haskell standard library calls it |return|,
which I think, because of its connotations,
is useful in practice but awful for learning.
Let me strongly note the following:
\textbf{the function |return| is not a control statement}
like the @return@ you may have seen in (many) other programming languages.
It is precisely the function |unit| defined above.

The categorists call it $\eta$, which is fine with me.

So now we have the following things that we can use in a monad:
\begin{itemize}
  \item
    |return| pulls a normal value up into a monad;
  \item
    |join| squashes one level of a monad;
  \item
    |fmap| pulls a function into a monad;
  \item
    |(>>=)| does an action that might introduce ``too much'' monad-ness
    (e.g., too many levels of |Just|s or nested lists)
    and then squashes out the extra stuff,
    so, as a result, you don't end up any more or less monad-y
    than how you started.
\end{itemize}

Let's visit a diagram in, say, the |Maybe| monad,
supposing we already have a function |f :: a -> b|:
\begin{equation*}
    \tikzset{%
        deeper/.style={red},        % more levels of monad
        shallower/.style={blue},    % fewer levels of monad
        across/.style={purple},     % across from |m^n a| to |m^n b|
    }
    \def\spacing{2pt}
    \begin{tikzcd}
        |a|
        \dar[across]{|f|}
        \rar[deeper]{|return|}
            & |Maybe a|
            \rar[deeper,yshift=\spacing]{|return|}
            \dar[across]{|fmap f|}
                & |Maybe (Maybe a)|
                \lar[shallower,yshift=-\spacing]{|join|}
                \dar[across]{|fmap (fmap f)|}
        \\
        |b|
        \rar[deeper]{|return|}
            & |Maybe b|
            \rar[deeper,yshift=\spacing]{|return|}
                & |Maybe (Maybe b)|
                \lar[shallower,yshift=-\spacing]{|join|}
    \end{tikzcd}
\end{equation*}

One very important fact about this diagram
is that we have a lot of freedom to move around within the monad---%
we can always take types to higher levels of monads with |return|,
and we can squash multiple monads to fewer with |join|---%
but \textbf{we can never escape the monad completely!}
In this case, that means that
we can never take a computation that might not have a result
(a |Maybe a|)
and force it to come up with some type of result on the spot (an |a|).

Think about it---suppose I give you a value
of type |Maybe beta| for some~|beta|,
which you know nothing about.
You might even know what that type \emph{is}:
it could be defined in some library that's not accessible to you.
Is it reasonable for me to ask you for some normal value of type |beta|?

Well, if I~happen to have given you a |Just b| for some |b :: beta|,
then, yeah, you could extract it and give it to me.
But if I~give you a |Nothing|,
there's no way you could give me a value!
Suppose |beta| actually represents the type of
all complete ordered fields not isomorphic to~$\mathbb{R}$---%
you'd be hard-pressed to give me a |beta| then!
Plus, even if |beta| were something mundane like |Int|,
you couldn't just give me |0|
because \emph{you don't know} that |beta| is |Int|;
your function has to work for \emph{all} types |beta|.

This diagram, by the way, is a commutative diagram in $\mathbf{Hask}$,
the category of Haskell types and functions.
This diagram and a few others must commute
for a functor (here, |Maybe|) to be a monad.
The categorists would write it like this:
\begin{equation*}
    \tikzset{%
        deeper/.style={red},        % more levels of monad
        shallower/.style={blue},    % fewer levels of monad
        across/.style={purple},     % across from |m^n a| to |m^n b|
    }
    \def\spacing{2pt}
    \begin{tikzcd}
        |a|
        \dar[across]{|f|}
        \rar[deeper]{\eta_{|A|}}
            & |M a|
            \rar[deeper,yshift=\spacing]{\eta_{|M a|}}
            \dar[across]{|M f|}
                & |M (M a)|
                \lar[shallower,yshift=-\spacing]{\mu_{|a|}}
                \dar[across]{|M (M f)|}
        \\
        |b|
        \rar[deeper]{\eta_{|b|}}
            & |M b|
            \rar[deeper,yshift=\spacing]{\eta_{|M b|}}
                & |M (M b)|
                \lar[shallower,yshift=-\spacing]{\mu_{|b|}}
    \end{tikzcd}
\end{equation*}

Note that $M$ is a functor (here, |M| is |Maybe|),
and $\eta : 1_{\mathbf{Hask}} \to M$
and $\mu : M^2 \to M$
are natural transformations.


\subsection{The real power: general monadic functions}

So far, we've looked at three monads in isolation:
|Maybe|, $[\,\cdot\,]$, and |Either e| (in particular, |Either String|).
But, as nice as it is to notice similarities among these type constructors,
we haven't yet really taken advantage of those similarities
by constructing machinery that can operate on any of them!

\begin{exercise}
Actually, that's false.
If we take the fundamental units of a monad
to be |fmap|, |return|, and |join|,
what function have we implemented just in terms of those
that would work in any monad?
\end{exercise}

The answer is |bind|---that's why I said it really wasn't anything new,
because we can construct it entirely from existing pieces.

\begin{exercise}
In actuality, though, Haskell doesn't take |join| to be a fundamental unit;
instead, a monad must provide |fmap|, |return|, and |bind|.
Can you implement |join| in terms of |bind|,
or is there some information lost in the way we approached it (with |join|)?
\end{exercise}

Well, think about the following definition:
\begin{spec}
join :: Monad m => m (m a) -> m a
join x = x >>= id
\end{spec}
(Note that |id :: a -> a| is the identity function.)

What does this mean?
Recall our intuition for |bind| as doing something
that introduces ``too much'' monad-ness
and then stripping off the extra stuff.
Here, we're saying that we \emph{already have} too much monad-ness,
so we just do nothing to the input and then strip off the extra stuff,
which is precisely what |join| does.

But we've already talked about |bind|.
Let's talk about another.

\medskip

Let me pose two problems.
\begin{itemize}
  \item
    Suppose we have an environment of variable bindings
    (the |Environment| type from the |CAS| module),
    and we have a list of |Symbol|s.
    We want to get all the variables' values,
    or fail if any of them doesn't exist.
    So, for example, if we have bindings for $x$ and $y$,
    then |[Var 'x', Var 'y']| might map to |Just [5, 10]|,
    but |[Var 'x', Var 'z']| would map to |Nothing|
    (not |[Just 5, Nothing]|).
  \item
    Suppose we have a list of symbolic |Expression|s
    and we want to evaluate them all
    and collect the results in a list,
    but if any of them returns an error
    we should stop and return that error instead.
    For example, |[exprA, exprB, exprC]|
    might map to |Right [valA, valB, valC]|,
    but |[exprA, badExpr, exprC]|
    might map to |Left "bad expression"|.
\end{itemize}

Can you see that these problems are similar?
They could each be described by the following process:
\begin{quote}
Suppose we have a bunch of values of type |a|;
that is, we have a list of type |[a]|.
Suppose also that we have a function |f :: a -> m b|
that takes an~|a| and creates a computation that returns a~|b|.
I'd like to create a new computation
that passes each of my values through this function,
evaluates them in sequence,
and gathers up the results in a (now monadic) list;
that is, my computation should have type |m [b]|.
\end{quote}
Could we write such a function that would work
for |Maybe|s, for lists, and for |Either String|s?
That's our goal for this subsection.

Before we proceed, however,
does this sound familiar?
Can you think of a similar, non-monadic function?

\begin{exercise}
Does it?
Can you?
\end{exercise}

The answer to the first part, of course, is, ``yes''!
We should be quite familiar with a function that does the following!
\begin{quote}
Suppose we have a bunch of values of type |a|;
that is, we have a list of type |[a]|.
Suppose also that we have a function |f :: a -> b|
that takes an~|a| and returns a~|b|.
I'd like to pass each of my values through this function
and gather up the results in a list;
that is, my result should have type |[b]|.
\end{quote}
This function's name, of course, is |map|,
and its type is |map :: (a -> b) -> [a] -> [b]|.

We thus want a monadic version of |map|; we'll call it |mapM|.
Comparing the two descriptions above,
we can see the evident type adjustments
to find that we're looking for
|mapM :: Monad a => (a -> m b) -> [a] -> m [b]|.

\begin{exercise}
In practice, is this different from just |map|?
How?
\end{exercise}

It sure is.
If |f :: a -> m b| and |as :: [a]|,
then |map f as :: [m b]|, while |mapM f as :: m [b]|.
These can be very different.
In the former case, any of the computations can fail
and we'll still get the results from the rest;
e.g., we could get
\begin{quote}
|[Just 3, Nothing, Just 7]|,
\end{quote}
or
\begin{quote}
|[Right "baboon", Left "animal escaped", Right "gorilla"]|.
\end{quote}

Let's implement |mapM| together.
What happens if the list we're given is empty?
Well, then the computation should always succeed,
and it should monadically contain the empty list---%
e.g., we might want |Just []| or |Right []|,
because it succeeded but there was nothing to act on.
The generic term for ``succeed'' is |return|.
So
\begin{spec}
mapM _ [] = return []
\end{spec}
(We ignore the function by matching it against an underscore.)

Now the recursive case.
Suppose we get a list that's not empty.
Then it has a head (first element) and a tail (other elements).
Call the list |x:xs| (where |x| is the head and |xs| is the tail).

What do we want?
An |m [b]|.
What do we have?
We have |x :: m a|, |xs :: [m a]|, and |f :: a -> m b|.
But we also have ourselves---|mapM :: Monad a => (a -> m b) -> [a] -> m [b]|.
So we can recursively call ourselves with |xs|
to get an |m [b]|.
\begin{note}
As with all recursively defined functions,
the proof of termination relies upon the fact that
the list is getting smaller at each step;
with each recursive call,
we're making progress toward the base case of the empty list.
\end{note}
Okay, so |mapM f xs :: m [b]|.
We can also clearly obtain |f x :: m b|.
Now we can do the following:
\begin{spec}
mapM f (x:xs) =
    x >>= \vx ->
        mapM f xs >>= \vxs ->
            return (f vx : vxs)
\end{spec}

Let's tease this apart with a type analysis.
\begin{itemize}
  \item
    Consider the outer binding
    (the one with |x| on the left-hand side,
    which starts on the second line of the definition).
    Its type must be |(>>=) :: m alpha_1 -> (alpha_1 -> m beta_1) -> m beta_1|
    for some values of |alpha_1| and |beta_1|.

    We know that the result of |mapM f (x:xs)| must be an |m [b]|,
    so we must have |beta_1 = [b]|.

    We also know that |x :: m a|, so we must have |alpha_1 = a|.

    Thus, the lambda expression on the right-hand side
    must have type |a -> m [b]|.
  \item
    Because the lambda expression has type |a -> m [b]|,
    we know that |vx| must be of type |a|,
    and the return value of the lambda must be of type |m [b]|.
  \item
    Now we have another binding;
    call its type |(>>=) :: m alpha_2 -> (alpha_2 -> m beta_2) -> m beta_2|.
    Because the binding is the return value of the enclosing lambda,
    which we know must have type |m [b]|,
    we must have |m beta_2 = m [b]|, so |beta_2 = [b]|.

    The left-hand side of the binding
    is the result of a recursive call to |mapM|.
    This recursive call will have return type |m [b]|,
    so we must have |m alpha_2 = m [b]|.
    Thus, |alpha_2 = [b]|.

    The right-hand side of the lambda must have type |alpha_2 -> m beta_2|.
    We know this is |[b] -> m [b]|.
    Thus, its parameter must be |vxs :: [b]|.
  \item
    Thus, in the innermost lambda expression (on the last line),
    we have access to |vx :: a| and |vxs :: [b]|.
    The expression |f vx| thus has type |b|,
    so we can write |(f vx : vxs) :: [b]|.
    We're almost done.
  \item
    The return value of the final lambda must be of type |m [b]|,
    and we just created a |[b]|.
    We bring this up into the monad by calling |return|.
\end{itemize}

Here's another use case for |mapM|.
Suppose, hypothetically, that we wanted to
download a hundred books from Springer while they're free for a limited time.

Suppose we have the URLs for all the books, so |books :: [URL]|.
Suppose we also have a function |download :: URL -> IO Filename|
that downloads books to a file.
\begin{haskellnote}[The |IO| type]
  This type means that |download| performs an I/O action
  and then returns the name of the file where the book is stored.
  For example, the action of getting user input has type |IO String|,
  because you do some I/O and then get a string.

  The |download| I/O action, presumably,
  downloads the contents of the given URL and saves them to disk,
  then returns the path to the file on disk.
\end{haskellnote}
Then we'd like to download all the books and get their URLs.
Here's how we do it:
\begin{spec}
downloadBooks :: IO [FilePath]
downloadBooks = mapM download books
\end{spec}
\begin{haskellnote}[I/O actions are ``sticky'']
  Note that |downloadBooks| is still an |IO| action,
  because we can't get out of the |IO|~monad!
  When the program is actually run,
  the Haskell runtime takes case of actually performing the actions;
  due to laziness, we don't (and can't) do this ourselves.%
  \footnotemark
\end{haskellnote}
\footnotetext{%
  Yes, |unsafePerformIO :: IO a -> a| exists.
  As its name implies, you should think twice before using it.
}

\subsection{The |do|-notation}

Okay: I'm about to introduce something that's very practically useful,
but only because it hides the details
of a lot of what we've been talking about.
It's a form of \emph{syntactic sugar}---%
that is, a nice syntax that's converted by the compiler
to something simpler.

It's called |do|-notation,
and even this name is potentially confusing:
\textbf{it doesn't ``do'' anything}.
In particular, it doesn't perform any I/O actions,
it doesn't prevent laziness,
and it doesn't change the execution order
(these are all common misconceptions).

Let me reiterate:
|do|-notation is \textbf{exactly the same} as what we've been doing,
but it looks nicer.

Here's some nonsense code:
\begin{spec}
longFormExample :: Foo -> Maybe Bar
longFormExample foo =
    flux foo >>= \bar ->
        baz foo (quux bar) >>= \zing ->
            return (zing bar)
\end{spec}
And here's what it looks like in |do|-notation:
\begin{spec}
shortFormExample :: Foo -> Maybe Bar
shortFormExample = do
    bar <- flux foo
    zing <- baz foo (quux bar)
    return (zing bar)
\end{spec}

That is, bindings can be made with |<-| instead of with |(>>=)|,
and remain in scope for the rest of the |do|-block.
The last line of the |do|-block is the result.

The evident advantage is that the |do|-notation removes nesting.

Here's our |mapM|:
\begin{spec}
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x:xs) = do
    vx <- x
    vxs <- mapM f xs
    return (f vx : vxs)
\end{spec}

\begin{exercise}
We wrote |join| in terms of |bind| and vice versa earlier.
Can we rewrite these in |do|-notation?
Do so.
\end{exercise}

Here's |join| in terms of |bind|:
\begin{spec}
join :: Monad m => m (m a) -> m a
join vx = do
    x <- x
    vx
\end{spec}

\begin{exercise}
Hold on---where's the |bind|?
\end{exercise}

It's hidden!
That's the point of |do|-notation: the bindings are implicit.

A consequence of this is that, yes,
we can write |bind| in terms of |join| using |do|-notation,
but it won't actually \emph{use} the |do|-notation:
\begin{spec}
bind :: Monad m => m a -> (a -> m b) -> m b
bind x f = do
    join (fmap f x)
\end{spec}

\subsection{A library of monads}

At the very beginning of this section, we mentioned some types of computations.
Now we'll list the corresponding monad types.

\begin{itemize}
  \item
    Some computations may sometimes fail to return a value;
    e.g., looking for an item in a list if it's not actually there.

    These computations can be normal functions that return a |Maybe a|.
    In |Maybe|, |join| takes a |Just (Just x)| to |Just x|
    and anything else to |Nothing|,
    and |return| takes |x| to |Just x|.

    The meaning of |fmap| is:
    ``if the computation succeeded, feed its output into this next one.''
  \item
    Some computations may sometimes return a helpful error message
    instead of the expected result;
    e.g., parsing a file that may be corrupt.

    A computation that might return an error of type |e|
    can be a normal function that returns an |Either e a|.
    In |Either e|, |join| takes |Right x| to |x|,
    and |Left e| to |Left e|;
    |return| takes |x| to |Right x|.

    The meaning of |fmap| is:
    ``if the computation succeeded, feed its output into this next one;
    otherwise, keep the error around.''
  \item
    Some computations may be nondeterministic;
    e.g., rolling a die.

    These computations can be normal functions that return a |[a]|.
    In $[\,\cdot\,]$, |join| is |concat|
    (it flattens |[[1, 2], [3, 4]]| to |[1, 2, 3, 4]|),
    and |return| takes |x| to |[x]|.

    The meaning of |fmap| is:
    ``take all the possible results of the computation
    and apply this next one to each of them.''
  \item
    Some computations may be nondeterministic and with non-uniform probability;
    e.g., rolling two dice and computing their sum.

    These computations can be normal functions that return a |Dist a|,
    where |Dist| is the distribution monad.
    It's basically like a list where each element has a probability.
    In |Dist|, |join| computes conditional probabilities by multiplication,
    and |return| takes |x| to the constant distribution
    that returns |x| with probability~$1$.

    (Speaking of ``probability~$1$''---%
    this monad doesn't handle cases with ``almost never'' or ``almost surely''
    as nicely as perhaps you'd like it to.
    It'll usually work, sure,
    but if you run into a case where there are
    infinitely many probability-$0$ elements of the distribution,
    it won't be able to discard those from the output set,
    and some computations might run forever
    even when you think they should terminate.
    Whether this is desirable or ``correct'' is up for debate.)

    The meaning of |fmap| is:
    ``transform all the possible values of my distribution in this way,
    without affecting the probabilities.''
  \item
    Some computations may require reading from some global configuration file
    that would be awkward or annoying to pass to every function;
    e.g., running a physics simulation with lots of parameters.

    These computations themselves are embodied by the |Reader r| monad.
    For example, if |gravityDirection| needs to read a |Settings| object,
    we might have |gravityDirection :: Reader Settings Vector2D|.
    In |Reader r|, |join| takes a computation that produces a |Reader r a|
    and just produces the result itself
    (its type is |Reader r (Reader r a) -> Reader r a|),
    and |return| takes |x| to the computation that always returns |x|
    without consulting the |Reader| object.

    The meaning of |fmap| is:
    ``make a new computation that
    first applies the old computation, which may access the |Reader| object,
    and then applies this `pure,' reader-less function to the result.''
  \item
    Some computations may want to perform I/O;
    e.g., writing to a file or getting user input.

    These computations themselves are embodied by the |IO| monad.
    For example, |getLine :: IO String| describes
    the action of receiving user input;
    it promises to return a |String|.
    In |IO|, |join| takes an I/O computation that returns another |IO| action
    and just produces the latter result itself
    (its type is |IO (IO a) -> IO a|),
    and |return| takes |x| to the computation that always returns |x|
    without actually performing any I/O.

    The meaning of |fmap| is:
    ``make a new I/O action that first performs the other I/O action
    and then applies this `pure' function to the result.''
  \item
    Some computations may want to update some persistent internal state;
    e.g., evaluating a computer program
    in a language that allows mutation of variables.

    These computations themselves are embodied by the |State s| monad.
    For example, if an |Environment| describes
    all the variables available in
    the current execution context of a Python program,
    then we might have |eval :: PyExpression -> State Environment PyValue|.

    The semantics are as with |Reader r|.
\end{itemize}

For a library of monadic functions,
like |mapM|,
go into GHCi and write @import Control.Monad@, then @:browse@.
That'll get you a bunch.
