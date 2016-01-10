\clearpage
\section{Some basic Haskell}

This document doesn't assume any knowledge of Haskell.
Let's introduce some basic concepts.

\subsection{Functions}

Haskell is all about \emph{functions}.
As in mathematics, a function takes input values to output values,
where the input values are in the domain
and the output values are in the codomain.
In Haskell, we write |f :: alpha -> beta|
to indicate that |f| has domain |alpha| and codomain |beta|.

Here's a function:
\begin{spec}
double :: Int -> Int
double x = 2 * x
\end{spec}
The \emph{type annotation} on the first line is not required;
Haskell will infer it if you omit it.
(In fact, Haskell will always infer it;
if you include it, Haskell will complain if yours is wrong.)

We could also write this as follows:
\begin{spec}
double :: Int -> Int
double = \x -> 2 * x
\end{spec}
The term |\x -> 2 * x| is called a \emph{lambda expression};
it is used to represent a notion of a mapping without assigning it a name.
Of course, in this case, we do immediately assign it a name.
But that need not be the case, as we will see soon.

\subsubsection{Functions are values}

A key concept is that functions are values just as much as |Int|s are.
So we can write something like this:
\begin{spec}
multiplyBy :: Int -> (Int -> Int)
multiplyBy x = (\y -> x * y)
\end{spec}
Thus, |multiplyBy| expects an |Int|,
and returns \emph{a function} from |Int| to |Int|.

In fact, this is so common that
we insist that \textbf{the |(->)| type operator is right-associative},
so |a -> (b -> c)| can be written just as |a -> b -> c|.

The reason this is common is because it enables multi-argument functions:
a ``function of two arguments'' is really just
a function that returns another function.
Here's another example:
\begin{spec}
addThree :: Int -> Int -> Int -> Int
addThree x = \y -> \z -> x + y + z
\end{spec}
And, of course, this is tedious, so we may just as easily write:
\begin{spec}
addThree' :: Int -> Int -> Int -> Int
addThree' x y z = x + y + z
\end{spec}

\subsubsection{Applying functions}

Suppose we want to add |5|, |6|, and |12| using |addThree'| from above.
First, we need to invoke |addThree'| with argument |5| to get a function.
Then, we call the resulting function with |6| to get a second function.
Finally, we call this last function with |12| to get the result.

So we want to write |((addThree' 5) 6) 12|.

Of course, this is equally tedious,
so we insist that \textbf{function application is left-associative},
so we can write the above as |addThree' 5 6 12|.

\subsubsection{Partial application}

Partial application falls very naturally out of the above.
Consider this example:
\begin{spec}
distance :: Float -> Float -> Float
distance a b = sqrt (a * a + b * b)

distanceFrom0 :: Float -> Float
distanceFrom0 = distance 0
\end{spec}
In the definition of |distanceFrom0|,
we just take our existing function and apply it to only one argument.
This leaves us with a function from |Float| to |Float|,
which does exactly what we want.

This pattern is very common:
\begin{spec}
double      = (*) 2
reciprocal  = (/) 1  -- note that we're specifying the first (left) operand
\end{spec}

\subsubsection{Higher-order functions}

Suppose we have a list of integers---in particular, suppose |xs :: [Int]|.
How might we double every element in this list?
We could write
\begin{spec}
double x        = x * 2
doubleAll xs    = map double xs
\end{spec}

Here, |map| is a built-in function.
Its type is |map :: (alpha -> beta) -> [alpha] -> [beta]|.
That is, provided with \emph{any} function
from some type |alpha| to another type |beta|,
and also a list of elements whose type is the domain of the function
(i.e., |alpha|),
|map| will produce the result of
applying the function to every value in the list.

We call |map| a \textbf{higher-order function}
because it accepts a function---in this case, of type |alpha -> beta|---%
as an argument.

Here's another higher-order function in use:
\begin{spec}
evenNumbersUnder100 :: [Int]
evenNumbersUnder100 = filter even [1..100]
\end{spec}
The |even :: Int -> Bool| function is a built-in predicate,
and |filter :: (alpha -> Bool) -> [alpha] -> [alpha]|
takes just those elements of a list matching the given predicate.

We can combine these, e.g., to write
\begin{spec}
square :: Int -> Int
square x = x * x

under1000 :: Int -> Bool
under1000 x = x < 1000

allTheNumbers :: [Int]
allTheNumbers = [1..]

squaresUnder1000 = filter under1000 (map square allTheNumbers)
\end{spec}

Note that we need parentheses in the higher-order function type signatures;
if we wrote |notMap :: alpha -> beta -> [alpha] -> [beta]|,
then |notMap| would just be a function taking three arguments---%
an |alpha|, a |beta|, and an |[alpha]|---%
none of which is a function.

\ignore{
\subsubsection{Combinators}

Combinators are a special type of higher-order functions
that just rearrange how the arguments flow around.
Here's a built-in example:
\begin{spec}
flip :: (alpha -> beta -> gamma) -> (beta -> alpha -> gamma)
flip f x y = f y x
\end{spec}
Thus, we can write
\begin{spec}
evenNumbersUnder100' = (flip filter) [1..100] even
\end{spec}
which is useful because now we can use partial application to write, e.g.,
\begin{spec}
filterNumbersUnder100 :: (Int -> Bool) -> [Int]
filterNumbersUnder100 = (flip filter) [1..100]

evenNumbersUnder100''   = filterNumbersUnder100 even
oddNumbersUnder100''    = filterNumbersUnder100 odd
\end{spec}

Another combinator is good old function composition, |(.)|:
\begin{spec}
(.) :: (beta -> gamma) -> (alpha -> beta) -> (alpha -> gamma)
(.) g f = \x -> g (f x)
\end{spec}
So we may write:
\begin{spec}
isSquareUnder1000 :: Int -> Bool
isSquareUnder1000 = under1000 . square

numbersWithSquareUnder1000 = filter isSquareUnder1000 allTheNumbers
\end{spec}

Note that when a function's name is all symbols,
we can write it in infix notation.
So we could also have defined |(.)| as
\begin{spec}
(g . f) x = g (f x)
\end{spec}

Finally, we have this interesting one:
\begin{spec}
($) :: (alpha -> beta) -> alpha -> beta
f $ x = f x
infixr 0 $
\end{spec}
\ignore{|$|}  % for vim; sorry!
At first glance, this may seem useless,
because why would we write |f $ x| when we could just write |f x|?
\ignore{|$|}  % for vim; sorry!
The benefit is in the |infixr| declaration;
this says that the operator should be right-associative
and have very low precedence.
So now, instead of writing
\begin{spec}
ys = filter phi (map f (filter psi (map g (xs))))
\end{spec}
we can write
\begin{spec}
ys' = filter phi $ map f $ filter psi $ map g $ xs
\end{spec}
which has fewer parentheses and is generally more readable.
}

\subsection{Data}

The other concept that's as important as functions is data:
the construction and use of data types.

You've already seen types like |Int|, |Bool|, and |String|.
But you can make your own types, too.
An \emph{algebraic data type} is a type with one or more \emph{variants}.
Suppose we want to represent vehicles,
which can either be cars or bicycles.
Suppose further that we want to keep track of
the make and model of each car
and the number of gears on each bike.
We can use the following declarations:
\begin{spec}
data Vehicle    =   Car String String
                |   Bike Int

myCar :: Vehicle
myCar = Car "BMW" "Jetta"

myBike :: Vehicle
myBike = Bike 18
\end{spec}

There are two important things to note here.
First, we've only added one type: |Vehicle|.
Note that we \emph{don't} write |myCar :: Car| or |myBike :: Bike|,
because |Car| and |Bike| are not types.

What are they, then?
Well, if |Car "BMW" "Jetta" :: Vehicle|,
and |"BMW" :: String| and |"Jetta" :: String|,
then we must have |Car :: String -> String -> Vehicle|.
Similarly, |Bike :: Int -> Vehicle|.
So \textbf{data constructors are just functions}.

\subsubsection{Pattern matching}

Let's write a function that gets the cost of a vehicle.
Everyone knows that vehicles with longer makes and models are more valuable,
so we'll use the formula $\mathit{cost_{car} = 10 \cdot length_{make} + length_{model}}$
and $\mathit{cost_{bike} = 5 \cdot gears}$.
That is:
\begin{spec}
cost :: Vehicle -> Int
cost (Car make model)   = 10 * length make + length model
cost (Bike gears)       = 5 * gears
\end{spec}

Note that we've written \emph{two} definitions for |cost|:
one for each variant of the input argument.
In fact, this is a pretty common practice:
\begin{spec}
factorial :: Int -> Int
factorial 0     = 1
factorial n     = n * factorial (n - 1)

length' :: [a] -> Int
length' []              = 0
length' nonEmptyList    = 1 + length' (tail nonEmptyList)
                        -- (where |tail [1, 2, 3] == [2, 3]|)
\end{spec}

You can use the |_| wildcard to match any pattern and ignore its value:
\begin{spec}
sameType :: Vehicle -> Vehicle -> Bool
sameType (Car _ _) (Car _ _)    = True
sameType (Bike _) (Bike _)      = True
sameType _ _                    = False     -- |Car| and |Bike| or vice versa
\end{spec}
\begin{spec}
data TrafficLight = Red | Yellow | Green

shouldGo :: TrafficLight -> Bool
shouldGo Red        = False
shouldGo _          = True      -- for both |Yellow| and |Green|
\end{spec}

We'll use pattern-matching extensively.

\subsubsection{Recursive data types}

Suppose we want to write a data type for a list of integers
(and suppose we ``forgot'' about the built-in list type).
A list can either be empty or not.
If it's not, it has a first element and the rest of the list,
and the rest of the list is just another list!
That is,
\begin{spec}
data IntList    =   Empty
                |   NonEmpty Int IntList

emptyList :: IntList
emptyList = Empty

oneTwoThree :: IntList
oneTwoThree = NonEmpty 1 (NonEmpty 2 (NonEmpty 3 Empty))
\end{spec}

The interesting thing here, of course,
is that an |IntList| can have another |IntList| as its field.
This makes it a recursive data type.
Think about this if it confuses you.
We'll use these extensively, too.

\subsubsection{Parametric data types}

Suppose we now want a |StringList|.
We could create another |StringList| type,
and maybe rename the old constructors |EmptyIntList| and |NonEmptyIntList|
so the names don't clash.
But this isn't a good solution;
any functions we make will have to be duplicated.
And of course we'd need to put in even more work to make a |BoolList|, etc.

Instead, what about a list of elements of some arbitrary type?
\begin{spec}
data List alpha     =   Empty
                    |   NonEmpty alpha (List alpha)

emptyIntList :: List Int
emptyIntList = Empty

helloWorld :: List String
helloWorld = NonEmpty "hello" (NonEmpty "world" Empty)
\end{spec}
Here, |alpha| is a \emph{type parameter}.
Note that |List Int| and |List String| and |List Bool| are all types.
For that matter, |List (List String)| is a type!
But, crucially, |List| by itself is \emph{not} a type---%
it is a \emph{type constructor}.
It needs a type parameter, |alpha|, to create a concrete type.

(In fact, we say that |List :: * -> *|,
where |::| here is read as ``has kind'';
kinds are like types of types,
and |*| is the kind of concrete types,
so |* -> *| is the kind of a type constructor
that takes one concrete type (|alpha|) and returns another (|List alpha|).)

\subsubsection{More}

There's a lot more to say about data types, but we won't.

\subsection{More}

There's a lot more to say about Haskell, but we won't.
Hopefully, the syntax will be readable enough---%
e.g., you can figure out what this means:
\begin{spec}
getMultiplesOfThree :: [Int] -> [Int]
getMultiplesOfThree xs = filter isMultipleOfThree xs
  where
    isMultipleOfThree n = (n `mod` 3) == 0
\end{spec}

From time to time, when we do want to explain something,
you may come across notes like these:
\begin{haskellnote}[Notes]
Please note this note.
This note is a sample note,
so I suppose it's a note about notes.
\end{haskellnote}
These are notes about the Haskell language itself.

With no further ado, we'll get right to it!
