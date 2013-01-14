% CSE 230: Winter 2013
% Higher Order Functions 
% Ranjit Jhala, UC San Diego 


# Functions Are Data

Lets define two functions ...

\begin{code}
plus1 :: Int -> Int
plus1 x = x + 1

minus1 :: Int -> Int
minus1 x = x - 1
\end{code}

... we can manipulate functions like ordinary data

\begin{code}
aPairOfFuns   = (plus1, minus1)
aListOfFuns   = [plus1, minus1, plus1, minus1]
aBigListoFuns = replicate 10 plus1 ++ replicate 5 minus1
\end{code}

Note that type is very descriptive

~~~~~{.haskell}
> :t aBigListoFuns
aBigListoFuns :: [Int -> Int]
~~~~~

# Functions Are Data

## This is a BIG DEAL

We can *pass around* functions 

- Take functions as **inputs**

- Return functions as **outputs**

# Taking Functions as Input

A toy example...

\begin{code}
doTwice f x = f (f x)
\end{code}

`doTwice` takes as input

- a function `f`
- an input `x` 

and returns as output

- result of applying `x` to `f` **two times**

# Taking Functions as Input

A toy example...

\begin{code}
doTwice f x = f (f x)
\end{code}

Lets run it,

~~~~~{.haskell}
ghc> doTwice plus1 10
12
ghc> doTwice minus1 100
98
~~~~~

# Taking Functions as Input

A toy example...

\begin{code}
doTwice f x = f (f x)
\end{code}

## Execution = Substitute Equals for Equals

~~~~~{.haskell}
doTwice plus1 10 
  == {- unfold doTwice -} 
     plus1 (plus1 10)	
  == {- unfold plus1 -}
     plus1 (10 + 1)
  == {- unfold plus1 -}
     (10 + 1) + 1
  == {- old-school arithmetic -}
     12
~~~~~

# Returning Functions as Output

Instead of writing `plus1`, `plus2`, `plus3` ...

\begin{code}
plusn :: Int -> (Int -> Int)
plusn n = f
          where f x = x + n
\end{code}

We can now use the above to write

\begin{code}
plus2   = plusn 2
plus10  = plusn 10
minus20 = plusn (-20)       -- why the parens?
\end{code}

which can be used ...

~~~~~{.haskell}
ghc> plus10 100
110

ghc> minus20 1000
980 
~~~~~

# Partial Application

# Lambda: The function that has no name

# Infix Operators and Sections



# Eliminate Ugly `if-then-else` with Patterns

One Equation Per Case

\begin{code}
clone' x 0 = []                -- base      case
clone' x n = x : clone x (n-1) -- inductive case
\end{code}

# You can write an `if-then-else` function!

Equations are rather more general...

\begin{code}
ifThenElse True  thenE elseE = thenE
ifThenElse False thenE elseE = elseE
\end{code}


# A Brief Word on *Laziness*

Is the following JavaScript function ...

~~~~~{.javascript}
function ifThenElse(cond, thenB, elseB) { 
  return cond ? thenB : elseB ;
}

var z = ifThenElse(true, 1, alert("DIE DIE DIE"));
~~~~~


<br>

... the *same-as* the Haskell equivalent?

~~~~~{.haskell}
ifThenElse True  thenE elseE = thenE
ifThenElse False thenE elseE = elseE

z = ifThenElse True 1 (error "DIE DIE DIE")
~~~~~



# Wrap Up: A "Hello World" Program

\begin{code}
main = do putStrLn "What is your name ?"
          n <- getLine
          putStrLn ("Happy New Year " ++ n)
\end{code}

