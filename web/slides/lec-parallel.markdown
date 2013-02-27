% CSE 230: Winter 2013
% Parallel Programming
% Ranjit Jhala, UC San Diego 

## Lecture based on Simon Marlowe's Excellent Tutorial

- [PDF](http://community.haskell.org/~simonmar/par-tutorial.pdf)

- [Code](https://github.com/simonmar/par-tutorial)


## So far

FP is awesome...

> - ... Super composable 
> - ... Abstractions
> - ... Types
> - ... Easy to Test
> - ... etc.

## So far

FP is awesome...

... but in the **"multicore era"** languages live or die on

> - **parallelism** 

> - **concurrency**

> - What does FP have to offer here?

## But first ...

What is the **difference** between ...

- **parallelism** 

- **concurrency**

... or are they the same thing?

## Parallelism vs. Concurrency

<br>

### Parallel Program 

> - Exploit computing resources to yield the **same answer but faster**. 

> - **Deterministic**, ie *should* return the *same* answer on *same* input. 

> - E.g. *Parallel Merge-Sort*

<br>

### Concurrent Program 

> - Models **independent** agents that **communicate** and **synchronize**. 

> - **Non-deterministic** as depends on the behavior of the individual agents.

> - E.g. *Concurrent Chat-Server*


## Parallelism vs. Concurrency

<br>

### *Different* Requirements

- **Parallel** Same answer but faster

- **Concurrent** Communication and Synchronization

## Parallelism vs. Concurrency

<br>

### *Different* Requirements

- **Parallel** Same answer but faster

- **Concurrent** Communication and Synchronization

### Haskell has *different* ideas **for each**...


## Parallel Haskell

> - How to make Haskell programs run **faster** 

> - By dividing work between multiple processors/cores

> - We *have* those multi cores, put them to work to make stuff quicker!


## Parallel Haskell: Dream of the 70s/80s

**Compiler automatically parallelize** programs?

> - Yes, in some domains (cf Fortran array computations)

> - No, in general... but **why** ?


## Why is Parallelizing Code Difficult?

> 1. Tracking data dependencies and **side effects**

> 2. Tradeoff between parallelism **benefits** vs. **overheads**

## Why is Parallelizing Code Difficult?

1. Tracking data dependencies and **side effects**

>   - Can't run `B | A` if input of `B` *depends on* output of `A`

>   - Can't determine dependencies easily ...

2. Tradeoff between parallelism **benefits** vs. **overheads**

>   - Chopping up and distributing has a *cost*

>   - Only worth it if pieces are *big enough*

## Why is Parallelizing Code Difficult?

1. Tracking data dependencies and **side effects**

2. Tradeoff between parallelism **benefits** vs. **overheads**

### Haskell solves (1) but not (2)

> 1. Pure FP to the rescue: all dependencies **explicit**

> 2. You are (mostly) on your own!

## Next: Parallel Haskell In Action!

1. (Parallel) Sudoku Solver

> - Treat algorithm as a black box...

2. (Parallel) KMeans Clustering 

> - Need to know how algorithm works...


## A Sudoku Solver

Lets see how to *parallelize* a Sudoku Solver

<br>

~~~~~{.haskell}
solve :: String -> Maybe Grid
~~~~~~

<br>

> - Details of solver are not important

## A Sudoku Solver

Lets see how to *parallelize* a Sudoku Solver

<br>

~~~~~{.haskell}
solve :: String -> Maybe Grid
~~~~~~

<br>

### `String` input problem description 

<br>

~~~~~{.haskell}
let puz = ".......2143.......6........2.15..........637...........68...4.....23........7...."
~~~~~

## A Sudoku Solver

~~~~~{.haskell}
solve :: String -> Maybe Grid
~~~~~~

<br>

### `Grid` output description

<br>

~~~~~{.haskell}
ghci> printGrid $ fromJust $ solve puz
 
 8  5  7 | 3  4  9 | 6  2  1 
 4  3  2 | 8  6  1 | 5  9  7 
 6  1  9 | 7  5  2 | 8  4  3 
---------+---------+---------
 2  7  1 | 5  8  3 | 9  6  4 
 9  4  5 | 1  2  6 | 3  7  8 
 3  8  6 | 4  9  7 | 2  1  5 
---------+---------+---------
 7  6  8 | 9  1  5 | 4  3  2 
 1  9  4 | 2  3  8 | 7  5  6 
 5  2  3 | 6  7  4 | 1  8  9 
~~~~~

## Solving Many Instances

~~~~~{.haskell}
import Sudoku
import Control.Exception
import System.Environment

main :: IO ()
main = do
    [f]   <- getArgs                    -- get filename containing puzzles
    grids <- fmap lines $ readFile f    -- parse file into puzzles
    mapM_ (evaluate . solve) grids      -- solve all puzzles
~~~~~

>-  Whats up with the `evaluate` ? ...

## Brief Digression: Laziness

> - Haskell is a **Lazy** Language

> - Many important consequences (not looked at in CSE230)

> - "Values are not computed **until** they are **needed**"

> - Wat?


## Brief Digression: Laziness

Lets look at an example ... 

<br>

~~~~~{.haskell}
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
~~~~~

## Brief Digression: Laziness

`fib` is a **really slow** function

<br>

~~~~~{.haskell}
ghci> fib 35
...
9227465
~~~~~

> - That took a **looong ...** time ... and yet ...

## Brief Digression: Laziness

`fib` is a **really slow** function

<br>

~~~~~{.haskell}
ghci> let z = fib 35
ghci> -- comes back instantly!
~~~~~

How does it finish **so fast**?

> - Result is **not actually computed**

~~~~~{.haskell}
ghci> :sprint z
_
~~~~~

## Brief Digression: Laziness

`fib` is a **really slow** function

<br>

Can **force** it to be computed by **demanding** to `show` it

~~~~~{.haskell}
ghci> z 
...
9227465

ghci> :sprint z
z = 9227465
~~~~~

## Brief Digression: Laziness

Whats up with the `evaluate` ? ...

<br> 

~~~~~{.haskell}
evaluate :: a -> IO a
~~~~~

<br> 

> - **Forces** Haskell to compute the value...
> - **Upto** top-level constructor (eg. `:` or `Just` or ...)

## Evaluate upto top-level constructor (WHNF)

`evaluate` **forces** Haskell to compute **upto** top-level constructor

<br> 

~~~~~{.haskell}
ghci> let fib2 n = (fib n, fib (n+1))
~~~~~

## Evaluate upto top-level constructor (WHNF)

`evaluate` **forces** Haskell to compute **upto** top-level constructor

<br> 

~~~~~{.haskell}
ghci> let fib2 n = (fib n, fib (n+1))
ghci> let r      = fib2 35 
~~~~~

> - Finishes instantly, because it did nothing...

~~~~~{.haskell}
ghci> :sprint r
r = _
~~~~~

> - We can **force** it ...

## Evaluate upto top-level constructor (WHNF)

`evaluate` **forces** Haskell to compute **upto** top-level constructor

<br> 

~~~~~{.haskell}
ghci> let fib2 n = (fib n, fib (n+1))
ghci> let r      = fib2 35 
ghci> r' <- evaluate r
ghci> :sprint r'
r' = (_,_)
~~~~~

> - Still didn't do much of course, hence so fast...

## Force full computation by asking for output ...

<br>

~~~~~{.haskell}
ghci> r
...
ghci> r
(9227465,14930352)

ghci> :sprint r
r = (9227465,14930352)

ghci> :sprint r' 
r' = (9227465,14930352)
~~~~~


## Back to: Solving Many Sudoku Instances

~~~~~{.haskell}
import Sudoku
import Control.Exception
import System.Environment

main :: IO ()
main = do
    [f]   <- getArgs                    -- get filename containing puzzles
    grids <- fmap lines $ readFile f    -- parse file into puzzles
    mapM_ (evaluate . solve) grids      -- solve all puzzles
~~~~~

-  Whats up with the `evaluate` ? ...

> -  `Just` care if solution exists ...

> - ... use `evaluate` to force computation to top-level constructor

> - **Parallelism anyone?**


