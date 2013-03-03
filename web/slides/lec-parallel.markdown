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


## Solving Many Sudoku Instances

Lets compile with runtime stats ...

~~~~~{.haskell}
$ ghc -O2 sudoku1.hs -rtsopts
~~~~~

... and run!

~~~~~{.haskell}
$ ./sudoku1 sudoku17.1000.txt +RTS -s
~~~~~

## Solving Many Sudoku Instances

Lets compile with runtime stats ...

~~~~~{.haskell}
$ ghc -O2 sudoku1.hs -rtsopts
~~~~~

... and run!

~~~~~{.haskell}
$ ./sudoku1 sudoku17.1000.txt +RTS -s
~~~~~

Not too shabby ... (*elapsed* is wall-clock time)

~~~~~{.haskell}
Total   time    2.11s  (  2.11s elapsed)
~~~~~


## Solving Many Sudoku Instances

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

**Parallelism anyone?**

> - `solve` is **embarrasingly** parallel?

> - Run different instances on different threads/cores/processes...

## Solving Many Sudoku Instances

> - Parallel programming = **ordering** things
>   - **Start** evaluating `a` in parallel, 
>   - **And then** evaluate `b` 

> - That is, all about **sequence** in which to **do** things ...

> - ... Parallel coordination performed in a **monad**


## Parallelism with the `Eval` Monad

There's a library for everything ... `Control.Parallel.Strategies` 

1. A Type for Parallel Computations Yielding `a` Values

~~~~~{.haskell}
data Eval a
~~~~~

## Parallelism with the `Eval` Monad

There's a library for everything ... `Control.Parallel.Strategies` 

1. A Type for Parallel Computations Yielding `a` Values

~~~~~{.haskell}
data Eval a
~~~~~

2. You can **sequence** such computations (or run in parallel) so ...

~~~~~{.haskell}
instance Monad Eval
~~~~~

## Parallelism with the `Eval` Monad

There's a library for everything ... `Control.Parallel.Strategies` 

~~~~~{.haskell}
data Eval a
instance Monad Eval
~~~~~

To **execute** a computation

~~~~~{.haskell}
runEval :: Eval a -> a
~~~~~

## Parallelism with the `Eval` Monad

There's a library for everything ... `Control.Parallel.Strategies` 

~~~~~{.haskell}
data Eval a
instance Monad Eval

runEval :: Eval a -> a
~~~~~

4. To **create** such computations

~~~~~
rpar :: a -> Eval a   
rseq :: a -> Eval a
~~~~~

> - `rpar x` : Evaluate `x` in **parallel**

> - `rseq x` : Evaluate `x` in **sequence** (wait till done)

## Solving Many Sudoku Instances ... in parallel!

See [sudoku2.hs](https://github.com/simonmar/par-tutorial/blob/master/code/sudoku2.hs)

~~~~~{.haskell}
main :: IO ()
main = do
    [f]   <- getArgs
    grids <- fmap lines $ readFile f

    let (as, bs) = splitAt (length grids `div` 2) grids

    evaluate $ runEval $ do
       a <- rpar (deep (map solve as))  -- spawn first half
       b <- rpar (deep (map solve bs))  -- spawn second half
       rseq a                           -- wait till done
       rseq b                           -- wait till done
       return ()
~~~~~

> - `deep` forces evaluation (since we're not printing out results...)


## Solving Many Sudoku Instances ... in parallel!

Lets compile ...

~~~~~{.haskell}
$ ghc -O2 sudoku2.hs -rtsopts -threaded
~~~~~

... and run

~~~~~{.haskell}
$ ./sudoku2 sudoku17.1000.txt +RTS -N2 -s
.
.  
  SPARKS: 2 (1 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)
.
.
  Total   time    2.66s  (  1.52s elapsed)
~~~~~

Speedup = 2.11/1.52 = 1.38 

> - Not so great with 2 cores, whats up?

## Solving Many Sudoku Instances ... in parallel!

**TODO:** Static 2-SPLIT Picture

    threadscope sudoku2.eventlog

## Static v. Dynamic Partitions

**Problem:** Work is not divided **evenly**

<br>

### Static Partitions

> - **Fixed** number of blocks determined at compile time
> - **Pro:** block is large
> - **Con:** blocks not equal sized
> - e.g. `2` blocks (list of puzzles `splitAt` middle)

### Dynamic Partitions

> - **Input-dependent** number of blocks
> - **Con:** blocks are smaller sized  
> - **Pro:** Run-time **balance-work** across cores
> - e.g. *separate* blocks *for each puzzle*


## GHC's Automatic Balancing: Sparks & Work Stealing

~~~~~{.haskell}
rpar e
~~~~~

### Spark

> - **Spark** = Argument to `rpar` (e.g. `e`)
> - Super lightweight *thread*

### Work Stealing

> - Runtime *collects* sparks in a *pool*
> - Lightweight (a single pointer), can create *thousands* of sparks
> - Assigns *spark* to *idle* cores via *work stealing* 

## Dynamic Partitioning 

~~~~~{.haskell}
rpar e
~~~~~

### Spark

> - **Spark** = Argument to `rpar` (e.g. `e`)
> - Super lightweight *thread*

### Work Stealing

> - Runtime *collects* sparks in a *pool*
> - Lightweight (a single pointer), can create *thousands* of sparks
> - Assigns *spark* to *idle* cores via *work stealing* 

### Dynamic Partitioning

- Use `rpar` to fire up bajillion sparks on input data!


## Dynamic Partitioning for Sudoku Solver 

### How to partition dynamically ?

~~~~~{.haskell}
main :: IO ()
main = do
    [f]   <- getArgs
    grids <- fmap lines $ readFile f

    let (as, bs) = splitAt (length grids `div` 2) grids

    evaluate $ runEval $ do
       a <- rpar (deep (map solve as))  -- spawn first half
       b <- rpar (deep (map solve bs))  -- spawn second half
       rseq a                           -- wait till done
       rseq b                           -- wait till done
       return ()
~~~~~

> - Solve **each** instance in **different** spark...

> - How?


## Dynamic Partitioning for Sudoku Solver 

### How to partition dynamically ?

~~~~~{.haskell}
main :: IO ()
main = do
    [f]     <- getArgs
    grids   <- fmap lines $ readFile f
    evaluate $ runEval $ parSolve grids 
~~~~~

> - Solve **each** instance in **different** spark...
> - ... by **recursively `rpar` each** puzzle

~~~~~{.haskell}
parSolve []       = return []
parSolve (x : xs) = do y  <- rpar (deep (solve x))
                       ys <- parSolve xs
                       return (y : ys)
~~~~~

> - Of course, we can do better ... 

> - ... factor out into `parMap`


## Dynamic Partitioning With a Parallel Map

Can **bottle** the parallel pattern in `parMap` 

~~~~~{.haskell}
parMap          :: (a -> b) -> [a] -> Eval [b] 
parMap f []     = return []
parMap f (x:xs) = do y  <- rpar (f x)
                     ys <- parMap xs
                     return (y:ys)
~~~~~

-> 

## Dynamic Partitioning With a Parallel Map

Can release the genie to solve sudoku in parallel [sudoku3.hs](https://github.com/ranjitjhala/par-tutorial/blob/master/code/sudoku3.hs)

~~~~~{.haskell}
main :: IO ()
main = do
    [f]     <- getArgs
    grids   <- fmap lines $ readFile f
    evaluate $ deep $ runEval $ parMap solve grids 
~~~~~

> - **Just changed 3 characters from original!**

> - Let's take it out for a spin!

## Dynamic Partitioning With a Parallel Map

Can release the genie to solve sudoku in parallel [sudoku3.hs](https://github.com/ranjitjhala/par-tutorial/blob/master/code/sudoku3.hs)

Compile ...

~~~~~{.haskell}
$ ghc -O2 sudoku3.hs -rtsopts -threaded
~~~~~

... and run

~~~~~{.haskell}
$ ./sudoku3 sudoku17.1000.txt +RTS -N2 -s
.  
  SPARKS: 1000 (1000 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)
.
  Total   time    3.10s  (  1.57s elapsed)
~~~~~

> - Created 1000 sparks (1 per puzzle)
> - Each spark was *converted* into real parallelism at runtime
> - Sparks *pruned* have been removed from the spark pool by the runtime system, 
>   - found to be already evaluated, or 
>   - because they were found to be not used by program.


## Dynamic Partitioning With a Parallel Map

Can release the genie to solve sudoku in parallel [sudoku3.hs](https://github.com/ranjitjhala/par-tutorial/blob/master/code/sudoku3.hs)

Not too bad, lets see if the cores are balanced...

**TODO:** Dynamic 2-SPLIT Picture

    threadscope sudoku3.eventlog

## Dynamic Partitioning With a Parallel Map

Can release the genie to solve sudoku in parallel [sudoku3.hs](https://github.com/ranjitjhala/par-tutorial/blob/master/code/sudoku3.hs)

Can just throw more cores at it ... **5.4 x speedup**

~~~~~{.haskell}
$ ./sudoku3 sudoku17.1000.txt +RTS -N2 -s
  Total   time    3.24s  (  1.64s elapsed)

$ ./sudoku3 sudoku17.1000.txt +RTS -N3 -s
  Total   time    3.47s  (  1.17s elapsed)

$ ./sudoku3 sudoku17.1000.txt +RTS -N4 -s
Total   time    3.30s  (  0.84s elapsed)

$ ./sudoku3 sudoku17.1000.txt +RTS -N6 -s
  Total   time    3.96s  (  0.67s elapsed)

$ ./sudoku3 sudoku17.1000.txt +RTS -N8 -s
  Total   time    4.64s  (  0.61s elapsed)

$ ./sudoku3 sudoku17.1000.txt +RTS -N10 -s
  Total   time    4.77s  (  0.50s elapsed)

$ ./sudoku3 sudoku17.1000.txt +RTS -N12 -s
  Total   time    5.28s  (  0.47s elapsed)

$ ./sudoku3 sudoku17.1000.txt +RTS -N14 -s
  Total   time    5.68s  (  0.44s elapsed)
~~~~~

## Dynamic Partitioning With a Parallel Map

Can release the genie to solve sudoku in parallel [sudoku3.hs](https://github.com/ranjitjhala/par-tutorial/blob/master/code/sudoku3.hs)

Better still, can just throw more cores at it ...

... upto a point!

~~~~~{.haskell}
$ ./sudoku3 sudoku17.1000.txt +RTS -N12 -s
  Total   time    5.28s  (  0.47s elapsed)

$ ./sudoku3 sudoku17.1000.txt +RTS -N14 -s
  Total   time    5.68s  (  0.44s elapsed)

$ ./sudoku3 sudoku17.1000.txt +RTS -N16 -s
  Total   time    6.97s  (  0.48s elapsed)

$ ./sudoku3 sudoku17.1000.txt +RTS -N18 -s
  Total   time    8.42s  (  0.52s elapsed)
~~~~~

Co-ordinating that many cores not **always** worth it...


## Parallel Strategies

> - We just saw how (parallel) computation pattern was bottled as `parMap`

> - Generalize the **bottling** using `Strategies`

## Parallel Strategies

~~~~~{.haskell}
type Strategy a = a -> Eval a
~~~~~

**Strategy:** A function that determines **how to** `Eval` an `a` value.

## Parallel Strategies

- We just saw how (parallel) computation pattern was bottled as `parMap`

- Generalize the **bottling** using `Strategies`

### Parallel Strategies: Functions that Determine How To Eval 

~~~~~{.haskell}
type Strategy a = a -> Eval a
~~~~~


> - Lets see some example strategies...

## Parallel Strategies

~~~~~{.haskell}
type Strategy a = a -> Eval a
~~~~~

Some Example `Strategy`s ...

~~~~~{.haskell}
rpar :: a -> Eval a     -- Eval-uate a in PARALLEL with current thread
rseq :: a -> Eval a     -- Eval-uate a in SEQUENCE in   current thread
~~~~~

... and so!

~~~~~{.haskell}
rpar :: Strategy a      -- Eval-uate a in PARALLEL with current thread
rseq :: Strategy a      -- Eval-uate a in SEQUENCE in   current thread
~~~~~

## Parallel Strategies

~~~~~{.haskell}
type Strategy a = a -> Eval a
~~~~~

Some Example `Strategy`s 

~~~~~{.haskell}
rdeepseq :: NFData a => Strategy a
rdeepseq x = rseq (deep x)
~~~~~

> - **Forces** the *full* (`deep`) evaluation of `x` ...

> - .. in current thread

## Using Strategies

~~~~~{.haskell}
using :: a -> Strategy a -> a
x `using` s = runEval (s x)
~~~~~

> 1. `s x :: Eval a`, says how to evaluate `x`
> 2. `runEval (s x)`, then executes the plan yielding the value


EXPLAIN `x using s == x` modulo lazy semantics


