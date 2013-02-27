% CSE 230: Winter 2013
% Parallel Programming
% Ranjit Jhala, UC San Diego 

## This lecture is based on

Material from **Simon Marlowe's Excellent Tutorial**

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

Does FP have anything to offer there?

> - **Yes**

## But first ...

What is the **difference** between ...

- **parallelism** 

- **concurrency**

... or are they the same thing?

## Parallelism vs. Concurrency

<br>

### Parallel Program 

> - Exploit computing resources to yield the **same answer but faster**. 

> - **Deterministic**, ie *should* return the same answer on the same input. 

> - E.g. *Parallel Merge-Sort*

<br>

### Concurrent Program 

> - Models **independent** agents that **communicate** and **synchronize**. 

> - **Non-deterministic** as depends on the behavior of the individual agents.

> - E.g. *Concurrent Chat-Server*

> - May want to combine -- but notions are quite different!


## Parallelism vs. Concurrency

- **Parallel** Same answer but faster

- **Concurrent** Communication and Synchronization

> - **Very** different requirements!

> - Haskell has super clever ideas for *each*. 


## Parallel Haskell

> - How to make Haskell programs run **faster** 

> - By dividing work between multiple processors/cores

> - We *have* those multi cores, put them to work to make stuff quicker!


## Parallel Haskell: Dream of the 70s/80s

> - Why can't **compiler automatically parallelize** programs?

> - Yes, in some domains (cf Fortran Array/Matrix computations)

> - No, in general... but **why** ?


## Why is Parallelizing Code Difficult?

> 1. Tracking data dependencies and **side effects**

> 2. Tradeoff between parallelism **benefits** vs. **overheads**

## Why is Parallelizing Code Difficult?

1. Tracking data dependencies and **side effects**

>   - Can't run `B | A` if `B` input *depends on* `A` output

>   - Can't determine dependencies easily ...

2. Tradeoff between parallelism **benefits** vs. **overheads**

>   - Chopping up and distributing has a *cost*

>   - Only worth it if pieces are *big enough*




