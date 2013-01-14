% CSE 230: Winter 2013
% Polymorphism and Computation Patterns
% Ranjit Jhala, UC San Diego 

\begin{code}
module HigherOrder where
import Prelude hiding ((++))
\end{code}

## Plan

- Polymorphic Functions (#polymorphism)

- Polymorphic Data (#polymorphic-data-structures)

- Bottling Computation Patterns (#bottling-computation-patterns)

# Polymorphic Functions

## Polymorphic functions

Many functions are **oblivious** to the type of their arguments

\begin{code}
doTwice f x = f (f x)
\end{code}

<br>

Works on **many** kinds of values

~~~~~{.haskell}
ghci> :type doTwice
doTwice :: (a -> a) -> a -> a
~~~~~~

### For ALL Types `a`

- **takes**   function `f :: a -> a`
- **takes**   value    `x` :: a`
- **returns** an       `a`

## Polymorphic functions

Many functions are **oblivious** to the type of their arguments

~~~~~{.haskell}
doTwice     :: (a -> a) -> a -> a  
doTwice f x = f (f x)
~~~~~

### Can call with `Int` values 

~~~~~{.haskell}
ghci> doTwice (+1) 0
2
~~~~~

<br>
Type parameter `a` is **instantiated** as `Int` 

## Polymorphic functions

Many functions are **oblivious** to the type of their arguments

~~~~~{.haskell}
doTwice     :: (a -> a) -> a -> a  
doTwice f x = f (f x)
~~~~~

### Or with on `String` values 

~~~~~{.haskell}
ghci> doTwice (++ "I don't care! ") "Isn't this neat ?"

"Isn't this neat ? I don't care! I don't care! "
~~~~~

<br>
Type parameter `a` is **instantiated** as `String` 

## `doTwice` is Polymorphic

Works on **many** kinds of values

~~~~~{.haskell}
doTwice     :: (a -> a) -> a -> a  
doTwice f x = f (f x)
~~~~~

### Crucial for **abstraction** 

- Doing twice is **independent of** operation and data
- Don't need **separate** versions for `Int` , `String` ...

## Polymorphism Enables Reuse

Works on **many** kinds of values

~~~~~{.haskell}
doTwice     :: (a -> a) -> a -> a  
doTwice f x = f (f x)
~~~~~

<br>

**Reuse** same `doTwice` across different **operators** and **data**

## Polymorphism: With Great Power ...

... comes great responsibility!

Recall infix **sections**

~~~~~{.haskell}
ghci> (10 <) 12
True
~~~~~

<br>

**Quiz:** What is the value of?

~~~~~{.haskell}
ghci> doTwice (10 <) 100
~~~~~

## Polymorphism: With Great Power ...

... comes great responsibility!

~~~~~{.haskell}
ghci> doTwice (10 <) 100
~~~~~

### Nasty Type Error

~~~~~{.haskell}
doTwice :: (a -> a) -- operator 
        -> a        -- input
        -> a        -- output
~~~~~

<br>

Operator should return **same** type as output ...

... but `(10 <) ::  Int -> Bool`

## Quiz: Are you following so far ?

\begin{code}
ex1 = doTwice doTwice
\end{code}

<br>

1. What is the **type of** `ex1` ?

2. What is the **type parameter** of `doTwice` **instantiated** as?


# Polymorphic Data Structures

# Bottling Computation Patterns



