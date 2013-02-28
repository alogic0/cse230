---
title: Homework #2, Due Sunday 2/3/13
---

This week's homework is presented as a literate Haskell file,
just like the lectures. This means that every line beginning with
`>` is interpreted as Haskell code by the compiler, while every other
line is ignored. (Think of this as the comments and code being reversed
from what they usually are.)
You can load this file into `ghci` and compile it with `ghc`
just like any other Haskell file, so long as you remember to save
it with a `.lhs` suffix.

To complete this homework, download [this file as plain text](hw2.lhs) and
answer each question, filling in code where
noted (some questions ask for explanations in addition to or instead
of code).
Your code must typecheck against the given type signatures.
Feel free to add your own tests to this file to exercise the functions
you write.  Submit your homework by sending this file, filled in
appropriately, to `cse230@goto.ucsd.edu` with the subject "HW2"; you
will receive a confirmation email after submitting.  Please note that
this address is unmonitored; if you have any questions about the
assignment, email Pat at `prondon@cs.ucsd.edu`.

This homework requires the graphics libraries from
The Haskell School of Expression:

> import Animation hiding (planets, translate)
> import Picture
> import Control.Applicative

Part 0: All About You
---------------------

Tell us your name, email and student ID, by replacing the respective
strings below

> myName  = "Write Your Name  Here"
> myEmail = "Write Your Email Here"
> mySID   = "Write Your SID   Here"



Part 1: All About `foldl`
-------------------------

Define the following functions by filling in the "error" portion:

1. Describe `foldl` and give an implementation:

> myFoldl :: (a -> b -> a) -> a -> [b] -> a
> myFoldl f b xs = error "Define me!"

2. Using the standard `foldl` (not `myFoldl`), define the list reverse function:

> myReverse :: [a] -> [a]
> myReverse xs = error "Define me!"

3. Define `foldr` in terms of `foldl`:

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr f b xs = error "Define me!"

4. Define `foldl` in terms of the standard `foldr` (not `myFoldr`):

> myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
> myFoldl2 f b xs = error "Define me!"

5. Try applying `foldl` to a gigantic list. Why is it so slow?
   Try using `foldl'` (from [Data.List](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#3))
   instead; can you explain why it's faster?

Part 2: Binary Search Trees
---------------------------

Suppose we have the following type of binary search trees:

> data BST k v = Emp 
>              | Bind k v (BST k v) (BST k v) 
>              deriving (Show)

Define a `delete` function for BSTs of this type:

> delete :: (Ord k) => k -> BST k v -> BST k v
> delete k t = error "Define me!"

Part 3: Animation
-----------------

This part of the homework constructs an animation of a model
solar system.
We begin by defining various helpful functions:

> translate :: (Float, Float) -> Picture -> Picture
> translate v p =
>     case p of
>       Region c r -> Region c (Translate v r)
>       p1 `Over` p2 -> (translate v p1) `Over` (translate v p2)
>       EmptyPic -> EmptyPic

> -- Translate a picture behavior by a given vector behavior
> translateB :: (Behavior Float, Behavior Float) -> Behavior Picture -> Behavior Picture
> translateB (x,y) p = lift2 translate (zipB (x,y)) p

> -- Convert a pair of behaviors into a pair behavior
> zipB :: (Behavior a, Behavior b) -> Behavior (a,b)
> zipB (Beh b1, Beh b2) = Beh (\t -> (b1 t, b2 t))

> -- Construct a circle behavior
> circ :: Behavior Float -> Behavior Shape
> circ r = ell r r

> sun :: Behavior Picture
> sun = reg (lift0 Yellow) (shape (circ 1))

The following define the main action of the solar system simulator.
You'll want to replace the right-hand side of `planets` with your
solar system.

> planets :: Behavior Picture
> planets = sun

> main :: IO()
> main = 
>   do animateB "Solar system" planets

Before starting the exercise proper, let's make our lives easier. By
making the Behavior type a member of the Applicative typeclass, we'll
give ourselves a way to avoid a lot of tedious "liftn" operations in
our code.
This may require providing additional definitions not explicitly
mentioned here. You should verify that your definition of the
applicative instance has the required properties (but don't need to
turn in a proof).
If you don't understand the above, some good references are
Chapter 18 of The Haskell School of Expression
and the
[section on applicative functors](http://en.wikibooks.org/wiki/Haskell/Applicative_Functors)
in the Haskell wikibook.

> instance Applicative Behavior where
>   pure x                 = error "Define me!"
>   (<*>) (Beh ab) (Beh a) = error "Define me!"

Next, use the provided function translateB to write a function

> orbit :: Behavior Picture -- the satellite
>       -> Behavior Picture -- the fixed body
>       -> Float            -- the frequency of the orbit
>       -> Float            -- the x-radius of the orbit
>       -> Float            -- the y-radius of the orbit
>       -> Behavior Picture
> orbit = error "Define me!"

that takes two picture behaviors and makes the first orbit around the
second at the specified distance and with the specified radii. That
is, the two pictures will be overlayed (using `over`) and, at each time
$t$, the position of the satellite will be translated by
$xradius * cos(t * frequency)$ in the $x$ dimension and by
$yradius * sin(t * frequency)$ in the $y$ dimension.

Test your function by creating another circle, `mercury`, colored red
and with radius `0.1`, and making it orbit around the sun with a
frequency of `2.0`, and with radii of `2.0` and `0.2` in the x and y axes,
respectively.

A problem you might have noticed is the overlay behavior of
planets. For this part modify orbit to put planets over or under each
other. Hint: you might find the lifted conditional `cond` from SOE
useful for this part.

Modify your functions (and write any support functions that you find
necessary) to make the orbital distances and planet sizes shrink and
grow by some factor (you can pass this factor as parameter to the
orbit function), according to how far the planets are from the
observer. For example, the earth and moon should look a little smaller
when they are going behind the sun, and the orbital distance of the
moon from the earth should be less.

Choose the scaling factor so that the solar system simulation looks
good to you.

*Optional:* Add some other planets, perhaps with their own moons. If
you like, feel free to adjust the parameters we gave above to suit
your own aesthetic or astronomical tastes. Make sure, though, that the
features requested in previous parts --- growing, shrinking,
occlusion, etc. --- remain clearly visible.

Credits
-------

Part 3 is taken from 
<a href="http://www.cis.upenn.edu/~bcpierce/courses/552-2008/">
UPenn's CIS 522
</a>.
