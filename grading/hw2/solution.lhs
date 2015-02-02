> {-# LANGUAGE FlexibleInstances #-}
> module Solution where

> import qualified Hw2 as H

---
title: Homework #2, Due Monday, January 31st
---

This week's homework is presented as a literate Haskell file,
just like the lectures.
This means that every line beginning with ">" is interpreted as
Haskell code by the compiler, while every other line is ignored.
(Think of this as the comments and code being reversed from what
they usually are.)
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
> import Test.QuickCheck
> import Data.Foldable as F hiding (foldl, foldr)
> import Data.Monoid
> import Control.Monad

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
> myFoldl f b []     = b
> myFoldl f b (x:xs) = myFoldl f (f b x) xs

2. Using `foldl`, define the list reverse function:

> myReverse :: [a] -> [a]
> myReverse = foldl (flip (:)) []

3. Define `foldr` in terms of `foldl`:

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr f b = foldl (flip f) b . reverse

4. Define `foldl` in terms of `foldr`:

> myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
> myFoldl2 f b = foldr (flip f) b . reverse

5. Try applying `foldl` to a gigantic list. Why is it so slow?
   Try using `foldl'` instead; can you explain why it's faster?

Part 2: Binary Search Trees
---------------------------

Suppose we have the following type of binary search trees:

Unification needs type constructors to be injective...
Allowing a flip makes the type constructor non-injective

Define a `delete` function for BSTs of this type:

> popGreatest :: (Ord k) => H.BST k v -> (Maybe (k, v), H.BST k v)
> popGreatest H.Emp            = (Nothing, H.Emp)
> popGreatest (H.Bind k v l r) = case popGreatest r of
>                                  (Nothing, _) -> (Just (k, v), l)
>                                  (kv, r')     -> (kv, H.Bind k v l r')

> delete :: (Ord k) => k -> H.BST k v -> H.BST k v
> delete k H.Emp                         = H.Emp
> delete k (H.Bind k' v l r) | k < k'    = H.Bind k' v (delete k l) r
>                            | k > k'    = H.Bind k' v l (delete k r)
>                            | otherwise = case popGreatest l of
>                                            (Nothing, _)        -> r
>                                            (Just (k2, v2), l') -> H.Bind k2 v2 l' r

> mapKeysToSelf :: H.BST k v -> H.BST k k
> mapKeysToSelf H.Emp            = H.Emp
> mapKeysToSelf (H.Bind k _ l r) = H.Bind k k (mapKeysToSelf l) (mapKeysToSelf r)

> instance Foldable (H.BST k) where
>  foldMap f H.Emp            = mempty
>  foldMap f (H.Bind _ v l r) = foldMap f l `mappend` foldMap f r `mappend` f v

> isBST :: (Ord k) => H.BST k k -> Bool
> isBST H.Emp            = True
> isBST (H.Bind k _ l r) = F.all (< k) l && F.all (> k) r && isBST l && isBST r

> genBST :: Int -> Int -> Int -> Gen (H.BST Int Int)
> genBST lb ub sz | sz <= 0   = return H.Emp
>                 | lb >= ub  = return H.Emp
>                 | otherwise = do k <- choose (lb, ub)
>                                  l <- genBST lb      (k - 1)  (sz `div` 2)
>                                  r <- genBST (k + 1) ub       (sz `div` 2)
>                                  return $ H.Bind k k l r

> instance Arbitrary (H.BST Int Int) where
>   arbitrary = sized $ genBST 0 100

> someBSTs = sample $ sized $ genBST 0 100

> checkBSTGen = quickCheck (isBST :: H.BST Int Int -> Bool)

> isBSTDeleted t k = isBST $ delete k t

> deleteProp = forAll arbitrary $ \t -> forAll (arbitrary :: Gen Int) $ \k -> isBSTDeleted t k

> checkBSTDelete = quickCheck deleteProp

Part 3: Animation
-----------------

This part of the homework constructs an animation of a model
solar system.
We begin by defining various helpful functions:

-- > translate :: (Float, Float) -> Picture -> Picture
-- > translate v p =
-- >     case p of
-- >       Region c r -> Region c (Translate v r)
-- >       p1 `Over` p2 -> (translate v p1) `Over` (translate v p2)
-- >       EmptyPic -> EmptyPic
-- 
-- > -- Translate a picture behavior by a given vector behavior
-- > translateB :: (Behavior Float, Behavior Float) -> Behavior Picture -> Behavior Picture
-- > translateB (x,y) p = lift2 translate (zipB (x,y)) p
-- 
-- > -- Convert a pair of behaviors into a pair behavior
-- > zipB :: (Behavior a, Behavior b) -> Behavior (a,b)
-- > zipB (Beh b1, Beh b2) = Beh (\t -> (b1 t, b2 t))
-- 
-- > -- Construct a circle behavior
-- > circ :: Behavior Float -> Behavior Shape
-- > circ r = ell r r
-- 
-- > sun :: Behavior Picture
-- > sun = reg (lift0 Yellow) (shape (circ 1))

The following define the main action of the solar system simulator.
You'll want to replace the right-hand side of `planets` with your
solar system.

-- > planets :: Behavior Picture
-- > planets = orbit earthmoon (orbit (orbitBody mercury) sun 2.0 2.0 0.2 (lift0 1.0)) 0.5 4.0 0.75 (lift0 1.0)
-- > earthmoon = orbit (orbitBody moon) earth 5.0 0.4 0.25
-- 
-- > hmain :: IO()
-- > hmain = 
-- >   do animateB "Solar system" planets
-- 
-- > main = hmain

You can avoid a lot of tedious "liftn" operations in your code if you
make the Behavior type a member of the Applicative typeclass. This may
require providing additional definitions not explicitly mentioned
here. You should verify that your definition of the applicative
instance has the required properties (but don't need to turn in a
proof).

-- > bfmap f (Beh g) = Beh (\t -> f (g t))

-- > instance Functor Behavior where
-- >   fmap = bfmap

-- > bpure x               = Beh $ \_ -> x
-- > bapp (Beh ab) (Beh a) = Beh (\t -> ab t $ a t)

-- > instance Applicative Behavior where
-- >   pure  = bpure
-- >   (<*>) = bapp

-- Use the provided function translateB to write a function

-- > orbitBody :: Behavior Picture -> Behavior Float -> Behavior Picture
-- > orbitBody p = \s -> lift2 scalePicture s p

-- > orbit :: (Behavior Float -> Behavior Picture) -- the satellite, in terms of its scale
-- >       -> Behavior Picture -- the fixed body
-- >       -> Float            -- the frequency of the orbit
-- >       -> Float            -- the x-radius of the orbit
-- >       -> Float            -- the y-radius of the orbit
-- >       -> Behavior Float   -- scale (this body)
-- >       -> Behavior Picture
-- > orbit s b f rx ry scale =
-- >   let scaler = (*) <$> scale
-- >       brx    = scaler <*> pure rx
-- >       bry    = scaler <*> pure ry
-- >       tb     = (*) <$> pure f <*> time
-- >       by     = bry * sin tb
-- >       p      = scalePicture <$> scale <*> (s $ scale * ((+) <$> (pure 1.5) <*> (sin tb)))
-- >       bs     = translateB (brx * cos tb, by) p
-- >       bb     = scalePicture <$> scale <*> b in
-- >     cond ((<) <$> by <*> pure 0) (bb `over` bs) (bs `over` bb)

-- that takes two picture behaviors and makes the first orbit around the
-- second at the specified distance and with the specified radii. That
-- is, the two pictures will be overlayed (using `over`) and, at each time
-- $t$, the position of the satellite will be translated by
-- $xradius * cos(t * frequency)$ in the $x$ dimension and by
-- $yradius * sin(t * frequency)$ in the $y$ dimension.

-- Test your function by creating another circle, `mercury`, colored red
-- and with radius `0.1`, and making it orbit around the sun with a
-- frequency of `2.0`, and with radii of `2.0` and `0.2` in the x and y axes,
-- respectively.

-- > mercury :: Behavior Picture
-- > mercury = reg (lift0 Red) (shape (circ 0.1))
-- 
-- > earth :: Behavior Picture
-- > earth = reg (lift0 Blue) (shape (circ 0.25))
-- 
-- > moon :: Behavior Picture
-- > moon = reg (lift0 White) (shape (circ 0.05))

-- A problem you might have noticed is the overlay behavior of
-- planets. For this part modify orbit to put planets over or under each
-- other. Hint: you might find the lifted conditional `cond` from SOE
-- useful for this part.

-- Modify your functions (and write any support functions that you find
-- necessary) to make the orbital distances and planet sizes shrink and
-- grow by some factor (you can pass this factor as parameter to the
-- orbit function), according to how far the planets are from the
-- observer. For example, the earth and moon should look a little smaller
-- when they are going behind the sun, and the orbital distance of the
-- moon from the earth should be less.

-- Choose the scaling factor so that the solar system simulation looks
-- good to you.

-- *Optional:* Add some other planets, perhaps with their own moons. If
-- you like, feel free to adjust the parameters we gave above to suit
-- your own aesthetic or astronomical tastes. Make sure, though, that the
-- features requested in previous parts --- growing, shrinking,
-- occlusion, etc. --- remain clearly visible.

-- > scalePicture :: Float -> Picture -> Picture
-- > scalePicture x (Region c r) = Region c (Scale (x, x) r)
-- > scalePicture x (p `Over` q) = scalePicture x p `Over` scalePicture x q
-- > scalePicture _ EmptyPic     = EmptyPic

-- Credits
-- -------

-- Part 3 is taken from 
-- <a href="http://www.cis.upenn.edu/~bcpierce/courses/552-2008/">
-- UPenn's CIS 522
-- </a>.
