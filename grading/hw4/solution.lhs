
---
title: Homework #4, Due Wednesday, March 2nd 
---

Preliminaries
=============

To complete this homework, download [this file]($root/homeworks/hw4.lhs) 
as plain text and answer each question, filling in code where it says
`"TODO"`. Your code must typecheck against the given type signatures. 
Feel free to add your own tests to this file to exercise the 
functions you write. Submit your homework by sending this file, 
filled in appropriately, to cse230@goto.ucsd.edu with the subject
“HW4”; you will receive a confirmation email after submitting. Please note
that this address is unmonitored; if you have any questions about the
assignment, email Pat at prondon@cs.ucsd.edu.

> {-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, OverlappingInstances, FlexibleInstances #-}

> module Solution where
> import qualified Hw4 as H
> import qualified Data.Map as Map

> import Control.Monad.State
> import Control.Monad.Error
> import Control.Monad.Writer

> import Test.QuickCheck 
> import Control.Monad (forM, forM_)
> import Data.List (transpose, intercalate)

> printStore :: H.Store -> IO ()
> printStore e = do putStrLn "Environment:"
>                   putStrLn $ show e

> quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n}

Problem 1: An Interpreter for WHILE++ 
=====================================

Previously, you wrote a simple interpreter for *WHILE*.
For this problem, you will use monad transformers to build
an evaluator for *WHILE++* which, adds exceptions and I/O 
to the original language.

As before, we have variables, and expressions.

-- > type Variable = String
-- > type Store    = Map.Map Variable Value
-- >
-- > data Value =
-- >     IntVal Int
-- >   | BoolVal Bool
-- >   deriving (Show)
-- >
-- > data Expression =
-- >     Var Variable
-- >   | Val Value  
-- >   | Op  Bop Expression Expression
-- >   deriving (Show)
-- >
-- > data Bop = 
-- >     Plus     
-- >   | Minus    
-- >   | Times    
-- >   | Divide   
-- >   | Gt        
-- >   | Ge       
-- >   | Lt       
-- >   | Le       
-- >   deriving (Show)

Programs in the language are simply values of the type

-- > data Statement =
-- >     Assign Variable Expression          
-- >   | If Expression Statement Statement
-- >   | While Expression Statement       
-- >   | Sequence Statement Statement        
-- >   | Skip
-- >   | Print String Expression
-- >   | Throw Expression
-- >   | Try Statement Variable Statement
-- >   deriving (Show)

The only new constructs are the `Print`, `Throw` and the `Try` statements. 

- `Print s e` should print out (eg to stdout) log the string corresponding 
  to the string `s` followed by whatever `e` evaluates to,

RJ: followed by a newline. Print "X: " (Val $ IntVal 3) should print "X: IntVal 3\n"

- `Throw e` evaluates the expression `e` and throws it as an exception, and

- `Try s x h` executes the statement `s` and if in the course of
  execution, an exception is thrown, then the exception comes shooting 
  up and is assigned to the variable `x` after which the *handler*
  statement `h` is executed.

**Note:** we don't have exceptions (yet), so if a variable
is not found (eg because it is not initialized) simply return 
the value `0`. In future assignments, we will add this as a 
case where exceptions are thrown (the other case being type errors.)

We will use the `State` [monad][2] to represent the world-transformer.
Intuitively, `State s a` is equivalent to the world-transformer 
`s -> (a, s)`. See the above documentation for more details. 
You can ignore the bits about `StateT` for now.

Use monad transformers to write a function

> type EvalMonad a = ErrorT H.Value (WriterT String (State H.Store)) a

> evalE :: (MonadState H.Store m, MonadError H.Value m, MonadWriter String m) => H.Expression -> m H.Value
> evalE (H.Val v) = return v
> evalE (H.Var x) = do m <- get
>                      case Map.lookup x m of
>                        Just v  -> return v
>                        Nothing -> throwError $ H.IntVal 0
> evalE (H.Op b e1 e2) = do v1 <- evalE e1
>                           v2 <- evalE e2
>                           evalOp b v1 v2
>  where evalOp H.Plus   (H.IntVal n) (H.IntVal m) = return $ H.IntVal $ n + m
>        evalOp H.Minus  (H.IntVal n) (H.IntVal m) = return $ H.IntVal $ n - m
>        evalOp H.Times  (H.IntVal n) (H.IntVal m) = return $ H.IntVal $ n * m
>        evalOp H.Divide (H.IntVal n) (H.IntVal m) =
>          if m /= 0 then return $ H.IntVal $ n `div` m else throwError $ H.IntVal 1
>        evalOp H.Gt     (H.IntVal n) (H.IntVal m) = return $ H.BoolVal $ n > m
>        evalOp H.Lt     (H.IntVal n) (H.IntVal m) = return $ H.BoolVal $ n < m
>        evalOp H.Ge     (H.IntVal n) (H.IntVal m) = return $ H.BoolVal $ n >= m
>        evalOp H.Le     (H.IntVal n) (H.IntVal m) = return $ H.BoolVal $ n <= m
>        evalOp _        _            _            = throwError $ H.IntVal 2

RJ: We don't want to pass the store here, do we?

> evalS :: (MonadState H.Store m, MonadError H.Value m, MonadWriter String m) => H.Statement -> m ()
> evalS (H.Assign x e) = do v <- evalE e
>                           s <- get
>                           put (Map.insert x v s)
>                           return ()
> evalS (H.If e s1 s2) = do b <- evalE e
>                           case b of 
>                             H.BoolVal True  -> evalS s1
>                             H.BoolVal False -> evalS s2
>                             _               -> throwError $ H.IntVal 2
> evalS (H.While e s)      = evalS $ H.If e (H.Sequence s $ H.While e s) H.Skip
> evalS (H.Sequence s1 s2) = evalS s1 >> evalS s2
> evalS H.Skip             = return ()
> evalS (H.Print s e)      = evalE e >>= \v -> tell $ s ++ show v ++ "\n"
> evalS (H.Throw e)        = evalE e >>= throwError
> evalS (H.Try s1 x s2)    = evalS s1 `catchError` \v -> evalS $ H.Sequence (H.Assign x (H.Val v)) s2

and use the above function to implement a second function

> maybeOfEither (Left v)  = Just v
> maybeOfEither (Right _) = Nothing

> execute :: H.Store -> H.Statement -> (H.Store, Maybe H.Value, String)
> execute sto s = let ((v, l), st) = flip runState sto (runWriterT $ runErrorT $ evalS s) in
>                   (st, maybeOfEither v, l)

such that `execute st s` returns a triple `(st', exn, log)` where 

- `st'` is the output state, 
- `exn` is possibly an exception (if the program terminates with an uncaught exception), 
- `log` is the log of messages generated by the `Print` statements.

Requirements
------------

In the case of exceptional termination, the `st'` should be the state *at
the point where the last exception was thrown, and `log` should include all
the messages upto that point -- make sure you stack your transformers
appropriately! 

- Reading an undefined variable should raise an exception carrying the value `IntVal 0`.

- Division by zero should raise an exception carrying the value `IntVal 1`.

- A run-time type error (addition of an integer to a boolean, comparison of
  two values of different types) should raise an exception carrying the value
  `IntVal 2`.

RJ: would be handy to have these as data values as before

Example 1
---------

If `st` is the empty state (all variables undefined) and `s` is the program

> mksequence = foldr H.Sequence H.Skip

-- > testprog1 = mksequence [Assign "X" $ Val $ IntVal 0,
-- >                         Assign "Y" $ Val $ IntVal 1,
-- >                         Print "hello world: " $ Var "X",
-- >                         If (Op Lt (Var "X") (Var "Y")) (Throw (Op Plus (Var "X") (Var "Y")))
-- >                                                        Skip,
-- >                         Assign "Z" $ Val $ IntVal 3]

-- > testprog2 = mksequence [Assign "X" $ Val $ IntVal 0,
-- >                         Assign "Y" $ Val $ IntVal 1,
-- >                         Try (If (Op Lt (Var "X") (Var "Y"))
-- >                                 (mksequence [Assign "A" $ Val $ IntVal 100,
-- >                                              Throw (Op Plus (Var "X") (Var "Y")),
-- >                                              Assign "B" $ Val $ IntVal 200])
-- >                                 Skip)
-- >                             "E"
-- >                             (Assign "Z" $ Op Plus (Var "E") (Var "A"))]

~~~~~{.haskell}
X := 0 ;
Y := 1 ;
print "hello world: " X;
if X < Y then
  throw (X+Y)
else 
  skip
endif;
Z := 3 
~~~~~

then `execute st s` should return the triple 

~~~~~{.haskell}
(fromList [("X", IntValue 0), ("Y",  IntValue 1), Just (IntValue 1), "hello world: 0"
~~~~~


Example 2
---------

If `st` is the empty state (all variables undefined) and `s` is the program

~~~~~{.haskell}
X := 0 ;
Y := 1 ;
try  
  if X < Y then
    A := 100;
    throw (X+Y);
    B := 200;
  else 
    skip
  endif;
catch E with
  Z := E + A
endwith
~~~~~

then `execute st s` should return the triple 

~~~~~{.haskell}
( fromList [("A", IntValue 100), ("E", IntValue 1)
           ,("X", IntValue 0), ("Y", IntValue 1)
 	   ,("Z", IntValue 101)]
, Nothing 
, "")
~~~~~

Problem 2: [Circuit Testing]
============================

Credit: [UPenn CIS552][1]

For this problem, you will look at a model of circuits in Haskell.

Signals
-------

A *signal* is a list of booleans.  

> newtype Signal = Sig [Bool]

By convention, all signals are infinite. We write a bunch of lifting
functions that lift boolean operators over signals.

> lift0 ::  Bool -> Signal
> lift0 a = Sig $ repeat a
> 
> lift1 ::  (Bool -> Bool) -> Signal -> Signal
> lift1 f (Sig s) = Sig $ map f s
> 
> lift2 ::  (Bool -> Bool -> Bool) -> (Signal, Signal) -> Signal
> lift2 f (Sig xs, Sig ys) = Sig $ zipWith f xs ys
> 
> lift22 :: (Bool -> Bool -> (Bool, Bool)) -> (Signal, Signal) -> (Signal,Signal)
> lift22 f (Sig xs, Sig ys) = 
>   let (zs1,zs2) = unzip (zipWith f xs ys)
>   in (Sig zs1, Sig zs2) 
> 
> lift3 :: (Bool->Bool->Bool->Bool) -> (Signal, Signal, Signal) -> Signal
> lift3 f (Sig xs, Sig ys, Sig zs) = Sig $ zipWith3 f xs ys zs
> 

Simulation
----------

Next, we have some helpers that can help us simulate a circuit by showing
how it behaves over time. For testing or printing, we truncate a signal to 
a short prefix 

> truncatedSignalSize = 20
> truncateSig bs = take truncatedSignalSize bs
> 
> instance Show Signal where
>   show (Sig s) = show (truncateSig s) ++ "..."
> 
> trace :: [(String, Signal)] -> Int -> IO ()
> trace desc count = do 
>   putStrLn   $ intercalate " " names
>   forM_ rows $ putStrLn . intercalate " " . rowS
>   where (names, wires) = unzip desc
>         rows           = take count . transpose . map (\ (Sig w) -> w) $ wires
>         rowS bs        = zipWith (\n b -> replicate (length n - 1) ' ' ++ (show (binary b))) names bs
> 
> probe :: [(String,Signal)] -> IO ()
> probe desc = trace desc 1
> 
> simulate :: [(String, Signal)] -> IO ()
> simulate desc = trace desc 20

Testing support (QuickCheck helpers)
------------------------------------

Next, we have a few functions that help to generate random tests

> instance Arbitrary Signal where
>   arbitrary = do 
>     x      <- arbitrary
>     Sig xs <- arbitrary
>     return $ Sig (x : xs)
> 
> arbitraryListOfSize n = forM [1..n] $ \_ -> arbitrary

To check whether two values are equivalent 

> class Agreeable a where
>   (===) :: a -> a -> Bool
> 
> instance Agreeable Signal where
>   (Sig as) === (Sig bs) = 
>     all (\x->x) (zipWith (==) (truncateSig as) (truncateSig bs))
> 
> instance (Agreeable a, Agreeable b) => Agreeable (a,b) where
>   (a1,b1) === (a2,b2) = (a1 === a2) && (b1 === b2)
> 
> instance Agreeable a => Agreeable [a] where
>   as === bs = all (\x->x) (zipWith (===) as bs)
> 

To convert values from boolean to higher-level integers

RJ: Overflow makes things crap out mysteriously!

> class Binary a where
>   binary :: a -> Integer
> 
> instance Binary Bool where
>   binary b = if b then 1 else 0
> 
> instance Binary [Bool] where
>   binary = foldr (\x r -> (binary x) + 2 *r) 0

And to probe signals at specific points.

> sampleAt n (Sig b) = b !! n
> sampleAtN n signals = map (sampleAt n) signals
> sample1 = sampleAt 0
> sampleN = sampleAtN 0


Basic Gates
-----------

The basic gates from which we will fashion circuits can now be described.

> or2 ::  (Signal, Signal) -> Signal
> or2 = lift2 $ \x y -> x || y 
> 
> xor2 :: (Signal, Signal) -> Signal
> xor2 = lift2 $ \x y -> (x && not y) || (not x && y)
> 
> and2 :: (Signal, Signal) -> Signal
> and2 = lift2 $ \x y -> x && y 
> 
> imp2 ::  (Signal, Signal) -> Signal
> imp2 = lift2 $ \x y -> (not x) || y 
>
> mux :: (Signal, Signal, Signal) -> Signal
> mux = lift3 (\b1 b2 select -> if select then b1 else b2)
>
> demux :: (Signal, Signal) -> (Signal, Signal)
> demux args = lift22 (\i select -> if select then (i, False) else (False, i)) args
>
> muxN :: ([Signal], [Signal], Signal) -> [Signal]
> muxN (b1,b2,sel) = map (\ (bb1,bb2) -> mux (bb1,bb2,sel)) (zip b1 b2)
>
> demuxN :: ([Signal], Signal) -> ([Signal], [Signal])
> demuxN (b,sel) = unzip (map (\bb -> demux (bb,sel)) b)


Basic Signals 
-------------

Similarly, here are some basic signals

> high = lift0 True
> low  = lift0 False
>
> str   ::  String -> Signal
> str cs = Sig $ (map (== '1') cs) ++ (repeat False)
>
> delay ::  Bool -> Signal -> Signal
> delay init (Sig xs) = Sig $ init : xs


Combinational circuits
----------------------

**NOTE** When you are asked to implement a circuit, you must **ONLY** use
the above gates or smaller circuits built from the gates.

For example, the following is a *half-adder* (that adds a carry-bit to a
single bit).

> halfadd :: (Signal, Signal) -> (Signal, Signal)
> halfadd (x,y) = (sum,cout)
>   where sum   = xor2 (x, y)
>         cout  = and2 (x, y)

Here is a simple property about the half-adder

> prop_halfadd_commut b1 b2 =
>   halfadd (lift0 b1, lift0 b2) === halfadd (lift0 b2, lift0 b1) 

We can use the half-adder to build a full-adder

> fulladd (cin, x, y) = (sum, cout)
>   where (sum1, c1)  = halfadd (x,y)
>         (sum, c2)   = halfadd (cin, sum1)
>         cout        = xor2 (c1,c2) 
> 
> test1a = probe [("cin",cin), ("x",x), ("y",y), ("  sum",sum), ("cout",cout)]
>   where cin        = high
>         x          = low
>         y          = high
>         (sum,cout) = fulladd (cin, x, y)

and then an n-bit adder

> bitAdder :: (Signal, [Signal]) -> ([Signal], Signal)
> bitAdder (cin, [])   = ([], cin)
> bitAdder (cin, x:xs) = (sum:sums, cout)
>   where (sum, c)     = halfadd (cin,x)
>         (sums, cout) = bitAdder (c,xs)
> 
> test1 = probe [("cin",cin), ("in1",in1), ("in2",in2), ("in3",in3), ("in4",in4),
>                ("  s1",s1), ("s2",s2), ("s3",s3), ("s4",s4), ("c",c)]
>   where
>     cin = high
>     in1 = high
>     in2 = high
>     in3 = low
>     in4 = high
>     ([s1,s2,s3,s4], c) = bitAdder (cin, [in1,in2,in3,in4])

The correctness of the above circuit is described by the following property
that compares the behavior of the circuit to the *reference implementation*
which is an integer addition function

> prop_bitAdder_Correct ::  Signal -> [Bool] -> Bool
> prop_bitAdder_Correct cin xs =
>   binary (sampleN out ++ [sample1 cout]) == binary xs + binary (sample1 cin)
>   where (out, cout) = bitAdder (cin, map lift0 xs) 
 
Finally, we can use the bit-adder to build an adder that adds two N-bit numbers

> adder :: ([Signal], [Signal]) -> [Signal]
> adder (xs, ys) = 
>    let (sums,cout) = adderAux (low, xs, ys)
>    in sums ++ [cout]
>    where                                        
>      adderAux (cin, [], [])     = ([], cin)
>      adderAux (cin, x:xs, y:ys) = (sum:sums, cout)
>                                   where (sum, c) = fulladd (cin,x,y)
>                                         (sums,cout) = adderAux (c,xs,ys)
>      adderAux (cin, [], ys)     = adderAux (cin, [low], ys)
>      adderAux (cin, xs, [])     = adderAux (cin, xs, [low])
> 
> test2 = probe [ ("x1", x1), ("x2",x2), ("x3",x3), ("x4",x4),
>                 (" y1",y1), ("y2",y2), ("y3",y3), ("y4",y4), 
>                 (" s1",s1), ("s2",s2), ("s3",s3), ("s4",s4), (" c",c) ]
>   where xs@[x1,x2,x3,x4] = [high,high,low,low]
>         ys@[y1,y2,y3,y4] = [high,low,low,low]
>         [s1,s2,s3,s4,c]  = adder (xs, ys)

And we can specify the correctness of the adder circuit by

> prop_Adder_Correct ::  [Bool] -> [Bool] -> Bool
> prop_Adder_Correct l1 l2 = 
>   binary (sampleN sum) == binary l1 + binary l2
>   where sum = adder (map lift0 l1, map lift0 l2) 

Problem: Subtraction
--------------------

1. Using `prop_bitAdder_Correct` as a model, write a speciﬁcation for a
single-bit subtraction function that takes as inputs a N-bit binary 
number and a single bit to be subtracted from it and yields as
outputs an N-bit binary number. Subtracting one from zero should
yield zero.

> snot :: Signal -> Signal
> snot a = xor2 (high, a)

> halfsub :: (Signal, Signal) -> (Signal, Signal)
> halfsub (x,y) = (sum,bout)
>   where sum   = xor2 (x, y)
>         bout  = and2 (y, snot x)

> bitSubtractor :: (Signal, [Signal]) -> ([Signal], Signal)
> bitSubtractor (bin, [])   = ([], bin)
> bitSubtractor (bin, x:xs) = ((and2 (sum, snot bout)):sums, bout)
>   where (sum, b)     = halfsub (x,bin)
>         (sums, bout) = bitSubtractor (b,xs)

> prop_bitSubtractor_Correct ::  Signal -> [Bool] -> Bool
> prop_bitSubtractor_Correct bin xs = 
>   binary (sampleN sum) == max 0 (binary xs - binary (sample1 bin))
>   where (sum,bout) = bitSubtractor (bin, map lift0 xs)
          
2. Using the `bitAdder` circuit as a model, deﬁne a `bitSubtractor` 
circuit that implements this functionality and use QC to check that 
your behaves correctly.

Problem: Multiplication
-----------------------

3. Using `prop_Adder_Correct` as a model, write down a QC speciﬁcation 
for a `multiplier` circuit that takes two binary numbers of arbitrary 
width as input and outputs their product.

> prop_Multiplier_Correct ::  [Bool] -> [Bool] -> Bool
> prop_Multiplier_Correct x y =
>   (binary x * binary y) == (binary $ sampleN $ multiplier (map lift0 x, map lift0 y))

4. Deﬁne a `multiplier` circuit and check that it satisﬁes your 
speciﬁcation. (Looking at how adder is deﬁned will help with this, 
but you’ll need a little more wiring. To get an idea of how the 
recursive structure should work, think about how to multiply two 
binary numbers on paper.)

> multiplier :: ([Signal], [Signal]) -> [Signal]
> multiplier (xs, [])   = map (const low) xs
> multiplier (xs, y:ys) = adder (map (curry and2 y) xs, low : multiplier (xs, ys))

[1]: http://www.cis.upenn.edu/~bcpierce/courses/552-2008/resources/circuits.hs


