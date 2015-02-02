-- Total: 40 points
--
-- Evaluator (18 points)
-- ---------
-- + Logging (3 points)
-- Throwing an exception manually (2 points each; 6 points)
--   + that exception is output
--   + the state is preserved as-was pre-exception
--   + the log is as-was pre-exception
-- Catching a manually thrown exception (2 points)
--   + state is as-was out of exception handler (1 point)
--   + no exception is reported (1 point)
-- Throwing an exception in each case of (1 point each; 4 points)
--   + divide by zero
--   + variable not found
--   + type error in base operator
--   + type error in if
-- Proper execute implementation (3 points)
--   matches output of evalS
--
-- Circuits (12 points)
-- --------
-- We don't have hooks into the properties --- they're hard-coded to check the functions
-- that the students wrote --- so I'm not going to grade them. Instead, I've distributed
-- the points I would've allocated into these over the implementations of the circuits
-- themselves.
--
-- Bitsubtractor property (1 point each)
--   - affirms 0 - 1 = 0 is correct
--   - rejects 5 - 1 = 5
--   - accepts 5 - 1 = 4
--   - accepts 5 - 0 = 5
-- Bitsubtractor itself (2 points each)
--   + 0 - 1 = 0
--   + 4 - 1 = 3
--   + 5 - 0 = 5
-- Multiplier property (1 point each)
--   - affirms 2 * 5 = 10
--   - rejects 2 * 5 = 9
-- Multiplier itself (2 points each)
--   + 2 * 5  = 10
--   + 0 * 12 = 0
--   + 1 * 3  = 3
--
-- Parsing (10 points)
-- -------
-- twoPowerEight (5 points)
-- gcd (5 points)

{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, OverlappingInstances, FlexibleInstances #-}

import Grade
import qualified Hw4 as H
import qualified Solution as S
import qualified Data.Map as M
import Data.Function
import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer
import Text.Parsec.String

import Control.DeepSeq

instance Eq (H.Value) where
  (H.IntVal n1)  == (H.IntVal n2)  = n1 == n2
  (H.BoolVal n1) == (H.BoolVal n2) = n1 == n2
  _              == _              = False

instance NFData H.Value where
  rnf (H.IntVal n)  = rnf n
  rnf (H.BoolVal n) = rnf n

execute evalS sto s = let ((v, l), st) = flip runState sto (runWriterT $ runErrorT $ evalS s) in
                      (st, S.maybeOfEither v, l)

runEvalS evalS = execute evalS M.empty

hEvalS = runEvalS H.evalS

sEvalS = runEvalS S.evalS

getStore (st, _, _)  = st
getExn   (_, exn, _) = exn
getLog   (_, _, log) = concat $ lines $ log

int = H.Val . H.IntVal

logProg = S.mksequence [H.Print "value zero is: " $ H.Val $ H.IntVal 0,
                        H.Print "value one is: " $ H.Val $ H.IntVal 1]

exnProg = S.mksequence [H.Print "value zero is: " $ H.Val $ H.IntVal 0,
                        H.Assign "X" $ H.Val $ H.IntVal 1,
                        H.Throw $ H.Val $ H.IntVal 15,
                        H.Print "should not print this " $ H.Val $ H.IntVal 5,
                        H.Assign "Y" $ H.Val $ H.IntVal 7]

catchProg = S.mksequence [H.Assign "X" $ H.Val $ H.IntVal 3,
                          H.Try (H.Throw $ H.Val $ H.IntVal 5)
                                "Y"
                                (H.Assign "X" $ H.Var "Y")]

dropLeading = reverse . dropWhile (== False) . reverse

runBitSubtractor (b, xs) =
    let (ys, _) = H.bitSubtractor (H.lift0 b, map H.lift0 xs) in
      dropLeading $ map H.sample1 $ ys

runMultiplier (x, y) = dropLeading . map H.sample1 $ H.multiplier (map H.lift0 x, map H.lift0 y)

mksequence = foldr H.Sequence H.Skip

twoPowerEight =
  mksequence [H.Assign "Y" (H.Val $ H.IntVal 1),
              H.Assign "Z" (H.Val $ H.IntVal 2),
              H.Assign "X" (H.Val $ H.IntVal 8),
              H.While (H.Op H.Gt (H.Var "X") (H.Val $ H.IntVal 0))
                       (mksequence [H.Assign "Y" (H.Op H.Times (H.Var "Y") (H.Var "Z")),
                                    H.Assign "X" (H.Op H.Minus (H.Var "X") (H.Val $ H.IntVal 1))])]

gcdProg =
  mksequence [H.Assign "A" (H.Val $ H.IntVal 15),
              H.Assign "B" (H.Val $ H.IntVal 21),
              H.While (H.Op H.Gt (H.Var "B") (H.Val $ H.IntVal 0))
                      (H.If (H.Op H.Gt (H.Var "A") (H.Var "B"))
                            (H.Assign "A" (H.Op H.Minus (H.Var "A") (H.Var "B")))
                            (H.Assign "B" (H.Op H.Minus (H.Var "B") (H.Var "A")))),
              H.Assign "G" (H.Var "A")]

grader :: GradeState ()
grader = do
  -- Evaluator
  grade "log output correct"
        (getLog . hEvalS)
        (getLog . sEvalS)
        (==)
        logProg
        3

  grade "manual exception throwing: program throws proper exception value"
        (getExn . hEvalS)
        (getExn . sEvalS)
        (==)
        exnProg
        2

  grade "manual exception throwing: state is not modified after exception is thrown"
        (getStore . hEvalS)
        (getStore . sEvalS)
        (==)
        exnProg
        2

  grade "manual exception throwing: log is not modified after exception is thrown"
        (getLog . hEvalS)
        (getLog . sEvalS)
        (==)
        exnProg
        2

  grade "exception handling: exception handler runs properly"
        (getStore . hEvalS)
        (getStore . sEvalS)
        (==)
        catchProg
        1

  grade "exception handling: no exception is reported after catch"
        (getExn . hEvalS)
        (getExn . sEvalS)
        (==)
        catchProg
        1

  grade "exception thrown on divide by zero"
        (getExn . hEvalS)
        (getExn . sEvalS)
        (==)
        (H.Assign "X" $ H.Op H.Divide (H.Val $ H.IntVal 1) (H.Val $ H.IntVal 0))
        1

  grade "exception thrown on accessing unbound variable"
        (getExn . hEvalS)
        (getExn . sEvalS)
        (==)
        (H.Assign "X" $ H.Var "Y")
        1

  grade "exception thrown on type error in arithmetic"
        (getExn . hEvalS)
        (getExn . sEvalS)
        (==)
        (H.Assign "X" $ H.Op H.Plus (H.Val $ H.BoolVal True) (H.Val $ H.IntVal 0))
        1

  grade "exception thrown on type error in conditional expression"
        (getExn . hEvalS)
        (getExn . sEvalS)
        (const $ const True)
        (H.If (H.Val $ H.IntVal 0) H.Skip H.Skip)
        1

  grade "execute output matches evalS output"
        hEvalS
        (H.execute M.empty)
        (==)
        exnProg
        3

  -- Circuits

  grade "bitSubtractor correctness"
        runBitSubtractor
        (const $ [])
        (==)
        (True, [])
        2

  grade "bitSubtractor correctness"
        runBitSubtractor
        (const $ [True, True])
        (==)
        (True, [False, False, True])
        2

  grade "bitSubtractor correctness"
        runBitSubtractor
        (const $ [False, False, True])
        (==)
        (False, [False, False, True])
        2

  grade "multiplier correctness"
        runMultiplier
        (const $ [])
        (==)
        ([], [False, False, True, True])
        2

  grade "multiplier correctness"
        runMultiplier
        (const $ [True, True])
        (==)
        ([True], [True, True])
        2

  grade "multiplier correctness"
        runMultiplier
        (const $ [False, True, False, True])
        (==)
        ([False, True], [True, False, True])
        2

  -- parsing, via runFile
  
  -- gradeParser "2^8" "twoPowerEight.imp" twoPowerEight 5

  -- gradeParser "gcd" "gcd.imp" gcdProg 5

  return ()

main :: IO ()
main = runGrader grader
