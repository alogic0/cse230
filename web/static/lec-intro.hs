#!/usr/bin/env runhaskell

pos :: Integer -> Bool 
pos x = x > 0

pat :: (Int, Int, Int) -> Int
pat (x, y, z) = x * (y + z )

-- | A recursive function

clone :: a -> Int -> [a]
clone x n = ifThenElse (n == 0) [] (x : clone x (n-1)) 

-- | Cleaner, with "pattern matching"

clone' x 0 = []
clone' x n = x : clone x (n-1)

-- | An If-Then-Else function

ifThenElse True thenExpr elseExpr  = thenExpr
ifThenElse False thenExpr elseExpr = elseExpr

-- if True then 1 else error "DIE DEI DIE"
--     ===> 1
--
-- In JavaScript:
--
-- function ifThenElse(cond, thenB, elseB) { 
--   return cond ? thenB : elseB ;

-- ifThenElse(true, 1, alert("DIE DIE DIE"));







main = do putStrLn "What is your name ?"
          n <- getLine
          putStrLn ("Happy New Year " ++ n)
