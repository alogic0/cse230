#!/usr/bin/env runhaskell

pos :: Integer -> Bool 
pos x = x > 0

pat :: (Int, Int, Int) -> Int
pat (x, y, z) = x * (y + z )







fons        :: a -> a -> [a] -> [a]
fons x y zs = x : y : zs 



ranjitFun :: Int -> a -> [a]
-- clone n x   = if n == 0 
--                 then [] 
--                 else x : (clone (n-1) x)
               
ranjitFun 0 x = []
ranjitFun n x = x : ranjitFun (n-1) x
               
repli n x 
  | n > 0     = x : repli (n-1) x
  | otherwise = [] 



-- clone 0 'a' = []
-- clone 1 'a' = ['a']
-- clone 2 'a' = ['a','a']
-- clone 3 'a' = ['a','a','a']








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


-- range lo hi = if lo > hi 
--                 then [] 
--                 else lo : (range (lo+1) hi)

range lo hi 
  | lo > hi   = []
  | otherwise = lo : range (lo + 1) hi

add []      = 0
add [1]     = 1
add [1,2,3] = 6

addUp        :: [Int] -> Int
addUp []     = 0
addUp (x:xs) = x + addUp xs







listAdd        :: [Int] -> Int
listAdd []     = 0
listAdd (x:xs) = x + listAdd xs


-- data Circle = C (Double, Double, Double)
-- C :: (Double, Double, Double) -> Circle
-- radius (C (_,_,r)) = r 
-- C (0,1,55)
--
--
--
--
--
-- data Circle = C Double Double Double
-- data Square = S Double Double Double
-- areaCircle (C x y r) = pi * r * r
-- areaSquare (S x y d) = d * d


-- data [a] = [] 
--          | (:) a [a]


-- data Circle = Kanye Double Double Double
-- Kanye :: Double -> Double -> Double -> Circle

data Shape  = Kanye Double Double Double
            | Square Double Double Double
            | Poly [(Double, Double)]
            deriving (Show)





{- 

area (Circle _ _ r)    = pi * r * r
area (Square _ _ d)    = d * d
-- area (Poly ps)         = areaPoly ps

areaPoly (p1:p2:p3:rest) = areaTriangle p1 p2 p3 + areaPoly (p1:p3:rest)
areaPoly _               = 0


areaTriangle = undefined -- fill in as exercise

c0 = Circle 0 0 2.3
c1 = Circle 0 1 1.31
s0 = Square 12 1 12312
p0 = Poly   [(0,0), (4,4), (92, 92)]

-}


act1 = putStrLn "This is a string on a line"
act2 = putStrLn "This is another string on a line"
act3 = putStrLn "This is the last string i promise you"





main = bob

zog :: [IO ()] 
zog = [bob, bob, bob]

do act1
   act2
   act3

bob :: IO ()
bob = do putStrLn "Hey? " 
         name <- getLine 
         putStrLn ("Hello " ++ name)


-- main = do putStrLn "What is your name ?"
--           n <- getLine
--           putStrLn ("Happy New Year " ++ n)
