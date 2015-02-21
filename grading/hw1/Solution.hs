module Solution where

import qualified Hw1 as H
import SOE

rectangle :: Float -> Float -> H.Shape
rectangle s1 s2 = H.Polygon [(0, 0), (s1, 0), (s1, s2), (0, s2)]

rtTriangle :: Float -> Float -> H.Shape
rtTriangle s1 s2 = H.Polygon [(0, 0), (s1, 0), (0, s2)]

sides :: H.Shape -> Int
sides (H.Rectangle _ _)    = 4
sides (H.Ellipse _ _)      = 42
sides (H.RtTriangle _ _)   = 3
{- Assume non-overlapping sides.  Consider the bowtie shape defined by
     Polygon [(0, 0), (1, 1), (1, 0), (0, 1)]
   This has four vertices but six sides. -}
sides (H.Polygon vs)       = if length vs <= 2 then 0 else fromIntegral $ length vs

-- bigger s e returns a new shape where each side is scaled by e
sBigger :: H.Shape -> Float -> H.Shape
sBigger (H.Rectangle s1 s2) e  = H.Rectangle (e * s1) (e * s2)
sBigger (H.Ellipse r1 r2) e    = H.Ellipse (e * r1) (e * r2)
sBigger (H.RtTriangle s1 s2) e = H.RtTriangle (e * s1) (e * s2)
sBigger (H.Polygon vs) e       = H.Polygon (expandVertices vs)
  where expandVertices [] = []
        expandVertices ((x, y) : vs) = (e * x, e * y) : expandVertices vs

-- bigger s e returns a new shape where the area is scaled by e
bigger :: H.Shape -> Float -> H.Shape
bigger s e = sBigger s (sqrt e)

dist :: (Float, Float) -> (Float, Float) -> Float
dist (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

epsilon = 0.0001 :: Float

perimeter :: H.Shape -> Float
perimeter (H.RtTriangle s1 s2) = s1 + s2 + sqrt (s1^2 + s2^2)
perimeter (H.Rectangle s1 s2)  = 2 * s1 + 2 * s2
perimeter (H.Polygon vs)       = foldl (+) 0 $ sides vs
  where sides vs = zipWith dist vs (tail vs ++ [head vs])
perimeter (H.Ellipse r1 r2) | r1 > r2   = ellipsePerim r1 r2
                            | otherwise = ellipsePerim r2 r1
  where ellipsePerim r1 r2 =
          let e       = sqrt (r1^2 - r2^2) / r1
              s       = scanl aux (0.25 * e^2) [2..]
              aux s i = nextEl e s i
              test x  = x > epsilon
              sSum    = foldl (+) 0 (takeWhile test s)
          in 2 * r1 * pi * (1 - sSum)
        nextEl e s i = s * (2 * i - 1) * (2 * 1 - 3) * (e^2) / (4 * i^2)

area :: H.Shape -> Float
area (H.Rectangle s1 s2)             = s1 * s2
area (H.RtTriangle s1 s2)            = 0.5 * s1 * s2
area (H.Ellipse r1 r2)               = pi * r1 * r2
area (H.Polygon (v1 : v2 : v3 : vs)) = triArea v1 v2 v3 + area (H.Polygon (v1 : v3 : vs))
  where triArea v1 v2 v3 = let a = dist v1 v2
                               b = dist v2 v3
                               c = dist v3 v1
                               s = 0.5 * (a + b + c)
                           in sqrt (s * (s - a) * (s - b) * (s - c))
area (H.Polygon _) = 0

hanoi 0 _ _ _       = return ()
hanoi 1 src dst _   = putStr ("move disc from " ++ src ++ " to " ++ dst ++ "\n")
hanoi n src dst tmp = do
  hanoi (n - 1) src tmp dst
  hanoi 1 src dst tmp
  hanoi (n - 1) tmp dst src

fillSquare w x y l =
  drawInWindow w
    (withColor Blue (polygon [(x, y), (x + l, y), (x + l, y + l), (x, y + l)]))

smin = 2

sierpinski w x y l =
  if l <= smin
  then fillSquare w x y l
  else do
    sierpinski w x y nl
    sierpinski w (x + nl) y nl
    sierpinski w (x + 2 * nl) y nl
    sierpinski w x (y + nl) nl
    sierpinski w (x + 2 * nl) (y + nl) nl
    sierpinski w x (y + 2 * nl) nl
    sierpinski w (x + nl) (y + 2 * nl) nl
    sierpinski w (x + 2 * nl) (y + 2 * nl) nl
      where nl = l `div` 3

main0 =
  runGraphics (
    do w <- openWindow "Sierpinski Carpet" (300, 300)
       sierpinski w 10 10 290
       k <- getKey w
       closeWindow w
    )

sierpinskiCarpet = main0

-- Part 2

myLength = foldr (\_ n -> n + 1) 0

f1 xs x = map (\f -> f x) xs
f2 = map

doubleEach :: [Int] -> [Int]
doubleEach = map (* 2)

pairAndOne :: [Int] -> [(Int, Int)]
pairAndOne = map (\a -> (a, a + 1))
-- solution I saw in someone's work:
pairAndOne' xs = zip xs $ map (+ 1) xs

addEachPair = map (uncurry (+))

-- This can be made more succinct with foldr1
maxList l = foldr max (head l) l
minList l = foldr min (head l) l

treeFold :: (a -> b) -> (b -> b -> b) -> H.Tree a -> b
treeFold f g (H.Leaf x)     = f x
treeFold f g (H.Branch l r) = treeFold f g l `g` treeFold f g r

treeSize :: H.Tree a -> Int 
treeSize = treeFold (const 1) (+)

treeHeight :: H.Tree a -> Int 
treeHeight (H.Leaf _) = 0
treeHeight (H.Branch l r) = 1 + max (treeHeight l) (treeHeight r)

fringe = treeFold (\x -> [x]) (++)

-- spaceClose w = do
--     k <- getKey w
--     if k == ' ' 
--         then closeWindow w
--         else spaceClose w

-- f1 :: [(a -> b)] -> a -> [b]
-- f1 _ _ = []
-- f2 :: (a -> b) -> [a] -> [b]
-- f2 _ _ = []

takeTree :: Int -> H.InternalTree a -> H.InternalTree a
takeTree _ H.ILeaf                       = H.ILeaf
takeTree n (H.IBranch x l r) | n > 0     = H.IBranch x (takeTree (n - 1) l) (takeTree (n - 1) r)
                             | otherwise = H.ILeaf
                                           
takeTreeWhile :: (a -> Bool) -> H.InternalTree a -> H.InternalTree a
takeTreeWhile p H.ILeaf                       = H.ILeaf
takeTreeWhile p (H.IBranch x l r) | p x       = H.IBranch x (takeTreeWhile p l) (takeTreeWhile p r)
                                  | otherwise = H.ILeaf
                                                
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x xs -> f x : xs) [] xs
