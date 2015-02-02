import Grade
import qualified Hw2 as H
import qualified Solution as S
import Animation hiding (planets, translate)
import Control.Applicative
import Control.Monad.State

import Control.DeepSeq
instance (NFData a, NFData b) => NFData (H.BST a b) where
  rnf H.Emp           = ()
  rnf (H.Bind k v l r) = rnf k `seq` rnf v `seq` rnf l `seq` rnf r `seq` ()


uncurry2 f (a, b)    = f a b
uncurry3 f (a, b, c) = f a b c
const2   x _ _       = x

insert :: (Ord k) => k -> v -> H.BST k v -> H.BST k v
insert k v H.Emp                        = H.Bind k v H.Emp H.Emp
insert k v (H.Bind k' v' l r) | k == k' = H.Bind k v l r
                              | k < k'  = H.Bind k' v' (insert k v l) r
                              | k > k'  = H.Bind k' v' l (insert k v r)

ofList :: (Ord k) => [(k, v)] -> H.BST k v
ofList = foldr (uncurry insert) H.Emp

toList :: H.BST k v -> [(k, v)]
toList H.Emp            = []
toList (H.Bind k v l r) = toList l ++ (k, v) : toList r

applyBehavior :: Behavior a -> Time -> a
applyBehavior (Beh fb) t = fb t

twiceTime :: Behavior Time
twiceTime = Beh (* 2)

isBST :: (Ord k) => k -> k -> H.BST k v -> Bool
isBST _ _ H.Emp = True
isBST lb ub (H.Bind k _ l r) = lb < k && k < ub && isBST lb k l && isBST k ub r

checkTrees :: (Ord k, Eq v, Bounded k) => H.BST k v -> H.BST k v -> Bool
checkTrees t1 t2 = isBST minBound maxBound t1 &&
                   isBST minBound maxBound t2 &&
                   toList t1 == toList t2

grader :: GradeState ()
grader = do
  -- myFoldl
  gradeManual "myFoldl legit implementation" (return ()) 1

  grade "myFoldl base case"
    (uncurry2 $ H.myFoldl $ const2 (0 :: Int))
    (uncurry2 $ foldl $ const2 (0 :: Int))
    (==) (1, ([] :: [Int])) 1

  grade "myFoldl with +"
    (uncurry2 $ H.myFoldl (+))
    (uncurry2 $ foldl (+))
    (==) (0, [1, 2, 3] :: [Int]) 1

  grade "myFoldl with -"
    (uncurry2 $ H.myFoldl (-))
    (uncurry2 $ foldl (-))
    (==) (0, [1, 2, 3] :: [Int]) 2

  -- myReverse
  gradeManual "myReverse in terms of foldl" (return ()) 1
  
  grade "myReverse base case"
    H.myReverse
    reverse
    (==) ([] :: [Int]) 1

  grade "myReverse simple list"
    H.myReverse
    reverse
    (==) ([1, 2, 3] :: [Int]) 1
  
  grade "myReverse"
    H.myReverse
    reverse
    (==) "a man, a plan, a canal, panama" 1

  -- myFoldr
  gradeManual "myFoldr in terms of foldl" (return ()) 1

  grade "myFoldr base case"
    (uncurry2 $ H.myFoldr $ const2 (0 :: Int))
    (uncurry2 $ foldr $ const2 (0 :: Int))
    (==) (1, ([] :: [Int])) 1

  grade "myFoldr with +"
    (uncurry2 $ H.myFoldr (+))
    (uncurry2 $ foldr (+))
    (==) (0, [1, 2, 3] :: [Int]) 1

  grade "myFoldr with -"
    (uncurry2 $ H.myFoldr (-))
    (uncurry2 $ foldr (-))
    (==) (0, [1, 2, 3] :: [Int]) 2

  -- myFoldl2
  gradeManual "myFoldl2 in terms of foldr" (return ()) 1

  grade "myFoldl2 base case"
    (uncurry2 $ H.myFoldl2 $ const2 (0 :: Int))
    (uncurry2 $ foldl $ const2 (0 :: Int))
    (==) (1, ([] :: [Int])) 1

  grade "myFoldl2 with +"
    (uncurry2 $ H.myFoldl2 (+))
    (uncurry2 $ foldl (+))
    (==) (0, [1, 2, 3] :: [Int]) 1

  grade "myFoldl2 with -"
    (uncurry2 $ H.myFoldl2 (-))
    (uncurry2 $ foldl (-))
    (==) (0, [1, 2, 3] :: [Int]) 2

  -- foldl'
  gradeManual "Explanation of foldl' plausible" (return ()) 2

  -- delete
  grade "delete on singleton tree"
    (uncurry2 H.delete)
    (uncurry2 S.delete)
    checkTrees (0 :: Int, ofList [(0, "Niki")])
    1

  grade "delete, promote left"
    (uncurry2 H.delete)
    (uncurry2 S.delete)
    checkTrees (3 :: Int, (H.Bind 3 "Niki"
                           (H.Bind 0 "Dimo" H.Emp H.Emp)
                           (H.Bind 5 "Don" H.Emp H.Emp)))
    1
  
  grade "delete, promote max from deep in tree"
    (uncurry2 H.delete)
    (uncurry2 S.delete)
    checkTrees (3 :: Int, H.Bind 3 "Niki"
                          (H.Bind 1 "Dimo"
                           (H.Bind 0 "Don" H.Emp H.Emp)
                           (H.Bind 2 "Panos" H.Emp H.Emp))
                          (H.Bind 4 "Zach" H.Emp H.Emp))
    3

  grade "delete, promote max from deep in tree 2"
    (uncurry2 H.delete)
    (uncurry2 S.delete)
    checkTrees (5 :: Int, H.Bind 5 "Pat"
                          (H.Bind 2 "Zach"
                           H.Emp
                           (H.Bind 4 "Ravi" (H.Bind 3 "Ming" H.Emp H.Emp) H.Emp))
                          H.Emp)
    3

  -- apparently we added this after the fact, so we can't
  -- depend on anyone having done it
  -- functor instance
  -- grade "fmap (+1) on behavior that doubles time"
  --   (applyBehavior $ fmap (+1) twiceTime)
  --   (applyBehavior $ S.bfmap (+1) twiceTime)
  --   (==)
  --   5
  --   3

  -- -- applicative functor instance
  -- grade "pure on constant 729"
  --   (applyBehavior $ pure 729)
  --   (applyBehavior $ S.bpure 729)
  --   (==)
  --   0
  --   1

  -- grade "(+) on pure behaviors that return 1 and 2 (using <$>, <*>)"
  --   (applyBehavior ((+) <$> pure 1 <*> pure 2))
  --   (applyBehavior $ pure 3)
  --   (==)
  --   0
  --   2

  -- solar system

  -- gradeManual "At least one planet is shown" H.main 5
  -- gradeManual "Occlusion is implemented" H.main 10
  -- gradeManual "Planets shrink as they go backwards" H.main 5

  return ()

main :: IO ()
main = runGrader grader
