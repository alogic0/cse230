import Grade
import Hw1 as H
import Solution as S
import XMLMain as X
import XMLTypes as T
import Play
import Data.Function
import Data.List
import Control.Monad.State
import qualified Control.Exception as E

import Control.DeepSeq

differenceOk :: Float -> Float -> Bool
differenceOk x y = abs (x - y) <= 0.1

gradeBigger :: H.Shape -> Float -> Score -> GradeState ()
gradeBigger s e w = do
  let originalArea      = S.area s
  let originalPerimeter = S.perimeter s
  let theirs            = H.bigger s e
  let theirArea         = S.area theirs
  let theirPerimeter    = S.perimeter theirs
  let print s           = liftIO $ putStrLn s
  mtheirs <- liftIO $ E.catch ((theirPerimeter) `deepseq` theirArea `deepseq` (E.evaluate ()) >>= return . OK) exceptionFail
  case mtheirs of 
   Exception e -> do
      print $ "Your function threw an exception: " ++ show e
      bumpScore w False
   _ -> do 
      print "-----------------"
      print $ "Testing shape scaling by " ++ show e ++ "x on shape:"
      print $ show s
      print "Your perimeter after scaling:"
      print $ show theirPerimeter
      print "Your area after scaling:"
      print $ show theirArea
      print $ "Spec was very ambiguous, so we only check that the result shape is bigger."
      bumpScore w $ (theirPerimeter > originalPerimeter) || (theirArea > originalArea)
 
applyf1f2 f1 f2 _ = f1 (f2 (*) [1, 2, 3, 4]) 5

grader :: GradeState ()
grader = do
  gradeMany "sides" H.sides S.sides (==) [(H.Rectangle 1 1, 1), (H.Ellipse 2 2, 1)]
  grade "'rectangle 1.0 1.0' has four sides"
    (S.sides . uncurry H.rectangle) (S.sides . uncurry S.rectangle) (==) (1, 1) 1
  grade "'rtTriangle 1.0 1.0' has three sides"
    (S.sides . uncurry H.rtTriangle) (S.sides . uncurry S.rtTriangle) (==) (1, 1) 1
  grade "line has zero sides" H.sides S.sides (==) (H.Polygon [(0, 0), (1, 1)]) 1

  gradeBigger (H.Rectangle 2.5 2.5) 3 1
  gradeBigger (H.RtTriangle 3 4) 5 1
  gradeBigger (H.rtTriangle 3 4) 5 1
  gradeBigger (H.Polygon [(1, 1), (2, 1), (3, 2), (2, 2)]) 4 1
  gradeBigger (H.Ellipse 1 1) 5 1

  gradeManual "hanoi 2" (H.hanoi 2 "moe" "larry" "curly") 1
  gradeManual "hanoi 3" (H.hanoi 3 "moe" "larry" "curly") 1

  gradeManual "sierpinskiCarpet" H.sierpinskiCarpet 5
  gradeManual "your own fractal" H.myFractal 5

  -- gradeMany "length" M.myLength S.myLength (==) [([], 1), ([1, 2], 1)]
  -- grade "length" M.myLength S.myLength (==) ["a", "b", "c"] 1

  -- Thrown out due to book edition differences.
  -- grade "f1, f2" (applyf1f2 M.f1 M.f2) (applyf1f2 S.f1 S.f2) (==) () 1

  gradeMany "lengthNonRecursive" H.lengthNonRecursive length (==) [([], 1), ([-1, 2, -3], 1)]
  gradeMany "doubleEach" H.doubleEach S.doubleEach (==) [([], 1), ([-1, 2, -3], 1)]
  gradeMany "doubleEachNonRecursive" H.doubleEachNonRecursive S.doubleEach (==) [([], 1), ([-1, 2, -3], 1)]
  gradeMany "pairAndOne" H.pairAndOne S.pairAndOne (==) [([], 1), ([-1, 2, -3], 1)]
  gradeMany "pairAndOneNonRecursive" H.pairAndOneNonRecursive S.pairAndOne (==) [([], 1), ([-1, 2, -3], 1)]
  gradeMany "maxList" H.maxList S.maxList (==) [([1], 1), ([-4, -3, -5], 1)]
  gradeMany "maxListNonRecursive" H.maxListNonRecursive S.maxList (==) [([1], 1), ([-4, -3, -5], 1)]
  gradeMany "minList" H.minList S.minList (==) [([1], 1), ([-4, -3, -5], 1)]
  gradeMany "minListNonRecursive" H.minListNonRecursive S.minList (==) [([1], 1), ([-4, -3, -5], 1)]

  let s1 = H.Branch (H.Leaf "a") (H.Leaf "b")
  let s2 = H.Branch s1 (H.Leaf "c")
  let s3 = H.Branch s1 s2
  gradeMany "treeSize" H.treeSize S.treeSize (==) [(H.Leaf "0", 1), (s1, 1), (s3, 1)]
  gradeMany "treeHeigth" H.treeSize S.treeSize (==) [(H.Leaf "0", 1), (s1, 1), (s3, 1)]
  gradeMany "fringe" (sort . H.fringe) (sort . S.fringe) (==) [(H.Leaf "0", 1), (s1, 1), (s3, 1)]

  let t1 = H.IBranch 3 H.ILeaf H.ILeaf
  let t2 = H.IBranch 5 t1 H.ILeaf
  let t3 = H.IBranch 8 t1 t2
  grade "takeTree" (uncurry H.takeTree) (uncurry S.takeTree) (==)
    (2, H.ILeaf :: H.InternalTree Int) 1
  grade "takeTree" (uncurry H.takeTree) (uncurry S.takeTree) (==)
    (1, t1) 1
  grade "takeTree" (uncurry H.takeTree) (uncurry S.takeTree) (==)
    (2, t1 :: H.InternalTree Int) 1
  grade "takeTree" (uncurry H.takeTree) (uncurry S.takeTree) (==)
    (2, t3) 1
  grade "takeTree" (uncurry H.takeTree) (uncurry S.takeTree) (==)
    (3, t3) 1
--   gradeMany "takeTree" (uncurry H.takeTree) (uncurry S.takeTree) (==)
--     [((2, H.ILeaf), 1), ((1, t1), 1), ((2, t1), 1), ((2, t3), 1), ((3, t3), 1)]
  grade "takeTreeWhile (const True)"
    (H.takeTreeWhile (const True)) (S.takeTreeWhile (const True)) (==) t3 1
  grade "takeTreeWhile (> 3)" (H.takeTreeWhile (> 3)) (S.takeTreeWhile (> 3)) (==) t3 1
  grade "takeTreeWhile odd" (H.takeTreeWhile odd) (S.takeTreeWhile odd) (==) t3 1

  grade "map (* 3)" ((H.myMap (* 3)) :: [Int] -> [Int]) ((S.myMap (* 3)) :: [Int] -> [Int]) (==) [5, 10, 15] 1
  grade "map odd" (H.myMap odd) (S.myMap odd) (==) [1, 2, 3] 1

  liftIO $ putStrLn "Testing your XML play converter..."
  liftIO $ putStrLn "Converting... "
  liftIO $ writeFile "dream.html" (T.xml2string (H.formatPlay play))
  ok <- liftIO $ X.testResults "hw1/dream.html" "hw1/sample.html"
  bumpScore 15 ok

  return ()

instance NFData a => NFData (H.InternalTree a) where
  rnf ILeaf           = ()
  rnf (IBranch v l r) = rnf v `seq` rnf l `seq` rnf r `seq` ()

main :: IO ()
main = runGrader grader
