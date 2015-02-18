{-# LANGUAGE ScopedTypeVariables #-}
import Grade
import qualified Hw2 as H
import qualified Solution as S
import qualified Data.Map as M
import Data.Function
import Control.Applicative
import Control.Monad.State
import Text.Parsec.String

import Control.DeepSeq
import qualified Control.Exception as X

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

isBST :: (Ord k) => k -> k -> H.BST k v -> Bool
isBST _ _ H.Emp = True
isBST lb ub (H.Bind k _ l r) = lb < k && k < ub && isBST lb k l && isBST k ub r

checkTrees :: (Ord k, Eq v, Bounded k) => H.BST k v -> H.BST k v -> Bool
checkTrees t1 t2 = isBST minBound maxBound t1 &&
                   isBST minBound maxBound t2 &&
                   toList t1 == toList t2

instance NFData H.Value where
  rnf (H.IntVal n)  = rnf n
  rnf (H.BoolVal n) = rnf n

instance Eq H.Value where
  H.IntVal n1 == H.IntVal n2 = n1 == n2
  H.BoolVal n1 == H.BoolVal n2 = n1 == n2

evalE :: (Show a, Eq b) => (a -> State H.Store b) -> (a, H.Store) -> b
evalE f (inp, s) = evalState (f inp) s

hEvalE = evalE H.evalE
sEvalE = evalE S.evalE

evalS :: (H.Statement -> State H.Store ()) -> (H.Statement, H.Store) -> H.Store
evalS f (stmt, s) = execState (f stmt) s

hEvalS = evalS H.evalS
sEvalS = evalS S.evalS

gradeEvalE msg =
  grade ("evalE, " ++ msg)
    hEvalE
    sEvalE
    (==)

gradeEvalS msg =
  grade ("evalS, " ++ msg)
    hEvalS
    sEvalS
    (==)

gradeExecS msg skip | skip =
  skipgrade ("execS, " ++ msg)
    (flip H.execS M.empty)
    (flip S.execS M.empty)
    (==)


gradeExecS msg skip =
  grade ("execS, " ++ msg)
    (flip H.execS M.empty)
    (flip S.execS M.empty)
    (==)

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

gradeParser msg file skip preparsed val | skip =
  do liftIO $ putStrLn "-----------------"
     liftIO $ putStrLn $ "Testing parser on file " ++ file
     -- liftIO (print "Your function threw an exception: TBD") >> bumpScore val False
     liftIO (print "Your function does not terminate") >> bumpScore val False
 
gradeParser msg file _  preparsed val =
  do liftIO $ putStrLn "-----------------"
     liftIO $ putStrLn $ "Testing parser on file " ++ file
     parse <- liftIO $ parseFromFile H.statementP file
     parse <- liftIO $ X.try $ X.evaluate parse
     case parse of
          Left (err :: X.SomeException) -> liftIO (print err) >> bumpScore val False
          Right (Right s)  ->
            let preanswer    = H.execS preparsed M.empty in
            let parsedanswer = H.execS s M.empty in
              do liftIO $ putStrLn "Answer on pre-parsed structure:"
                 liftIO $ S.printStore preanswer
                 liftIO $ putStrLn "sss = "
                 liftIO $ print s
                 liftIO $ putStrLn "Answer on parsed input: "
                 liftIO $ S.printStore parsedanswer
                 bumpScore val (preanswer == parsedanswer)

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

  -- evalE
  
  gradeEvalE "values"
    (H.Val $ H.IntVal 25, M.empty)
    1
    
  gradeEvalE "var present"
    (H.Var "X", M.fromList [("X", H.IntVal 25)])
    2

  gradeEvalE "compound expression"
    (H.Op H.Divide (H.Op H.Plus (H.Var "Y") (H.Val $ H.IntVal 5))
                   (H.Val $ H.IntVal 5),
     M.fromList [("Y", H.IntVal 25)])
    2

  -- evalS

  gradeEvalS "basic assignment"
    (H.Assign "X" (H.Val $ H.IntVal 1066), M.empty)
    2
     
  gradeEvalS "basic sequence"
    (mksequence [H.Assign "X" (H.Val $ H.IntVal 1066), H.Assign "X" (H.Val $ H.IntVal 1776)],
     M.empty)
    2

  gradeEvalS "increment variable"
    (H.Assign "X" (H.Op H.Plus (H.Var "X") (H.Val $ H.IntVal 1)),
     M.fromList [("X", H.IntVal 2)])
    2

  gradeEvalS "branching, taken"
    (H.If (H.Op H.Gt (H.Var "X") (H.Val $ H.IntVal 0))
          (H.Assign "Y" (H.Val $ H.IntVal 1))
          (H.Assign "Y" (H.Val $ H.IntVal 2)),
     M.fromList [("X", H.IntVal 12)])
    2

  gradeEvalS "branching, not taken"
    (H.If (H.Op H.Gt (H.Var "X") (H.Val $ H.IntVal 0))
          (H.Assign "Y" (H.Val $ H.IntVal 1))
          (H.Assign "Y" (H.Val $ H.IntVal 2)),
     M.fromList [("X", H.IntVal 0)])
    2

  gradeEvalS "basic while"
    (H.While (H.Op H.Gt (H.Var "X") (H.Val $ H.IntVal 0))
             (H.Assign "X" (H.Op H.Minus (H.Var "X") (H.Val $ H.IntVal 1))),
     M.fromList [("X", H.IntVal 5)])
    3

  -- execS

  gradeExecS "compute 2^8" False
    twoPowerEight
    5

  gradeExecS "compute GCD (15, 21)" False -- True
    gcdProg
    5

  -- parsing, via runFile
  
  gradeParser "2^8" "hw2/twoPowerEight.imp" False twoPowerEight 5

  gradeParser "gcd" "hw2/gcd.imp" False gcdProg 5

  return ()

main :: IO ()
main = runGrader grader
