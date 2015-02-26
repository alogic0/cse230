import Grade
import Test.QuickCheck
import System.Random
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM
import System.Timeout
import Data.Function
import qualified Data.Map as Map
import qualified Hw3 as H
import qualified Solution as S
import qualified Control.Exception as X

import Control.DeepSeq
import qualified Control.Exception as X

doOp (S.BSTadd k v) = H.bstInsert k v 
doOp (S.BSTdel k)   = H.bstDelete k 

ofBSTops ::  Ord k => [S.BSTop k v] -> H.BST k v
ofBSTops = foldr doOp H.Emp

prop_insert_bso = forAll (listOf S.genBSTadd) $ S.isBSO . ofBSTops

prop_insert_bal = forAll (listOf S.genBSTadd) $ S.isBal . ofBSTops

prop_insert_map = forAll (listOf S.genBSTadd) $ \ops -> 
  S.toBinds (ofBSTops ops) == Map.toAscList (S.mapOfBSTops ops)
  
treeKeys :: H.BST a Char -> [a]
treeKeys = map fst . S.toBinds

prop_delete_bso = forAll S.genBal $ \t -> forAll (listOf S.genBSTdel) $ \ops ->
  do k <- elements $ treeKeys t
     return $ S.isBSO $ H.bstDelete k t

prop_delete_map = forAll S.genBal $ \t -> forAll (listOf S.genBSTdel) $ \ops ->
  do k <- elements $ treeKeys t
     return $ S.toBinds (H.bstDelete k t) == filter (\(k', _) -> k /= k') (S.toBinds t)

prop_delete_bal = forAll S.genBal $ \t -> forAll (elements $ treeKeys t) $ \k ->
  S.isBal (H.bstDelete k t)

prop_genBal = forAll H.genBal S.isBal

prop_genBalBSO = forAll H.genBal S.isBSO

printLn :: String -> GradeState ()
printLn = liftIO . putStrLn

gradeQC :: Testable prop => String -> prop -> Score -> GradeState ()
gradeQC n p w = do printLn "-----------------"
                   printLn $ "Testing " ++ n
                   er <- liftIO $ timeout 4000000 $ quickCheckResult p
                   case er of
                     Nothing -> do printLn "Timeout after four seconds!"
                                   bumpScore w False
                     Just r  ->
                         case r of
                           Success _ _ _        -> bumpScore w True
                           Failure {reason = s} -> do printLn $ "Test failed: " ++ s
                                                      bumpScore w False
                           GaveUp _ _ _         -> error "Gave up!"

dropLeading = reverse . dropWhile (== False) . reverse

runBitSubtractor (b, xs) =
    let (ys, _) = H.bitSubtractor (H.lift0 b, map H.lift0 xs) in
      dropLeading $ map H.sample1 $ ys

runMultiplier (x, y) = dropLeading . map H.sample1 $ H.multiplier (map H.lift0 x, map H.lift0 y)

grader :: GradeState ()
grader = do
  -- BST Manipulation (24 points)

  --- Insertion
  gradeQC "insertion adds all the items" prop_insert_map 2
  gradeQC "binary search order after insertion" prop_insert_bso 3

  --- Deletion
  gradeQC "correct items after deletion" prop_delete_map 2
  gradeQC "binary search order after deletion" prop_delete_bso 3

  --- Arbitrary Balanced Tree Generation
  gradeQC "arbitrary balanced trees are in fact balanced" prop_genBal 2
  gradeQC "arbitrary balanced trees are also BSO" prop_genBalBSO 2

  --- Maintaining Balance Through Insert
  gradeQC "insertion maintains balance" prop_insert_bal 5

  --- Maintaining Balance Through Delete
  gradeQC "deletion maintains balance" prop_delete_bal 5


  -- Circuits (24 points)
  -- --------
  -- We don't have hooks into the properties --- they're hard-coded to check the functions
  -- that the students wrote --- so I'm not going to grade them. Instead, I've distributed
  -- the points I would've allocated into these over the implementations of the circuits
  -- themselves.
  --
  -- Bitsubtractor property (4 point each)
  --   - affirms 0 - 1 = 0 is correct
  --   - rejects 5 - 1 = 5
  --   - accepts 5 - 1 = 4
  --   - accepts 5 - 0 = 5
  -- Bitsubtractor itself (8 points each)
  --   + 0 - 1 = 0
  --   + 4 - 1 = 3
  --   + 5 - 0 = 5
  -- Multiplier property (4 point each)
  --   - affirms 2 * 5 = 10
  --   - rejects 2 * 5 = 9
  -- Multiplier itself (8 points each)
  --   + 2 * 5  = 10
  --   + 0 * 12 = 0
  --   + 1 * 3  = 3

  grade "bitSubtractor correctness"
        runBitSubtractor
        (const $ [])
        (==)
        (True, [])
        4

  grade "bitSubtractor correctness"
        runBitSubtractor
        (const $ [True, True])
        (==)
        (True, [False, False, True])
        4

  grade "bitSubtractor correctness"
        runBitSubtractor
        (const $ [False, False, True])
        (==)
        (False, [False, False, True])
        4

  grade "multiplier correctness"
        runMultiplier
        (const $ [])
        (==)
        ([], [False, False, True, True])
        4

  grade "multiplier correctness"
        runMultiplier
        (const $ [True, True])
        (==)
        ([True], [True, True])
        4

  grade "multiplier correctness"
        runMultiplier
        (const $ [False, True, False, True])
        (==)
        ([False, True], [True, False, True])
        4

  return ()

main :: IO ()
main = runGrader grader
