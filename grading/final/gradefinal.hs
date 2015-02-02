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
import qualified Final as F
import qualified Solution as S
import qualified Control.Exception as X

import Control.DeepSeq
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, OverlappingInstances, FlexibleInstances #-}

-- TODO: - Make sure everyone is using the updated mgu code
--       - Verify each log to be sure that the mgu is not screwing them up;
--         should we try both versions of the code in case of lost points?
--       - Check bugfixes for ti:
--         - ELet: s2 *after* s1
--         - ti_top generalization

instance NFData F.TVbl where
  rnf (F.TV s) = rnf s 

instance NFData F.Type where
  rnf F.TInt         = ()
  rnf F.TBool         = ()
  rnf (F.TVbl v)     = rnf v
  rnf (F.TArr t1 t2) = rnf t1 `seq` rnf t2
  rnf (F.TCom t1 t2) = rnf t1 `seq` rnf t2
  rnf (F.TList t)    = rnf t

instance NFData F.Scheme where
  rnf (F.Forall vs t) = rnf vs `seq` rnf t

doOp (S.BSTadd k v) = F.bstInsert k v 
doOp (S.BSTdel k)   = F.bstDelete k 

ofBSTops ::  Ord k => [S.BSTop k v] -> F.BST k v
ofBSTops = foldr doOp F.Emp

prop_insert_bso = forAll (listOf S.genBSTadd) $ S.isBSO . ofBSTops

prop_insert_bal = forAll (listOf S.genBSTadd) $ S.isBal . ofBSTops

prop_insert_map = forAll (listOf S.genBSTadd) $ \ops -> 
  S.toBinds (ofBSTops ops) == Map.toAscList (S.mapOfBSTops ops)
  
treeKeys :: F.BST a Char -> [a]
treeKeys = map fst . S.toBinds

prop_delete_bso = forAll S.genBal $ \t -> forAll (listOf S.genBSTdel) $ \ops ->
  do k <- elements $ treeKeys t
     return $ S.isBSO $ F.bstDelete k t

prop_delete_map = forAll S.genBal $ \t -> forAll (listOf S.genBSTdel) $ \ops ->
  do k <- elements $ treeKeys t
     return $ S.toBinds (F.bstDelete k t) == filter (\(k', _) -> k /= k') (S.toBinds t)

prop_delete_bal = forAll S.genBal $ \t -> forAll (elements $ treeKeys t) $ \k ->
  S.isBal (F.bstDelete k t)

prop_genBal = forAll F.genBal S.isBal

prop_genBalBSO = forAll F.genBal S.isBSO

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

-- STM Stuff

rounds = 100

testProduceConsumeTwice nt sp sc = do
  c1 <- F.newFiniteChan sp
  c2 <- F.newFiniteChan sc
  forM [1..nt] $ \id -> do
    forkIO $ producer id c1
    forkIO $ consumer id c1 c2
  let total = nt * rounds
  (ok, _) <- foldM (checkFIFO c2) (True, Map.empty) [1..total]
  return ok
    where
      producer id fc = do
        forM_ [1..rounds] (\i -> do F.writeFiniteChan fc (id, i)) 
        return ()
      consumer id fcin fcout = do
        forM_ [1..rounds] (\_ -> do (idp, i) <- F.readFiniteChan fcin
                                    F.writeFiniteChan fcout (id, idp, i))
        return ()
      checkFIFO fc (ok, m) _ = do
        (idc, idp, i) <- F.readFiniteChan fc
        return (ok && (i > Map.findWithDefault 0 (idc, idp) m), Map.insert (idc, idp) i m)

stmWeight = 3

gradeSTM nt sp sc = do
  printLn "-----------------"
  printLn "Testing producer, consumer/producer, consumer with four second timeout and..."
  printLn $ "  " ++ show nt ++ " of each of the first two"
  printLn $ "  " ++ show sp ++ " producer queue slots"
  printLn $ "  " ++ show sc ++ " consumer queue slots"
  -- printLn $ "Readchan, writechan not implemented"
  -- bumpScore stmWeight False
  let value = testProduceConsumeTwice nt sp sc
  mmmok <- liftIO $ timeout 4000000 $ X.catch ((X.evaluate value ) >>= return . OK) exceptionFail
  case mmmok of
    Nothing -> do printLn "Timed out!"
                  bumpScore stmWeight False
    Just (OK iok) -> do ok <- liftIO $ iok
                        printLn $ "FIFO order preserved: " ++ show ok
                        bumpScore stmWeight ok
    Just (Exception e) -> do printLn $ "Your function threw an exception: " ++ show e
                             bumpScore stmWeight False

gradeChannelSize w = do
  printLn "-----------------"
  printLn "Testing finite channels obey size constraints..."

  -- printLn $ "Channels not implemented"
  -- bumpScore w False
  fc <- liftIO $ F.newFiniteChan 1
  liftIO $ F.writeFiniteChan fc 1
  ok <- liftIO $ timeout 1000000 $ F.writeFiniteChan fc 2
  case ok of
    Nothing -> do printLn "Writing 2 items to 1 item channel blocks - ok"
                  bumpScore w True
    Just _  -> do printLn "Writing 2 items to 1 item channel doesn't block!"
                  bumpScore w False

-- Type Inference

tvars :: F.Type -> [F.TVbl]
tvars (F.TVbl v)       = [v]
tvars F.TInt           = []
tvars F.TBool          = []
tvars (t1 `F.TArr` t2) = tvars t1 ++ tvars t2
tvars (t1 `F.TCom` t2) = tvars t1 ++ tvars t2
tvars (F.TList t)      = tvars t

compareTypes :: F.Type -> F.Type -> Bool
compareTypes t1 t2 =
    let sub = Map.fromList $ zip (tvars t1) (map F.TVbl $ tvars t2) in
      t2 == F.apply sub t1

gradeType :: F.Exp -> F.Type -> Score -> GradeState ()
gradeType e t w = do printLn "-----------------"
                     printLn $ "Testing program:"
                     printLn $ show $ S.prExp e
                     printLn $ "Expecting type:"
                     printLn $ show $ S.prType t
                     let value = F.typeInference S.env e
                     theirs <- liftIO $ X.catch (value `deepseq` X.evaluate value >>= return . OK) exceptionFail
                     case theirs of
                       Exception e -> do printLn $ "Your function threw an exception: " ++ show e
                                         bumpScore w False
                       OK (Left _) -> do printLn "Failed to infer type"
                                         bumpScore w False
                       OK (Right (F.Forall _ t')) ->
                             do printLn "Got type:"
                                printLn $ show $ S.prType t'
                                bumpScore w $ compareTypes t t'

gradeUntypeable :: F.Exp -> Score -> GradeState ()
gradeUntypeable e w = do printLn "-----------------"
                         printLn $ "Testing untypeable program:"
                         printLn $ show $ S.prExp e
                         theirs <- liftIO $ X.catch (X.evaluate (F.typeInference S.env e) >>= return . OK) exceptionFail
                         case theirs of
                           Exception e -> do printLn $ "Your function threw an exception: " ++ show e
                                             bumpScore w False
                           OK (Left _) -> do printLn "Confirmed expression is untypeable."
                                             bumpScore w True
                           OK (Right (F.Forall _ t)) ->
                                 do printLn "Got type:"
                                    printLn $ show $ S.prType t
                                    bumpScore w False

eva = F.EV "a"
evb = F.EV "b"
evc = F.EV "c"
evd = F.EV "d"

evx = F.EV "x"
evy = F.EV "y"
evz = F.EV "z"

tva = F.TVbl $ F.TV "a"
tvb = F.TVbl $ F.TV "b"

eThree = F.ELit (F.LInt 3)

-- Tuple microtests

eTup1 = F.EAbs evx $ (F.EApp F.eInc $ F.EVbl evx) `F.ECom` F.EVbl evx
tTup1 = F.tArrs [F.TInt, F.TInt `F.TCom` F.TInt]

eTup2 = F.EAbs evx $ F.EVbl evx `F.ECom` (F.EApp F.eInc $ F.EVbl evx)
tTup2 = tTup1

eTup3 = F.EAbs evx $ ((F.EApp F.eInc $ F.EVbl evx) `F.ECom` F.EVbl evx) `F.ECom` F.EVbl evx
tTup3 = F.tArrs [F.TInt, (F.TInt `F.TCom` F.TInt) `F.TCom` F.TInt]

eTup4 = F.EFst $ eThree `F.ECom` F.ELit (F.LBool False)
tTup4 = F.TInt

eTup5 = F.ESnd $ eThree `F.ECom` F.ELit (F.LBool False)
tTup5 = F.TBool

eTup6 = F.EAbs evx $ F.ELet evy (F.eZero `F.ECom` (F.EApp F.eInc $ F.EVbl evx)) (F.EVbl evx)
tTup6 = F.tArrs [F.TInt, F.TInt]

eTup7 = F.EAbs evx $ F.EFst (F.EVbl evx)
tTup7 = F.tArrs [tva `F.TCom` tvb, tva]

eTup8 = F.EFst $ F.ELit (F.LInt 0)

-- List microtests

eList1 = F.ELet evx (F.ENil) $ F.ELet evy (F.ENil) (F.EVbl evx `F.ECom` F.EVbl evy)
tList1 = (F.TList tva) `F.TCom` (F.TList tvb)

eList2 = F.ECons eThree F.ENil
tList2 = F.TList F.TInt

eList3 = F.ECons eList2 F.ENil
tList3 = F.TList tList2

eList4 = F.ECons eThree eThree

eList5 = F.EIsNil eList2
tList5 = F.TBool

eList6 = F.EIsNil eThree

eList7 = F.EDcons F.ENil
tList7 = tva `F.TCom` F.TList tva

eList8 = F.EDcons eThree

-- letrec tests

eZBody = F.eIf (F.EIsNil (F.EVbl evx))
               F.ENil
               (F.eIf (F.EIsNil (F.EVbl evy))
                      F.ENil
                      (F.ELet eva (F.EDcons $ F.EVbl evx)
                        (F.ELet evb (F.EDcons $ F.EVbl evy)
                          (F.ECons (F.EFst (F.EVbl eva) `F.ECom` F.EFst (F.EVbl evb))
                                   (F.eApps [F.EVbl evz, F.ESnd (F.EVbl eva), F.ESnd (F.EVbl evb)])))))

eZip = F.ERec evz
         (F.EAbs evx $ F.EAbs evy eZBody)
         (F.EVbl evz)
tZip = F.tArrs [F.TList tva, F.TList tvb, F.TList (tva `F.TCom` tvb)]

eFold = F.ERec evz
          (F.EAbs eva
            (F.EAbs evb
              (F.EAbs evc
                (F.eIf (F.EIsNil $ F.EVbl evc)
                       (F.EVbl evb)
                       (F.eApps [F.EVbl evz,
                                 F.EVbl eva,
                                 F.eApps [F.EVbl eva, F.EVbl evb, F.EFst (F.EDcons $ F.EVbl evc)],
                                 F.ESnd (F.EDcons $ F.EVbl evc)])))))
          (F.EVbl evz)
tFold = F.tArrs [F.tArrs [tva, tvb, tva], tva, F.TList tvb, tva]

ePoly = F.ERec
          evz
          (F.EAbs evx $ F.EVbl evx)
          (F.eApps [F.EVbl evz, eThree] `F.ECom` F.eApps [F.EVbl evz, F.ELit (F.LBool True)])
tPoly = F.TInt `F.TCom` F.TBool

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
  gradeQC "arbitrary balanced trees are also BSO" prop_genBal 2
  
  --- Maintaining Balance Through Insert
  gradeQC "insertion maintains balance" prop_insert_bal 5

  --- Maintaining Balance Through Delete
  gradeQC "deletion maintains balance" prop_delete_bal 5

  -- Type Inference (25 points)

  --- Extension With Pairs (2 points each?)

  -- Subst application after first element: fun a -> (a + 3, a)
  gradeType eTup1 tTup1 1

  -- Subst application after second element: fun a -> (a, a + 3)
  gradeType eTup2 tTup2 1

  -- Subst returned from whole thing: fun a -> ((a, a + 3), a)
  gradeType eTup3 tTup3 1

  -- fst: fst (3, true)
  gradeType eTup4 tTup4 1

  -- snd: snd (3, true)
  gradeType eTup5 tTup5 1

  -- Subst propagates through fst: fun a -> let f = fst (0, a + 3) in a
  gradeType eTup6 tTup6 1

  -- Wrapping fst: fun a -> fst a
  gradeType eTup7 tTup7 1

  -- Tuple operator fails on non-tuple: fst 3
  gradeUntypeable eTup8 1

  --- Extension With Lists

  -- Nil: let a = Nil in let b = Nil in (a, b) (Need to have distinct type vars)
  gradeType eList1 tList1 1

  -- Cons, simple: Cons (3, Nil)
  gradeType eList2 tList2 1

  -- Cons, nested: Cons (Cons (3, Nil), Nil)
  gradeType eList3 tList3 1

  -- Cons, should fail: Cons (3, 3)
  gradeUntypeable eList4 1
            
  -- IsNil, returns boolean: IsNil (Cons (3, Nil))
  gradeType eList5 tList5 1

  -- IsNil, errors out on non-list: IsNil 3
  gradeUntypeable eList6 1

  -- Dcons, success: Dcons Nil
  gradeType eList7 tList7 1

  -- Dcons, failure: Dcons 3
  gradeUntypeable eList8 1

  --- Extension With Recursion
  -- foldl: let rec foldl f b xs = match xs with [] -> b | x : xs -> foldl f (f b x) xs
  gradeType eFold tFold 3

  -- zip:
  --  let rec zip xs ys = match xs with [] -> [] | x : xs -> match ys with [] -> [] | y : ys -> (x, y) : zip xs ys
  gradeType eZip tZip 5

  -- Polymorphism inside body: let id x = x in let a = id 3 in id False
  -- Lowered in value because I'm not sure how clear this goal was and I'm
  -- only 99.9% certain I did not tell someone at some point to ignore quantifiers.
  gradeType ePoly tPoly 1
  
  -- STM Queues (16 points)
  -- Test plain old FIFO in one thread

  forM_ [(1, 1, 1), (2, 1, 1), (5, 2, 1), (5, 3, 5), (10, 5, 1)]
            (\(nt, sp, sc) -> do gradeSTM nt sp sc)

  gradeChannelSize 1

  return ()

main :: IO ()
main = runGrader grader
