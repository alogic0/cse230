import Grade
import qualified Hw3 as H
import qualified Solution as S
import qualified Data.Map as M
import Data.Function
import Control.Applicative
import Control.Monad.State
import Text.Parsec.String

import Control.DeepSeq
import qualified Control.Exception as X

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
     parse <- liftIO $ parseFromFile H.statementsP file
     case parse of
          Left err -> liftIO (print err) >> bumpScore val False
          Right s  ->
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
  -- Pong
  
  gradeManual "Pong: playing field behaves as expected (display, bouncing)" H.playPong 4
  gradeManual "Pong: one paddle moves with mouse, ball bounces off" (return ()) 2
  gradeManual "Pong: other paddle moves with a/d, ball bounces off" (return ()) 2
  gradeManual "Pong: increment score when ball goes off top" (return ()) 2
  gradeManual "Pong: game resets after incrementing score (bottom)" (return ()) 2

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
  
  gradeParser "2^8" "twoPowerEight.imp" False twoPowerEight 5

  gradeParser "gcd" "gcd.imp" False gcdProg 5
    
  return ()

main :: IO ()
main = runGrader grader
