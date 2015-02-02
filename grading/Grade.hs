module Grade where

import qualified Control.Exception as X
import Control.Monad.State
import System.IO as IO
import System.Timeout
import Control.DeepSeq


type Score        = Int
data Grade        = Grade { total :: Score, weight :: Score, questions :: Int, numCorrect :: Int }
type GradeState a = StateT Grade IO a

bumpScore :: Score -> Bool -> GradeState ()
bumpScore w correct = do let score = if correct then w else 0
                         liftIO $
                           putStrLn $ "Score: " ++ show score ++ " of " ++ show w ++ " points."
                         g <- get
                         put $ Grade (total g + score) (weight g + w)
                           (questions g + 1) (numCorrect g + if correct then 1 else 0)
                         return ()

data TestResult a b = Exception b
                    | OK a


myevaluate x = 
  do -- y <- X.evaluate x 
     let y = rnf x
     return x

exceptionFail :: X.SomeException -> IO (TestResult a X.SomeException)
exceptionFail e = return $ Exception e

skipgrade :: (Show a, Show b, NFData b) =>
         String -> (a -> b) -> (a -> b) -> (b -> b -> Bool) -> a -> Score -> GradeState ()
skipgrade fname f g cmp x w = do
  let print = liftIO . putStrLn
  print "-----------------"
  print $ "Testing " ++ fname ++ " with input: "
  print $ show x
  print $ "Your function does not terminate: out of memory (requested 2097152 bytes)"
  bumpScore w False
 

grade :: (Show a, Show b, NFData b) =>
         String -> (a -> b) -> (a -> b) -> (b -> b -> Bool) -> a -> Score -> GradeState ()
grade fname f g cmp x w = do
  let print = liftIO . putStrLn
  print "-----------------"
  print $ "Testing " ++ fname ++ " with input: "
  print $ show x
  mtheirs <- liftIO $ X.catch ((f x) `deepseq` (X.evaluate (f x)) >>= return . OK) exceptionFail
  case mtheirs of
    Exception e -> do
      print $ "Your function threw an exception: " ++ show e
      bumpScore w False
    OK theirs   -> do
      let ours    = g x
      let correct = cmp theirs ours
      print $ "Our expected output:"
      print $ show ours
      print $ "Your output:"
      print $ show theirs
      bumpScore w correct

gradeNil fname x w = do
  let print = liftIO . putStrLn
  print "-----------------"
  print $ "Ignoring " ++ fname ++ ", which crashes when run."
  print "Input would be:"
  print $ show x
  bumpScore w False
 
gradeMany :: (Show a, Show b, NFData b) =>
             String -> (a -> b) -> (a -> b) -> (b -> b -> Bool) -> [(a, Score)] -> GradeState ()
gradeMany fname theirs ours cmp = mapM_ (uncurry $ grade fname theirs ours cmp)

gradeManual :: String -> IO () -> Score -> GradeState ()
gradeManual s a w = do let print = liftIO . putStrLn
                       print $ "Grading " ++ s ++ " manually..."
                       liftIO $ hFlush stdout
                       liftIO $ timeout (1 * (10^6))a
                       print "Ok?"
                       liftIO $ hFlush stdout
                       ok <- liftIO $ getLine
                       bumpScore w $ ok == "y"

runGrader :: GradeState () -> IO ()
runGrader gs = do
  (_, g) <- runStateT gs (Grade 0 0 0 0)
  putStrLn "================="
  putStrLn $ "Answered " ++ show (numCorrect g) ++ " correctly out of " ++ show (questions g) ++
    " for a score of " ++ show (total g) ++ " out of " ++ show (weight g) ++ "."
