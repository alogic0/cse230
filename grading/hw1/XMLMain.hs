module XMLMain where 

import XMLTypes
import Play

firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds) 
     | c==d = firstDiff cs ds 
     | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)

testResults :: String -> String -> IO Bool
testResults file1 file2 = do 
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> do
      putStr "Success!\n"
      return True
    Just (cs,ds) -> do
      putStr "Results differ: '"
      putStr (take 20 cs)
      putStr "' vs '"
      putStr (take 20 ds)
      putStr "'\n"
      return False
