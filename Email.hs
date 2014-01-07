import Data.List
import System.Random.Shuffle

inputfile  = undefined
outputfile = undefined

toSEP       = map tx
  where
    tx '\r' = '\n'
    tx '\t' = ' '
    tx c    = c

rows   = fmap words . tail . lines . toSEP  

sids   = sort . fmap (!! 1) . rows 
emails = sort . fmap ((++ "@eng.ucsd.edu") . last) . rows

dumpSids :: FilePath -> IO ()
dumpSids z
  = do str <- readFile z
       ids <- shuffleM $ sids str 
       writeFile "dumpSids" $ unlines ids 
