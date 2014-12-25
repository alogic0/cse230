
import Data.List
import Data.List.Split

inFile  = "CSE230Wi14Scores.csv"
outFile = "Zog.csv" 

main = do str      <- readFile inFile
          let ls    = tail . lines $ str
          let rows  = map (splitOn ",") ls
          let erows = map (concatMap (splitOn " ")) rows
          let srows = sortBy (\x y -> compare (x !! 1) (y !! 1)) erows
          let str'  = unlines $ map (intercalate ",") srows
          writeFile outFile str'
