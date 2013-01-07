#!/usr/bin/env runhaskell

main = do putStrLn "What is your name ?"
          n <- getLine
          putStrLn ("Happy New Year " ++ n)
