#!/usr/bin/env runhaskell

import Control.Concurrent hiding (readMVar)
import Control.Monad
import System.IO
import Data.IORef
import System.Environment (getArgs)
import System.Random 

import qualified Data.ByteString as B
import Network.HTTP
import Network.Browser
import Network.URI
import Data.Time
import Text.Printf

-----------------------------------------------------------
-- | 1. Mutable State Via IORef ---------------------------
-----------------------------------------------------------

newtype AccountIO = AIO (IORef Int)

newAccountIO ::  Int -> IO AccountIO
newAccountIO n 
  | n >= 0 
  = liftM AIO (newIORef n)
  | otherwise
  = do putStrLn "Do I look like a communist?!!"
       liftM AIO (newIORef 0)

showBalanceIO ::  AccountIO -> IO ()
showBalanceIO (AIO r) 
  = do bal <- readIORef r
       putStrLn $ "Current Balance: " ++ show bal

depositIO ::  AccountIO -> Int -> IO ()
depositIO (AIO r) n
  = do bal <- readIORef r
       if (bal + n < 0) 
         then putStrLn $ "Sorry, cannot withdraw. Balance below " ++ show n 
         else writeIORef r (bal + n)

main1 :: IO ()
main1 = do a <- newAccountIO 0
           mapM_ (depositIO a) (replicate 50 10) 
           showBalanceIO a   -- should be $500


-----------------------------------------------------------
-- | 2. Forking A Thread ----------------------------------
-----------------------------------------------------------

main2 = do hSetBuffering stdout NoBuffering
           forkIO $ forever (putChar 'A') -- thread that writes 'A'
           forkIO $ forever (putChar 'B') -- thread that writes 'B'
           threadDelay (10^5)                             -- shutdown after 1 sec

-----------------------------------------------------------
-- | 3. Randomly Fuzzing the Thread Scheduler -------------
-----------------------------------------------------------

toss       :: Int -> Int -> IO Int
toss lo hi = getStdRandom (randomR (lo, hi))

pauseRandom = do n <- toss 0 10
                 threadDelay $ n * (10 ^ 5)

main3 = do hSetBuffering stdout NoBuffering
           forkIO $ forever (putChar 'A' >> pauseRandom) -- thread that writes 'A'
           forkIO $ forever (putChar 'B' >> pauseRandom) -- thread that writes 'B'
           threadDelay (10^6)                             -- shutdown after 1 sec

-----------------------------------------------------------
-- | 3. Data Races due to sharing -------------------------
-----------------------------------------------------------

-- Bank account revisited; depositing with different threads, and fuzzed scheduler

depositIO' ::  AccountIO -> Int -> IO ()
depositIO' (AIO r) n
  = do bal <- readIORef r
       -- pauseRandom  -- comment out and you get right answer
       if (bal + n < 0) 
         then putStrLn $ "Sorry, cannot withdraw. Balance below " ++ show n 
         else writeIORef r (bal + n)

main4 ::  IO ()
main4 = do a <- newAccountIO 0
           mapM_ (forkIO . depositIO' a) (replicate 5 10) 
           showBalanceIO a   -- should be $50

-----------------------------------------------------------
-- | 4. MVars: Vanilla ------------------------------------
-----------------------------------------------------------

{- Shared Message-Box-Variables MVar
    
    -- Message Box has EITHER an `a` or is EMPTY
    data MVar a

    -- Create an EMPTY Reference
    newEmptyMVar :: IO (MVar a)
    
    -- Create a FULL reference
    newMVar	     :: a -> IO (MVar a)

    -- Blocks until box is full, takes out contents
    takeMVar	 :: MVar a -> IO a

    -- Overwrites contents with new contents
    putMVar	     :: MVar a -> a -> IO ()

 Uses:

 (1) An MVar is a useful container for shared mutable state, e.g. common design pattern 
     where threads need read and write access to some state, is to represent the state 
     value as an ordinary immutable Haskell data structure stored in an MVar.
 
 (2) An MVar is a one-place channel, which can be used for asynchronous communication
     between two threads.
 
 (3) An MVar () is a lock; takeMVar acquires the lock and putMVar releases it again. 
     An MVar used in this way can protect shared mutable state or critical sections.

-}

-----------------------------------------------------------
-- | 5. MVars: BankAccount/Deposit ------------------------
-----------------------------------------------------------


-- Bank account revisited; using MVars, depositing with different
-- threads, and fuzzed scheduler

newtype AccountMV = AMV (MVar Int)

newAccountMV :: Int -> IO AccountMV
newAccountMV n 
  | n >= 0 
  = liftM AMV (newMVar n)
  | otherwise
  = do putStrLn "Do I look like a communist?!!"
       liftM AMV (newMVar 0)

readMVar :: MVar a -> IO a
readMVar r = do x <- takeMVar r     -- read the value ...
                putMVar r x         -- ... also put the value back in!
                return x

showBalanceMV ::  AccountMV -> IO ()
showBalanceMV (AMV r) 
  = do bal <- readMVar r
       putStrLn $ "Current Balance: " ++ show bal

depositMV :: AccountMV -> Int -> IO ()
depositMV (AMV r) n
  = do bal <- takeMVar r            -- ALL other threads will now be blocked!
       pauseRandom                  -- No matter, you get right answer
       if (bal + n < 0) 
         then do putMVar r bal      -- Put the balance back in
                 putStrLn $ "Cannot withdraw, balance below " ++ show n 
         else putMVar r (bal + n)   -- Put the extra MONEY into the account 

main5 :: IO ()
main5 = do a <- newAccountMV 0
           mapM_ (forkIO . depositMV a) (replicate 5 10) 
           showBalanceMV a          -- should be $50, but why so slow?

-----------------------------------------------------------
-- | 6. Asynchrony Via MVars ------------------------------
-----------------------------------------------------------

{- Can implement "asynchronous" function calls (aka "futures", "promises") using MVars
   Other languages "features" are Haskell's, "functions"...
 -}
   
-- | A type representing an Asynchronous Computation
newtype Async a = Async (MVar a)

-- | Function to execute an IO action `async`-hronously 

async :: IO a -> IO (Async a)
async action = do m <- newEmptyMVar
                  forkIO (action >>= putMVar m)
                  return (Async m)

-- | Function to `wait` for the result of `Async` computation
wait :: Async a -> IO a
wait (Async m) = readMVar m

-- | Application: Download a bunch of URLs asynchronously,
-- that is, without blocking on each other

-- | A list of URLs

urls = [ "http://www.google.com"
       , "http://www.buzzfeed.com"
       , "http://www.reddit.com/r/haskell"
       , "http://www.nytimes.com"
       ]

-- | Reading a SINGLE URL

timeDownload url = do (page, time) <- timeit $ getURL url
                      printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

-- | Reading ALL the URLs in sequence

main6 = do (_, time) <- timeit $ mapM timeDownload urls
           printf "TOTAL download time: %.2fs\n" time

-- | Reading ALL the URLs with `async` 

main7 = do (_, time) <- timeit $ (mapM (async . timeDownload ) urls >>= mapM wait)
           printf "TOTAL download time: %.2fs\n" time


-- | Generalize into `asyncMapM`

asyncMapM :: (a -> IO b) -> [a] -> IO [b]
asyncMapM f xs = mapM (async . f) xs >>= mapM wait

-- | Reading ALL URLs with `asyncMapM`

main8 = do (_, time) <- timeit $ asyncMapM timeDownload urls
           printf "TOTAL download time: %.2fs\n" time


-----------------------------------------------------------
-- | 7. Lock/Synchronize Via MVars ------------------------
-----------------------------------------------------------

-- `synchronize` is NOT a keyword, JUST a function...

synchronize :: MVar b -> IO a -> IO a
synchronize lock action 
  = do x <- takeMVar lock
       z <- action
       putMVar lock x 
       return z
     
-- TODO: deposit with GLOBAL BANK lock


-- TODO: deposit with LOCAL ACCOUNT lock

-----------------------------------------------------------
-- | 8. Transfer ------------------------------------------
-----------------------------------------------------------

-- TODO: attempt with global lock
-- TODO: attempt with local lock
-- TODO: attempt ...
-- screwed 

-----------------------------------------------------------
-- | 9. STM -----------------------------------------------
-----------------------------------------------------------

-- TODO: deposit with STM

          
-----------------------------------------------------------
-- | Top-level Driver -------------------------------------
-----------------------------------------------------------

-- main = putStrLn "Hello world"

main       = getArgs >>= ( go . head )
  where 
    go "1" = main1
    go "2" = main2
    go "3" = main3
    go "4" = main4
    go "5" = main5
    go "6" = main6
    go "7" = main7
    go "8" = main8
    go cmd = putStrLn $ "Say what? " ++ cmd


----------------------------------------------------------

-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.

-- | Returns the number of realtime seconds an action takes 
-- https://github.com/simonmar/par-tutorial/blob/master/code/TimeIt.hs

timeit :: IO a -> IO (a,Double)
timeit io = do
     t0 <- getCurrentTime
     a <- io
     t1 <- getCurrentTime
     return (a, realToFrac (t1 `diffUTCTime` t0))

-- | Simple wrapper around HTTP, allowing proxy use
-- https://github.com/simonmar/par-tutorial/blob/master/code/GetURL.hs

getURL :: String -> IO B.ByteString
getURL url = do
  Network.Browser.browse $ do
    setCheckForProxy True
    setDebugLog Nothing
    setOutHandler (const (return ()))
    (_, rsp) <- request (getRequest' (escapeURIString isUnescapedInURI url))
    return (rspBody rsp)
  where
   getRequest' :: String -> Request B.ByteString
   getRequest' urlString =
    case parseURI urlString of
      Nothing -> error ("getRequest: Not a valid URL - " ++ urlString)
      Just u  -> mkRequest GET u

