
import Control.Concurrent
import Control.Monad
import System.IO


-----------------------------------------------------------
-- | 0. Mutable State via IORef ---------------------------
-----------------------------------------------------------

main0 = do r <- newIORef 0 
           incR r
           s <- readIORef r 
           print s  
          
incR  :: IORef Int -> IO () 
incR  = do v <- readIORef r 	        
          writeIORef r (v+1)

-----------------------------------------------------------
-- | 1. Forking A Thread ----------------------------------
-----------------------------------------------------------

main1 = do hSetBuffering stdout NoBuffering
           forkIO $ forever (putChar 'A')
           forkIO $ forever (putChar 'B')
           threadDelay (10^6)

-----------------------------------------------------------
-- | 2. Sharing Memory Via IORef --------------------------
-----------------------------------------------------------

newtype Account1 = A1 (IORef Int)

newAccount1 n 
  | n > 0 
  = liftM A1 (newIORef n)
  | otherwise
  = do putStrLn "Do I look like a communist?!!"
       liftM A1 (newIORef 0)

showBalance1 (A1 r) 
  = do bal <- readIORef r
       putStrLn $ "Current Balance: " ++ show bal

deposit1 (A1 r) n
  = do bal <- readIORef r
       if (bal + n < 0) 
         then putStrLn $ "Sorry, cannot withdraw. Balance below " ++ show n 
         else writeIORef r (bal + n)

main2 = do a <- newAccount1
           forM_ (replicate 50 10) $ deposit1 a
           showBalance1 a   -- should be $500

-----------------------------------------------------------
-- | 3. Data Races due to sharing -------------------------
-----------------------------------------------------------

-----------------------------------------------------------
-- | 4. MVars ---------------------------------------------
-----------------------------------------------------------

-----------------------------------------------------------
-- | 5. Synchronizing Via MVars ---------------------------
-----------------------------------------------------------

synchronize :: MVar b -> IO a -> IO a
synchronize lock act 
  = do x <- takeMVar lock
       z <- act
       putMVar lock x 
       return z
      


-----------------------------------------------------------
-- | 6. Asynchrony Via MVars ------------------------------
-----------------------------------------------------------






          
-----------------------------------------------------------
-- | Top-level Driver -------------------------------------
-----------------------------------------------------------

main       = liftM (runChoice . head) getArgs  
  where 
    ch "0" = main0
    ch "1" = main1
    ch "2" = main1
    ch "3" = main1
    ch "4" = main1
    ch "5" = main1

