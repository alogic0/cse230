--Code from "Beautiful Concurrency" by Simon Peyton-Jones
--http://research.microsoft.com/en-us/um/people/simonpj/papers/stm/beautiful.pdf

module Main where
import Control.Concurrent
import Control.Concurrent.STM
import System.Random

type Account = TVar Int

transfer :: Account -> Account -> Int -> IO () 
transfer from to amount = atomically $ do 
  deposit  to	amount 
  withdraw from amount

withdraw :: Account -> Int -> STM () 
withdraw acc amount = do 
  bal <- readTVar acc 
  writeTVar acc (bal - amount)

deposit :: Account -> Int -> STM () 
deposit acc amount = withdraw acc (-amount)

limitedWithdraw :: Account -> Int -> STM () 
limitedWithdraw acc amount = do 
  bal <- readTVar acc 
  if amount > 0 && amount > bal
    then retry 
    else writeTVar acc (bal - amount)

{- This function is defined in Control.Concurrent.STM 
check :: Bool -> STM () 
check True = return () 
check False = retry
-}

limitedWithdraw1 :: Account -> Int -> STM () 
limitedWithdraw1 acc amount = do 
  bal <- readTVar acc 
  check (amount <= 0 || amount <= bal)
  writeTVar acc (bal - amount)

limitedWithdraw2 :: Account -> Account -> Int -> STM () 
-- (limitedWithdraw2 acc1 acc2 amt) withdraws amt from acc1, 
-- if acc1 has enough money, otherwise from acc2. 
-- If neither has enough, it retries. 
limitedWithdraw2 acc1 acc2 amt = 
  (limitedWithdraw acc1 amt) `orElse` (limitedWithdraw acc2 amt)

-- limitedWithdrawN generalizes limitedWithdraw2 to a list of accounts
limitedWithdrawN accs amt = error "TODO"





{- Section 4: The Santa Claus Problem -}
helper1 :: Group -> IO () -> IO () 
helper1 group do_task = do { (in_gate, out_gate) <- joinGroup group
                           ; passGate in_gate 
                           ; do_task
                           ; passGate out_gate }

meetInStudy :: Int -> IO ()
meetInStudy id = putStrLn("Elf "++ show id ++ " meeting in the study.")

deliverToys :: Int -> IO ()
deliverToys id = putStrLn ("Reindeer " ++ show id ++ " delivering toys.")

elf1, reindeer1 :: Group -> Int -> IO ()
elf1      gp id = helper1 gp (meetInStudy id)
reindeer1 gp id = helper1 gp (deliverToys id)

{- 
  A Gate has a fixed capacity, n, which we specify when we create the gate.
  It has a mutable remaining capacity, which is decremented whenever a helper
  calls passGate to go through the gate.  The gate starts out with 0 remaining
  capacity, so no helpers can pass through.  Santa enables the gate by calling
  operateGate, which sets the remaining capacity back to n.
-}
data Gate = MkGate Int (TVar Int)

newGate    :: Int -> STM Gate
newGate n = do { tv <- newTVar 0
               ; return (MkGate n tv)}

passGate   :: Gate -> IO ()
passGate (MkGate n tv) 
  = atomically (do { n_left <- readTVar tv
                   ; check (n_left > 0)
                   ; writeTVar tv (n_left -1) })

operateGate :: Gate -> IO ()
operateGate (MkGate n tv) 
  = do { atomically (writeTVar tv n)
       ; atomically (do { n_left <- readTVar tv
                        ; check (n_left == 0) })}

{-
  A group is created empty, with a specified capacity.
  An elf/reindeer may join a group by calling joinGroup, a call
  which blocks if the group is full.  Santa calls awaitGroup to wait
  for the group to be full.  When it is, he gets the Group's gates *and*
  the group is immediately re-initialized with fresh Gates so that another
  group of eager elves/reindeer can start assembling.
-}
data Group = MkGroup Int (TVar(Int, Gate, Gate))

newGroup   :: Int -> IO Group
newGroup n = 
  atomically ( do { g1 <- newGate n;  g2 <- newGate n
                  ; tv <- newTVar(n,g1,g2)
                  ; return (MkGroup n tv) } )

joinGroup  :: Group -> IO (Gate,Gate)
joinGroup (MkGroup n tv) =
  atomically (do { (n_left, g1, g2) <- readTVar tv
                 ; check (n_left > 0)
                 ; writeTVar tv (n_left -1, g1, g2)
                 ; return (g1,g2) } )

awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (MkGroup n tv) =
  do { (n_left, g1, g2) <- readTVar tv
     ; check (n_left == 0)
     ; new_g1 <- newGate n; new_g2 <- newGate n
     ; writeTVar tv (n, new_g1, new_g2)
     ; return (g1,g2) }

elf :: Group -> Int -> IO ThreadId
elf gp id = forkIO (forever (do {elf1 gp id; randomDelay }))

reindeer :: Group -> Int -> IO ThreadId
reindeer gp id = forkIO (forever (do {reindeer1 gp id; randomDelay }))

randomDelay :: IO ()
-- Delay for a random time between 1 and 1,000,000 microseconds
randomDelay = do { waitTime <- getStdRandom (randomR (1, 10000000))
                 ; threadDelay waitTime }

forever :: IO () -> IO ()
forever act = do { act; forever act }

choose :: [(STM a, a -> IO ())] -> IO ()
choose choices = do { act <- atomically (foldr1 orElse actions)
                    ; act }
  where
    actions :: [STM (IO ())]
    actions = [ do {val <- guard; return (rhs val)} 
              | (guard, rhs) <- choices ]
                  

santa_orig :: Group -> Group -> IO ()
santa_orig elf_gp rein_gp =
  do { putStrLn "----------"
     ; (task, (in_gate, out_gate)) 
         <- atomically( orElse
               (chooseGroup rein_gp "deliver toys")
               (chooseGroup elf_gp  "meet in my study"))
     ; putStrLn ("Ho! Ho! Ho! Let's " ++ task)
     ; operateGate in_gate
         -- Now the helpers do their task
     ; operateGate out_gate }
     where
       chooseGroup :: Group -> String -> STM(String, (Gate,Gate))
       chooseGroup gp task = do { gates <- awaitGroup gp
                                 ; return (task, gates) }

santa :: Group -> Group -> IO ()
santa elf_gp rein_gp 
  = do { putStrLn "----------"
       ; choose [(awaitGroup rein_gp, run "deliver toys."),
                 (awaitGroup elf_gp,  run "meet in my study.")]}
  where 
    run :: String -> (Gate,Gate) -> IO ()
    run task (in_gate, out_gate)
      = do { putStrLn ("Ho! Ho! Ho! Let's " ++ task)
           ; operateGate in_gate
           ; operateGate out_gate }

main = do { elf_group <- newGroup 3
          ; sequence_ [elf elf_group n | n <- [1..10] ]
 
          ; rein_group <- newGroup 9
          ; sequence_ [reindeer rein_group n | n <- [1..9] ]

          ; forever (santa elf_group rein_group) }
