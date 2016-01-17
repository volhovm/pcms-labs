{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures      #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Loops
import           Debug.Trace
import           Data.Attoparsec.ByteString.Char8  hiding (take)
import           Data.Array.IO
import           Data.Array.MArray
import           Data.Either                       (rights)
import           Data.List                         (sortOn, nubBy)
import qualified Data.Map.Strict                   as M
import qualified Data.HashSet                      as S
import           Prelude                           hiding (takeWhile,log)
import           System.Random
import           System.IO
import           System.Exit
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.Int
import           Data.List                         hiding (takeWhile)

-- Coeffs
----------------------------------------------------------------------

filename = "./artificialtests/test9"
-- start state, transaction, movement
logsOn = False
mutateArray = [0, 2, 4]
generationSize = 500        -- size of mutants
maxStepsTotal = 300         -- max steps number (not emulating more)
sndMutationPrb = 0.2        -- double mutant?
canMutatePart = 1           -- inf -- only elite mutates. 1 -- everybody
restartProb = 0.0008        -- probability of destroying everybody
eliteStrangers = 3          -- randoms, who mutate as elite. at least 1
eliteSize = 5
strangers = 30              -- strangers,
eliteDeathChance = 0.005
threadNum = 3
--fitnessC = 0.5

withProbability p m d = do
  let n :: Int
      n = ceiling $ 1 / p
  chance ← randomRIO (0,n)
  if chance == (n `div` 2)
  then m else d

-- Parsing
----------------------------------------------------------------------

spaces = many space
int = decimal <* spaces
double' = double <* spaces
cast = fromInteger . toInteger
--cast' = realToFrac
fromRight (Right a) = a
runParser p bs = fromDone $ parse p bs
                 where fromDone (Done s r) = (s, r)
                       fromDone (Partial f) = fromDone $ f ""
-- Array modifications
----------------------------------------------------------------------

modArray :: (MArray IOUArray a IO) ⇒
            (a → a) → IOUArray (Int,Int) a → (Int,Int) → IO ()
modArray op a i = do m ← readArray a i
                     writeArray a i (op m)

copyArray :: (MArray a e m, Ix i) ⇒ a i e → m (a i e)
copyArray = mapArray id

--printArray :: (Show e) ⇒ MArray IOUArray e IO → IO ()
printArray arr = do
  putStrLn ""
  ((0,0), (n,m)) ← getBounds arr
  forM_ [0..n] (\i → do
    forM_ [0..m] (\j → do
      putStr =<< show <$> readArray arr (i,j)
      putStr " ")
    putStrLn "")

-- IO
----------------------------------------------------------------------

processIO :: (Handle → Handle → IO ()) → IO()
processIO handling = do
  input ← openFile (filename ++ ".in") ReadMode
  output ← openFile (filename ++ ".out") WriteMode
  handling input output
  hClose input
  hClose output

-- field, m, n, k
readField :: Parser ([[Bool]], Int, Int, Int)
readField = do
  (m,n,k) ← (,,) <$> int <*> int <*> int
  list ← replicateM m (replicateM m readApple)
  return (list,m,n,k)
  where readApple = do c ← (char '*' <|> char '.') <* spaces
                       return $ if c == '*' then True else False

log s = if logsOn then putStrLn s else return ()


-- Algo
----------------------------------------------------------------------

data Apple = Apple Bool
instance Show Apple where
  show (Apple True) = "*"
  show (Apple False) = "."
type Field = IOArray (Int,Int) Apple

data Movement = L | R | M deriving (Show,Eq,Ord,Enum)
data Edge = Edge Movement Int deriving Eq
data EdgePair = NoEdges | EdgePair Edge Edge deriving Eq
instance Show Edge where
  show (Edge m t)  = show m ++ " " ++ show t
instance Show EdgePair where
  show (EdgePair e1 e2) = "<" ++ show e1 ++ ", " ++ show e2 ++ ">"
  show NoEdges = "<NoEdges>"
type AutArray = IOArray Int EdgePair
data Automaton = Automaton { autarr     :: AutArray
                           , startState :: Int
                           }

copyAutomaton :: Automaton → IO Automaton
copyAutomaton Automaton{..} = do
  autarr ← copyArray autarr
  return Automaton{..}

printAutomaton :: Automaton → Handle → IO ()
printAutomaton Automaton{..} output = do
  (0,n) ← getBounds autarr
  forM_ [0..n] (\i → do
    (EdgePair (Edge m1 t1) (Edge m2 t2)) ← readArray autarr i
    let str = show (t1 + 1) ++ " " ++
              show (t2 + 1) ++ " " ++
              show m1 ++ " " ++ show m2
    hPutStrLn output str)


normalizeAutomaton :: Automaton → IO Automaton
normalizeAutomaton Automaton{..} = do
  (0,n) ← getBounds autarr
  copy ← copyArray autarr
  forM_ [0..n] $ \i → do
    EdgePair (Edge m1 i1) (Edge m2 i2) ← readArray copy (i + startState)
    writeArray autarr i $ EdgePair (Edge m1 ((n + i1 + startState) `mod` n))
                                   (Edge m2 ((n + i2 + startState) `mod` n))
  return Automaton{startState = 0, ..}

mutate :: Automaton → IO Automaton
mutate aut@Automaton{..} = do
  ch ← randomRIO (0, 1000::Double)
  (0, n) ← getBounds autarr
  (randmv :: Movement) ← toEnum <$> randomRIO (0, 2)
  randb ← randomIO
  rand0 ← randomRIO (0, n)
  rand1 ← randomRIO (0, n)
  let s = realToFrac $ sum mutateArray
      el = 1000.0 / s
      vr :: Int → Double
      vr i = (sum (take (i+1) mutateArray)) * el
  case ch of
    -- random state
    a | a < (vr 0) → do return Automaton {startState = rand0, ..}
    -- random transaction
    a | a < (vr 1) → do (EdgePair e1@(Edge m1 _) e2@(Edge m2 _)) ←
                          readArray autarr rand0
                        writeArray autarr rand0 $
                          if randb
                          then EdgePair e1 (Edge m2 rand1)
                          else EdgePair (Edge m1 rand1) e2
                        return aut
    -- random modification of movement
    _              → do (EdgePair e1@(Edge _ d1) e2@(Edge _ d2)) ←
                          readArray autarr rand0
                        writeArray autarr rand0 $
                          if randb
                          then EdgePair e1 (Edge randmv d2)
                          else EdgePair (Edge randmv d1) e2
                        return aut

crossover :: Automaton → Automaton → IO (Automaton, Automaton)
crossover a1 a2 = do
  (0, n) ← getBounds $ autarr a1
  (o1 :: AutArray) ← newArray (0, n) NoEdges
  (o2 :: AutArray) ← newArray (0, n) NoEdges
  forM_ [0..n] $ \i → do
    e1 ← readArray (autarr a1) i
    e2 ← readArray (autarr a2) i
    randb ← randomIO
    writeArray o1 i $ if randb then e1 else e2
    writeArray o2 i $ if randb then e2 else e1
  return (Automaton { autarr = o1, startState = startState a1 },
          Automaton { autarr = o2, startState = startState a2 })

data Direction = DL | DR | DU | DD
turnLeft DL = DD
turnLeft DD = DR
turnLeft DR = DU
turnLeft DU = DL
turnRight DL = DU
turnRight DU = DR
turnRight DR = DD
turnRight DD = DL
data EmulateState = EmulateState { applesLeft :: Int
                                 , totalSteps :: Int
                                 , visited :: S.HashSet (Int,Int)
                                 , efield :: Field
                                 , aut :: Automaton
                                 , position :: (Int,Int)
                                 , direction :: Direction
                                 , state :: Int
                                 }

instance Show EmulateState where
  show EmulateState{..} = "apples left: " ++ show applesLeft ++ ", " ++
                          "total steps: " ++ show totalSteps ++ ", " ++
                          "states visited: " ++ show (S.size visited)

loopEm :: EmulateState → IO EmulateState
loopEm s@EmulateState{..} | applesLeft <= 0   = log "Nice" >> return s
loopEm s@EmulateState{..} | applesLeft < 0    = log "WTF!!" >> return s
loopEm s@EmulateState{..} | totalSteps >= maxStepsTotal = return s
loopEm EmulateState{..} = do
  let Automaton{..} = aut
  ((0,0),(m,_)) ← getBounds efield
  let m' = m + 1
      resolveDir DL (a,b) = (a, (m' + b - 1) `mod` m')
      resolveDir DR (a,b) = (a, (b + 1) `mod` m')
      resolveDir DU (a,b) = ((m' + a - 1) `mod` m', b)
      resolveDir DD (a,b) = ((a + 1) `mod` m', b)
      nextcell = resolveDir direction position
  (Apple isAp) ← readArray efield nextcell
  (EdgePair a b) ← readArray autarr state
  let (Edge move newstate) = if isAp then b else a
  when (isAp && move == M) $ writeArray efield nextcell $ Apple False
  loopEm EmulateState{ applesLeft = (if isAp && move == M
                                     then (\x → x-1) else id) applesLeft
                     , position = if move == M then nextcell else position
                     , direction = case move of
                                    M → direction
                                    L → turnLeft direction
                                    R → turnRight direction
                     , state = newstate
                     , totalSteps = totalSteps + 1
                     , visited = S.insert position visited
                     , ..
                     }

emulateExecution :: Field → Automaton → Int → Int →
                    IO (Double, EmulateState)
emulateExecution field' aut@Automaton{..} apples maxSteps = do
  field ← copyArray field'
  st@EmulateState{..} ←
    loopEm EmulateState { position = (0,0)
                        , totalSteps = 0
                        , direction = DR
                        , state = startState
                        , efield = field
                        , applesLeft = apples
                        , visited = S.empty
                        , ..
                        }
  return (100 / (0.00000000000002 + cast applesLeft +
           abs (cast (totalSteps - maxSteps)))
--            + abs (0.5 * (cast maxStepsTotal) - fitnessC * (cast $ S.size visited))
         , st)

randomAutomaton :: Int → IO Automaton
randomAutomaton statesN = do
  let n = statesN - 1
  (autarr :: AutArray) ← newArray (0, statesN-1) NoEdges
  startState ← randomRIO (0, n)
  forM_ [0..n] $ \i → do
    o1 ← randomRIO (0, n)
    o2 ← randomRIO (0, n)
    (m1 :: Movement) ← toEnum <$> randomRIO (0, 2)
    (m2 :: Movement) ← toEnum <$> randomRIO (0, 2)
    writeArray autarr i $ EdgePair (Edge m1 o1) (Edge m2 o2)
  return $ Automaton{..}

hasUnreachableEdges :: Automaton → IO Bool
hasUnreachableEdges Automaton{..} = do
  (0, n) ← getBounds autarr
  thisset ← bfs autarr 0 S.empty
  return $ (S.fromList [0..n]) /= thisset
  where bfs :: AutArray → Int → S.HashSet Int → IO (S.HashSet Int)
        bfs arr state used =
          if (state `S.member` used)
          then return used
          else do (EdgePair (Edge _ l) (Edge _ r)) ← readArray arr state
                  let newused = state `S.insert` used
                  S.union <$> (bfs arr l newused) <*> (bfs arr r newused)

data EvolState = EvolState { j :: Integer
                           , field :: Field
                           , maxStates :: Int
                           , maxSteps :: Int
                           , totalApples :: Int
                           , generation :: [Automaton]
                           , resultMV :: MVar Automaton
                           }

randomAutomatonWChecks :: Int → IO Automaton
randomAutomatonWChecks maxStates = do
  a ← randomAutomaton maxStates
  b ← hasUnreachableEdges a
  if b then randomAutomatonWChecks maxStates else return a

evolLoop :: EvolState → IO ()
evolLoop EvolState{..} = do
  solutionFound ← not <$> isEmptyMVar resultMV
  if solutionFound then return () else do
  log "Evolutioning!"
--  emulated ← rights <$> forkMapM
--    (\a → ((,) a) <$> emulateExecution field a totalApples maxSteps) generation
  emulated ← mapM
    (\a → ((,) a) <$> emulateExecution field a totalApples maxSteps)
    generation
--  emulated ← mapParM threadNum
--    (\a → ((,) a) <$> emulateExecution field a totalApples maxSteps)
--    generation
  log "Computed fitness"
  let species = reverse $ sortOn (fst . snd) emulated
      elite = if length species > eliteSize
              then take eliteSize $ map fst species
              else map fst species
      king = head elite
      kingStats = snd $ snd $ head species
  putStrLn $ show j ++ ". King stats: " ++ (show $ snd $ head species)
  log $ "Elite: " ++ (unlines $ map (show . snd) (take 10 species))
  log ""
--  threadDelay $ 20*100000
  if totalSteps kingStats <= maxSteps &&
     applesLeft kingStats == 0
  then do _ ← tryPutMVar resultMV king
          return ()
  else do
    log "Creating new generation"
    let elite' = nubBy (\a b → autarr a == autarr b) elite
    elite ← withProbability eliteDeathChance
               -- boom, a comet crushed!
              (do putStrLn "KILL THE ELITE!!!"
                  return $ (drop (eliteSize `div` 2) elite'))
              (return elite')
    elite ← do strgrs ← replicateM eliteStrangers $
                 randomAutomatonWChecks maxStates
               return $ strgrs ++ elite
    generation ← iterateUntilM
      ((> generationSize) . length)
      (\gen → do let proportion = min (length gen - 1) $
                       2 + (length gen - 1) `div` canMutatePart
                 rdm1 ← randomRIO (0, proportion)
                 rdm0 ← randomIO
                 if rdm0 then do
                   (mutated :: Automaton) ←
                     mutate =<< (copyAutomaton $ gen !! rdm1)
                   mutated ← withProbability sndMutationPrb
                     (mutate mutated) (return mutated)
                   return $ gen ++ [mutated]
                 else do
                   rdm2 ← iterateUntil ((/=) rdm1) $
                            randomRIO (0, (proportion))
                   (ch1, ch2) ← crossover (gen !! rdm1) (gen !! rdm2)
                   return $ gen ++ [ch1, ch2])
      elite
    generation ← withProbability 0.5 (do strgrs ← replicateM strangers $
                                           randomAutomatonWChecks maxStates
                                         return $ strgrs ++ generation)
                                     (return generation)
    withProbability restartProb
      -- One more time
      (do putStrLn "Meh, let's begin from the end"
          emulateEvolution EvolState{..})
      (evolLoop EvolState{ j = j + 1
                         , ..})

emulateEvolution :: EvolState → IO ()
emulateEvolution EvolState{..} = do
  log "Creating!"
  let   -- initial generation
  generation ← replicateM generationSize $ randomAutomatonWChecks maxStates
  log "Initial generation created, come on!"
  evolLoop EvolState{..}
  where

main :: IO ()
main = processIO $ \input output → do
  -- Field input
  str0 ← BS.hGetContents input
  let (_, (fieldList,m,n,k)) = runParser readField str0
  let totalApples = sum $ map (\b → if b then 1::Int else 0) $ concat fieldList
  (field :: Field) ← newListArray ((0,0),(m-1,m-1)) $ map Apple $ concat fieldList
  printArray field
  log $ "Apples left: " ++ show totalApples

--  s2 ← sol2
--  res ← emulateExecution field s2 totalApples k
--  putStrLn $ show res

-- It's OK not to initialize generation here
  resultMV ← newEmptyMVar
  replicateM_ threadNum $ forkIO $ emulateEvolution $
     EvolState { maxStates = n
               , maxSteps = k
               , j = 0
               , ..}
  result ← readMVar resultMV
-- void $ normalizeAutomaton result
  putStrLn "Succeeded"
  printAutomaton result output


--  auts ← replicateM 50 $ randomAutomatonWChecks n
--  forM_ auts ((flip printAutomaton) stdout)


--mapParM :: Int → (a → IO b) → [a] → IO [b]
--mapParM 0 _ _   = return []
--mapParM n foo l = do
--  res ← newEmptyMVar
--  let (xs1,xs2) = splitAt (length l `div` n) l
--  forkIO $ void $ mapM foo xs1 >>= putMVar res
--  xs1' ← takeMVar res
--  xs2' ← mapParM (pred n) foo xs2
--  return $ xs1' ++ xs2'

--allWrites :: Int → [Int → AutArray → IO ()]
--allWrites statesN = do
--  o1 ← [(0::Int)..statesN-1]
--  o2 ← [(0::Int)..statesN-1]
--  m1 ← [L, R, M]
--  m2 ← [L, R, M]
--  return $ \row autarr → writeArray autarr row $
--             EdgePair (Edge m1 o1) (Edge m2 o2)
--
--allAutomatons :: Int → [IO Automaton]
--allAutomatons statesN = do
--  writes ← sublists statesN (allWrites statesN)
--  startState ← [0..statesN-1]
--  return $ do
--    (autarr :: AutArray) ← newArray (0, statesN-1) NoEdges
--    forM_ [0..statesN-1] (\n → (writes !! n) n autarr)
--    return Automaton{..}
--  where sublists :: Int → [a] → [[a]]
--        sublists k list = [(list !! i):xs | i ← [0..(length list - 1)],
--                                            xs ← sublists (k-1) (withoutelem i list)]
--        withoutelem i list = let (_:xs, xs') = splitAt (i+1) list in xs ++ xs'
--
--sol2 :: IO Automaton
--sol2 = do
--  (autarr :: AutArray) ← newArray (0, 0) NoEdges
--  writeArray autarr 0 $ EdgePair (Edge R 0) (Edge M 0)
--  return $ Automaton{startState = 0,..}
