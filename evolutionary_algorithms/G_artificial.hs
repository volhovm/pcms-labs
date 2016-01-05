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
import           Debug.Trace
import           Data.Attoparsec.ByteString.Char8
import           Data.Array.IO
import           Data.Array.MArray
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.Int
import           Data.List                         hiding (takeWhile)
import qualified Data.Map.Strict                   as M
import qualified Data.HashSet                      as S
import           Prelude                           hiding (takeWhile)
import           System.Random
import           System.IO
import           System.Exit

filename = "./artificialtests/test3"

spaces = many space
int = decimal <* spaces
double' = double <* spaces
cast = fromInteger . toInteger
fromRight (Right a) = a
runParser p bs = fromDone $ parse p bs
                 where fromDone (Done s r) = (s, r)
                       fromDone (Partial f) = fromDone $ f ""

----------------------------------------------------------------------

data Apple = Apple Bool
instance Show Apple where
  show (Apple True) = "*"
  show (Apple False) = "."
type Field = IOArray (Int,Int) Apple

data Movement = L | R | M deriving (Show,Eq,Ord)
data Edge = Edge Movement Int deriving Eq
data EdgePair = NoEdges | EdgePair Edge Edge deriving Eq
instance Show Edge where
  show (Edge m t)  = show m ++ " " ++ show t
instance Show EdgePair where
  show (EdgePair e1 e2) = "<" ++ show e1 ++ ", " ++ show e2 ++ ">"
type Automaton = IOArray Int EdgePair

modArray :: (MArray IOUArray a IO) ⇒
            (a → a) → IOUArray (Int,Int) a → (Int,Int) → IO ()
modArray op a i = do m ← readArray a i
                     writeArray a i (op m)

--printArray :: (Show e) ⇒ MArray IOUArray e IO → IO ()
printArray arr = do
  putStrLn ""
  ((0,0), (n,m)) ← getBounds arr
  forM_ [0..n] (\i → do
    forM_ [0..m] (\j → do
      putStr =<< show <$> readArray arr (i,j)
      putStr " ")
    putStrLn "")

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
                                 , field :: Field
                                 , aut :: Automaton
                                 , position :: (Int,Int)
                                 , direction :: Direction
                                 , state :: Int
                                 , totalSteps :: Int
                                 }
loopEm EmulateState{..} | applesLeft <= 0   = return totalSteps
loopEm EmulateState{..} | totalSteps >= 300 = return totalSteps
loopEm EmulateState{..} = do
--  putStrLn $ "------ STEP " ++ show totalSteps
--  putStrLn $ "position: " ++ show position
--  printArray field
  ((0,0),(m,_)) ← getBounds field
  let m' = m + 1
      resolveDir DL (a,b) = (a, (m' + b - 1) `mod` m')
      resolveDir DR (a,b) = (a, (b + 1) `mod` m')
      resolveDir DU (a,b) = ((m' + a - 1) `mod` m', b)
      resolveDir DD (a,b) = ((a + 1) `mod` m', b)
      nextcell = resolveDir direction position
--  putStrLn $ "nextcell: " ++ show nextcell
  (Apple isAp) ← readArray field nextcell
  (EdgePair a b) ← readArray aut state
  let e@(Edge move newstate) = if isAp then b else a
--  putStrLn $ "Edge : " ++ show e
  when (isAp && move == M) $ writeArray field nextcell $ Apple False
  loopEm EmulateState{ applesLeft = (if isAp && move == M
                                     then (\x → x-1) else id) applesLeft
                     , position = if move == M then nextcell else position
                     , direction = case move of
                                    M → direction
                                    L → turnLeft direction
                                    R → turnRight direction
                     , state = newstate
                     , totalSteps = totalSteps + 1
                     , ..
                     }

emulateExecution :: Field → Automaton → Int → IO Int
emulateExecution field aut applesLeft = do
  loopEm EmulateState { position = (0,0)
                      , state = 0
                      , totalSteps = 0
                      , direction = DR
                      , ..
                      }

processIO :: (Handle → Handle → IO ()) → IO()
processIO handling = do
  input ← openFile (filename ++ ".in") ReadMode
  output ← openFile (filename ++ ".out") WriteMode
  handling input output
  hClose input
  hClose output

-- field, steps, states
readField :: Parser ([[Bool]], Int, Int, Int)
readField = do
  (m,n,k) ← (,,) <$> int <*> int <*> int
  list ← replicateM m (replicateM m readApple)
  return (list,m,n,k)
  where readApple = do c ← (char '*' <|> char '.') <* spaces
                       return $ if c == '*' then True else False

randomAutomaton :: Int → IO Automaton
randomAutomaton statesN = do
  (aut :: Automaton) ← newArray (0, statesN-1) NoEdges
  forM_ [0..statesN-1] (\n → do
    o1 ← randomRIO (0, statesN-1)
    o2 ← randomRIO (0, statesN-1)
    (m1 :: Int) ← randomRIO (0, 2)
    (m2 :: Int) ← randomRIO (0, 2)
    writeArray aut n $ EdgePair (Edge (formatDir m1) o1)
                                (Edge (formatDir m2) o2))
  return aut
  where formatDir 0 = L
        formatDir 1 = R
        formatDir _ = M

allWrites :: Int → [Int → Automaton → IO ()]
allWrites statesN = do
  o1 ← [(0::Int)..statesN-1]
  o2 ← [(0::Int)..statesN-1]
  m1 ← [L, R, M]
  m2 ← [L, R, M]
  return $ \row aut → writeArray aut row $ EdgePair (Edge m1 o1) (Edge m2 o2)

allAutomatons :: Int → [IO Automaton]
allAutomatons statesN = do
  writes ← sublists statesN (allWrites statesN)
  return $ do
    (aut :: Automaton) ← newArray (0, statesN-1) NoEdges
    forM_ [0..statesN-1] (\n → (writes !! n) n aut)
    return aut
  where sublists :: Int → [a] → [[a]]
        sublists k list = [(list !! i):xs | i ← [0..(length list - 1)],
                                            xs ← sublists (k-1) (withoutelem i list)]
        withoutelem i list = let (_:xs, xs') = splitAt (i+1) list in xs ++ xs'

hasUnreachableEdges :: Automaton → IO Bool
hasUnreachableEdges aut = do
  (0, n) ← getBounds aut
  thisset ← bfs aut 0 S.empty
  return $ (S.fromList [0..n]) == thisset
  where bfs :: Automaton → Int → S.HashSet Int → IO (S.HashSet Int)
        bfs aut state used = if (state `S.member` used) then return used
                         else do (EdgePair (Edge _ l) (Edge _ r)) ← readArray aut state
                                 let newused = state `S.insert` used
                                 S.union <$> (bfs aut l newused) <*> (bfs aut r newused)

sol2 :: IO Automaton
sol2 = do
  (aut :: Automaton) ← newArray (0, 0) NoEdges
  writeArray aut 0 $ EdgePair (Edge R 0) (Edge M 0)
  return aut

main :: IO ()
main = processIO $ \input output → do
  str0 ← BS.hGetContents input
  let (str1, (fieldList,m,n,k)) = runParser readField str0
  let totalApples = sum $ map (\b → if b then 1 else 0) $ concat fieldList
  (field :: Field) ← newListArray ((0,0),(m-1,m-1)) $ map Apple $ concat fieldList
  printArray field
  putStrLn $ "Apples left: " ++ show totalApples

  allAuts ← newMVar $ allAutomatons n
  bestResult ← newMVar 300
  isExit ← newMVar False

  -- lol, a whole raft of args
  forM_ [0..2] (\s → forkIO $ loopFind 0 n m totalApples k
                     output fieldList bestResult isExit allAuts)
  loopFind 0 n m totalApples k output fieldList bestResult isExit allAuts
  where
    loopFind j n m totalApples k output fieldList bestResult isExit allAuts =
          do ex ← readMVar isExit
             when (ex) exitSuccess

             allauts ← readMVar allAuts
             when (null allauts) exitFailure
             autprod ← modifyMVar allAuts (\(a:as) → do
                                            putStrLn $ "Length: " ++ (show $ length allauts)
                                            return (as,a))
             aut ← autprod
             isBad ← hasUnreachableEdges aut
             when isBad (loopFind j n m totalApples k output fieldList bestResult isExit allAuts)

             field ← newListArray ((0,0),(m-1,m-1)) $ map Apple $ concat fieldList
             res ← emulateExecution field aut totalApples
             putStrLn $ "Sad, res: " ++ show res
             curmin ← readMVar bestResult
             when (res < curmin) $ putStrLn $ "Step " ++ show j ++ ": " ++ show res
             if (res < curmin && res <= k)
             then do when (res <= k) (do
                     putStrLn "Success"
                     forM_ [0..n-1]
                       (\i → do (EdgePair (Edge m1 t1) (Edge m2 t2)) ←
                                  readArray aut i
                                let str = show (t1 + 1) ++ " " ++
                                          show (t2 + 1) ++ " " ++
                                          show m1 ++ " " ++ show m2
                                hPutStrLn output str
                                putStrLn str))
                     hClose output
                     modifyMVar isExit (\_ → return (True,()))
                     exitSuccess
             else do modifyMVar bestResult
                      (\curmin → return (if res < curmin then res else curmin, ()))
                     loopFind (j + 1) n m totalApples k output fieldList bestResult isExit allAuts
