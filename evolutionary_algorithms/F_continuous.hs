{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Debug.Trace
import           Data.Attoparsec.ByteString.Char8
import           Data.Array.IO
import           Data.Array.MArray
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List                         hiding (takeWhile)
import           Data.Int
import qualified Data.Map.Strict                   as M
import           Prelude                           hiding (takeWhile)
import           System.IO

filename = "continuous"

times :: Int → Parser a → Parser [a]
times 0 _ = return []
times 1 p = return <$> p
times n p = liftM2 (:) p (times (n - 1) p)

spaces = many space
int :: Parser Int
double' :: Parser Double
int = decimal <* spaces
double' = double <* spaces

cast = fromInteger . toInteger

runParser p bs = fromDone $ parse p bs
                 where fromDone (Done s r) = (s, r)

type Matrix = IOUArray (Int,Int) Double
type Automaton = IOUArray (Int,Int) Int

processIO :: (Handle → Handle → IO ()) → IO()
processIO handling = do
  input ← openFile (filename ++ ".in") ReadMode
  output ← openFile (filename ++ ".out") WriteMode
  handling input output
  hClose input
  hClose output

type LocalState = Either BS.ByteString BS.ByteString
loopIO :: BS.ByteString → (BS.ByteString → IO LocalState) → IO BS.ByteString
loopIO value cont =
  cont value >>= either return
                        (\bs → loopIO bs cont)

modArray op a i e = do m ← readArray a i
                       writeArray a i (m `op` e)
addArray = modArray (+)
subArray = modArray (-)
divArray = modArray (/)

swapRows arr i j = do
  --putStrLn $ "Swapping rows " ++ show i ++ " " ++ show j
  (_,(_,n)) ← getBounds arr
  forM_ [0..n] (\k → do a ← readArray arr (i,k)
                        b ← readArray arr (j,k)
                        writeArray arr (j,k) a
                        writeArray arr (i,k) b)

normalizeRow :: Matrix → Int → IO Bool
normalizeRow arr i = do
  (_,(n,m)) ← getBounds arr
  piv ← readArray arr (i, i)
  --putStrLn $ "Pivot: " ++ show piv
  if piv == 0
    then do notNullRow ← foldM (\j row → do
                                val ← readArray arr (row, i)
                                if j == i && val /= 0
                                then return row
                                else return j)
                             i [i..n]
            if notNullRow == i
            then return False
            else do swapRows arr i notNullRow
                    forM_ [i..m] (\j → divArray arr (i, j) piv)
                    return True
    else do forM_ [i..m] (\j → divArray arr (i, j) piv)
            return True

solveMatrix :: Matrix → IO ()
solveMatrix arr = do
  ((0,0),(n,m)) ← getBounds arr
  when (m /= n + 1) $ putStrLn "This matrix won't be solved correctly!!!"
  forM_ [0..n] (\pivotrow → do
    res ← normalizeRow arr pivotrow -- divide it on pivot and swap if needed
    --putStrLn $ "Normalized row " ++ show pivotrow
    if not res then return ()
    else do forM_ [pivotrow+1..n] (\row → do
              curpivot ← readArray arr (row, pivotrow)
              forM_ [pivotrow..m] (\col → do
                partner ← readArray arr (pivotrow, col)
                temp ← readArray arr (row,col)
                writeArray arr (row,col) (temp - partner * curpivot))))
  backLoop arr M.empty n
  where backLoop :: Matrix → M.Map Int Double → Int → IO ()
        backLoop _ _ (-1) = return ()
        backLoop arr mp row = do
          --putStrLn $ "Backloop: " ++ show row
          ((0,0),(n,m)) ← getBounds arr
          vl ← readArray arr (row,row)
          if vl == 0
          then do forM_ [row+1..n] (\j → writeArray arr (row,j) 0)
                  backLoop arr (M.insert row 0 mp) (row-1)
          else do forM_ [row+1..n] (\j → do tmp ← readArray arr (row,j)
                                            writeArray arr (row,j) 0
                                            subArray arr (row,m) (tmp * mp M.! j))
                  lastEl ← readArray arr (row,m)
                  backLoop arr (M.insert row lastEl mp) (row-1)

--printArray :: (Show e) ⇒ MArray IOUArray e IO → IO ()
printArray arr = do
  putStrLn ""
  ((0,0), (n,m)) ← getBounds arr
  forM_ [0..n] (\i → do
    forM_ [0..m] (\j → do
      putStr =<< show <$> readArray arr (i,j)
      putStr " ")
    putStrLn "")

data EmulateState =
  EmulateState { automaton :: Automaton
               , state :: Int
               , path :: [Char]
               , outs :: [Double]
               , trace :: M.Map (Int,Int) Int
               , coeff :: Double
               , matrix :: Matrix
               }

-- return the count of φ{ij}
emulate :: EmulateState → IO ()
emulate EmulateState{..} | null path = return ()
emulate EmulateState{..} = do
  let
    index = (state, if head path == '0' then 0 else 1)
    count = M.lookup index trace
    newset = Data.List.nub $ index : M.keys trace
    newtrace :: M.Map (Int,Int) Int
    newtrace = maybe (M.insert index 1 trace)
                     (\_ → M.insertWith (+) index 1 trace)
                     count
    conv (i, j) = 2*i + j
  newstate ← readArray automaton index
  (_,(_,n2)) ← getBounds matrix
  forM_ newset (\ind → do
    forM_ (M.assocs newtrace) (\(ind2, indcount) → do
      addArray matrix (conv ind, conv ind2) $ coeff * cast indcount)
    addArray matrix (conv ind, n2) $ coeff * head outs)
  emulate $ EmulateState { state = newstate
                         , path = tail path
                         , outs = tail outs
                         , trace = newtrace
                         , ..
                         }

main :: IO ()
main = processIO $ \input output → do
  let fromRight (Right a) = a
  str0 ← BS.hGetContents input
  let (str1, n) = runParser int str0
      (str2, m) = runParser int str1
  (automata :: Automaton) ← newArray ((0, 0), (n - 1, 1)) 0
  (solution :: Matrix) ← newArray ((0, 0), (2*n - 1, 2*n)) 0
  let (str3, autlist) = runParser (n `times` ((,) <$> int <*> int)) str2
  forM_ ([0..n-1] `zip` autlist) (\(i, (a, b)) →
                                   do writeArray automata (i, 0) (a-1)
                                      writeArray automata (i, 1) (b-1))
  _ ← loopIO str3 (fillMatrix automata solution)
--  printArray automata
--  printArray solution
--  putStrLn "------------------------SOLVING MATRIX-----------------------_"
  solveMatrix solution
 -- printArray solution
  forM_ [0..n-1] (\j → do e1 ← readArray solution (2*j,   2*n)
                          e2 ← readArray solution (2*j+1, 2*n)
                          hPutStrLn output $ show e1 ++ " " ++ show e2
                          putStrLn $ show e1 ++ " " ++ show e2)
  where fillMatrix aut sol str =
          case feed (parse parseTest str) "" of
            Done str' (len, ins, outs) → do
              --putStrLn "emulating.."
              emulate EmulateState { automaton = aut
                                   , state = 0
                                   , path = BS.unpack ins
                                   , matrix = sol
                                   , trace = M.empty
                                   , coeff = 2.0 / (cast len)
                                   , ..
                                   }
              --printArray sol
              return $ Right str'
            a → do --putStrLn $ "end emulating :: " ++ show a
                   return $ Left ""
        parseTest = do
          len ← int
          ins ← takeWhile (inClass "01") <* spaces
          outs ← len `times` double' <* spaces
          return (len, ins, outs)
