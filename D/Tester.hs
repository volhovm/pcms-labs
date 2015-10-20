-- This one is written by Mukhutdinov Dima
module Main where

import System.Environment
import System.IO
import Data.List
import Control.Monad

merge [] ys = ys
merge (x:xs) ys = x : merge ys xs

shft, negp :: (Num a, Num b) => (a, b) -> (a, b)
negp (a, b) = (-a, b)
shft (a, b) = (a - 200000, b + 1000)

generatePolygon :: Int -> [(Int, Int)]
generatePolygon n
    | n `mod` 2 == 1 = (0, -1000000) : generatePolygon (n - 1)
    | otherwise = half ++ reverse (map negp half)
    where half = merge ups (map shft ups)
          ups = map (\k -> (500000, k)) [0, 1000..(n `div` 4 - 1) * 1000]

main = do
  [num, filename] <- getArgs
  file <- openFile filename WriteMode
  let n = read num :: Int
      p = generatePolygon n
  hPutStrLn file num
  forM_ p $ \(x, y) -> hPutStrLn file $ show x ++ " " ++ show y
  hClose file
