{-# LANGUAGE UnicodeSyntax, ImplicitPrelude, OverloadedStrings #-}
module Main where

-- OK

import Prelude hiding (takeWhile, (**))
import System.IO
import Debug.Trace (trace)
import qualified Data.ByteString.Char8 as BS
import Data.Array
import Data.Attoparsec.ByteString.Char8
import qualified Data.Set as S
import Data.List (sortBy)
import Control.Monad
import Control.Applicative((<$>), (<*), (<|>))

fromLeft :: Either a b → b
fromLeft (Right b) = b

times :: Int → Parser a → Parser [a]
times 0 p = return []
times 1 p = return <$> p
times n p = liftM2 (:) p (times (n - 1) p)

(**) = BS.append
--space = char ' '
space' = skipSpace <|> return ()
decimal' = decimal <* space'

processIO :: (Handle → Handle → IO()) → IO()
processIO handling = do
  input ← openFile "problem1.in" ReadMode
  output ← openFile "problem1.out" WriteMode
  handling input output
  hClose input
  hClose output

data Automaton = Automaton {
    isTerminal :: Int → Bool
  , aArray :: (Array (Int, Char) [Int])
  , aN :: Int
  , aM :: Int
  , aK :: Int
  }

instance Show Automaton where
  show (Automaton i a n m k) = show (map i [0..n-1]) ++ show a ++ " " ++
                               show n ++ " " ++ show m ++ " " ++ show k

nnc :: Parser ((Int, Char), Int)
nnc = liftM3 (\a b c → ((a - 1, c), b - 1)) decimal' decimal' (letter_ascii <* space')

inputPattern :: Parser (BS.ByteString, Automaton)
inputPattern = do
  str ← takeWhile isAlpha_ascii <* space'
  n ← decimal'
  m ← decimal'
  k ← decimal'
  accepting ← S.fromList <$> map (\x → x-1) <$> k `times` decimal'
  states ← accumArray (\e a -> a:e) [] ((0,'a'), (n-1,'z')) <$> m `times` nnc
  return (str, Automaton
                (\x → S.member x accepting)
                states
                n m k)

ifAcceptsGen :: Int → BS.ByteString → Automaton → Bool
ifAcceptsGen n str a | BS.length str == 0 =
                         (isTerminal a) n
ifAcceptsGen n str a =
  let vertices = ((aArray a) ! (n, BS.head str)) in
   if (null vertices) then False else
     foldl1 (||) (map (\v → ifAcceptsGen v (BS.tail str) a) vertices)

ifAccepts :: BS.ByteString → Automaton → Bool
ifAccepts = ifAcceptsGen 0

main :: IO ()
main = processIO $ \input output → do
  pre ← (parseOnly inputPattern <$> BS.hGetContents input)
  let (str, aut) = fromLeft $ pre
  BS.hPutStrLn output $ if (ifAccepts str aut) then "Accepts" else "Rejects"
