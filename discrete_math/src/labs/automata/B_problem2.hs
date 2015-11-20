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
import qualified Data.Foldable as F
import Control.Monad
import Control.Applicative((<$>), (<*), (<|>))

fromLeft :: Either a b → b
fromLeft (Right b) = b

times :: Int → Parser a → Parser [a]
times 0 _ = return []
times 1 p = return <$> p
times n p = liftM2 (:) p (times (n - 1) p)

(**) = BS.append
--space = char ' '
space' = skipSpace <|> return ()
decimal' = decimal <* space'

processIO :: (Handle → Handle → IO()) → IO()
processIO handling = do
  input ← openFile "problem2.in" ReadMode
  output ← openFile "problem2.out" WriteMode
  handling input output
  hClose input
  hClose output

data Automaton = Automaton {
    isTerminal :: Int → Bool
  , aArray :: ! (Array (Int, Char) [Int])
  , aN :: ! Int
  , aM :: ! Int
  , aK :: ! Int
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
  return (str, Automaton (\x → S.member x accepting) states n m k)

ifAccepts :: S.Set Int → BS.ByteString → Automaton → Bool
ifAccepts s str a | BS.null str = F.any (isTerminal a) s
ifAccepts s _ _ | S.null s      = False
ifAccepts s str a =
  let c = BS.head str in
   ifAccepts
   (S.foldl
    (\set state → S.union set $ S.fromList ((aArray a) ! (state, c)))
    S.empty s)
   (BS.tail str) a

main :: IO ()
main = processIO $ \input output → do
  pre ← (parseOnly inputPattern <$> BS.hGetContents input)
  let (str, aut) = fromLeft $ pre
  hPutStrLn output $ if (ifAccepts (S.singleton 0) str aut) then "Accepts" else "Rejects"
