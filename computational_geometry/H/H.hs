{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Array
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List
import           Data.Int
import           Debug.Trace
import           Prelude
import           System.IO

filename = "inside"

newtype StateT s m a = StateT { runState :: s → m (s, a) }
type Parser = StateT BS.ByteString (Either String)

instance (Functor m) ⇒ Functor (StateT s m) where
  fmap foo state = StateT $ \s → (\(a, b) → (a, foo b)) <$> (runState state) s

instance (Functor m, Monad m) ⇒ Applicative (StateT s m) where
  pure = return
  (<*>) = ap

instance (Monad m) ⇒ Monad (StateT s m) where
  return a = StateT $ \s → return (s, a)
  a >>= b  = StateT $ \s → let m1 = (runState a) s in
                           m1 >>= (\(s1, a1) → runState (b a1) s1)

int :: Parser Int64
int = StateT $ \s → case BS.readInt s of
  Nothing → Left "Couldn't parse int"
  Just (i', s')  → Right (s', cast i')

(<~>) :: (Char → Bool) → (Char → Bool) → (Char → Bool)
(<~>) foo1 foo2 c = foo1 c || foo2 c

parsepred :: (Char → Bool) → Parser ()
parsepred pred =  StateT $ \s → Right (BS.dropWhile pred s, ())
spaces = parsepred isSpace
wspaces = parsepred (\s → s `elem` (" " :: String))
skipchar = StateT $ \s → Right (BS.drop 1 s, ())

times :: Int → Parser a → Parser [a]
times 0 _ = return []
times 1 p = return <$> p
times n p = liftM2 (:) p (times (n - 1) p)

-------------------------

data Point = Point { x :: !Int64, y :: !Int64 }
             deriving (Eq, Ord)

instance Show Point where
  show a = show (x a) ++ " " ++ show (y a)

len :: Point → Int64
len (Point x y) = x * x + y * y

(~-) :: Point → Point → Point
(~-) a b = Point (x a - x b) (y a - y b)

-- turn a b c -- turn ab, ac
turn :: Point → Point → Point → Int64
turn p a b = ((x a - x p) * (y b - y p)) -
             ((y a - y p) * (x b - x p))

--------------------------

data Answer = INSIDE | BORDER | OUTSIDE
                                deriving Show
type Points = Array Int Point

cast :: (Num a, Integral a, Num b) ⇒ a → b
cast = fromInteger . toInteger

parsePoint :: Parser Point
parsePoint = Point <$> (int <* skipchar) <*> (int <* skipchar)

parseInput :: Parser (Int, Points)
parseInput = do
  n ← int <* spaces
  points@(a:b:c:_) ← (cast n) `times` parsePoint
  _ ← int <* spaces -- skip m, we don't need it
  let points' = if turn a b c >= 0 then points else reverse points
  return $ (cast n, listArray (0, cast $ (length points') - 1) points')

mainLoop :: BS.ByteString → (BS.ByteString → IO BS.ByteString) → IO ()
mainLoop s foo = do
  s' ← foo s
  if BS.null s'
    then return ()
    else mainLoop s' foo

d2 a b = (a + b) `div` 2

binSearch :: Int → Int → Int →                -- from, to, median
             Points → Point → Point → Point → -- points, anchor, array median, target
             (Int, Int)                       -- result
binSearch from med to arr a arrmed p
  | to - from == 1            = (from, to)
  | turn a arrmed p > 0  = let med' = (d2 med to) in
                            binSearch med  med' to  arr a (arr ! med')  p
  | turn a arrmed p < 0  = let med' = (d2 from med) in
                            binSearch from med' med arr a (arr ! med') p
  | turn a arrmed p == 0 = if med == (cast $ (length (indices arr) - 1))
                           then (med, med-1)
                           else (med, med+1)

decideSector :: Point → Point → Point → Answer
decideSector b c p | turn b c p == 0 = BORDER
                   | turn b c p >  0 = INSIDE
decideSector _ _ _                   = OUTSIDE

resolveBorder v1 v2 = if len v1 > len v2 then OUTSIDE else BORDER

getResolution :: Int → Points → Point → Answer
getResolution n arr p
  | p == (arr ! 0)                      = BORDER
  | turn (arr ! 0) (arr ! 1) p < 0 ||
    turn (arr ! 0) (arr ! (n-1)) p > 0  = OUTSIDE
  | turn (arr ! 0) (arr ! 1) p == 0     =
    let v1 = p ~- (arr ! 0)
        v2 = (arr ! 1) ~- (arr ! 0)
        in resolveBorder v1 v2
  | turn (arr ! 0) (arr ! (n-1)) p == 0 =
    let v1 = p ~- (arr ! 0)
        v2 = (arr ! (n-1)) ~- (arr ! 0)
        in resolveBorder v1 v2
  | otherwise =
      let o@(b, c) = binSearch 0 (n `div` 2) (n-1) arr (arr ! 0) (arr ! (n `div` 2)) p in
      decideSector (arr ! b) (arr ! c) p

processIO :: (Handle → Handle → IO ()) → IO()
processIO handling = do
  input ← openFile (filename ++ ".in") ReadMode
  output ← openFile (filename ++ ".out") WriteMode
  handling input output
  hClose input
  hClose output

main :: IO ()
main = processIO $ \input output → do
  let fromRight (Right a) = a
  content ← BS.hGetContents input
  let (pointsStr, (n, pointsArr)) = fromRight $ (runState parseInput) content
  mainLoop pointsStr
    (\curstr → do
        let (curstr', point) = fromRight $ (runState parsePoint) curstr
            res = getResolution n pointsArr point
        hPutStrLn output $ show res
        --putStrLn $ show point ++ ": " ++ show res
        return curstr'
    )
