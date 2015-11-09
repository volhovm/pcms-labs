{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, UnicodeSyntax, BangPatterns,
DeriveDataTypeable #-}
module Main where

-- OK

import System.IO
import Prelude
import Control.Monad
import Data.Char
import Data.List
import Data.Fixed
import Data.Maybe
import Data.Typeable
import qualified Data.ByteString.Lazy.Char8 as BS

----------------------- Library

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

----------------------- Structures

data E24 = E24
     deriving (Typeable)
instance HasResolution E24 where
    resolution _ = 1000000000000000000000000
-- | resolution of 10^-21
type Yocto  = Fixed E24


data Point = Point { _x :: Integer, _y :: Integer }
             deriving (Eq, Ord)

instance Show Point where
  show a = show (_x a) ++ " " ++ show (_y a)

(~-), (~+) :: Point → Point → Point
(~-) a b = Point (_x a - _x b) (_y a - _y b)
(~+) a b = Point (_x a + _x b) (_y a + _y b)

(~.) :: Point → Point → Integer
(~.) a b = _x a * _x b + _y a * _y b

----------------------- Parsing

int :: Parser Integer
int = StateT $ \s → case BS.readInt s of
  Nothing → Left "Couldn't parse int"
  Just (i', s')  → Right (s', toInteger $ i')

spaces :: Parser ()
spaces = StateT $ \s → Right (BS.dropWhile isSpace s, ())


times :: Integer → Parser a → Parser [a]
times 0 _ = return []
times 1 p = return <$> p
times n p = liftM2 (:) p (times (n - 1) p)


parseInput :: Parser ([Point], [Point])
parseInput = do
  n1 ← int <* spaces
  points1 ← n1 `times` (Point <$> (int <* spaces) <*> (int <* spaces))
  n2 ← int <* spaces
  points2 ← n2 `times` (Point <$> (int <* spaces) <*> (int <* spaces))
  return (points1, points2)


---------------------- Algorithmic

-- turn a b c -- turn ab, ac
turn :: Point → Point → Integer
turn a b = (_x a) * (_y b) - (_y a) * (_x b)
turn' :: Point → Point → Point → Point → Integer
turn' a b c d = turn (b ~- a) (d ~- c)
turn'' :: Point → Point → Point → Integer
turn'' a b c = turn' b c a b

pairComparator' :: Point → Point → Ordering
pairComparator' (Point x1 y1) (Point x2 y2) = case compare x1 x2 of
  EQ → compare y2 y1
  x → x

mostLeftUpper :: [Point] → [Point]
mostLeftUpper p = let elem' = minimumBy pairComparator' p in
                let index = fromJust $ elemIndex elem' p in
                 let (s1, s2) = splitAt index p in
                  s2 ++ s1

mostRightDown :: [Point] → [Point]
mostRightDown p = let elem' =  maximumBy pairComparator' p in
                   let index = fromJust $ elemIndex elem' p in
                    let (s1, s2) = splitAt index p in
                     s2 ++ s1

dist1r :: Point → Yocto
dist1r (Point a b) = let a' = fromInteger a
                         b' = fromInteger b in
                              a' * a' + b' * b'

dist2r :: Point → Point → Yocto
dist2r a b = dist1r (b ~- a)

distKostyl2r :: (Point, Yocto, Point) → Point → Yocto
distKostyl2r ((Point x1 y1), p, (Point x2 y2)) (Point x3 y3) =
  let (x, y) = ((fromInteger x1) + p * (fromInteger x2),
                (fromInteger y1) + p * (fromInteger y2))
      (x', y') = ((fromInteger x3) - x, (fromInteger y3) - y)
  in x' * x' + y' * y'

-- From here: http://geomalgorithms.com/a02-_lines.html
dist21r :: (Point, Point) → Point → Yocto
dist21r (p0, p1) p =
  let v = p1 ~- p0
      w = p ~- p0
      c1 = w ~. v in
      if c1 <= 0
      then dist2r p p0
      else
        let c2 = v ~. v in
        if c2 <= c1
        then dist2r p p1
        else distKostyl2r (p0, ((fromInteger c1) / (fromInteger c2)), v) p

-- First bool means "if first polygon caliper dominates", that
-- is if line of support is formed with it's first two points.
-- If it's false, then second polygon dominates
antipodalPairs :: Bool → [Point] → [Point] → Yocto
antipodalPairs _ (a:b:[]) (c:d:[]) =  foldl1 min [dist21r (a, b) c,
                                                     dist21r (a, b) d,
                                                     dist21r (c, d) a,
                                                     dist21r (c, d) b]
antipodalPairs True a@(c0:c1:[]) (c2:b) =
  min (dist21r (c0, c1) c2) $ antipodalPairs True a b
antipodalPairs False (c1:a) b@(c0:c2:[]) =
  min (dist21r (c0, c2) c1) $ antipodalPairs False a b
antipodalPairs True (p1:a@(c1:n1:_)) b@(c2:n2:_) =
  min (dist21r (p1, c1) c2) $
   if turn' n2 c2 c1 n1 >= 0
     then antipodalPairs True a b
     else antipodalPairs False a b
antipodalPairs False a@(c1:n1:_) (p2:b@(c2:n2:_)) =
  min (dist21r (p2, c2) c1) $
   if turn' c2 n2 n1 c1 >= 0
     then antipodalPairs True a b
     else antipodalPairs False a b
antipodalPairs _ _ _ = 1000000000000000000

main :: IO ()
main = do
  content ← BS.getContents
  case (runState parseInput) content of
   Left err → putStrLn err
   Right (_, (p1, p2)) →
     -- find points to start, swap if needed
     let (s1, s2) = (mostLeftUpper p1, mostRightDown p2) in
     let (s1', s2') = if s1 > s2
                      then (mostLeftUpper p2, mostRightDown p1)
                      else (s1, s2) in
     let (s1'', s2'') = ((s1' ++ [head s1']),
                         (s2' ++ [head s2'])) in
     -- get initial caliper direction
     let ifleft = let (a:b:_) = s1''
                      (c:d:_) = s2'' in
                   turn' d c a b >= 0 in
      -- fun fact : self-written sqrt for Fixed a class doesn't work (WA6)
     putStrLn . show . sqrt . read . show $ (antipodalPairs ifleft s1'' s2'')
