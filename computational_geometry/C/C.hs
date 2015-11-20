{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, UnicodeSyntax, BangPatterns #-}
module Main where

import Prelude
import Control.Applicative
import Data.Char
import Data.List
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS

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

int :: Parser Int
int = StateT $ \s → case BS.readInt s of
  Nothing → Left "Couldn't parse int"
  Just (i', s')  → Right (s', i')

spaces :: Parser ()
spaces = StateT $ \s → Right (BS.dropWhile isSpace s, ())

times :: Int → Parser a → Parser [a]
times 0 _ = return []
times 1 p = return <$> p
times n p = liftM2 (:) p (times (n - 1) p)

data Point = Point { x :: !Int, y :: !Int }
             deriving (Eq, Ord)

instance Show Point where
  show a = show (x a) ++ " " ++ show (y a)

(~-) :: Point → Point → Point
(~-) a b = Point (x a - x b) (y a - y b)

parseInput :: Parser [Point]
parseInput = do
  n ← int <* spaces
  points ← n `times` (Point <$> (int <* spaces) <*> (int <* spaces))
  return points


-- turn a b c -- turn ab, ac
turn :: Point → Point → Point → Ordering
turn p a b = compare (((x a - x p) * (y b - y p)) - ((y a - y p) * (x b - x p))) 0

rev :: Ordering → Ordering
rev GT = LT
rev LT = GT
rev EQ = EQ

buildHull :: [Point] → [Point] → [Point]
buildHull [] stack = stack
buildHull o@(x:xs) stack@(a:b:xs') = if turn a b x == LT
                                     then buildHull xs $ x:stack -- add
                                     else buildHull o $ b:xs'    -- pop
buildHull (o:xs) xs' = buildHull xs $ o:xs'

graham :: [Point] → [Point]
graham points = let (leftMost:points') = sort points in
  buildHull (sortBy (\a b → rev (turn leftMost a b)) points') [leftMost]


main :: IO ()
main = do
  content ← BS.getContents
  case (runState parseInput) content of
   Left err → putStrLn err
   Right (_, points) → let reslist = graham points in
                        putStrLn $ (show (length reslist)) ++ "\n" ++
                        (unlines $ map show reslist)
