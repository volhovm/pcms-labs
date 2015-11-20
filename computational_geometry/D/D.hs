{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, UnicodeSyntax, BangPatterns #-}
module Main where

-- IT'S A SKETCH & DOESN'T WORK
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.56.5306&rep=rep1&type=pdf
-- seems like double linked circular lists in haskell
-- are somewhat exotic and writing them is extremly time-consuming

import Prelude hiding (pred)
import Control.Applicative
import Data.Char
import Data.List
import Control.Monad
import qualified Data.Set as S
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
  content ← BS.getContents
  case (runState parseInput) content of
   Left err → putStrLn err
   Right (_, points) → putStrLn $ unlines $
                       map (\(a, b, c) → show a ++ show b ++ show c) $
                       triangulateS (fromList points) S.empty

--- Structures part

data Point = Point { _x :: !Int, _y :: !Int }
           deriving (Eq, Ord)

instance Show Point where
  show a = show (_x a) ++ " " ++ show (_y a)

(~-) :: Point → Point → Point
(~-) a b = Point (_x a - _x b) (_y a - _y b)

data DoubleList a = Cons (DoubleList a) a (DoubleList a)
                  | Nil

dlrem :: DoubleList a → DoubleList a
dlrem Nil = error "List can't be empty"
dlrem (Cons (Cons a b _) _ Nil) = Cons a b Nil
dlrem (Cons Nil _ (Cons _ b c)) = Cons Nil b c
dlrem (Cons (Cons a b _) _ o) = Cons a b o
dlrem _ = error "List doesn't have sufficient elements for removal"

succ, pred :: DoubleList a → DoubleList a
succ (Cons _ x b) = b
pred (Cons a x _) = a

mid :: DoubleList a → a
mid (Cons _ x _) = x

instance Show a => Show (DoubleList a) where
  show Nil = "Nil"
  show (Cons left x right) = "Cons <left> " ++ show x ++ " " ++ show right

fromList :: [a] -> DoubleList a
fromList xs = fromList' Nil xs
    where
        fromList' :: DoubleList a -> [a] -> DoubleList a
        fromList' _ [] = Nil
        fromList' prev (x:xs) = self
            where self = Cons prev x (fromList' self xs)

--- Parser part

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

parseInput :: Parser [(Point, Int)]
parseInput = do
  n ← int <* spaces
  points ← n `times` (Point <$> (int <* spaces) <*> (int <* spaces))
  return (points `zip` [1..n])

--- Algorithmic part

-- turn a b c -- turn ab, ac
turn :: Point → Point → Point → Ordering
turn p a b = compare
             (((_x a - _x p) * (_y b - _y p)) - ((_y a - _y p) * (_x b - _x p)))
             0

triangulateS :: DoubleList (Point, Int) → S.Set Point → [(Int, Int, Int)]
triangulateS (Cons Nil a b) = triangulate b

isAnEar :: DoubleList (Point, Int) → S.Set Point → Point → Bool
isAnEar = undefined

triangulate :: DoubleList (Point, Int) → S.Set Point → [(Int, Int, Int)]
triangulate (Cons Nil a (Cons _ b (Cons _ c Nil))) _ = []
triangulate (Cons (Cons Nil a _) b (Cons _ c Nil)) _ = []
triangulate (Cons (Cons (Cons Nil a _) b _) c Nil) _ = []
triangulate p@(Cons _ (x, n0) _) r =
  if isAnEar p r x
  then
    (snd $ mid $ pred $ pred p,
     snd $ mid $ pred $ p,
     n0) :
    let p' = dlrem $ pred p in
    let r' = if S.member x r
                then
    triangulate (dlrem $ pred p) (
  else undefined
