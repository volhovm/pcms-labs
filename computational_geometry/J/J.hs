{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List
import           Data.Int
import           Data.Fixed
import           Data.Typeable hiding (cast)
import           Debug.Trace
import           Prelude
import           System.IO

filename = "tower"

data E24 = E24
     deriving (Typeable)
instance HasResolution E24 where
    resolution _ = 1000000000000000000000000
-- | resolution of 10^-21
type Yocto  = Fixed E24


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

data Point = Point { x :: !Yocto, y :: !Yocto }
             deriving (Eq, Ord)

instance Show Point where
  show a = show (x a) ++ " " ++ show (y a)

len :: Point → Yocto
len (Point x y) = x * x + y * y

(~-) :: Point → Point → Point
(~-) a b = Point (x a - x b) (y a - y b)

--------------------------

cast :: (Num a, Integral a, Num b) ⇒ a → b
cast = fromInteger . toInteger

parsePoint :: Parser Point
parsePoint = Point <$> (cast <$> (int <* skipchar))
                   <*> (cast <$> (int <* skipchar))

parseInput :: Parser (Int, [Point])
parseInput = do
  n ← cast <$> int <* spaces
  points ← n `times` parsePoint
  return (n, points)

processIO :: (Handle → Handle → IO ()) → IO()
processIO handling = do
  input ← openFile (filename ++ ".in") ReadMode
  output ← openFile (filename ++ ".out") WriteMode
  handling input output
  hClose input
  hClose output

genRange :: Yocto → Yocto → Yocto → Yocto → Yocto → [Point]
genRange l r d u step =
  concatMap (\xcord →
              map (\ycord → Point xcord ycord) [d,(d+step)..u])
            [l,(l+step)..r]

distance :: [Point] → Point → Yocto
distance points p = foldl (\prev p' → max prev (abs $ len $ p' ~- p)) 0 points

findMinimum :: Yocto → Yocto → Yocto → Yocto → -- l, r, d, u
               Yocto → Point → [Point] →          -- step, prevmin, points
               (Point, Yocto)                     -- minimum
findMinimum l r d u step prevmin points =
  let distances = map (\p → (p,distance points p)) $ prevmin:(genRange l r d u step)  in
  foldl1 (\a@(_,d1) b@(_,d2) → if d1 < d2 then a else b) distances

mainLoop :: Yocto → Int → Point → Yocto →
            (Yocto → Int → Point → Yocto →
              IO (Either (Point, Yocto) (Yocto, Int, Point, Yocto))) →
            IO (Point, Yocto)
mainLoop s p c d foo = do
  res ← foo s p c d
  case res of
    Left o              → return o
    Right (s',p',c',d') → mainLoop s' p' c' d' foo


main :: IO ()
main = processIO $ \input output → do
  let fromRight (Right a) = a
  content ← BS.hGetContents input
  let (_, (n, points@(point1:_))) = fromRight $ (runState parseInput) content
      maxs@(l, r, d, u) =
        foldl
        (\(l,r,d,u) p → let l' = if x p < l then x p else l
                            r' = if x p > r then x p else r
                            d' = if y p < d then y p else d
                            u' = if y p > u then y p else u
                        in (l',r',d',u'))
        (x point1, x point1, y point1, y point1)
        points
      firststep = if (abs $ r - l) < 1000 ||
                     (abs $ u - d) < 1000
                  then 1
                  else 512
      firstmin = findMinimum (l-1) (r+1) (d-1) (u+1) firststep point1 points
      maxiterations = 25
  --putStrLn $ "Range: " ++ show maxs
  --putStrLn $ "Firstmin: " ++ show firstmin
  result ← mainLoop (firststep / 2) 0 (fst firstmin) (snd firstmin) $
   \step iteration point@(Point px py) val →
     if iteration == maxiterations
     then return $ Left (point, val)
     else do let newstep = step / 2
                 (newpoint, newval) =
                   findMinimum (px - 5 * step)
                               (px + 5 * step)
                               (py - 5 * step)
                               (py + 5 * step)
                               newstep
                               point
                               points
             --putStrLn $ show step ++ " " ++ show newpoint ++ " " ++ show newval
             return $ Right (newstep, iteration + 1, newpoint, newval)
  let sqrt' = sqrt . read . show
  hPutStrLn output $ show (snd result) ++ "\n" ++ show (fst result)
  putStrLn $ show (sqrt' $ snd result) ++ "\n" ++ show (fst result)
