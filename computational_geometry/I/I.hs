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
import           Debug.Trace
import           Prelude
import           System.IO

filename = "point"

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

cast :: (Num a, Integral a, Num b) ⇒ a → b
cast = fromInteger . toInteger

parsePoint :: Parser Point
parsePoint = Point <$> (int <* skipchar) <*> (int <* skipchar)

parseHeader :: Parser (Int, Point, Point)
parseHeader = (,,) <$> (cast <$> int <* wspaces) <*> parsePoint <*> parsePoint


processIO :: (Handle → Handle → IO ()) → IO()
processIO handling = do
  input ← openFile (filename ++ ".in") ReadMode
  output ← openFile (filename ++ ".out") WriteMode
  handling input output
  hClose input
  hClose output

sign x | x > 0 = 1
sign x | x < 0 = -1
sign _         = 0

-- (a,b) intersects with (c,d) ?
intersects :: Point → Point → Point → Point → Bool
intersects a b c d = let t1 = sign $ turn a b c
                         t2 = sign $ turn a b d
                         t3 = sign $ turn c d a
                         t4 = sign $ turn c d b in
                     t1 /= t2 && t3 /= t4

mainLoop :: BS.ByteString → Point → Int →
            (BS.ByteString → Point → Int → IO (BS.ByteString, Point, Int)) →
            IO ()
mainLoop s p c foo = do
  (s', p', c') ← foo s p c
  if BS.null s'
    then return ()
    else mainLoop s' p' c' foo

main :: IO ()
main = processIO $ \input output → do
  let fromRight (Right a) = a
  content ← BS.hGetContents input
  let (pointsStr, (n, target, firstpoint)) = fromRight $ (runState parseHeader) content
      targetTwin = Point (1 + x target) 10000000000
  if firstpoint == target
  then hPutStrLn output "YES" >> return ()
  else mainLoop pointsStr firstpoint 0
        (\curstr prevpoint counter →
            if curstr == "MARKER"
            then do let res = intersects target targetTwin prevpoint firstpoint
                        sumcount = if res then counter + 1 else counter
                        inside = if (sumcount `mod` 2) == 0 then "NO" else "YES"
                    hPutStrLn output inside
                    putStrLn inside
                    putStrLn $ "counter is " ++ show sumcount
                    return ("", Point 228 228, 228)
            else do let (curstr', point) = fromRight $ (runState parsePoint) curstr
                        res = intersects target targetTwin prevpoint point
                    if point == target
                    then do hPutStrLn output "YES"
                            putStrLn "YES"
                            return ("", Point 228 228, 228)
                    else return (if BS.null curstr' then "MARKER" else curstr',
                                point,
                                if res then counter + 1 else counter)
        )
