{-# LANGUAGE UnicodeSyntax, ImplicitPrelude, OverloadedStrings #-}
module Main where

import Prelude hiding (takeWhile, (**))
import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8
import Data.Int()
import Data.Typeable
import Data.Fixed
import Control.Monad
import Control.Applicative((<|>))

-- OK :cool:

filename = "mutation"

fromRight (Right b) = b
fromInt = fromInteger . toInteger
fromDone (Done a b) = (a, b)

times :: Monad m ⇒ Int → BS.ByteString →
          (BS.ByteString → (m a, BS.ByteString)) → m a
times 1 s foo = fst $ foo s
times i s foo = do
  let (mon, s') = foo s
  mon
  times (i - 1) s' foo

space' = skipSpace <|> return ()
decimal', digitInt :: Parser Int
decimal' = decimal <* space'
digitInt = read <$> (\x → [x]) <$> digit

processIO :: (Handle → Handle → IO()) → IO()
processIO handling = do
  input ← openFile (filename ++ ".in") ReadMode
  output ← openFile (filename ++ ".out") WriteMode
  handling input output
  hClose input
  hClose output

data BTPair = BTPair !BS.ByteString !BS.ByteString
              deriving Show

{-# INLINE byteChar #-}
{-# INLINE getVecs #-}
byteChar = (takeWhile1 $ isDigit) <* space'
getVecs = BTPair <$> byteChar <*> byteChar
inputPattern = do
  n ← decimal'
  m ← decimal'
  return (n, m)

{-# INLINE countDiffs' #-}
countDiffs' :: BTPair → Int
countDiffs' (BTPair a b) =
  sum $ BS.zipWith (\s1 s2 → if s1 /= s2 then 1 else 0) a b

main :: IO ()
main = processIO $ \input output → do
  (s0, (n, m)) ← fromDone <$> parse inputPattern <$>
                 BS.hGetContents input
  let
    fraclen :: Pico
    fraclen = (fromInteger . toInteger) n
    fraclen' = 1 / fraclen
  times m s0 $ \s →
    let
      (s', vecs) = fromDone $
                   case parse getVecs s of
                   o@(Partial _) → feed o "\n end"
                   x → x
      diffs = countDiffs' vecs
      sames = n - diffs in
     (hPutStrLn output $ showFixed True $
     (fraclen' ^ diffs) * ((1 - fraclen') ^ sames), s')
