{-# LANGUAGE UnicodeSyntax, ImplicitPrelude, OverloadedStrings #-}
module Main where

-- Not finished.
-- It's too tedious to read trees into IOUArrays, and then process
-- them and so on...

import Prelude hiding (takeWhile, (**))
import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8
import Data.Int()
import qualified Data.IntMap.Strict as IM
import Data.Array.IO
import Data.Typeable
import Data.Fixed

import Control.Monad
import Control.Applicative((<|>))

filename = "trees"

(**) = BS.append
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
lexem a = a <* space'

processIO :: (Handle → Handle → IO()) → IO()
processIO handling = do
  input ← openFile (filename ++ ".in") ReadMode
  output ← openFile (filename ++ ".out") WriteMode
  handling input output
  hClose input
  hClose output

data ChoiceTree = Choice !Int !Int !ChoiceTree !ChoiceTree
                | Leaf !Int !Int

toStr :: (Show a) ⇒ a → BS.ByteString
toStr = BS.pack . show

showChoice :: ChoiceTree → BS.ByteString
showChoice (Choice a b c d e) =
  "choice " ** toStr a ** " " ** toStr b ** " " ** toStr c ** "\n"
  ** showChoice d ** "\n" ** showChoice e
showChoice (Leaf x) = "leaf " ** toStr x

{-# INLINE keyword #-}
keyword = lexem $ string "choice" <|> string "leaf"

--buildTree :: Parser ChoiceTree
--buildTree = do
--  k ← keyword
--  tree ← (if k == "choice"
--          then (Choice <$> decimal' <*>
--                <*> buildTree <*> buildTree)
--          else (Leaf <$> decimal'))
--  return tree

--reduce :: IM.IntMap Int → ChoiceTree → ChoiceTree
--reduce _ o@(Leaf _) = o
--reduce map o@(Choice p s1 s2 tl tr) =
--  if member p map
--  then let val = map ! p in
--        if (s1 ==

genArray :: IO (IOUArray Int ChoiceTree)
genArray = undefined

main :: IO ()
main = processIO $ \input output → do
  (s0, n) ← fromDone <$> parseOnly decimal' <$> BS.hGetContents input
  parsearr ← newArr (0, n-1)
  BS.putStrLn . showChoice $ res
