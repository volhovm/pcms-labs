-- | Module that provides parsing capabilities for haskll grammar
module Grammar.Parser (parseFile) where

import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString                  as BS
import           Data.Char                        (isUpper)
import           Universum

import           Grammar.Expression               (Expression (..), GrammarDef (..),
                                                   Term (..), TokenExp (..), Variant (..))

(&^&) :: (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
(&^&) a b = \x -> a x && b x

f0 :: [Char]
f0 = ['ф']
f1 :: Text
f1 = "ф"
f2, f3 :: ByteString
f2 = "ф"
f3 = "фф"

{-
λ> f0
"\1092"
λ> f1
"\1092"
λ> BS.unpack $ encodeUtf8 f1
[209,132]
λ> f2
"D"
λ> BS.unpack f2
[68]
λ> BS.unpack $ f2
[68,68]
-}

spaces :: Parser ()
--spaces = skipWhile $ isSpace
spaces = notImplemented

--name :: Parser Text
--name = A.many1 $ A.satisfy A.isAlpha_ascii
--
--tokenName :: Parser Text
--tokenName = (:) <$> A.satisfy (A.isAlpha_ascii &^& isUpper)
--                <*> A.many' (A.satisfy A.isAlpha_ascii)

pTokenExp :: Parser TokenExp
pTokenExp = notImplemented

parseFile :: BS.ByteString -> GrammarDef
parseFile = notImplemented
