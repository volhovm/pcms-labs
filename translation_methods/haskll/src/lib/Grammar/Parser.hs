{-# LANGUAGE TemplateHaskell #-}

-- | Module that provides parsing capabilities for haskll grammar
module Grammar.Parser () where

import qualified Data.ByteString    as BS
import           Data.Char          (isUpper)
import           Data.FileEmbed     (embedStringFile)
import qualified Data.Text          as T
import           Text.Megaparsec    (Parsec)
import           Universum

import           Grammar.Expression (Expression (..), GrammarDef (..), Term (..),
                                     TokenExp (..), Variant (..))

t :: Text
t = $(embedStringFile "resources/test1.g")

(&^&) :: (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
(&^&) a b = \x -> a x && b x

--spaces :: Parsec ()
----spaces = skipWhile $ isSpace
--spaces = notImplemented

--name :: Parser Text
--name = A.many1 $ A.satisfy A.isAlpha_ascii
--
--tokenName :: Parser Text
--tokenName = (:) <$> A.satisfy (A.isAlpha_ascii &^& isUpper)
--                <*> A.many' (A.satisfy A.isAlpha_ascii)

--pTokenExp :: Parsec TokenExp
--pTokenExp = notImplemented
--
--parseFile :: BS.ByteString -> GrammarDef
--parseFile = notImplemented
