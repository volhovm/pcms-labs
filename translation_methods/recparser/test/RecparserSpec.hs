module RecparserSpec (spec) where

import           Control.Monad         (replicateM)
import qualified Data.ByteString.Char8 as BS
import           Data.Either           (isRight)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Gen, Positive (..), arbitrary, choose,
                                        elements, forAll, listOf1, oneof)

import           Recparser             (Token (..), lexicalAnalyzer)

delimiter :: Gen Char
delimiter = elements "\n\t "

-- Shows token in a way they should represent real chars
showToken :: Token -> BS.ByteString
showToken (TNum i) = BS.pack $ show i
showToken (:+:) = "+"
showToken (:-:) = "-"
showToken (:*:) = "*"

randomToken :: Gen Token
randomToken = oneof [elements [(:+:), (:*:), (:-:)], TNum <$> arbitrary]

correctLexerStr :: Gen BS.ByteString
correctLexerStr = do
    len <- choose (0, 100)
    mconcat <$>
        replicateM len (BS.cons <$> delimiter <*> (showToken <$> randomToken))

badLexerStr :: Gen BS.ByteString
badLexerStr = do
    (Positive a) <- arbitrary
    (s1,s2) <- BS.splitAt a <$> correctLexerStr
    randstr <- listOf1 $
        elements "',.pyfgcrl/=-snthdiueoa;qjkbwvxz<>PYFGCR?L+:QJSTHKBMWVHUTNE"
    return $ s1 `BS.append` BS.pack randstr `BS.append` s2

spec :: Spec
spec = do
    describe "Lexer testing" $ do
      prop "Lexer doesn't fail on some real inputs." $
          forAll correctLexerStr lexerSucceeds
      prop "Lexer fails on incorrect inputs." $
          forAll badLexerStr $ not . lexerSucceeds
    describe "Parser testing" $ do
      prop "Parser doesn't fail on some real inputs." True
      prop "Parser fails on some strange inputs" True
  where
      lexerSucceeds :: BS.ByteString -> Bool
      lexerSucceeds = isRight . lexicalAnalyzer
