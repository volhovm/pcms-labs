{-# LANGUAGE StandaloneDeriving #-}

module RecparserSpec (spec) where

import           Control.Monad         (replicateM)
import qualified Data.ByteString.Char8 as BS
import           Data.Either           (isLeft, isRight)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..), Gen, Positive (..),
                                        arbitrary, choose, elements, forAll,
                                        listOf1, oneof)

import           Recparser             (PA (..), PB (..), Token (..),
                                        lexicalAnalyzer, parseGrammar)

delimiter :: Gen Char
delimiter = elements "\n\t "

-- With Delimiter
wd :: BS.ByteString -> Gen BS.ByteString
wd s = BS.snoc s <$> delimiter

-- Shows token in a way they should represent real chars
showToken :: Token -> BS.ByteString
showToken (TNum i) = BS.pack $ show i
showToken (:+:) = "+"
showToken (:-:) = "-"
showToken (:*:) = "*"

instance Arbitrary Token where
    arbitrary = oneof [elements [(:+:), (:*:), (:-:)], TNum <$> arbitrary]

spoilString :: BS.ByteString -> Gen BS.ByteString
spoilString s = do
    (Positive a) <- arbitrary
    let (s1,s2) = BS.splitAt a s
    randstr <- listOf1 $
        elements "',.pyfgcrl/=-snthdiueoa;qjkbwvxz<>PYFGCR?L:QJSTHKBMWVHUTNE"
    return $ s1 `BS.append` BS.pack randstr `BS.append` s2

correctLexerStr, badLexerStr :: Gen BS.ByteString
correctLexerStr = do
    len <- choose (0, 100)
    mconcat <$>
        replicateM len (BS.cons <$> delimiter <*> (showToken <$> arbitrary))
badLexerStr = correctLexerStr >>= spoilString

instance Arbitrary PA where
    arbitrary =
        oneof [PANum <$> arbitrary, PAPrefix <$> arbitrary <*> arbitrary]

instance Arbitrary PB where
    arbitrary = oneof [ (:+) <$> arbitrary
                      , (:-) <$> arbitrary
                      , (:*) <$> arbitrary
                      , (:++) <$> arbitrary <*> arbitrary
                      , (:--) <$> arbitrary <*> arbitrary
                      , (:**) <$> arbitrary <*> arbitrary]

bsh :: (Show a) => a -> BS.ByteString
bsh = BS.pack . show

showPA :: PA -> Gen BS.ByteString
showPA (PAPrefix i pb) = BS.append <$> wd (bsh i) <*> showPB pb
showPA (PANum i) = wd (bsh i)

showPB :: PB -> Gen BS.ByteString
showPB ((:+) pa) = (flip BS.snoc '+' <$> showPA pa) >>= wd
showPB ((:-) pa) = (flip BS.snoc '-' <$> showPA pa) >>= wd
showPB ((:*) pa) = (flip BS.snoc '*' <$> showPA pa) >>= wd
showPB ((:++) pa pb) = BS.concat <$> sequence [showPA pa, wd "+", showPB pb]
showPB ((:--) pa pb) = BS.concat <$> sequence [showPA pa, wd "-", showPB pb]
showPB ((:**) pa pb) = BS.concat <$> sequence [showPA pa, wd "*", showPB pb]

deriving instance (Show PA)
deriving instance (Show PB)

correctGrammar, badGrammar :: Gen BS.ByteString
correctGrammar = arbitrary >>= showPA
badGrammar = correctGrammar >>= spoilString

spec :: Spec
spec = do
    describe "Lexer testing" $ do
      prop "Lexer doesn't fail on some real inputs." $
          forAll (lexicalAnalyzer <$> correctLexerStr) isRight
      prop "Lexer fails on incorrect inputs." $
          forAll (lexicalAnalyzer <$> badLexerStr) isLeft
    describe "Parser testing" $ do
      prop "Parser doesn't fail on some real inputs." $
          forAll (paired parseGrammar <$> correctGrammar) (isRight . fst)
      prop "Parser fails on some strange inputs" $
          forAll (paired parseGrammar <$> badGrammar) (isLeft . fst)
  where
    paired f x = (f x, x)
