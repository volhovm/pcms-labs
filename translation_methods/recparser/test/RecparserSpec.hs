{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module RecparserSpec (spec) where

import           Control.Monad         (replicateM)
import qualified Data.ByteString.Char8 as BS
import           Data.Either           (isLeft, isRight)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..), Gen, Positive (..),
                                        arbitrary, choose, elements, forAll,
                                        listOf1, oneof, verbose)

import           Recparser             (PA (..), PB (..), PC (..), PD (..),
                                        Token (..), lexicalAnalyzer,
                                        parseGrammar)

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
        elements "pyfgcrl/=snthdiueoa;qjkbwvxz<>PYFGCR?L:QJSTHKBMWVHUTNE"
    return $ s1 `BS.append` BS.pack randstr `BS.append` s2

correctLexerStr, badLexerStr :: Gen BS.ByteString
correctLexerStr = do
    len <- choose (0, 500)
    mconcat <$>
        replicateM len (BS.cons <$> delimiter <*> (showToken <$> arbitrary))
badLexerStr = correctLexerStr >>= spoilString

instance Arbitrary PA where
    arbitrary =
        oneof [PANum <$> arbitrary, PAPrefix <$> arbitrary <*> arbitrary]

instance Arbitrary PB where
    arbitrary = PB <$> arbitrary <*> arbitrary

instance Arbitrary PC where
    arbitrary = oneof [ (:+) <$> arbitrary
                      , (:-) <$> arbitrary
                      , (:*) <$> arbitrary ]

instance Arbitrary PD where
    arbitrary = oneof [return PDε, PDB <$> arbitrary]

(##) :: BS.ByteString -> BS.ByteString -> BS.ByteString
(##) = BS.append

bsh :: (Show a) => a -> BS.ByteString
bsh = BS.pack . show

showPA :: PA -> Gen BS.ByteString
showPA (PAPrefix i pb) = (##) <$> wd (bsh i) <*> showPB pb
showPA (PANum i) = wd (bsh i)

showPB :: PB -> Gen BS.ByteString
showPB (PB pa pc) = BS.concat <$> sequence [showPA pa, BS.singleton <$> delimiter, showPC pc]

showPC :: PC -> Gen BS.ByteString
showPC ((:+) pd) = (flip BS.snoc '+' <$> showPD pd) >>= wd
showPC ((:-) pd) = (flip BS.snoc '-' <$> showPD pd) >>= wd
showPC ((:*) pd) = (flip BS.snoc '*' <$> showPD pd) >>= wd

showPD :: PD -> Gen BS.ByteString
showPD PDε = return ""
showPD (PDB pb) = showPB pb

deriving instance (Show PA)
deriving instance (Show PB)
deriving instance (Show PC)
deriving instance (Show PD)

correctGrammar, badGrammar :: Gen BS.ByteString
correctGrammar = arbitrary >>= showPA
badGrammar = correctGrammar >>= spoilString

spec :: Spec
spec = do
    describe "Lexer testing" $ do
      prop "Lexer doesn't fail on some real inputs." $
          verbose $ forAll correctLexerStr $ isRight . lexicalAnalyzer
      prop "Lexer fails on incorrect inputs." $
          verbose $ forAll badLexerStr $ isLeft . lexicalAnalyzer
    describe "Parser testing" $ do
      prop "Parser doesn't fail on some real inputs." $
          verbose $ forAll correctGrammar $ isRight . parseGrammar
      prop "Parser fails on some strange inputs" $
          verbose $ forAll badGrammar $ isLeft . parseGrammar
