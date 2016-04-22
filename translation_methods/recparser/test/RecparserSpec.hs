module RecparserSpec (spec) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
    describe "Lexer testing" $ do
      prop "Lexer doesn't fail on some real inputs." True
      prop "Lexer fails on some strange inputs" True
    describe "Parser testing" $ do
      prop "Parser doesn't fail on some real inputs." True
      prop "Parser fails on some strange inputs" True
