{-# LANGUAGE RecordWildCards #-}
module Lexer (lexicalAnalyzer, Token (..)) where

import           Control.Monad.Except             (ExceptT (..), throwError)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Loops              (iterateUntilM)
import           Data.Attoparsec.ByteString.Char8 (decimal, parseOnly, signed)
import qualified Data.ByteString.Char8            as BS
import           Data.Char                        (isSpace)
import           System.IO                        (Handle)

data Token = TNum Integer | (:+:) | (:-:) | (:*:)
             deriving (Show,Read)

data LexerState = LexerState
    { lData   :: BS.ByteString
    , lPos    :: Int
    , lTokens :: [Token]
    }

instance Show LexerState where
    show LexerState{..} =
        mconcat
            [ "Lexer exception: unexpected char "
            , [lData `BS.index` lPos]
            , " at position "
            , show lPos]

lexicalAnalyzer :: Handle -> ExceptT LexerState IO [Token]
lexicalAnalyzer handle = do
    str <- liftIO $ BS.hGetContents handle
    (reverse . lTokens) <$>
        iterateUntilM condition localLexer (LexerState str 0 [])
  where
    condition LexerState{..} = lPos >= BS.length lData
    localLexer :: LexerState -> ExceptT LexerState IO LexerState
    localLexer s@LexerState{..}
      | isSpace (lData `BS.index` lPos) = skip s
    localLexer s@LexerState{..}
      | (lData `BS.index` lPos) `elem` ("*-+" :: String) =
          next 1 s $ read $ ':' : (lData `BS.index` lPos) : ":"
    localLexer s@LexerState{..} = do
        let numE = parseOnly (signed decimal) $ BS.drop lPos lData
        either
            (const $ throwError s)
            (\num ->
                  next (length $ show num) s $ TNum num)
            numE
    skip LexerState{..} = return LexerState{lPos = lPos + 1, ..}
    next n LexerState{..} t =
        return
            LexerState
            { lPos = lPos + n
            , lTokens = t : lTokens
            , .. }
