{-# LANGUAGE RecordWildCards #-}

module Lexer (lexicalAnalyzer, Token (..)) where

import           Control.Monad.State              (StateT (..), evalStateT, get,
                                                   modify)
import           Data.Attoparsec.ByteString.Char8 (decimal, parseOnly, signed)
import qualified Data.ByteString.Char8            as BS
import           Data.Char                        (isSpace)

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

lexicalAnalyzer :: BS.ByteString -> Either LexerState [Token]
lexicalAnalyzer str =
    evalStateT localLexer (LexerState str 0 [])
  where
    localLexer :: StateT LexerState (Either LexerState) [Token]
    localLexer = do
        s <- get
        if lPos s >= BS.length (lData s)
        then return $ reverse $ lTokens s
        else do
            case s of
               LexerState{..} | isSpace (lData `BS.index` lPos) -> skip
               LexerState{..} | (lData `BS.index` lPos) `elem` ("*-+" :: String) ->
                   next 1 $ read $ ':' : (lData `BS.index` lPos) : ":"
               LexerState{..} ->
                   let numE = parseOnly (signed decimal) $ BS.drop lPos lData
                   in either (const $ StateT Left)
                             (\num ->
                                   next (length $ show num) (TNum num))
                             numE
            localLexer
    skip :: StateT LexerState (Either LexerState) ()
    skip = modify $ \LexerState{..} -> LexerState{lPos = lPos + 1, ..}
    next :: Int -> Token -> StateT LexerState (Either LexerState) ()
    next n t =
         modify $ \LexerState{..} ->
            LexerState
            { lPos = lPos + n
            , lTokens = t : lTokens
            , .. }
