module Recparser.Lexer (lexicalAnalyzer, Token (..)) where

import           Control.Monad.State              (StateT (..), evalStateT, get,
                                                   modify)
import           Data.Attoparsec.ByteString.Char8 (decimal, parseOnly, signed)
import qualified Data.ByteString.Char8            as BS
import           Data.Char                        (isSpace)
import           Data.Either.Combinators          (fromRight', isRight)

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
            , show lPos
            , " on input: "
            , BS.unpack lData]

lexicalAnalyzer :: BS.ByteString -> Either LexerState [Token]
lexicalAnalyzer str =
    evalStateT localLexer (LexerState str 0 [])
  where
    localLexer :: StateT LexerState (Either LexerState) [Token]
    localLexer = do
        s@LexerState{..} <- get
        if lPos >= BS.length lData
        then return $ reverse lTokens
        else do
            let readDecimal = parseOnly (signed decimal) $ BS.drop lPos lData
                num = fromRight' readDecimal
            case s of
               _ | isSpace (lData `BS.index` lPos) -> skip
               _ | isRight readDecimal -> next (length $ show num) (TNum num)
               _ | (lData `BS.index` lPos) `elem` ("*-+" :: String) ->
                   next 1 $ read $ ':' : (lData `BS.index` lPos) : ":"
               _ -> StateT Left
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
