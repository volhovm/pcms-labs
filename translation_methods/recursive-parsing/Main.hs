{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception                (throwIO)
import           Control.Monad                    (void, when)
import           Control.Monad.Except             (ExceptT (..), catchError,
                                                   runExceptT, throwError)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Loops              (iterateUntilM)
import           Control.Monad.State              (State, evalState, get,
                                                   modify, put)
import           Data.Attoparsec.ByteString.Char8 (decimal, parseOnly, signed)
import qualified Data.ByteString.Char8            as BS
import           Data.Char                        (isSpace)
import           Data.Either.Combinators          (fromLeft', fromRight',
                                                   isLeft)
import           Data.Tree                        (Tree (..))
import           Data.Tree.Pretty                 (drawVerticalTree)
import           Debug.Trace                      (trace)
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree
import           System.IO                        (Handle, IOMode (ReadMode),
                                                   openFile)
import qualified Turtle                           as T

{-
Part 1:

Initial thoughts:
A → A A *
A → A A +
A → A A -
A → n

Equal to:
A → A A * | A A + | A A - | n

After left recursion elimination:
A → n B | n
B → A * | A + | A - | A * B | A + B | A - B

No left factoring here, so the grammar is in its best
state now.
-}

-- Part 2

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

-- Part 3


data ParserError = ParserError String
type Parser a = ExceptT String (State [Token]) a

data PA = PAPrefix Integer PB
        | PANum Integer

data PB = (:+) PA
        | (:-) PA
        | (:*) PA
        | (:++) PA PB
        | (:--) PA PB
        | (:**) PA PB

{-
A → n B | n
B → A * | A + | A - | A * B | A + B | A - B
-}

popToken :: Parser Token
popToken = do
    tokens <- get
    case tokens of
        [] -> throwError "Unexpected end of input"
        x:xs -> put xs >> return x

pushToken :: Token -> Parser ()
pushToken t = modify (t :)

parsePA :: Parser PA
parsePA = do
    number <- popToken
    case number of
        (TNum n) ->
            (PAPrefix n <$> parsePB) `catchError` const (return (PANum n))
        t -> do
            pushToken number
            throwError $ "Expected number, got other stuff: " ++ show t

parsePB :: Parser PB
parsePB = do
    a <- parsePA
    opToken <- popToken
    bBoxed <- (Just <$> parsePB) `catchError` (const $ return Nothing)
    case (opToken, bBoxed) of
        ((:+:),Nothing) -> return $ (:+) a
        ((:-:),Nothing) -> return $ (:-) a
        ((:*:),Nothing) -> return $ (:*) a
        ((:+:),Just b) -> return $ (:++) a b
        ((:-:),Just b) -> return $ (:--) a b
        ((:*:),Just b) -> return $ (:**) a b
        (t,_) -> do
            pushToken t
            throwError ("Expected +,- or *, got: " ++ show t)

runParser :: [Token] -> Either String PA
runParser = evalState (runExceptT parsePA)

parseGrammar :: Handle -> IO PA
parseGrammar handle = do
    tokens <- runExceptT (lexicalAnalyzer handle)
    when (isLeft tokens) $ error $ "Lexer failed: " ++ show (fromLeft' tokens)
    putStrLn $ "Tokens: " ++ show tokens
    let parsed = runParser $ fromRight' tokens
    when (isLeft parsed) $ error $ "Parser failed: " ++ fromLeft' parsed
    return $ fromRight' parsed

-- Task 4

prettyPrintTree :: Tree String -> String
prettyPrintTree = drawVerticalTree

toTreePA (PANum i) = Node "A" [Node (show i) []]
toTreePA (PAPrefix i pb) = Node "A" [Node (show i) [], toTreePB pb]

toTreePB ((:+) pa) = Node "B" [toTreePA pa, Node "+" []]
toTreePB ((:-) pa) = Node "B" [toTreePA pa, Node "-" []]
toTreePB ((:*) pa) = Node "B" [toTreePA pa, Node "*" []]
toTreePB ((:++) pa pb) = Node "B" [toTreePA pa, Node "+" [], toTreePB pb]
toTreePB ((:--) pa pb) = Node "B" [toTreePA pa, Node "-" [], toTreePB pb]
toTreePB ((:**) pa pb) = Node "B" [toTreePA pa, Node "*" [], toTreePB pb]


instance Show PA where
    show t = prettyPrintTree $ toTreePA t

instance Show PB where
    show t = prettyPrintTree $ toTreePB t

getTree :: IO (Tree String)
getTree = do
  test0 <- openFile "test4.in" ReadMode
  toTreePA <$> parseGrammar test0

cast = fromInteger . toInteger

mainText :: IO ()
mainText = prettyPrintTree <$> getTree >>= putStrLn

main :: IO ()
main = do
    tree <- getTree
    let maxStringLength = maximum $ fmap length tree
        svg :: QDiagram SVG V2 Double Any
        svg =
            renderTree
                (\(a :: String) ->
                      text a <> roundedRect (cast $ length a) 1.2 0.1 # fc grey #
                      lw 2)
                (~~)
                (symmLayout'
                     (with & slHSep .~ (max 1.5 (cast maxStringLength)) &
                      slVSep .~
                      1.5)
                     tree) #
            centerXY #
            pad 1.1
    renderSVG "diagram.svg" (mkWidth 1000) svg
    void $ T.shell "xdg-open ./diagram.svg" T.empty
