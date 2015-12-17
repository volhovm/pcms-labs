{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module GenTuring where

import qualified Control.Exception   as E
import           Data.Maybe
import qualified Data.Set            as S
import           Control.Monad       (when)
import           Language.Haskell.TH
import           System.IO

type State = String
data Rule =  Rule { ruleName :: State
                  , ruleFoo  :: Char -> (Rule, Char, Direction)
                  }
-- wrote it using this source:
-- http://hackage.haskell.org/package/template-haskell-2.7.0.0/docs/src/Language-Haskell-TH-Syntax.html
mkRule :: Name -> Q [Dec]
mkRule fooName =
  let fooNameStr  = nameBase fooName
      fooName_    = mkName $ "_" ++ fooNameStr
      fooName_Str = "_" ++ fooNameStr
  in
  return $ [FunD fooName_
           [Clause
            []
            (NormalB $ AppE (AppE (ConE 'Rule) (LitE $ StringL $ fooName_Str))
                             (VarE fooName))
            []
           ]]

-- almost no difference
mkRuleS :: String -> Q [Dec]
mkRuleS fooNameStr =
  let fooName     = mkName $ fooNameStr
      fooName_    = mkName $ "_" ++ fooNameStr
      fooName_Str = "_" ++ fooNameStr
  in
  return $ [FunD fooName_
           [Clause
            []
            (NormalB $ AppE (AppE (ConE 'Rule) (LitE $ StringL $ fooName_Str))
                             (VarE fooName))
            []
           ]]


type Solution = (TuringMachine, String)
data Direction = L | R | D              -- D for down


instance Show Direction where
  show L = "<"
  show R = ">"
  show D = "^"

data TuringMachine = TuringMachine { charset :: String
                                   , states  :: S.Set String
                                   , start   :: State
                                   , accept  :: State
                                   , reject  :: State
                                   , blank   :: Char
                                   , rules   :: [Rule]
                                   }


showRules :: [Rule] -> [Char] -> IO String
showRules rules charset = do
    concat <$>
      (mapM (\rule → concat <$>
                    mapM (\char → showRule rule char)
                    charset)
      rules)

-- default value
safeRun  :: Rule -> Char ->
            IO a -> ((Rule, Char, Direction) -> IO a) ->
            IO a
safeRun Rule{..} c def handling =
  E.handle (\(e :: E.SomeException) -> def)
           (do handling =<< (E.evaluate $ ruleFoo c))

-- Exception safe!
showRule :: Rule -> Char -> IO String
showRule r@Rule{..} c = safeRun r c (return "")
               (\((Rule rule' _), chn', dir) ->
                   return $ ruleName ++ " " ++
                            [c] ++ " -> " ++
                            rule' ++ " " ++
                            [chn'] ++ " " ++
                            show dir ++ "\n")

toString :: TuringMachine -> IO String
toString TuringMachine{..} = do
    rules_s <- showRules rules charset
    return $ "start: "  ++ start   ++ "\n" ++
             "accept: " ++ accept  ++ "\n" ++
             "reject: " ++ reject  ++ "\n" ++
             "blank: "  ++ [blank] ++ "\n" ++
             rules_s

typeCheckMachine :: TuringMachine -> IO (Maybe String)
typeCheckMachine m@TuringMachine{..}
  | not $ start `S.member` states  =
    return $ Just "Start  is not in state list"
  | not $ accept `S.member` states =
    return $ Just "Accept is not in state list"
  | not $ reject `S.member` states =
    return $ Just "Reject is not in state list"
typeCheckMachine _                   = return $ Nothing

processIO :: String → (Handle → IO ()) → IO ()
processIO output handling =
  E.bracket (openFile output WriteMode) hClose handling

processSolution :: Solution → IO ()
processSolution (machine, filename) =
  processIO filename $ \to →
    do typeCheckRes <- typeCheckMachine machine
       when (isJust typeCheckRes)
            (putStrLn $ "Your machine got a problem: "
             ++ fromJust typeCheckRes)
       hPutStrLn to =<< toString machine
       processIO (filename ++ ".py") $ \topyth →
         do mtStr <- toString machine
            hPutStrLn topyth $ "output = \"\"\"" ++ mtStr ++ "\"\"\"    \n\
            \f = open(\"" ++ filename ++ "\", \"w\") \n\
            \f.write(output)                         \n\
            \f.close()                               \n\
            \exit(0)"
