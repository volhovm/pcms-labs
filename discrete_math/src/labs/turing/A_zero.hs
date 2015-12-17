{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE OverloadedStrings   #-}

import qualified Data.ByteString.Char8  as     BS
import           Control.Exception.Base
import           System.IO

outputFile = "zero.out"

processIO :: (Handle → IO()) → IO()
processIO handling =
  bracket (openFile outputFile WriteMode)
          (hClose)
          handling

main = processIO $ \s → BS.hPutStrLn s solution

solution :: BS.ByteString
solution = "\
\start: s\n\
\accept: ac\n\
\reject: rj\n\
\blank: _\n\
\s _ -> ac _ ^\n\
\s 0 -> n _ >\n\
\n 0 -> s _ >\n\
\n _ -> rj _ >\
\"
