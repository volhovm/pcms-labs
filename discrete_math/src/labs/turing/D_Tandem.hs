{-# LANGUAGE UnicodeSyntax                        #-}
{-# LANGUAGE OverloadedStrings                    #-}
{-# LANGUAGE RecordWildCards                      #-}
{-# LANGUAGE TemplateHaskell                      #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures      #-}

module D_Tandem where

import           GenTuring
import qualified Data.Set  as S

_ac = Rule "_ac" undefined
_rej = Rule "_rej" undefined

_s  = Rule "_s" s
s a = (_add_plus_toend, a, D)

rules = [ _s ]

solutionB =
    TuringMachine { rules = D_Tandem.rules, .. }
    where
      charset = "012_+"
      states  = (S.fromList ["_rj", "_ac"])
                `S.union`
                (S.fromList $ map ruleName D_Tandem.rules)
      start   = "_s"
      reject  = "_rj"
      accept  = "_ac"
      blank   = '_'

main = processSolution (solutionB, "convertto2.out")
