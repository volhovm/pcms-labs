{-# LANGUAGE UnicodeSyntax                        #-}
{-# LANGUAGE OverloadedStrings                    #-}
{-# LANGUAGE RecordWildCards                      #-}
{-# LANGUAGE TemplateHaskell                      #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures      #-}

module L_Factorial where

import           GenTuring
import qualified Data.Set  as S

_ac = Rule "_ac" undefined
_rej = Rule "_rej" undefined

_s  = Rule "_s" s

rules = [ _s
        ]

solutionB =
    TuringMachine { rules = L_Factorial.rules, .. }
    where
      charset = "012_+"
      states  = (S.fromList ["_rj", "_ac"])
                `S.union`
                (S.fromList $ map ruleName L_Factorial.rules)
      start   = "_s"
      reject  = "_rj"
      accept  = "_ac"
      blank   = '_'

main = processSolution (solutionB, "convertto2.out")
