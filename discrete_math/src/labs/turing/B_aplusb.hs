{-# LANGUAGE UnicodeSyntax                        #-}
{-# LANGUAGE OverloadedStrings                    #-}
{-# LANGUAGE RecordWildCards                      #-}
{-# LANGUAGE TemplateHaskell                      #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures      #-}

module B_APlusB where

import           GenTuring
import qualified Data.Set  as S

-- $(mkRuleS "s")
-- $(mkRuleS "prestart")

_ac = Rule "_ac" undefined
_rej = Rule "_rej" undefined

_s  = Rule "_s" s
s a = (_prestart, a, D)

_prestart = Rule "_prestart" prestart
prestart a = (_right_to_plus_dec, a, R)

_decrement_left_pre    = Rule "_decrement_left_pre" decrement_left_pre
decrement_left_pre '0' = (_decrement_left_carry, '1', L)
decrement_left_pre '1' = (_right_to_plus,        '0', R)

_decrement_left_carry    = Rule "_decrement_left_carry" decrement_left_carry
decrement_left_carry '1' = (_right_to_plus,        '0', R)
decrement_left_carry '0' = (_decrement_left_carry, '1', L)
decrement_left_carry '_' = (_right_to_plus_finish, '_', R)

_right_to_plus_dec    = Rule "_right_to_plus_dec" right_to_plus_dec
right_to_plus_dec '+' = (_decrement_left_pre, '+', L)
right_to_plus_dec a   = (_right_to_plus_dec,  a,   R)

_right_to_plus    = Rule "_right_to_plus" right_to_plus
right_to_plus '+' = (_increment_right , '+', R)
right_to_plus a   = (_right_to_plus   , a,   R)

_increment_right    = Rule "_increment_right" increment_right
increment_right '_' = (_increment_left_pre, '_', L)
increment_right a   = (_increment_right,    a,   R)

_increment_left_pre    = Rule "_increment_left_pre" increment_left_pre
increment_left_pre '0' = (_left_to_plus,         '1', L)
increment_left_pre '1' = (_increment_left_carry, '0', L)

_increment_left_carry = Rule "_increment_left_carry" increment_left_carry
increment_left_carry '0' = (_left_to_plus, '1', L)
increment_left_carry '1' = (_increment_left_carry, '0', L)
increment_left_carry '+' = (_rmove_right_pre, '+', R)

_rmove_right_pre = Rule "_rmove_right_pre" rmove_right_pre
rmove_right_pre _ = (_rmove_right_blanking, '1', R)

_rmove_right_blanking = Rule "_rmove_right_blanking" rmove_right_blanking
rmove_right_blanking '_' = (_left_to_plus, '0', L)
rmove_right_blanking _   = (_rmove_right_blanking, '0', R)

_left_to_plus    = Rule "_left_to_plus" left_to_plus
left_to_plus '0' = (_left_to_plus, '0', L)
left_to_plus '1' = (_left_to_plus, '1', L)
left_to_plus '+' = (_decrement_left_pre, '+', L)

_right_to_plus_finish  = Rule "_right_to_plus_finish" right_to_plus_finish
right_to_plus_finish '+'= (_ac,                  '_',  R)
right_to_plus_finish _  = (_right_to_plus_finish, '_', R)

rules = [ _s
        , _prestart
        , _decrement_left_pre
        , _decrement_left_carry
        , _right_to_plus_dec
        , _right_to_plus
        , _increment_right
        , _increment_left_pre
        , _increment_left_carry
        , _rmove_right_pre
        , _rmove_right_blanking
        , _left_to_plus
        , _right_to_plus_finish
        ]

solutionB =
    TuringMachine { rules = B_APlusB.rules, .. }
    where
      charset = "01_+"
      states  = (S.fromList ["_rj", "_ac"])
                `S.union`
                (S.fromList $ map ruleName B_APlusB.rules)
      start   = "_s"
      reject  = "_rj"
      accept  = "_ac"
      blank   = '_'

main = processSolution (solutionB, "aplusb.out")
