{-# LANGUAGE UnicodeSyntax                        #-}
{-# LANGUAGE OverloadedStrings                    #-}
{-# LANGUAGE RecordWildCards                      #-}
{-# LANGUAGE TemplateHaskell                      #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures      #-}

module H_ConvertTo2 where

import           GenTuring
import qualified Data.Set  as S

_ac = Rule "_ac" undefined
_rej = Rule "_rej" undefined

_s  = Rule "_s" s
s a = (_add_plus_toend, a, D)

_add_plus_toend    = Rule "_add_plus_toend" add_plus_toend
add_plus_toend '_' = (_add_zero_toend, '+', R)
add_plus_toend  a  = (_add_plus_toend, a, R)

_add_zero_toend    = Rule "_add_zero_toend" add_zero_toend
add_zero_toend '_' = (_left_to_plus, '0', L)

_decrement_left_pre    = Rule "_decrement_left_pre" decrement_left_pre
decrement_left_pre '2' = (_right_to_plus,        '1', R)
decrement_left_pre '1' = (_right_to_plus,        '0', R)
decrement_left_pre '0' = (_decrement_left_carry, '2', L)

_decrement_left_carry    = Rule "_decrement_left_carry" decrement_left_carry
decrement_left_carry '2' = (_right_to_plus,        '1', R)
decrement_left_carry '1' = (_right_to_plus,        '0', R)
decrement_left_carry '0' = (_decrement_left_carry, '2', L)
decrement_left_carry '_' = (_right_to_plus_finish, '_', R)

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
left_to_plus '+' = (_decrement_left_pre, '+', L)
left_to_plus a   = (_left_to_plus, a, L)

_right_to_plus_finish  = Rule "_right_to_plus_finish" right_to_plus_finish
right_to_plus_finish '+'= (_ac,                  '_',  R)
right_to_plus_finish _  = (_right_to_plus_finish, '_', R)

rules = [ _s
        , _add_plus_toend
        , _add_zero_toend
        , _decrement_left_pre
        , _decrement_left_carry
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
    TuringMachine { rules = H_ConvertTo2.rules, .. }
    where
      charset = "012_+"
      states  = (S.fromList ["_rj", "_ac"])
                `S.union`
                (S.fromList $ map ruleName H_ConvertTo2.rules)
      start   = "_s"
      reject  = "_rj"
      accept  = "_ac"
      blank   = '_'

main = processSolution (solutionB, "convertto2.out")
