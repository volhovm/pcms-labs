output = """start: _s
accept: _ac
reject: _rj
blank: _
_s 0 -> _prestart 0 ^
_s 1 -> _prestart 1 ^
_s _ -> _prestart _ ^
_s + -> _prestart + ^
_prestart 0 -> _right_to_plus_dec 0 >
_prestart 1 -> _right_to_plus_dec 1 >
_prestart _ -> _right_to_plus_dec _ >
_prestart + -> _right_to_plus_dec + >
_decrement_left_pre 0 -> _decrement_left_carry 1 <
_decrement_left_pre 1 -> _right_to_plus 0 >
_decrement_left_carry 0 -> _decrement_left_carry 1 <
_decrement_left_carry 1 -> _right_to_plus 0 >
_decrement_left_carry _ -> _right_to_plus_finish _ >
_right_to_plus_dec 0 -> _right_to_plus_dec 0 >
_right_to_plus_dec 1 -> _right_to_plus_dec 1 >
_right_to_plus_dec _ -> _right_to_plus_dec _ >
_right_to_plus_dec + -> _decrement_left_pre + <
_right_to_plus 0 -> _right_to_plus 0 >
_right_to_plus 1 -> _right_to_plus 1 >
_right_to_plus _ -> _right_to_plus _ >
_right_to_plus + -> _increment_right + >
_increment_right 0 -> _increment_right 0 >
_increment_right 1 -> _increment_right 1 >
_increment_right _ -> _increment_left_pre _ <
_increment_right + -> _increment_right + >
_increment_left_pre 0 -> _left_to_plus 1 <
_increment_left_pre 1 -> _increment_left_carry 0 <
_increment_left_carry 0 -> _left_to_plus 1 <
_increment_left_carry 1 -> _increment_left_carry 0 <
_increment_left_carry + -> _rmove_right_pre + >
_rmove_right_pre 0 -> _rmove_right_blanking 1 >
_rmove_right_pre 1 -> _rmove_right_blanking 1 >
_rmove_right_pre _ -> _rmove_right_blanking 1 >
_rmove_right_pre + -> _rmove_right_blanking 1 >
_rmove_right_blanking 0 -> _rmove_right_blanking 0 >
_rmove_right_blanking 1 -> _rmove_right_blanking 0 >
_rmove_right_blanking _ -> _left_to_plus 0 <
_rmove_right_blanking + -> _rmove_right_blanking 0 >
_left_to_plus 0 -> _left_to_plus 0 <
_left_to_plus 1 -> _left_to_plus 1 <
_left_to_plus + -> _decrement_left_pre + <
_right_to_plus_finish 0 -> _right_to_plus_finish _ >
_right_to_plus_finish 1 -> _right_to_plus_finish _ >
_right_to_plus_finish _ -> _right_to_plus_finish _ >
_right_to_plus_finish + -> _ac _ >
"""    
f = open("aplusb.out", "w") 
f.write(output)                         
f.close()                               
exit(0)
