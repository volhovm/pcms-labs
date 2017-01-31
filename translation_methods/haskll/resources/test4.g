grammar Arithmetic;

@imports {
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Maybe (fromJust)
}

@members {
foo :: a
foo = undefined
}

start returns [Int val]: sum     { let val = sum };

sum returns [Int val]
    : mult sum1                  { let val = mult + sum1 };
sum1 returns [Int val]
    : PLUS mult sum1             { let val = mult + sum1 }
    | EPSILON                    { let val = 0 }
    ;
mult returns [Int val]
    : single mult1               { let val = single * mult1 };
mult1 returns [Int val]
    : MULT single kek=mult1      { let val = single * kek }
    | EPSILON                    { let val = 1 }
    ;
single returns [Int val]
    : NUM                        { let val = readTextUnsafe $ tokenText tokenNUM
                                   let valkek = undefined }
    | PARENL sum PARENR          { let val = sum }
    ;

WHITESPACE: SKIP /[ \t\n]+/;
NUM:             /\d+/;
PLUS:            /\+/;
MULT:            /\*/;
PARENL:          /\(/;
PARENR:          /\)/;
