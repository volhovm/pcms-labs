grammar Arithmetic;

@imports {
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
}

sum: mult sum1;
sum1: (PLUS mult sum1)?;
mult: single mult1;
mult1: (MULT single mult1)?;
single: NUM | PARENL sum PARENR;

WHITESPACE: SKIP /[ \t]+/;
NUM:             /\d+/;
PLUS:            /\+/;
MULT:            /\*/;
PARENL:          /\(/;
PARENR:          /\)/;
