grammar Arithmetic;

@imports {
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
}

start: sum;

sum: mult sum1;
sum1: PLUS mult sum1 | EPSILON;
mult: single mult1;
mult1: MULT single mult1 | EPSILON;
single: NUM | PARENL sum PARENR;

WHITESPACE: SKIP /[ \t\n]+/;
NUM:             /\d+/;
PLUS:            /\+/;
MULT:            /\*/;
PARENL:          /\(/;
PARENR:          /\)/;
