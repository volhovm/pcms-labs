func : pureType '->' pureType2;

pureType
    : 'Int' | 'Bool' | 'Char' | '[' pureType ']';

TRUE:  SKIP /True/;
FALSE:      /False/;
INT:        /[+-]?[0-9]+/;
NL:         /[\r\n]+/;
