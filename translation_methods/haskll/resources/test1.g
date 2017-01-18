func : pureType '->' (decls+=pureType);

topLevel: Nl? (f+=func Nl)* f+=func Nl? EOF              { Helpers.showResult($f); };

pureType
    : 'Int' | 'Bool' | 'Char' | '[' pureType ']' | ('aoeu' | 'kek')*;

TRUE:  SKIP /True/;
FALSE:      /False/;
INT:        /[+-]?[0-9]+/;
NL:         /[\r\n]+/;
