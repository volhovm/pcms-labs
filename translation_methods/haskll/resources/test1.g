topLevel: NL? (f+=func NL)* f+=func NL? EOF              { Helpers.showResult($f); };

func returns [String res]
    : NAME '::' type (NL decls+=decl[$type.argnum, $NAME.text])+
                                                        { $res = Helpers.genFunc($Name.text,$decls); };

type returns [Int argnum]
    : type ('->'|'-->') t2=type               { $argnum = 1 + $t2.argnum; }
    | '(' t=type ')'                        { $argnum = $t.argnum; }
    | pureType                               { $argnum = 0;}
    ;

pureType
    : 'Int' | 'Bool' | 'Char' | '[' pureType ']' | ('aoeu' | 'kek')*;

TRUE:  SKIP /True/;
FALSE:      /False/;
INT:        /[+-]?[0-9]+/;
NL:         /[\r\n]+/;
