grammar Functional;

topLevel: (func NEWLINE)* func EOF;
func: NAME '::' polytype NEWLINE (decl)+;
funcname: NAME;

polytype: ('forall' | '∀') typevar '.' (polytype | monotype);
monotype
    : monotype ('->'|'→') pureType
    | '(' monotype ')'
    | pureType;
pureType
    : typevar
    | primType;
primType: ('Int' | 'Bool' | 'Char' | '[' pureType ']');
typevar: NAME;

decl: funcname args '=' term;
args: ('_' | NAME)*;
term
    : primValue
    | 'let' (decl NEWLINE)* decl 'in'
    | term 'where' (decl NEWLINE)* decl
    | term term
    | '(' term ')'
    | funcname;
primValue: INT | CHARLIT | STRLIT;

// Order is important (omg)
BOT:        '_';
INT:        [+-]?[0-9]+;
NAME:       [a-z][a-zA-Z\'_0-9]*;
CHARLIT:    [\'][a-zA-Z0-9]?[\'];
STRLIT:     [\"][a-zA-Z0-9]*[\"];
NEWLINE:    [\r\n]+;
WHITESPACE: [ \t\r\n]+ -> skip;
