grammar Functional;

topLevel
    : (func NEWLINE)* func EOF;
func
    : NAME '::' polytype NEWLINE (decl)+;
funcname
    : NAME;

polytype
    : ('forall' | '∀') typevar '.' (polytype | monotype);
monotype
    : monotype ('->'|'→') pureType
    | '(' monotype ')'
    | pureType;
pureType
    : typevar
    | primType;
primType
    : ('Int' | 'Bool' | 'Char' | '[' pureType ']');
typevar
    : NAME;

decl
    : funcname args '=' NAME;
args
    : ('_' | ARGNAME)*;
term
    : primValue
    | 'let' NAME '=' term 'in'
    | term 'where' NAME '=' term;
primValue
    : INT | '\'' (CHAR)? '\'' | '\"' (CHAR)* '\"';

BOT:        '_';
ARGNAME:    [a-z][a-zA-Z\'_0-9]+;
//NAME:       [a-z_][a-zA-Z\'_0-9]+;
NAME:       [a-zA-Z0-9]+;
CHAR:       [a-zA-Z0-9];
INT:        ('-'|'+')?[0-9];
NEWLINE:    [\r\n]+;
WHITESPACE: [ \t\r\n]+ -> skip;
