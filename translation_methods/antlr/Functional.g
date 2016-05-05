grammar Functional;

topLevel: (func NEWLINE)* func EOF;
func: NAME '::' polytype NEWLINE (decl)+;
funcname: NAME;

// Types
polytype: ('forall' | '∀') typevar '.' (polytype | monotype);
monotype
    : monotype ('->'|'→') pureType
    | '(' monotype ')'
    | pureType;
pureType: typevar | primType;
primType: ('Int' | 'Bool' | 'Char' | '[' pureType ']');
typevar: NAME;

// Terms
decl: funcname args ('|' condition)? '=' term;
args: (BOT | NAME)*;
term
    : 'let' (decl NEWLINE)* decl 'in'
    | term 'where' (decl NEWLINE)* decl
    | 'if' condition 'then' term 'else' term
    | term term
    | '(' term ')'
    | funcname
    | '[' (primValue ',')* primValue ']'
    | primValue
    ;

condition
    : condition ('&&' | '||' | '~>') condition
    | ('true' | 'false')
    | intTerm ('==' | '/=' | '<' | '>' | '<=' | '>=') intTerm
    ;

intTerm
    : intTerm ('*'|'+'|'-') intTerm
    | INT
    ;

primValue returns [String s]
    : prim = (INT | CHARLIT | STRLIT) { $s = $prim.text; };

// Order is important (omg)
BOT:        '_';
INT:        [+-]?[0-9]+;
NAME:       [a-z][a-zA-Z\'_0-9]*;
CHARLIT:    [\'][a-zA-Z0-9]?[\'];
STRLIT:     [\"][a-zA-Z0-9]*[\"];
NEWLINE:    [\r\n]+;
WHITESPACE: [ \t\r\n]+ -> skip;
