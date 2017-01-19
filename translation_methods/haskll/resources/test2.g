grammar Functional;

@members {
  , freeVars :: [String]
  , asArgs   :: Bool
}

topLevel: NL? (f+=func NL)* f+=func NL? EOF              { Helpers.showResult($f); };

func returns [String res]
    : NAME DOUBLECOLON type (NL decls+=decl[$type.argnum, $NAME.text])+
                                                         { $res = Helpers.genFunc($NAME.text,$decls); };

// Types
type returns [Int argnum]
    : pureType ARROW t2=type                             { $argnum = 1 + $t2.argnum; }
    | PARENL t=type PARENR                               { $argnum = $t.argnum; }
    | pureType                                           { $argnum = 0;}
    ;

pureType: PURETYPE | PARENSQL pureType PARENSQR;

// Terms
decl[Int argnum, String fooname] returns [String res]
//    : {$fooname.equals(getCurrentToken().getText())}? NAME args '|' NL? {freeVars.addAll($args.res);} cond=term '=' NL? e1=retterm
    : NAME args VERTBAR NL? cond=term EQUALS NL? e1=retterm     { freeVars.addAll($args.res);
                                                           Helpers.checkArgs($fooname,$argnum,$args.argnum);
                                                           $res = Helpers.genDecl($args.res,$cond.res,$e1.res);
                                                           freeVars.removeAll($args.res); }
    | NAME args EQUALS NL? e2=retterm
//    | {$fooname.equals(getCurrentToken().getText())}? NAME args '=' NL? {freeVars.addAll($args.res);} e2=retterm
                                                         { freeVars.addAll($args.res);
                                                           Helpers.checkArgs($fooname,$argnum,$args.argnum);
                                                           $res = Helpers.genDecl($args.res,$e2.res);
                                                           freeVars.removeAll($args.res); }
    ;

args returns [[String] res, Int argnum]
    : (a+=HOLE | a+=NAME)*                               { $argnum = $a.size(); $res = Helpers.genArgs($a); };

retterm returns [String res, Bool ret]
    : t=term                                             { if (!$t.ret) { $res = "return " + $t.res; $ret = true; }
                                                           else { $res = $t.res; $ret = false; } };

term returns [String res, Bool ret]
    : IF t1=term NL? THEN rt2=retterm NL? ELSE rt3=retterm
                                                         { $res = Helpers.genIfThenElse($t1.res,$rt2.res,$rt3.res); $ret = ($rt2.ret || $rt3.ret); }
    | t1=term INFIX t2=term                              { $res = Helpers.genINFIX($INFIX.text,$t1.res,$t2.res); $ret = false; }
    | PARENL t=term PARENR                               { $res = "(" + $t.res + ")"; $ret = $t.ret; }
    | (LET NL? (f+=func NL)* f+=func NL?) IN (NL? rt=retterm)
                                                         { $res = Helpers.genScoped($rt.res,$f); $ret = $rt.ret; }
    | primValue                                          { $res = $primValue.res; $ret = false; }
//    | n=NAME {asArgs = true;} (ts+=primValue)+
    | n=NAME (ts+=primValue)+                            { asArgs = true;
                                                           $res = Helpers.genApplication($n.text,$ts);
                                                           $ret = false; asArgs = false; }
    ;

primValue returns [String res]
    : prim=(INTLIT | CHARLIT | STRLIT | TRUE | FALSE)       { $res = $prim.text; }
    | n=NAME                                             { $res = Helpers.genFuncOrVar($n.text,freeVars,asArgs); }
    | PARENSQL NL? (NL? e+=primValue NL? COMMA)* NL? e+=primValue NL? PARENSQR
                                                         { $res = Helpers.genListConstr($e); }
    ;

WHITESPACE: SKIP /[ \t]+/;
HOLE:            /_/;
TRUE:            /True/;
FALSE:           /False/;
ARROW:           /->|→/;
DOUBLECOLON:     /::/;
PURETYPE:        /(Int|Bool|Char)/;
VERTBAR:         /\|/;
EQUALS:          /=/;
COMMA:           /,/;
LET:             /let/;
IF:              /if/;
THEN:            /then/;
ELSE:            /else/;
PARENL:          /\(/;
PARENR:          /\)/;
PARENSQL:        /\[/;
PARENSQR:        /\]/;
INTLIT:          /[+-]?[0-9]+/;
INFIX:           /([\+\-\*\<\>\^\/] | '==' | '/=' | '<=' | '>=')/;
NAME:            /[a-z][a-zA-Z\'_0-9]*/;
CHARLIT:         /[\'][a-zA-Z0-9]?[\']/; // test comment
STRLIT:          /[\"][a-zA-Z0-9]*[\"]/;
NL:              /[\r\n]+/;
