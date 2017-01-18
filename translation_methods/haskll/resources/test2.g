grammar Functional;

@members {
  , freeVars :: [String]
  , asArgs   :: Bool
}

topLevel: NL? (f+=func NL)* f+=func NL? EOF              { Helpers.showResult($f); };

func returns [String res]
    : NAME '::' type (NL decls+=decl[$type.argnum, $NAME.text])+
                                                         { $res = Helpers.genFunc($NAME.text,$decls); };

// Types
type returns [Int argnum]
    : pureType ('->'|'→') t2=type                        { $argnum = 1 + $t2.argnum; }
    | '(' t=type ')'                                     { $argnum = $t.argnum; }
    | pureType                                           { $argnum = 0;}
    ;

pureType
    : 'Int' | 'Bool' | 'Char' | '[' pureType ']';

// Terms
decl[Int argnum, String fooname] returns [String res]
//    : {$fooname.equals(getCurrentToken().getText())}? NAME args '|' NL? {freeVars.addAll($args.res);} cond=term '=' NL? e1=retterm
    : NAME args '|' NL? cond=term '=' NL? e1=retterm  { freeVars.addAll($args.res);
                                                           Helpers.checkArgs($fooname,$argnum,$args.argnum);
                                                           $res = Helpers.genDecl($args.res,$cond.res,$e1.res);
                                                           freeVars.removeAll($args.res); }
    | NAME args '=' NL? e2=retterm
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
    : 'if' t1=term NL? 'then' rt2=retterm NL? 'else' rt3=retterm
                                                         { $res = Helpers.genIfThenElse($t1.res,$rt2.res,$rt3.res); $ret = ($rt2.ret || $rt3.ret); }
    | t1=term INFIX t2=term                              { $res = Helpers.genINFIX($INFIX.text,$t1.res,$t2.res); $ret = false; }
    | '(' t=term ')'                                     { $res = "(" + $t.res + ")"; $ret = $t.ret; }
    | ('let' NL? (f+=func NL)* f+=func NL?) 'in' (NL? rt=retterm)
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
    | '[' NL? (NL? e+=primValue NL? ',')* NL? e+=primValue NL? ']'
                                                         { $res = Helpers.genListConstr($e); }
    ;

WHITESPACE: SKIP /[ \t]+/;
HOLE:            /_/;
TRUE:            /True/;
FALSE:           /False/;
INTLIT:             /[+-]?[0-9]+/;
INFIX:           /([\+\-\*\<\>\^\/] | '==' | '/=' | '<=' | '>=')/;
NAME:            /[a-z][a-zA-Z\'_0-9]*/;
CHARLIT:         /[\'][a-zA-Z0-9]?[\']/;
STRLIT:          /[\"][a-zA-Z0-9]*[\"]/;
NL:              /[\r\n]+/;
