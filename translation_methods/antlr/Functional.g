grammar Functional;

topLevel: Nl? (f+=func Nl)* f+=func Nl? EOF              { Helpers.showResult($f); };

func returns [String res]
    : Name '::' type (Nl decls+=decl[$type.argnum, $Name.text])+
                                                         { $res = Helpers.genFunc($Name.text,$decls); };

// Types
type returns [int argnum]
    : <assoc=right> type ('->'|'→') t2=type              { $argnum = 1 + $t2.argnum; }
    | '(' t=type ')'                                     { $argnum = $t.argnum; }
    | pureType                                           { $argnum = 0;}
    ;

pureType
    : 'Int' | 'Bool' | 'Char' | '[' pureType ']';

// Terms
decl[int argnum, String fooname] returns [String res]
    : {$fooname.equals(getCurrentToken().getText())}? Name args '|' Nl? cond=term '=' Nl? e1=retterm
                                                         { Helpers.checkArgs($fooname,$argnum,$args.argnum);
                                                           $res = Helpers.genDecl($args.res,$cond.res,$e1.res); }
    | {$fooname.equals(getCurrentToken().getText())}? Name args '=' Nl? e2=retterm
                                                         { Helpers.checkArgs($fooname,$argnum,$args.argnum);
                                                           $res = Helpers.genDecl($args.res,$e2.res); }
    ;

args returns [List<String> res, int argnum]
    : (a+=Hole | a+=Name)*                               { $argnum = $a.size(); $res = Helpers.genArgs($a); };

retterm returns [String res, boolean ret]
    : t=term                                             { if (!$t.ret) { $res = "return " + $t.res; $ret = true; }
                                                           else { $res = $t.res; $ret = false; } };

term returns [String res, boolean ret]
    : ('let' Nl? (f+=func Nl)* f+=func Nl?) 'in'<assoc=right> (Nl? rt=retterm)
                                                         { $res = Helpers.genScoped($rt.res,$f); $ret = $rt.ret; }
    | 'if' t1=term Nl? 'then' rt2=retterm Nl? 'else' rt3=retterm
                                                         { $res = Helpers.genIfThenElse($t1.res,$rt2.res,$rt3.res); $ret = ($rt2.ret || $rt3.ret); }
    | t=term (ts+=term)+                                 { $res = Helpers.genApplication($t.res,$ts); $ret = false; }
    | t1=term Infix t2=term                              { $res = Helpers.genInfix($Infix.text,$t1.res,$t2.res); $ret = false; }
    | '(' t=term ')'                                     { $res = "(" + $t.res + ")"; $ret = $t.ret; }
    | '[' Nl? (Nl? e+=term Nl? ',')* Nl? e+=term Nl? ']' { $res = Helpers.genListConstr($e); $ret = false; }
    | n=Name                                             { $res = $n.text.trim(); $ret = false; }
    | primValue                                          { $res = $primValue.res; $ret = false; }
    ;

primValue returns [String res]
    : prim = (Int | Charlit | Strlit | True | False)     { $res = $prim.text; };

Whitespace: [ \t]+ -> skip;
Hole:       '_';
True:       'True';
False:      'False';
Int:        [+-]?[0-9]+;
Infix:      ([\+\-\*\<\>\^] | '==' | '/=' | '<=' | '>=');
Name:       [a-z][a-zA-Z\'_0-9]*;
Charlit:    [\'][a-zA-Z0-9]?[\'];
Strlit:     [\"][a-zA-Z0-9]*[\"];
Nl:         [\r\n]+;
