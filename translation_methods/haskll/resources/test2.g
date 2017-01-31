grammar Functional;

@members {

genFunc :: Text -> Int -> [Text] -> Text
genFunc fooname argSize decls =
    "def " <> fooname <> "(" <>
    T.intercalate ", " (map (\i -> "_" <> show i) [0 .. argSize - 1]) <>
    "):\n" <> T.intercalate "\n" decls

genDecl :: [Text] -> Text -> Text
genDecl args mainterm =
    T.intercalate "\n" bindings <> "\n" <> mainterm
  where
    bindings = map (\(a,i) -> a <> " = _" <> show i) $
               args `zip` [0..length args - 1]

}

start: NL? manyFunc func NL? EOF   { putText $ T.intercalate "\n" manyFunc <> "\n" <> func };

manyFunc returns [[Text] res]
    : func NL manyFunc            { let res = func : manyFunc }
    | EPSILON                     { let res = [] }
    ;

func returns [Text res]
    : NAME DOUBLECOLON functype NL { let fooname = tokenName tokenNAME
                                     let argsnum = functype - 1 }
      decl[argsnum fooname]
      decls[argsnum fooname]      { let res = genFunc fooname functype (decl:decls) }
    ;

functype returns [Int argnum]
    : pureType functypeCont   { let argnum = 1 + functypeCont }
    | PARENL functype PARENR  { let argnum = functype }
    ;

functypeCont returns [Int argnum]
    : ARROW functype          { let argnum = functype }
    | EPSILON                 { let argnum = 0 }
    ;

pureType: PURETYPE | PARENSQL pureType PARENSQR;

decls [Int argnum, Text fooname] returns [[Text] res]
    : decl[argnum fooname] NL decls[argnum fooname] { let res = decl : decls}
    | EPSILON                                       { let res = [] }
    ;

holeorname returns [Text res]
    : HOLE { let res = tokenName tokenHOLE }
    | NAME { let res = tokenName tokenNAME }
    ;

args returns [[Text] res]
    : holeorname args { let res = holeorname : args }
    | EPSILON         { let res = [] }
    ;

decl[Int argnum, Text fooname] returns [Text res]
    : NAME args EQUALS NL? HOLE
    { when (length args /= argnum) $ panic $ "decl: number of args in type " <> show argnum <> " doesn't match number of args in decl " <> show (length args)
      when (tokenName tokenNAME /= fooname) $ panic "decl: fooname doesn't match"
      let res = genDecl args "" }
    ;


// // Terms
// decl[Int argnum, String fooname] returns [String res]
// //    : {$fooname.equals(getCurrentToken().getText())}? NAME args '|' NL? {freeVars.addAll($args.res);} cond=term '=' NL? e1=retterm
//     : NAME args VERTBAR NL? cond=term EQUALS NL? e1=retterm     { freeVars.addAll($args.res);
//                                                            Helpers.checkArgs($fooname,$argnum,$args.argnum);
//                                                            $res = Helpers.genDecl($args.res,$cond.res,$e1.res);
//                                                            freeVars.removeAll($args.res); }
//     | NAME args EQUALS NL? e2=retterm
// //    | {$fooname.equals(getCurrentToken().getText())}? NAME args '=' NL? {freeVars.addAll($args.res);} e2=retterm
//                                                          { freeVars.addAll($args.res);
//                                                            Helpers.checkArgs($fooname,$argnum,$args.argnum);
//                                                            $res = Helpers.genDecl($args.res,$e2.res);
//                                                            freeVars.removeAll($args.res); }
//     ;
//


// retterm returns [String res, Bool ret]
//     : t=term                                             { if (!$t.ret) { $res = "return " + $t.res; $ret = true; }
//                                                            else { $res = $t.res; $ret = false; } };
//
// term returns [String res, Bool ret]
//     : IF t1=term NL? THEN rt2=retterm NL? ELSE rt3=retterm
//                                                          { $res = Helpers.genIfThenElse($t1.res,$rt2.res,$rt3.res); $ret = ($rt2.ret || $rt3.ret); }
//     | t1=term INFIX t2=term                              { $res = Helpers.genINFIX($INFIX.text,$t1.res,$t2.res); $ret = false; }
//     | PARENL t=term PARENR                               { $res = "(" + $t.res + ")"; $ret = $t.ret; }
//     | LET NL? (f+=func NL)* f+=func NL? IN (NL? rt=retterm)
//                                                          { $res = Helpers.genScoped($rt.res,$f); $ret = $rt.ret; }
//     | primValue                                          { $res = $primValue.res; $ret = false; }
// //    | n=NAME {asArgs = true;} (ts+=primValue)+
//     | n=NAME (ts+=primValue)+                            { asArgs = true;
//                                                            $res = Helpers.genApplication($n.text,$ts);
//                                                            $ret = false; asArgs = false; }
//     ;
//
// primValue returns [String res]
//     : prim=(INTLIT | CHARLIT | STRLIT | TRUE | FALSE)       { $res = $prim.text; }
//     | n=NAME                                             { $res = Helpers.genFuncOrVar($n.text,freeVars,asArgs); }
//     | PARENSQL NL? (NL? e+=primValue NL? COMMA)* NL? e+=primValue NL? PARENSQR
//                                                          { $res = Helpers.genListConstr($e); }
//     ;

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
HOLE:            /hole/;
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
