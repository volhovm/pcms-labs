grammar Functional;

@members {

genFunc :: Text -> Int -> [Text] -> Text
genFunc fooname argSize decls =
    "def " <> fooname <> "(" <>
    T.intercalate ", " (map (\i -> "_" <> show i) [0 .. argSize - 1]) <>
    "):\n" <> T.intercalate "\n" decls

genDecl :: [Text] -> Text -> Text -> Text
genDecl args grd mainterm =
    bindingsNL <>
    if T.null grd
    then T.intercalate "\n" (map ("  "<>) $ T.lines mainterm)
    else "  if " <> grd <> ":\n" <>
         T.intercalate "\n" (map ("    "<>) $ T.lines mainterm)
  where
    bindingsNL = T.concat (map (<>"\n") bindings)
    bindings = map (\(a,i) -> "  def " <> a <> "(): return _" <> show i) $
               args `zip` [0..length args - 1]

convertInfix :: Text -> Text -> Text -> Text
convertInfix "/=" a b    = a <> " != " <> b
convertInfix "`div`" a b = a <> " // " <> b
convertInfix "`mod`" a b = a <> " % " <> b
convertInfix pseudo a b
    | "`" `T.isPrefixOf` pseudo =
    (T.drop 1 $ T.dropEnd 1 $ pseudo) <> "(" <> a <> ", " <> b <> ")"
convertInfix infx a b = T.intercalate " " $ [a, infx, b]

}

start returns [Text res]
    : NL* func NL? manyFunc NL* EOF
    { let res = func <> "\n\n" <> T.intercalate "\n\n" manyFunc };

manyFunc returns [[Text] res]
    : func NL? manyFunc { let res = func : manyFunc }
    | EPSILON           { let res = [] }
    ;

func returns [Text res]
    : NAME DOUBLECOLON functype NL { let fooname = tokenText tokenNAME
                                     let argsnum = functype - 1 }
      decl[argsnum fooname] NL?
      decls[argsnum fooname]       { let res = genFunc fooname argsnum (decl:decls) }
    ;

functype returns [Int argnum]
    : pureType functypeCont   { let argnum = 1 + functypeCont }
    | PARENL functype PARENR  { let argnum = functype }
    ;

functypeCont returns [Int argnum]
    : ARROW functype { let argnum = functype }
    | EPSILON        { let argnum = 0 }
    ;

pureType: PURETYPE | PARENSQL pureType PARENSQR;

decls [Int argnum, Text fooname] returns [[Text] res]
    : decl[argnum fooname] NL? decls[argnum fooname] { let res = decl : decls}
    | EPSILON                                        { let res = [] }
    ;

holeorname returns [Text res]
    : HOLE { let res = tokenText tokenHOLE }
    | NAME { let res = tokenText tokenNAME }
    ;

args returns [[Text] res]
    : holeorname args { let res = holeorname : args }
    | EPSILON         { let res = [] }
    ;

decl[Int argnum, Text fooname] returns [Text res]
    : NAME args guard EQUALS NL? retterm
    { when (length args /= argnum) $ panic $ "decl: number of args in type " <> show argnum <> " doesn't match number of args in decl " <> show (length args)
      when (tokenText tokenNAME /= fooname) $ panic "decl: fooname doesn't match"
      let res = genDecl args guard retterm }
    ;

guard returns [Text res]
    : VERTBAR term { let res = fst term }
    | EPSILON      { let res = "" }
    ;

retterm returns [Text res]
    : term { let res = if not (snd term) then "return " <> fst term else fst term }
    ;

term returns [Text res, Bool ret]
    : basicTerm termInfix
        { let ret = snd basicTerm
          let res = if null termInfix then fst basicTerm else foldl (\c (infx,b) -> convertInfix infx c b) (fst basicTerm) termInfix }
    ;

termInfix returns [[(Text,Text)] terms]
    : INFIX basicTerm termInfix { let terms = (tokenText tokenINFIX, fst basicTerm):termInfix }
    | EPSILON                   { let terms = [] }
    ;

basicTerm returns [Text res, Bool ret]
//    : IF t1=term NL? THEN rt2=retterm NL? ELSE rt3=retterm
//      { let res = "if (" <>$res = Helpers.genIfThenElse($t1.res,$rt2.res,$rt3.res); $ret = ($rt2.ret || $rt3.ret); }
    : PARENL term PARENR              { let res = "(" <> fst term <> ")"
                                        let ret = snd term }
    | LET NL? func NL? IN NL? retterm { let ret = True
                                        let res = func <> "\n" <> retterm }
    | primToken                       { let ret = False
                                        let res = primToken }
    | NAME appArgs                    { let ret = False
                                        let res = tokenText tokenNAME <> "(" <> T.intercalate ", " appArgs <> ")" }
    ;

appArgs returns [[Text] res]
    : NAME appArgs                    { let res = (tokenText tokenNAME <> "()") : appArgs }
    | PARENL term PARENR              { let res = ["(" <> fst term <> ")"] }
    | EPSILON                         { let res = [] }
    ;

primToken returns [Text res]
    : INTLIT  { let res = tokenText tokenINTLIT }
    | CHARLIT { let res = tokenText tokenCHARLIT }
    | STRLIT  { let res = tokenText tokenSTRLIT }
    | TRUE    { let res = "True" }
    | FALSE   { let res = "False" }
    ;

WHITESPACE: SKIP /[ \t]+/;
COMMENT:    SKIP /--[^\n]*/;
HOLE:            /_/;
TRUE:            /True/;
FALSE:           /False/;
ARROW:           /->|→/;
DOUBLECOLON:     /::/;
PURETYPE:        /(Int|Bool|Char|String|Text)/;
VERTBAR:         /\|/;
COMMA:           /,/;
LET:             /let/;
IN:              /in/;
IF:              /if/;
THEN:            /then/;
ELSE:            /else/;
HOLE:            /hole/;
PARENL:          /\(/;
PARENR:          /\)/;
PARENSQL:        /\[/;
PARENSQR:        /\]/;
INTLIT:          /[+-]?[0-9]+/;
INFIX:           /(==)|(\/=)|(<=)|(>=)|([\+\-\*\<\>\^\/\%])|(\`(\w)[\d\w]*\`)/;
EQUALS:          /=/;
NAME:            /[a-z][a-zA-Z\'_0-9]*/;
CHARLIT:         /\'((\\\')|[^\'])\'/;
STRLIT:          /\"((\\\")|[^\"])*\"/;
NL:              /[\r\n]/;
