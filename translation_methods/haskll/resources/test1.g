grammar TopKek;

@imports {
import Universum
import qualified Data.Map as M
}

@members {
    extraMap :: M.Map
    someOtherShit :: [Text]
}

// Some comment
topLevel: NL? (f+=func NL)* f+=func NL? EOF              { Helpers.showResult($f); }; // some comment

func returns [String res]
    : NAME DOUBLECOLON type (NL decls+=decl[$type.argnum, $NAME.text])+
                                                        { $res = Helpers.genFunc($Name.text,$decls); };

type returns [Int argnum]
    : pureType (ARROW1 | ARROW2) t2=type           { argnum <- 1 + t2 ^. argnum; }
    | PARENL t=type PARENR                         { argnum <- t ^. argnum; }
    ;

pureType
    : PURETYPE | PARENSQL pureType PARENSQR;

TRUE:  SKIP /True/;
ARROW1:      /->/;
ARROW2:      /-->/;
DOUBLECOLON: /::/;
PURETYPE:    /(Int|Bool|Char)/;
PARENL:      /\(/;
PARENR:      /\)/;
PARENSQL:    /\[/;
PARENSQR:    /\]/;
FALSE:       /False/;
INT:         /[+-]?[0-9]+/;
NL:          /[\r\n]+/;
