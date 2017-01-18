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
    : NAME '::' type (NL decls+=decl[$type.argnum, $NAME.text])+
                                                        { $res = Helpers.genFunc($Name.text,$decls); };

type returns [Int argnum]
    : pureType ('->'|'-->') t2=type          { argnum <- 1 + t2 ^. argnum; }
    | '(' t=type ')'                         { argnum <- t ^. argnum; }
    ;

pureType
    : 'Int' | 'Bool' | 'Char' | '[' pureType ']' | ('aoeu' | 'kek')*;

TRUE:  SKIP /True/;
FALSE:      /False/;
INT:        /[+-]?[0-9]+/;
NL:         /[\r\n]+/;
