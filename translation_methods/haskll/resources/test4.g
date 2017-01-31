grammar Arithmetic;

//@imports {
//import Data.Maybe (fromJust)
//import Data.Monoid ((<>))
//import Data.Maybe (fromJust)
//}


start returns [Int val, Integer travDepth]
    : sum[1]                                       { let (val,travDepth) = sum }
    ;

sum[Integer travDepthIn] returns [Int val, Integer travDepth]
    : mult[travDepthIn] sum1[(snd mult)]           { let val = fst mult + fst sum1
                                                     let travDepth = snd sum1 + 1 }
    ;

sum1[Integer travDepthIn] returns [Int val, Integer travDepth]
    : PLUS mult[travDepthIn] sum1[(snd mult)]      { let val = fst mult + fst sum1
                                                     let travDepth = snd sum1 + 1 }
    | EPSILON                                      { let val = 0
                                                     let travDepth = travDepthIn + 1 }
    ;

mult[Integer travDepthIn] returns [Int val, Integer travDepth]
    : single[travDepthIn] mult1[(snd single)]      { let val = fst single * fst mult1
                                                     let travDepth = snd mult1 + 1 }
    ;

mult1[Integer travDepthIn] returns [Int val, Integer travDepth]
    : MULT single[travDepthIn] mult1[(snd single)] { let val = fst single * fst mult1
                                                     let travDepth = snd mult1 + 1 }
    | EPSILON                                      { let val = 1
                                                     let travDepth = travDepthIn + 1 }
    ;

single[Integer travDepthIn] returns [Int val, Integer travDepth]
    : NUM                                          { let val = readTextUnsafe $ tokenText tokenNUM
                                                     let travDepth = travDepthIn + 1 }
    | PARENL sum[travDepthIn] PARENR               { let val = fst sum
                                                     let travDepth = snd sum + 1}
    ;

WHITESPACE: SKIP /[ \t\n]+/;
NUM:             /\d+/;
PLUS:            /\+/;
MULT:            /\*/;
PARENL:          /\(/;
PARENR:          /\)/;
