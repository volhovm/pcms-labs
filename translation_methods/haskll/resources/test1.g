func : pureType '->' (decls+=pureType);

pureType
    : 'Int' | 'Bool' | 'Char' | '[' pureType ']';

True:       'True';
False:      'False';
Int:        [+-]?[0-9]+;
Nl:         [\r\n]+;
