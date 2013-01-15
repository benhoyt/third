\ Environment query routine for Third

BASE @ HEX  \ work in hex

WORDLIST CONSTANT ENV?WID  ( -- wid )  \ separate wordlist for ENVIRONMENT?

\ return info i*x about query string c-addr u, return false if not found
: ENVIRONMENT?  ( c-addr u -- i*x true | false )
   ENV?WID SEARCH-WORDLIST IF  EXECUTE TRUE  ELSE  FALSE  THEN ;

GET-ORDER  ENV?WID SWAP 1+ SET-ORDER  \ add ENV?WID to search order
GET-CURRENT  DEFINITIONS  \ compile into ENV?WID wordlist

\        i*x          c-addr u             type  meaning
\ 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
         0FF CONSTANT MAX-CHAR           \ u     maximum value for a character
    MAX-CHAR CONSTANT /COUNTED-STRING    \ n     max # chars in counted string
         050 CONSTANT /HOLD              \ n     size of HOLD buffer
        0100 CONSTANT /PAD               \ n     size of scratch PAD
           8 CONSTANT ADDRESS-UNIT-BITS  \ n     bits/address unit (byte)
       FALSE CONSTANT FLOORED            \ flag  using florred division?
       07FFF CONSTANT MAX-N              \ n     maximum signed single
       0FFFF CONSTANT MAX-U              \ u     maximum unsigned single
MAX-U MAX-N 2CONSTANT MAX-D              \ d     maximum signed double
MAX-U MAX-U 2CONSTANT MAX-UD             \ ud    maximum unsigned double
         080 CONSTANT STACK-CELLS        \ n     max cells on data stack
         07F CONSTANT RETURN-STACK-CELLS \ n     max cells on return stack
           8 CONSTANT WORDLISTS          \ n     max wordlists on search order
        TRUE CONSTANT CORE               \ flag  is this wordlist present?
        TRUE CONSTANT CORE-EXT           \ ...   ...
        TRUE CONSTANT DOUBLE
        TRUE CONSTANT DOUBLE-EXT
        TRUE CONSTANT EXCEPTION
        TRUE CONSTANT EXCEPTION-EXT
        TRUE CONSTANT FACILITY
        TRUE CONSTANT FACILITY-EXT
        TRUE CONSTANT FILE
        TRUE CONSTANT FILE-EXT
        TRUE CONSTANT TOOLS
        TRUE CONSTANT TOOLS-EXT
        TRUE CONSTANT SEARCH-ORDER
        TRUE CONSTANT SEARCH-ORDER-EXT
        TRUE CONSTANT STRING
        TRUE CONSTANT STRING-EXT

PREVIOUS SET-CURRENT  BASE !  \ restore search order and BASE
