\ Words Third doesn't need but are here for completeness

BASE @ DECIMAL  \ make sure we're in decimal

VARIABLE FENCE  \ can't forget further back than the fence
' COLD FENCE !  \ disallow FORGETting any Kernel stuff

: FORGET  ( "<spaces>name" -- )  \ forget all words back to and including name
   ' >HP  \ get name's header pointer
   DUP FENCE @ U<  -15 AND THROW  MARKED ;  \ can't forget before FENCE

: TIB  ( -- c-addr )  \ return the address of the terminal input buffer
   SOURCE DROP ;

( return the address of a cell containing the number of chars in the
  terminal input buffer )
: #TIB  ( -- a-addr )
   SRC ;  \ the first cell of SRC is what we are looking for

( convert chars at c-addr1+1 to ud2 respective to BASE, add to ud1,
  and stop when an unconvertable character is hit at c-addr2 )
: CONVERT  ( ud1 c-addr1 -- ud2 c-addr2 )
   CHAR+ TRUE >NUMBER  DROP ;  \ true = max unsigned number

VARIABLE SPAN  \ number of chars ACCEPTed by EXPECT

: EXPECT  ( c-addr +n -- )  \ receive string of at most +n chars to c-addr
   ACCEPT  SPAN ! ;

: QUERY  ( -- )  \ refill input source from the user input device (keyboard)
   IBUF @  DUP MAX-LINE ACCEPT  SOURCE! ;

: [COMPILE]  ( "<spaces>name" -- )  \ compile name's xt into current definition
   ' COMPILE, ; IMMEDIATE

: >C"  ( c-addr1 u -- c-addr2 )  \ backtrack to get counted string, unCOUNT
   DROP 1- ;

: C"  ( "ccc<quote>" -- )  \ parse and compile string ccc
   [CHAR] " PARSE  POSTPONE SLITERAL  \ compile the string
   POSTPONE >C" ; IMMEDIATE  \ E( -- c-addr )

\ rotate u+1 cells on the stack so xu is the top cell on the stack
: ROLL  ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )  \ never never ever ROLL
   DUP 1+ PICK >R >R  \ get xu from underneath
   SP@ DUP CELL+  R> CELLS MOVE  \ move xu-1 ... x0 down one cell
   DROP  R> ;  \ drop old x0 and put xu in its place

\ pick uth item (zero based) from the control flow stack
: CS-PICK  ( S: u -- ) ( C: csu ... cs0 -- csu ... cs0 csu )
   PICK ;  \ same as normal PICK in Third

\ rotate u+1 items on the control flow stack so csu is top of stack
: CS-ROLL  ( S: u -- ) ( C: csu csu-1 ... cs0 -- csu-1 ... cs0 csu )
   ROLL ;  \ same as normal ROLL in Third

BASE !  \ restore BASE
