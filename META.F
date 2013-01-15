\ Third metacompiler

BASE @ DECIMAL  \ make sure we're in decimal
WARNINGS @ FALSE WARNINGS !  \ disable "word redefined" warnings

\ Housekeeping stuff

: ROOT  ( -- )  \ set order to: forth / forth
   ONLY FORTH  DEFINITIONS ;

ROOT

: SERR  ( a u -- )  \ display string and ABORT
   CR  .S  SPACE TYPE  ROOT  TRUE ABORT" " ;

: WID'  ( wid "name" -- xt )  \ parse name and find in given wordlist
   NAME ROT SEARCH-WORDLIST 0= IF  S" Undefined!" SERR  THEN ;

VARIABLE T-CSP  \ metacompilation saved depth

: !T-CSP  ( -- )  DEPTH T-CSP ! ;  \ store stack depth
: ?T-CSP  ( -- )  DEPTH T-CSP @ XOR IF  S" Wrong depth!" SERR  THEN ;

!T-CSP  \ use it now

\ General image handling words

VARIABLE ISEG  \ target image segment
VARIABLE #IMAGE  \ maximum image size

: >IMAGE  ( a -- seg ofs )  \ return host seg:ofs given target address
   #IMAGE @ OVER U< IF  S" Outside image!" SERR  THEN
   ISEG @ SWAP ;

CODE TDALLOC  ( paras -- seg flag )  \ allocate given # paras thru DOS
   BX POP  $ 048 # AH MOV  $ 021 INT  \ allocate bx paras, seg in ax
   BX BX SBB  AX PUSH  BX PUSH  NEXT
END-CODE

: T-ALLOC  ( u -- )  \ allocate image u bytes in size
   DUP #IMAGE !  15 + 4 RSHIFT  \ round to next paragraph size (16 byte)
   TDALLOC ABORT" Can't T-ALLOC!"  DUP ISEG !  \ allocate ISEG
   0 #IMAGE @ $ 0FF LFILL ;  \ fill image with $FF bytes

\ far file write used for T-SAVE
CODE LWRITE  ( seg addr u fileid -- ax carry-flag )
   BX POP  CX POP  DX POP  \ bx = file handle, cx = # bytes, dx = addr
   DS POP  $ 040 # AH MOV  $ 021 INT  \ write bytes to file
   CS PUSH  DS POP  \ restore DS
   BX BX SBB  AX PUSH BX PUSH  NEXT  \ carry-flag true means error
END-CODE

\ Wordlists

WORDLIST CONSTANT WID-TARGET  \ host copy of new kernel names
WORDLIST CONSTANT WID-INSIDE  \ words used inside target definitions
WORDLIST CONSTANT WID-OUTSIDE  \ words used outside target definitions

: :I  ( "name" -- )  \ create colon definition "name" in INSIDE
   WID-INSIDE SET-CURRENT  : ;

: ;I  ( -- )  \ end INSIDE colon definition
   POSTPONE ;  DEFINITIONS ; IMMEDIATE

: [I]  ( "name" -- )  \ compile word now from INSIDE
   WID-INSIDE WID' COMPILE, ; IMMEDIATE

: :O  ( "name" -- )  \ create colon definition "name" in OUTSIDE
   WID-OUTSIDE SET-CURRENT  : ;

: ;O  ( -- )  \ end OUTSIDE colon definition
   POSTPONE ;  DEFINITIONS ; IMMEDIATE

: [O]  ( "name" -- )  \ compile word now from OUTSIDE
   WID-OUTSIDE WID' COMPILE, ; IMMEDIATE

\ Late binding, ie., finding names when words are executed

: (BIND  ( a u wid -- )  \ runtime word for bind, evaluate "a u" with wid
   GET-ORDER DUP BEGIN  ?DUP WHILE  1- ROT >R  REPEAT  >R  \ save order
   1 SET-ORDER  ['] EVALUATE CATCH IF  S" Binding not found!" SERR  THEN
   R> DUP BEGIN  ?DUP WHILE  1- R> ROT ROT  REPEAT  SET-ORDER ;

: BIND  ( wid "name" -- )  \ bind name from wid into current definition
   NAME  POSTPONE SLITERAL  POSTPONE LITERAL  ['] (BIND COMPILE, ;

: EVOKE  ( "name" -- )  \ bind name to be executed from OUTSIDE
   WID-OUTSIDE BIND ; IMMEDIATE

: EMPLOY  ( "name" -- )  \ bind name to be executed from TARGET
   WID-TARGET BIND ; IMMEDIATE

\ Target data sizes

16 CONSTANT #BITS  \ number of bits in a cell
:O #BITS  ( -- n )  #BITS ;O  \ put in OUTSIDE as well

:O CHARS  ( n -- n )  ;O  \ do nothing words INSIDE and OUT :-)
:I CHARS  ( n -- n )  ;I  \ assumes 1 char = 1 address unit
:O CHAR+  ( a -- a' )  1+ ;O  \ move address along one char
:O CHAR-  ( a -- a' )  1- ;O  \ move address back one char

:O CELLS  ( n -- #au )  [ #BITS 8 / ] LITERAL * ;O  \ n cells to # of au
:O CELL+  ( a -- a' )  1 [O] CELLS + ;O  \ move addr along one cell
:O CELL-  ( a -- a' )  1 [O] CELLS - ;O  \ move addr back one cell

\ Target memory

VARIABLE T-DP  \ target dictionary pointer (HERE)

:O ORG  ( a -- )  T-DP ! ;O  \ set target's HERE
:O HERE  ( -- a )  T-DP @ ;O  \ store target's HERE
:O ALLOT  ( n -- )  T-DP +! ;O  \ increment target's HERE

:O C@  ( a -- c )  >IMAGE LC@ ;O  \ target characters
:O C!  ( c a -- )  >IMAGE LC! ;O
:O C,  ( c -- )  [O] HERE  1 [O] CHARS [O] ALLOT  [O] C! ;O
:O COUNT  ( a -- a u )  DUP  [O] CHAR+ SWAP  [O] C@ ;O

:O @  ( a -- n )  >IMAGE L@ ;O  \ target cells
:O !  ( n a -- )  >IMAGE L! ;O
:O ,  ( n -- )  [O] HERE  1 [O] CELLS [O] ALLOT  [O] ! ;O

:O REL,  ( xt -- )  [O] HERE [O] CELL+ - [O] , ;O
:O CALL,  ( xt -- )  $ 0E8 [O] C, [O] REL, ;O  \ Intel relative CALL to xt

: MOVE>T  ( host-src target-dest u -- )  \ move bytes from host to target
   >R CS@ ROT ROT  >IMAGE R> LMOVE ;

:O FILL  ( target-addr u char -- )  \ fill u chars at target-addr with char
   2>R  >IMAGE  2R> LFILL ;O

:O S,  ( a u -- )  \ compile string as counted string in target
   DUP [O] C,  \ compile count
   [O] HERE  OVER [O] ALLOT  SWAP MOVE>T ;O  \ allot space and move to target

:O ALIGNED  ( a -- a' )  \ align address giving a'
   ;O  \ don't want alignment in Third
\  1 [O] CELLS 1- +  1 [O] CELLS 1- INVERT AND ;O

:O ALIGN  ( -- )  \ align target HERE
   ;O
\  [O] HERE [O] ALIGNED [O] ORG ;O

:O COMPILE,  ( xt -- )  [O] , ;O  \ compile execution token

\ Dictionary headers

VARIABLE HEAD?  \ true means header in target as well as host

:O |  ( -- )  \ make next word have no header in target
   FALSE HEAD? ! ;O

VARIABLE T-LAST  \ points to most recently defined name in target

: T-HEADER,  ( "name" -- "name" )  \ compile header in target if necessary
   HEAD? @ IF
      >IN @  NAME  \ save >IN and parse name
      [O] HERE  T-LAST @ [O] ,  \ link to previous word
      T-LAST !  \ update LAST
      [O] S,  >IN !  \ compile name as counted string, restore >IN
   THEN  TRUE HEAD? ! ;  \ make sure HEAD? is TRUE again

VARIABLE H-LAST  \ data space of last host header

: H-HEADER,  ( xt "name" -- )  \ compile host version of header
   WID-TARGET SET-CURRENT  CREATE  DEFINITIONS  \ create in TARGET vocab
   HERE H-LAST !  \ update H-LAST
   0 , ,  \ compile flags cell followed by xt
   DOES>  ( -- )  2@ IF  S" Target IMMEDIATE!" SERR  THEN
   [O] COMPILE, ;  \ compile target's xt

: T-CODE  ( "name" -- )  \ create target code header
   T-HEADER,  [O] HERE H-HEADER, ;  \ target header then host

: T-COLON  ( "name" -- )  \ create target colon header
   T-CODE  EVOKE (: [O] CALL, ;  \ compile Intel CALL (:

: T-FLAG  ( bits -- )  \ OR bits into target's flags
   T-LAST @ [O] CELL+  DUP >R  [O] C@ OR  R> [O] C! ;

:O IMMEDIATE  ( -- )  \ make last target word immediate
   EVOKE =IMMED  DUP T-FLAG  \ or target's IMMED flag, assumes header present
   H-LAST @ ! ;O  \ also set flags in last host header

\ Compiler words

:O ,"  ( "ccc<quote>" -- )  \ parse and compile string ccc to target
   [CHAR] " PARSE  [O] S, ;O

:I ABORT"  ( "str" -- )  \ abort and display str at runtime if flag true
   EMPLOY (ABORT"  [O] ," ;I  \ E( flag -- )

:I ."  ( "str" -- )  \ display str at runtime
   EMPLOY (."  [O] ," ;I  \ E( -- )

:I S"  ( "str" -- )  \ return compiled str address and count at runtime
   EMPLOY (S"  [O] ," ;I  \ E( -- a u )

:I LITERAL  ( x -- )  \ push inline literal x at runtime
   EMPLOY (LIT [O] , ;I  \ E( -- x )

:I 2LITERAL  ( xd -- )  \ push double inline literal xd at runtime
   SWAP [I] LITERAL [I] LITERAL ;I

: X#  ( radix "ccc" -- d true | n false )  \ parse and convert to # with radix
   BASE @ >R  BASE !  \ save BASE and set radix
   NAME  ['] NUMBER? CATCH  \ try to convert to binary
   R> BASE !  THROW ;  \ restore base before THROWing

: #O  ( flag -- )  \ number in interpret mode, simply drop X# flag
   DROP ;

: #I  ( d true | n false -- )  \ compile number as target literal
   IF  [I] 2LITERAL EXIT  THEN [I] LITERAL ;

:O '  ( "name" -- xt )  \ return execution token of target name
   WID-TARGET WID'  >BODY CELL+ @ ;O

:I [']  ( "name" -- xt )  \ compile xt of target name as literal
   [O] ' [I] LITERAL ;I

\ Note: this bit is a little tricky, yet clean and nice. The
\ metacompiler actually stays in interpreting STATE the whole time,
\ and therefore no IMMEDIATE words are needed. This is done by simply
\ switching the search order when [ or ] is executed. Also note that
\ words can be found as soon as they are defined, ie., you can't
\ reference a previous word of the same name (doing so will RECURSE).

VARIABLE T-STATE  \ isn't actually needed for metacompiling

:O ]  ( -- )  \ switch to metacompiler's compiling mode
   TRUE T-STATE !
   WID-TARGET WID-INSIDE 2 SET-ORDER ;O  \ target words and "immediate" words

:O :  ( "name" -- )  \ begin target colon definition
   T-COLON [O] ] !CSP ;O

:I [  ( -- )  \ switch to metacompiler's interpret mode
   FALSE T-STATE !
   WID-OUTSIDE 1 SET-ORDER  DEFINITIONS ;I  \ defining into OUTSIDE

:I ;  ( -- )  \ end target colon definition
   ?CSP EMPLOY EXIT [I] [ ;I

:I POSTPONE  ( "name" -- )  \ postpone name's compilation semantics
   WID-TARGET WID'  >BODY 2@  ( xt flags -- )
   0= IF  EMPLOY COMPILE  \ non-immediate word
   THEN  [O] COMPILE, ;I  \ compile xt to target

\ Control structure words

: MARK  ( -- orig )  [O] HERE 0 [O] , ;  \ mark forward branch
:I THEN  ( orig -- )  [O] HERE SWAP [O] ! ;I  \ resolve forward branch
:I IF  ( orig -- )  EMPLOY (IF MARK ;I  \ conditional forward
:I AHEAD  ( -- orig )  EMPLOY (ELSE MARK ;I  \ unconditional forward
:I ELSE  ( orig1 -- orig2 )  [I] AHEAD  SWAP [I] THEN ;I

:I BEGIN  ( -- dest )  [O] HERE ;I  \ begin loop
:I UNTIL  ( dest -- )  EMPLOY (IF [O] , ;I  \ conditional loop
:I AGAIN  ( dest -- )  EMPLOY (ELSE [O] , ;I  \ unconditional loop
:I WHILE  ( dest -- orig dest )  [I] IF SWAP ;I  \ conditional forward
:I REPEAT  ( orig dest -- )  [I] AGAIN [I] THEN ;I  \ unconditional backward

:I DO  ( -- do-sys )  EMPLOY (DO  MARK  [O] HERE ;I
:I ?DO  ( -- do-sys )  EMPLOY (?DO  MARK  [O] HERE ;I
:I LOOP  ( do-sys -- )  EMPLOY (LOOP [O] ,  [I] THEN ;I
:I +LOOP  ( do-sys -- )  EMPLOY (+LOOP [O] ,  [I] THEN ;I

\ note: can't actually execute the CREATE DOES> word while metacompiling
:I DOES>  ( -- )  \ give last CREATEd word a runtime definition
   EMPLOY (DOES>  EVOKE (: [O] CALL, ;I

\ Defining words

ALSO ASSEMBLER  \ search assembler wordlist too

WID-OUTSIDE WID' HERE IS THERE  \ assemble to target
WID-OUTSIDE WID' C@   IS TC@
WID-OUTSIDE WID' C!   IS TC!
WID-OUTSIDE WID' C,   IS TC,
WID-OUTSIDE WID' ,    IS T,

: T-ASM  ( -- )  !CSP  LINIT ALSO ASSEMBLER ;  \ initialise assembler

DEFINITIONS  \ define into ASSEMBLER

: END-CODE  ( -- )  PREVIOUS ?CSP LCHECK ;  \ end target CODE or PROC def

ROOT  \ back to normal search order

:O EQU  ( n "name" -- )  CONSTANT ;O
:O LABEL  ( "name" -- )  [O] HERE [O] EQU ;O

:O CODE  ( "name" -- )  T-CODE T-ASM ;O  \ begin target code definition
:O PROC  ( "name" -- )  [O] LABEL T-ASM ;O  \ referencable but headerless
:O DUAL  ( "name" -- )  \ target CODE and meta LABEL
   >IN @ >R  [O] CODE  R> >IN !  [O] LABEL ;O

: DUO  ( xt "name" -- )  >IN @  T-CODE  SWAP [O] CALL,  >IN ! ;

:O CREATE  ( "name" -- )  \ name return's address when executed
   EVOKE (VAR DUO  [O] LABEL ;O  \ LABEL it, we can use it while interpreting

:O VARIABLE  ( "name" -- )  [O] CREATE 0 [O] , ;O
:O CONSTANT  ( n "name" -- )  EVOKE (CONST DUO  DUP [O] EQU  [O] , ;O

:O DEFER  ( "name" -- )  T-CODE  EVOKE (DEFER [O] CALL,  0 [O] , ;O
:O >BODY ( xt -- a )  1+ [O] CELL+ ;O  \ skip CALL instruction
:O IS  ( xt "name" -- )  [O] ' [O] >BODY [O] ! ;O

\ Words same for target and host

:O (  POSTPONE ( ;O    :I (  [O] ( ;I  \ comments
:O \  POSTPONE \ ;O    :I \  [O] \ ;I
:O .(  POSTPONE .( ;O  :I .(  [O] .( ;I  \ immediate display

:O B#   2 X# #O ;O   :I B#   2 X# #I ;I  \ binary
:O D#  10 X# #O ;O   :I D#  10 X# #I ;I  \ decimal
:O H#  16 X# #O ;O   :I H#  16 X# #I ;I  \ hex

:O DUP DUP ;O     :O DROP DROP ;O   :O SWAP SWAP ;O   :O OVER OVER ;O
:O + + ;O         :O - - ;O         :O * * ;O         :O / / ;O
:O AND AND ;O     :O OR OR ;O       :O XOR XOR ;O     :O . . ;O
:O MIN MIN ;O     :O MAX MAX ;O
:O INVERT INVERT ;O                 :O NEGATE NEGATE ;O
:O CHAR CHAR ;O   :I [CHAR] [O] CHAR [I] LITERAL ;I
:O CR CR ;O       :O INCLUDE INCLUDE ;O
:O .S .S ;O       :O BYE BYE ;O
:O HEX HEX ;O     :O DECIMAL DECIMAL ;O
:O [IF] POSTPONE [IF] ;O            :O [ELSE] POSTPONE [ELSE] ;O
:O [THEN] POSTPONE [THEN] ;O        :O WORDS WID-TARGET WIDS ;O

: META[  ( -- )  \ initialise meta-compiler
   TRUE HEAD? !  0 T-LAST !  $ 0100 [O] ORG  \ set variables
   $ 08000 T-ALLOC  DECIMAL  !T-CSP  [I] [ ;  \ allocate image and setup

: T-SAVE  ( a u -- )  \ save image to runnable COM file named in string
   W/O CREATE-FILE THROW >R  \ create file
   $ 0100 >IMAGE  [O] HERE OVER -  \ write HERE-$100 bytes from ISEG:100
   R@ LWRITE >IOR THROW DROP  \ THROW ior on error
   R> CLOSE-FILE THROW ;  \ close newly created file

:O ]META  ( "filename.com" -- )  \ initialise and save target
   DECIMAL  ?T-CSP  \ check stack
   [O] HERE EVOKE DP [O] !  \ set target's dictionary pointer
   T-LAST @ EVOKE FORTH-WORDLIST [O] !  \ set target's wordlist chain
   S" COLD" WID-TARGET SEARCH-WORDLIST  0= ABORT" No COLD!"
   >BODY CELL+ @  \ get COLD's execution token from WID-TARGET
   EVOKE BOOT 1+ [O] !  \ patch target loadup point to execute COLD
   NAME T-SAVE  ROOT  \ save target
   [O] HERE $ 0100 - . ." bytes " ;O

?T-CSP  WARNINGS !  BASE !  \ check stack, restore WARNINGS and BASE
