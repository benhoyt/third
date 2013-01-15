\ A simple SEE decompiler for Third

BASE @ DECIMAL  \ make sure we're in decimal

: CELL-  ( a-addr1 -- a-addr2 )  \ move a-addr1 to previous cell address
   [ 1 CELLS ] LITERAL - ;

: >HP  ( xt -- hp )  \ find the header pointer given an execution token
   DUP  [ MAXN 1+ ] LITERAL 0 DO
      1-  \ move back one byte each loop
      2DUP  COUNT  $ 07F AND +  \ mask out immediate bit
      = IF  LEAVE  THEN  \ is this the name?
   LOOP  NIP  CELL- ;  \ skip back to start of link field

: .XT  ( xt -- )  \ display name of xt
   >HP .NAME ;

: @+  ( a-addr -- a-addr' x )  \ fetch cell and move a-addr along
   DUP CELL+ SWAP @ ;

: STABLE  ( x table -- value true | x false )  \ search table for x, give value
   @+  0 ?DO  \ loop thru table
      2DUP @ = IF  NIP CELL+ @  TRUE  UNLOOP EXIT  THEN  \ found it
      CELL+ CELL+  \ move to next entry in table
   LOOP  DROP FALSE ;  \ x not in table

: L.  ( ip c-addr u -- ip' )  \ display c-addr u and literal at ip
   TYPE SPACE  @+ . ;

: B.  ( ip c-addr u -- ip' )  \ display c-addr u and relative branch offset
   TYPE  @+ OVER -  \ calculate relative branch offset
   DUP 0< 0= IF  ." +"  THEN . ;  \ prefix with +/- sign (zero is +)

: ".  ( -- )  \ display a " character followed by a space
   [CHAR] " EMIT  SPACE ;

: S.  ( ip c-addr u -- ip' )  \ display c-addr u and string at ip
   TYPE ".  COUNT  2DUP TYPE  +  ". ;

:NONAME  ( ip -- ip' )  ." DOES> " >BODY ;
:NONAME  ( ip -- ip' )  ." POSTPONE " @+ .XT ;
:NONAME  ( ip -- ip' )  S" ABORT" S. ;
:NONAME  ( ip -- ip' )  S" S" S. ;
:NONAME  ( ip -- ip' )  S" ." S. ;
:NONAME  ( ip -- ip' )  ." +LOOP " CELL+ ;
:NONAME  ( ip -- ip' )  ." ?DO " CELL+ ;
:NONAME  ( ip -- ip' )  ." LOOP " CELL+ ;
:NONAME  ( ip -- ip' )  ." DO " CELL+ ;
:NONAME  ( ip -- ip' )  S" ELSE" B. ;
:NONAME  ( ip -- ip' )  S" IF" B. ;
:NONAME  ( ip -- ip' )  @+ . ;  \ literal value, just display

CREATE SPECIALS  \ special execution token handlers
   12 ,  \ # of specials in table
   ' (LIT ,     ,
   ' (IF ,      ,
   ' (ELSE ,    ,
   ' (DO ,      ,
   ' (LOOP ,    ,
   ' (?DO ,     ,
   ' (+LOOP ,   ,
   ' (." ,      ,
   ' (S" ,      ,
   ' (ABORT" ,  ,
   ' COMPILE ,  ,
   ' (DOES> ,   ,

:NONAME  ( xt -- )  \ DEFERred word
   ." DEFER " DUP .XT  ." IS " >BODY @ .XT ;  \ display what it points to

:NONAME  ( xt -- )  \ CONSTANT or VALUE
   DUP >BODY ?  ." VALUE " .XT ;  \ display contents

:NONAME  ( xt -- )  \ VARIABLE or CREATE
   ." CREATE " .XT ;

CREATE CALLS  \ call instruction handlers
   3 ,  \ # of entries in table
   ' (VAR ,     ,
   ' (CONST ,   ,
   ' (DEFER ,   ,

: CODED  ( xt -- )  \ show CODEd word
   ." CODE " .XT ." ... END-CODE " ;  \ could disassemble but we won't

: IMMED?  ( xt -- immed-flag )  \ true if xt is immediate
   >HP CELL+ C@ $ 080 AND ;

: COLON  ( xt -- )  \ decompile COLON definition
   ." : " DUP .XT SPACE  \ display name of word
   >BODY  BEGIN  \ skip past CALL (: instruction
      @+  DUP ['] EXIT <> WHILE  \ loop till exit
      SPECIALS STABLE IF  EXECUTE  \ it's special, execute handler
      ELSE  DUP IMMED? IF  ." POSTPONE "  THEN .XT  THEN  \ normal word
   REPEAT  2DROP  ." ; " ;

: CALLER  ( xt -- caller-xt )  \ return xt's CALLed xt
   1+ @+ + ;

: SEE-XT  ( xt -- )  \ decompile and display definition of xt
   CR  DUP  \ be sure to CR, dupe xt for later
   DUP CALLER  DUP ['] (: <> IF  \ colon definition?
      CALLS STABLE IF  EXECUTE  \ nope, must be VARIABLE CONSTANT DEFER
      ELSE  DROP CODED  THEN  \ or CODE
   ELSE  DROP COLON  \ otherwise it's a colon definition
   THEN  IMMED? IF  ." IMMEDIATE " THEN ;  \ if it's IMMEDIATE, say so!

: SEE  ( "<spaces>name" -- )  \ decompile and display definition of name
   ' SEE-XT ;

BASE !  \ restore BASE
