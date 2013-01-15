\ Quick data file creator

\ This is rather Quick :-) and dirty, but it works ok.

DECIMAL  \ make sure we're in DECIMAL

: QUICK-SAND  ( c-addr u -- c-addr u' )  \ return first word in c-addr u
   2DUP BEGIN  DUP WHILE  \ loop till end of string
      OVER C@  DUP BL <> SWAP cTAB - AND WHILE  \ and till we hit space or tab
      1 /STRING  \ move string along one
   REPEAT THEN  NIP - ;  \ calculate u'

: WRITE-DAT  ( f-dat -- )  \ write one index record to f-dat
   PAD >QLINES @ 0> IF  \ only write if >QLINES > 0
      DUP  PAD QSIZE ROT WRITE-FILE THROW  \ write record to f-dat
   THEN DROP ;

2VARIABLE QFPOS  \ stored file position

: NEXT-DAT  ( f-dat -- )  \ write one record and setup new one in PAD
   WRITE-DAT  \ write previous record
   QFPOS 2@ PAD 2!  \ move file position to PAD
   PAD >QNAME MAXN BLANK  \ make sure >QNAME is blank padded
   RAD MAX-LINE QUICK-SAND  PAD >QNAME SWAP MOVE  \ move name to PAD
   0 PAD >QLINES ! ;  \ reset #lines count

: NEXT-QUICK  ( f-txt -- flag )  \ read next quick reference line
   >R  BEGIN  R@ FILE-POSITION THROW  QFPOS 2!  \ save file position
      RAD MAX-LINE R@ READ-LINE THROW  WHILE  \ read lines till EOF
      IF  1 PAD >QLINES +!  \ increment line count for all non-blank lines
         RAD C@ BL <> IF  R> DROP  TRUE EXIT  THEN  \ true means not EOF
      THEN
   REPEAT  1 PAD >QLINES +!  \ count last line read
   R> 2DROP  FALSE ;  \ end of file

: QUICK-DAT  ( -- )  \ create the QUICK.DAT quick reference index file
   S" QUICK.DAT" W/O CREATE-FILE THROW
   S" QUICK.TXT" R/O OPEN-FILE THROW
   -1000 PAD >QLINES !  \ tells NEXT-DAT it's the first time round
   BEGIN  2DUP NEXT-QUICK WHILE  \ get quick reference words till EOF
      NEXT-DAT  \ write one record to Quick.Dat
   REPEAT  WRITE-DAT  \ write out last unwritten record
   CLOSE-FILE THROW  \ close Quick.Txt
   DUP FILE-SIZE THROW  QSIZE UM/MOD NIP .  ." words "
   CLOSE-FILE THROW ;  \ close Quick.Dat

.( Creating index file Quick.Dat ... )  QUICK-DAT
DEPTH [IF]  -22 THROW  [THEN]  CR .( Success!) CR
