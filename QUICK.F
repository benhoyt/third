\ Quick reference help viewer for Third

BASE @  DECIMAL  \ make sure we're in DECIMAL

: .QUICK  ( fileid -- )  \ read and display line from fileid
   >R  RAD DUP MAX-LINE  R> READ-LINE THROW DROP  CR TYPE ;

: GOT-QUICK  ( #lines ud -- )  \ display quick reference word from Quick.Txt
   S" QUICK.TXT" R/O OPEN-FILE THROW >R
   R@ REPOSITION-FILE THROW  \ go to quick reference for this word
   R>  SWAP 0 ?DO  DUP .QUICK  LOOP  \ display #lines of help
   CLOSE-FILE THROW ;  \ close the Quick.Txt file

: >QLINES  ( record -- quick#lines )  [ 2 CELLS ] LITERAL + ;
: >QNAME   ( record -- quickname )    [ 3 CELLS ] LITERAL + ;

MAXN >QNAME CONSTANT QSIZE  \ size of one Quick.Dat record

: QUICKY  ( c-addr u -- flag )  \ display quick reference for word c-addr u
   S" QUICK.DAT" R/O OPEN-FILE THROW >R  \ open quick index data file
   BEGIN  RAD QSIZE R@ READ-FILE THROW  \ read one record to RAD
      WHILE  \ loop while not end-of-file
      RAD >QNAME MAXN -TRAILING  2OVER COMPARE 0= IF  \ is this the word?
         2DROP  R> CLOSE-FILE THROW  \ yep, close file
         RAD >QLINES @ RAD 2@ GOT-QUICK  TRUE EXIT  \ and display quick help
      THEN
   REPEAT  R> DROP 2DROP  FALSE ;

: QUICK  ( "<spaces>name ... " -- )  \ display quick reference help for names
   BEGIN  PARSE-WORD DUP WHILE  \ parse until end of line
      2DUP QUICKY 0= IF  CR 2DUP TYPE ."  not found! "  THEN 2DROP
   REPEAT  2DROP  QUIT ;  \ QUIT so no silly "ok" is shown

BASE !  \ restore BASE
