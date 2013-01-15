\ CORE extension and misc words for Third

BASE @  DECIMAL  \ save BASE and change to DECIMAL

\ Various CORE Extension words

0 CONSTANT FALSE  -1 CONSTANT TRUE  \ true and false flags

: 0<>  ( x -- flag )  \ true if x is not equal to zero
   0= INVERT ;  \ could be 0= 0=

: 0>  ( n -- flag )  \ true if n is greater than zero
   0 > ;

: <>  ( x1 x2 -- flag )  \ true if x1 is not equal to x2
   = INVERT ;  \ could be = 0=

: U>  ( u1 u2 -- flag )  \ true if u1 is greater than u2
   SWAP U< ;

0 CONSTANT CASE  IMMEDIATE  \ start a CASE..ENDCASE structure

: OF  ( -- of-sys )  \ start an OF..ENDOF clause
   POSTPONE OVER  POSTPONE =  \ E( x1 x2 -- x1 | )
   POSTPONE IF  POSTPONE DROP ; IMMEDIATE

: ENDOF  ( case-sys1 of-sys -- case-sys2 )  \ end an OF..ENDOF clause
   POSTPONE ELSE ; IMMEDIATE  \ resolve the OF jump address

: ENDCASE  ( case-sys -- )  \ end a CASE..ENDCASE structure
   POSTPONE DROP  \ E( x -- )  drop the CASE selector
   BEGIN  ?DUP WHILE  \ resolve all the ELSEs until we hit a 0 from CASE
   POSTPONE THEN  REPEAT ; IMMEDIATE

: VALUE  ( x "name" -- )  \ create a value "name" which returns x at runtime
   CONSTANT ;  \ same code as CONSTANT

: ADDR  ( "name" -- data-addr )  \ return name's data address
   ' >BODY ;

: ADDR>  ( "name" -- )  \ return name's data address at runtime
   ADDR  STATE @ IF  POSTPONE LITERAL  THEN ; IMMEDIATE  \ E( -- data-addr )

: TO  ( x "name" -- )  \ store x in VALUE name's data cell at runtime
   ADDR  STATE @  \ this is a state-smart word
   IF  POSTPONE LITERAL  POSTPONE !  \ compiling, set later
   ELSE  !  THEN ; IMMEDIATE  \ interpreting, set now

: :NONAME  ( -- xt )  \ begin an un-named colon definition, return its xt
   HERE  \ the execution token of the new word is HERE
   ['] (: CALL,  \ compile call to do-colon
   0 LAST !  \ LAST=0 tells ; not to REVEAL anything
   ]  !CSP ;  \ enter compiling mode and save check stack pointer

\ Miscellaneous words

: $  ( "<spaces>word" -- n|d )  \ parse word with BASE in hex
   BASE @ >R  HEX  NAME EVALUATE  R> BASE ! ; IMMEDIATE

: `  ( "name" -- char )  \ return the first character of name at runtime
   STATE @ IF  POSTPONE [CHAR]  ELSE  CHAR  THEN ; IMMEDIATE

: WIDE  ( -- u )  \ return screen width (number of columns)
   0 $ 044A L@ ;  \ read from BIOS memory

: HIGH  ( -- u )  \ return screen height (number of rows)
   0 $ 0484 LC@  1+ ;  \ works on EGA+

: DEFER  ( "name" -- )  \ create a DEFERred word called name
   HEADER  ['] (DEFER CALL,  \ execute the xt at runtime
   ['] (VAR , ;  \ xt is initially (VAR which is a no-operation

: IS  ( xt "name" -- )  \ set DEFERred word's execution token to xt
   POSTPONE TO ; IMMEDIATE  \ IS is the same as TO

: IS?  ( defer-xt -- is-xt )  \ return DEFERred word's execution token
   >BODY @ ;  \ use like: ' MY-DEFER IS?

: CHAINED  ( "name" -- )  \ chain current word to named DEFERred word
   '  DUP IS? COMPILE,  \ compile DEFERred word's current xt
   >BODY  LAST-XT SWAP ! ; IMMEDIATE  \ set DEFER to current word

: AHEAD  ( -- orig )  \ mark unconditional forward branch resolvable by THEN
   POSTPONE (ELSE  >MARK ; IMMEDIATE

\ Dictionary words display

VARIABLE ACROSS  \ column for justifying words
VARIABLE PAGING  TRUE PAGING !  \ shall we do "Press a key..." paging?
VARIABLE DOWN  \ row for PAGING

: PAGINATE  ( -- )  \ reset the paging DOWN count
   0 DOWN ! ;

: PAGER  ( -- )  \ do a CR and display "Press a key..." if needed
   CR  PAGING @ IF
      1 DOWN +!  DOWN @ HIGH 2 - > IF
         PAGINATE  ." Press a key..."  KEY DROP  13 EMIT
   THEN THEN ;

: ?CR  ( u -- )  \ do a CR if ACROSS+u is past the right margin
   DUP ACROSS @ +  DUP WIDE 1- > IF  PAGER  SWAP  THEN  \ CR if necessary
   ACROSS !  DROP ;  \ adjust ACROSS (or reset it to u)

: WIDS  ( wid -- )  \ display words in wordlist identified by wid
   CR PAGINATE  0 ACROSS !  \ reset justifying stuff
   0 OVER  \ tally of words in wordlist = 0
   BEGIN  @ ?DUP WHILE  \ go down chain until NULL link
      DUP  >NAME  DUP 1+ ?CR  TYPE SPACE  \ display the word
      SWAP 1+ SWAP  \ increment tally
   REPEAT  CR . ." words in " .WID ;  \ display number of words in wid

: WORDS  ( -- )  \ display words in top wordlist of search order
   CONTEXT @ @ WIDS ;

\ Memory dump

: ?  ( a-addr -- )  \ display contents of cell at a-addr
   @ . ;

: .2  ( ud -- )  \ used for display with leading zeroes
   # #  #> TYPE  SPACE ;

: DU8  ( addr -- )  \ dump 8 hex bytes at addr
   SPACE  8 OVER +  SWAP DO  I C@  0 <# .2  LOOP ;

: .C  ( char -- )  \ display a char or . if it's a control char
   DUP BL < IF  DROP [CHAR] .  THEN EMIT ;

: DU16  ( addr -- )  \ dump 16 ASCII bytes at addr
   SPACE  16 OVER +  SWAP DO  I C@ .C  LOOP ;

: DULINE  ( addr -- )  \ display address and dump a 16-byte line
   DUP 0 <# # # .2  DUP DU8  DUP 8 + DU8  DU16 ;

: DUMP  ( addr u -- )  \ dump u bytes starting from addr
   PAGINATE  BASE @ >R  HEX  \ do it in hex
   OVER + SWAP ?DO  PAGER I DULINE  16 +LOOP  \ dump 16 bytes at a time
   SPACE  R> BASE ! ;  \ space and restore BASE

\ Stack display

: .S  ( -- )  \ display values on stack without removing them
   DEPTH  ." ["  DUP 0 .R  ." ] "  \ number of items on stack
   DUP 0 ?DO  DUP I -  PICK .  LOOP  \ display values
   DROP  ." -- " ;  \ -- means top of stack

\ Conditional compilation

( compare string c-addr1 u1 to string c-addr2 u2, return 0 if they
  are identical, -1 if string1 < string2, or 1 if string1 > string2 )
: COMPARE  ( c-addr1 u1 c-addr2 u2 -- -1 | 0 | 1 )
   ROT  2DUP SWAP -  >R  \ u1-u2 on R:
   MIN  \ note, MIN is signed, strings cannot be > 32767 in length
   S=  \ S= is case sensitive or not, depending on SENSE or NONSENSE
   ?DUP IF  R> DROP EXIT  THEN  \ strings not equal to length of shorter
   R> DUP IF  0< 2* 1+  THEN ;  \ -1|0|1 now dependant on lengths

( parse and discard, including nested [IF]..[ELSE]..[THEN]s
  and [IF]..[THEN]s until a matching [THEN] is found )
: [ELSE]  ( -- )
   1 BEGIN  \ nesting level starts at 1
      BEGIN  PARSE-WORD DUP  \ parse next word till input buffer empty
      WHILE  2DUP  S" [IF]"  COMPARE 0=  \ is it an [IF]?
         IF  2DROP  1+  \ found [IF] so increment nesting level
         ELSE  2DUP  S" [ELSE]"  COMPARE 0=  \ found an [ELSE]?
            IF  2DROP  1- DUP IF  1+  THEN  \ unchanged unless [ELSE]..[ELSE]
            ELSE  S" [THEN]"  COMPARE 0=  \ a [THEN]?
               IF  1-  THEN  THEN THEN  \ yep, decrement nesting level
         ?DUP 0= IF  EXIT  THEN  \ if nesting level = 0 we're done
      REPEAT  2DROP
   REFILL 0= UNTIL  \ refill input buffer till end-of-file
   DROP ; IMMEDIATE

( if flag is true, do nothing, otherwise parse and discard, including
  nested [IF]..[ELSE]..[THEN]s and [IF]..[THEN]s, until a matching
  [ELSE] or [THEN] is found )
: [IF]  ( flag -- )
   0= IF  POSTPONE [ELSE]  THEN ; IMMEDIATE

: [THEN]  ( -- )  \ do absolute, positively nothing! (immediate word)
   ; IMMEDIATE

\ MARKER and ANEW implementations

: WMARKED  ( addr wid -- )  \ forget all words >= addr, in wid
   TUCK  \ tuck wid underneath for later
   BEGIN  @ DUP WHILE  \ fetch next link until end of chain
      2DUP SWAP U<  \ loop until link is less than forget addr
   UNTIL THEN  NIP SWAP ! ;  \ set new link for this wordlist

: MARKED  ( addr -- )  \ forget words >= addr, in all wordlists
   WID-LINK @ BEGIN
      2DUP WMARKED  \ forget words wordlist at a time
      CELL+ @  \ get wid of next wordlist in linked list
   ?DUP 0= UNTIL  \ last link in chain is zero
   WID-LINK @  \ find first wid below addr (for new WID-LINK)
   BEGIN  2DUP U< WHILE  CELL+ @  REPEAT
   WID-LINK !  DP ! ;  \ set new WID-LINK and HERE

: MARKER  ( "name" -- )  \ define a marker called name
   HERE  CREATE ,  \ data is name's header pointer
   DOES>  ( -- )  @ MARKED ;  \ forget back to name (inclusive) when executed

( if name is defined as a MARKER, forget back to and including it,
  then define "name" as a MARKER which forgets itself when executed )
: ANEW  ( "name" -- )
   >IN @  NAME SALL IF  EXECUTE  ELSE 2DROP THEN  >IN !
   MARKER ;

BASE !  \ restore BASE
