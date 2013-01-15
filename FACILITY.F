\ Facility and terminal words for Third

\ Must be loaded on top of Third with extensions and assembler

BASE @  HEX  \ change BASE to HEXadecimal for CODEing

TRUE CONSTANT EMIT?  \ we are always ready for EMIT in Third

CODE EKEY  ( -- u )  \ wait for and return one keyboard event u
   AH AH SUB  016 INT  AX PUSH  NEXT  \ returns al = ASCII, ah = scancode
END-CODE

CODE ?EKEY  ( -- u flag )  \ return true and keyboard event if available
   BX BX SUB  1 # AH MOV  016 INT  \ returns ZF set if no key available
   AX PUSH  \ push u on stack
   1 L JZ  BX DEC  \ ZF clear means key available, flag = true
1 L:
   BX PUSH  NEXT  \ push flag
END-CODE

: EKEY?  ( -- flag )  \ return true if keyboard event available
   ?EKEY NIP ;

: >CHAR  ( u -- char )  \ mask only the low byte of u giving char
   0FF AND ;

( try to convert keyboard event u to character, return u false if key
  doesn't convert, char true if successful conversion )
: EKEY>CHAR  ( u -- u false | char true )
   DUP 0300 = IF  >CHAR  TRUE EXIT  THEN  \ handle ^@ (Ctrl-2) specially
   DUP >CHAR 0= IF  FALSE EXIT  THEN  \ if low byte = 0 then function key
   DUP 0E0 = IF  TRUE EXIT  THEN  \ handle 'à' (Alt-224) specially
   DUP >CHAR 0E0 = IF  FALSE EXIT  THEN  \ if low byte = $E0 then enhanced key
   >CHAR  TRUE ;  \ everything else is ASCII

: BKEY  ( -- char )  \ wait for one keyboard character (BIOS version)
   0 BEGIN  DROP  EKEY EKEY>CHAR UNTIL ;  \ EKEY until it converts to char

DEFER KEY?  ( -- flag )  \ flag is true if keyboard character available

: BKEY?  ( -- flag )  \ keyboard char available? (BIOS version)
   BEGIN  ?EKEY WHILE  \ any key available?
      EKEY>CHAR NIP IF  TRUE EXIT  THEN  \ yep, does it convert to char?
      EKEY DROP  \ no, eat up the keyboard event
   REPEAT  DROP  FALSE ;  \ no key at all

' BKEY? IS KEY?  \ vectored KEY? initially uses BIOS version
' BKEY IS KEY  \ change vectored KEY from DOS to BIOS version

CODE AT-XY  ( x y -- )  \ set text cursor position to x,y (zero based)
   2 # AH MOV  BH BH SUB  \ ah=2 = set cursor position, bh = page 0
   CX POP  DX POP  CL DH MOV  \ dh = row (x), dl = column (y)
   BP PUSH  010 INT  BP POP  NEXT  \ set pos (some BIOSes change BP!)
END-CODE

CODE GET-XY  ( -- x y )  \ return text cursor position x,y (zero based)
   3 # AH MOV  BH BH SUB  \ ah=2 = get cursor position, bh = page 0
   BP PUSH  010 INT  BP POP  \ dh = row, dl = column
   DL BL MOV  BX PUSH  \ zero extend and push column
   DH BL MOV  BX PUSH  NEXT  \ and same with row
END-CODE

CODE DARK  ( -- )  \ clear screen
   BP PUSH  0600 # AX MOV  \ ah=6 = scroll window, al=0 = clear screen
   7 # BH MOV  \ colour to clear screen to
   CX CX SUB  \ cl,ch = 0,0 upper left coordinates of window to clear
   0 # PUSH  ES POP  \ dl,dh = max screen window (from BIOS memory)
   ES: 44A [] DL MOV  \ # of columns
   ES: 484 [] DH MOV  DH INC  \ # of rows
   010 INT  BP POP  NEXT  \ blank out window!  (some BIOSes change BP)
END-CODE

: PAGE  ( -- )  \ clear screen and return text cursor to 0,0
   DARK  0 0 AT-XY ;

( return the system time and date:
  second 0..59, minute 0..59, hour 0..23
  day 1..31, month 1..12, year 1998..yyyy )
CODE TIME&DATE  ( -- second minute hour day month year )
   BX BX SUB  \ bx = zero for pushing
   02C # AH MOV  021 INT  \ $2C is DOS get time function
   DH BL MOV  BX PUSH  \ push second, minute and hour
   CL BL MOV  BX PUSH
   CH BL MOV  BX PUSH
   02A # AH MOV  021 INT  \ $2A is DOS get date function
   DL BL MOV  BX PUSH  \ push day, month, and year
   DH BL MOV  BX PUSH
   CX PUSH  NEXT
END-CODE

: TICKER  ( -- tick# )  \ return an 18.2Hz (55ms) ticker
   0 046C L@ ;  \ read low word of BIOS ticker from 0040:006C

DECIMAL  \ BASE changes to decimal here

: MS  ( u -- )  \ delay for u millseconds (with 55ms resolution)
   27 +  0 55 UM/MOD NIP  TICKER +  \ round to nearest 55ms increment
   BEGIN  TICKER  OVER U< 0= UNTIL  DROP ;  \ tick .. tock ..

BASE !  \ restore BASE
