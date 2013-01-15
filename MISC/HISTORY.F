\ Simple history-saving ACCEPT for Third

\ The non-ANS things in here are GET-XY and the EKEY values

BASE @  DECIMAL  \ make sure we are decimal

WORDLIST CONSTANT History  \ put all internal words in History
GET-ORDER History SWAP 1+ SET-ORDER
GET-CURRENT  DEFINITIONS

HEX
4800 CONSTANT =Up     5000 CONSTANT =Down   4B00 CONSTANT =Left
4D00 CONSTANT =Right  0E08 CONSTANT =Back   5300 CONSTANT =Del
1C0D CONSTANT =Done   4700 CONSTANT =Home   4F00 CONSTANT =End
DECIMAL

0 VALUE Buf  0 VALUE Most  \ user buffer and length
VARIABLE Pos  VARIABLE End  \ position and end
2VARIABLE XY  \ starting x,y position

1024 CONSTANT hBuf#  \ history buffer
CREATE hBuf  hBuf# CHARS ALLOT
VARIABLE hPos  VARIABLE hEnd

: hPlace  ( a u dest -- )  \ place string and extra count char
  2DUP 2>R  CHAR+ SWAP CHARS MOVE
  2R@ C!  2R> OVER 1+ CHARS + C! ;

: Cursor  ( -- )  \ move cursor to current position
  XY 2@  SWAP Pos @ + SWAP  AT-XY ;

: Display  ( -- )  \ display whole buffer
  XY 2@ AT-XY  Buf End @ TYPE ;

: Partial  ( -- )  \ display buffer from pos to end
  Cursor  Buf End @ Pos @ /STRING TYPE ;

: hChange  ( -- )  \ change history to hPos and redisplay
  End @
  hPos @ COUNT Most MIN  DUP End !  DUP Pos !
  Buf SWAP  CHARS MOVE  Display  \ update display
  End @ - SPACES  Cursor ;  \ erase extra length

: hAdd  ( -- )  \ add current buffer to history
  BEGIN
    hEnd @ hBuf - 1 CHARS /  End @ 2 + +  hBuf# > WHILE  \ space in buffer?
    hEnd @ hBuf = ABORT" hACCEPT buffer too small!"  \ not space enough
    hBuf COUNT 1+   DUP 1+ NEGATE >R
    CHARS +  hBuf  OVER hEnd @ SWAP - MOVE  \ remove first string
    R> hEnd +!
  REPEAT
  Buf End @ hEnd @  DUP hPos !  OVER 2 + hEnd +!  hPlace ;

: Up  ( -- )  \ go up (to previous) in history
  hPos @ hBuf U> IF
    hPos @ 1 CHARS - C@  2 + NEGATE hPos +!  hChange
  THEN ;

: Down  ( -- )  \ go down (to next) in history
  hPos @ hEnd @ <> IF  \ anything in history?
    hPos @ COUNT CHARS + CHAR+  DUP hEnd @ <> IF  \ anything down?
      hPos !  hChange
    ELSE DROP THEN
  THEN ;

: Left  ( -- )  \ go left in buffer
  Pos @ 0> IF  -1 Pos +!  Cursor  THEN ;

: Right  ( -- )  \ go right in buffer
  Pos @ End @ < IF  1 Pos +!  Cursor  THEN ;

: Back  ( -- )  \ backspace
  Pos @ IF  \ only if not at start
    Buf Pos @ CHARS +  DUP 1 CHARS -
    End @ Pos @ - CHARS MOVE  \ move chars down
    -1 End +!  -1 Pos +!  Partial SPACE Cursor
  THEN ;

: Del  ( -- )  \ delete
  Pos @ End @ <> IF  Right Back  THEN ;

: Insert  ( char -- )  \ insert character in buffer
  End @ Most = IF  DROP EXIT  THEN  \ ignore if max chars
  DUP 9 = IF  DROP BL  THEN  \ replace tab with space
  Buf Pos @ CHARS + DUP
  DUP CHAR+  End @ Pos @ - CHARS MOVE  \ move stuff up a char
  C!  1 End +!  Partial  1 Pos +!  Cursor ;  \ put char in and display

: Home  ( -- )  \ go to start of buffer
  0 Pos !  Cursor ;

: SETI  ( -- )  \ go to end of buffer
  End @ Pos !  Cursor ;

SET-CURRENT

: hClear  ( -- )  \ clear and init history buffer
  hBuf hEnd ! ;

: hAccept  ( c-addr +n1 -- +n2 )  \ history version of ACCEPT
  TO Most  TO Buf  hEnd @ hPos !
  0 Pos !  0 End !  GET-XY XY 2!
  BEGIN
    EKEY DUP =Done <> WHILE  \ stop when done key pressed
    CASE
      =Up    OF  Up     ENDOF
      =Down  OF  Down   ENDOF
      =Left  OF  Left   ENDOF
      =Right OF  Right  ENDOF
      =Back  OF  Back   ENDOF
      =Del   OF  Del    ENDOF
      =Home  OF  Home   ENDOF
      =End   OF  SETI   ENDOF
      DUP EKEY>CHAR IF  Insert  \ insert characters
      ELSE  DROP  THEN  \ ignore unknowns
    ENDCASE
  REPEAT  DROP  End @  DUP IF hAdd THEN ;

hClear  ' hAccept IS ACCEPT  \ init and use the history

PREVIOUS  BASE !  \ restore order
