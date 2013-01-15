\ Third mode 13h graphics routines

VOCABULARY GRAPHICS  \ special vocabulary for GRAPHICS
ALSO GRAPHICS DEFINITIONS  \ define into GRAPHICS

$ 0A000 CONSTANT VSEG  \ VGA video segment
320 CONSTANT WIDE  \ screen width
200 CONSTANT HIGH  \ screen height

CODE MODE  ( mode -- )  \ switch to BIOS video mode
   AX POP  AH AH SUB  $ 010 INT  NEXT
END-CODE

: GFX  ( -- )  \ switch to mode 13h, 320x200/256 graphics mode
   $ 013 MODE ;

: TEXT  ( -- )  \ switch to text mode
   3 MODE ;

: XY>  ( x y -- seg addr )  \ convert x,y location to screen address
   WIDE * +  VSEG SWAP ;

: PSET  ( col x y -- )  \ put pixel at x,y in colour 0..255
   DUP HIGH U< IF
      OVER WIDE U< IF  XY> LC! EXIT  THEN
   THEN  2DROP DROP ;  \ x,y out of screen bounds

: PGET  ( x y -- col )  \ get colour 0..255 of pixel at x,y
   DUP HIGH U< IF
      OVER WIDE U< IF  XY> LC@ EXIT  THEN
   THEN  2DROP -1 ;  \ colour -1 means x,y out of bounds

: CLS  ( col -- )  \ fill screen in colour
   >R  VSEG 0 [ WIDE HIGH * ] LITERAL  R> LFILL ;

VARIABLE PEN  \ TRUE if pen is on, ie., we are drawing
VARIABLE X  VARIABLE Y  \ x,y location of pen
VARIABLE COL  \ colour of pen
VARIABLE DIR  \ direction of pen 0,1,2,3 = up,left,down,right

: OFF  ( a -- )  \ store FALSE in cell
   FALSE SWAP ! ;

: ON  ( a -- )  \ store TRUE in cell
   TRUE SWAP ! ;

: CENTRE  ( -- )  \ centre pen location
   [ WIDE 2/ ] LITERAL X !  [ HIGH 2/ ] LITERAL Y ! ;

: PLOT  ( -- )  \ put pixel at current location if pen is on
   PEN @ IF  COL @ X @ Y @ PSET  THEN ;

: COLOUR  ( n -- )  \ set pen colour and re-plot
   COL !  PLOT ;

: LEFT  ( -- )  \ turn pen anti-clockwise 90 degrees
   DIR @  1+ 3 AND  DIR ! ;

: RIGHT  ( -- )  \ turn pen clockwise 90 degrees
   DIR @  1- 3 AND  DIR ! ;

CREATE DIRS  \ x,y change for each direction
   0 , -1 ,  -1 , 0 ,  0 , 1 ,  1 , 0 ,

: MOVE  ( n -- )  \ move pen, drawing if PEN is ON
   DIR @ 2* CELLS  DIRS + 2@  \ get change in x,y
   ROT 0 ?DO  2DUP X +! Y +!  PLOT  LOOP 2DROP ;  \ put n pixels

: INIT  ( -- )  \ initialise "turtle" graphics
   GFX  CENTRE  15 COLOUR  PEN ON  0 DIR !  PLOT ;

: FUN  ( * / -- )  \ test
   CENTRE  2>R  99 BEGIN
      DUP MOVE  LEFT  2R@ */
   DUP 0> 0= UNTIL  DROP 2R> 2DROP ;

CR .( Type INIT 10 20 FUN for a demo, and TEXT to get back to text mode.)
