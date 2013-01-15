\ Stupid little nibbles game

VARIABLE Seed  HERE Seed !  \ simple random number generator
: Rand  ( -- u )  Seed @  12349 *  1+  DUP Seed ! ;
: choose  ( u -- 0..u-1 )  Rand  UM* NIP ;

80 CONSTANT NWIDE  24 CONSTANT NHIGH  \ nibbles screen width, height
CHAR * CONSTANT SEGMENT  \ one segment of snake
CHAR & CONSTANT POINTY  \ point thingummy

: CARRAY  ( size "name" -- )  \ create a character array
   CREATE  CHARS ALLOT
   DOES>  ( x y -- c-addr )  >R  NWIDE * + CHARS  R> + ;

NWIDE NHIGH * CARRAY WORLD  \ alias of the nibbles screen

VARIABLE POINTS  \ current points
VARIABLE DONE  \ finished game?
VARIABLE SPEED  \ slowdown factor
2VARIABLE DIR  \ x,y direction
VARIABLE X  VARIABLE Y  \ x,y position

: FINISHED  ( -- )  \ flag we're done with the game
   TRUE DONE ! ;

: SET-DIR  ( x y -- )  \ set nibbles direction
   DIR 2! ;

: UP     ( -- )   0 -1 SET-DIR ;
: DOWN   ( -- )   0  1 SET-DIR ;
: LEFT   ( -- )  -1  0 SET-DIR ;
: RIGHT  ( -- )   1  0 SET-DIR ;

: DO-KEY  ( char -- )  \ process keystroke
   CASE
      [CHAR] q OF  FINISHED  ENDOF
      [CHAR] i OF  UP  ENDOF
      [CHAR] k OF  DOWN  ENDOF
      [CHAR] j OF  LEFT  ENDOF
      [CHAR] l OF  RIGHT  ENDOF
   ENDCASE ;

: NIBBLER  ( -- )  \ plot a snake segment at current position
   X @ Y @ AT-XY  SEGMENT EMIT  \ draw it on screen
   SEGMENT  X @ Y @ WORLD  C! ;  \ but it in buffer

: PUT#  ( -- )  \ put new random # on screen
   NWIDE CHOOSE NHIGH CHOOSE  2DUP AT-XY
   POINTY EMIT  WORLD POINTY SWAP C! ;

: WALLS  ( -- )  \ check whether walls are hit
   X @ NWIDE U< IF  Y @ NHIGH U< IF  EXIT  THEN THEN
   FINISHED ;  \ out of screen bounds, flag done

: HIT  ( -- )  \ check whether a collision has occured
   X @ Y @ WORLD C@  \ get char on "screen"
   CASE
      SEGMENT OF  FINISHED  ENDOF  \ it's part of a snake, die
      POINTY OF  10 POINTS +!  PUT#  ENDOF  \ it's a pointy thing
   ENDCASE ;

: MOTION  ( -- )  \ move snake in current direction
   DIR 2@  Y +!  X +!  WALLS  HIT ;

: SPEEDY  ( -- )  \ slow the thing down to a playable speed
   SPEED @ MS ;

: STATUS  ( -- )  \ display points etc on bottom of screen
   0 24 AT-XY  POINTS @ 5 .R ;

: NINIT  ( -- )  \ initialise nibbles
   0 0 WORLD  NWIDE NHIGH * CHARS  ERASE
   NWIDE 2/ X !  NHIGH 2/ Y !  LEFT
   100 SPEED !  FALSE DONE !  PAGE
   0 POINTS !  PUT# ;

: NIBBLES  ( -- )  \ play the game
   NINIT
   BEGIN  STATUS  NIBBLER  SPEEDY
      KEY? IF  KEY DO-KEY  THEN MOTION
   DONE @ UNTIL ;
