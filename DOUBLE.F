\ Double number words for Third

BASE @  DECIMAL  \ make sure we're in decimal

( Note: double numbers on the stack are stored with the most
  significant cell on the top of the stack )

: 2LITERAL  ( x1 x2 -- )  \ compile x1 x2 as double number literal
   SWAP  \ E( -- x1 x2 ) return x1 x2 at execution time
   POSTPONE LITERAL POSTPONE LITERAL ; IMMEDIATE

: 2CONSTANT  ( x1 x2 "name" -- )  \ create two-constant called "name"
   CREATE , ,  \ x2 is first in memory
   DOES>  ( -- x1 x2 )  2@ ;  \ return x1 x2 when name is executed

: 2VARIABLE  ( "name" -- )  \ create two-variable called "name"
   CREATE  0 , 0 , ;  \ E( -- a-addr ) return data addr when name executes

: D-  ( d1 d2 -- d3 )  \ subtract d2 from d1 giving d3
   DNEGATE D+ ;

: D0<  ( d -- flag )  \ flag is true if d < 0
   NIP 0< ;

: D0=  ( xd -- flag )  \ flag is true if d = 0
   OR 0= ;

CODE D2*  ( d1 -- d2 )  \ multiply d1 by 2 by zero-filled bit shift left
   BX POP  AX POP  \ bx:ax = d1
   AX AX ADD  BX BX ADC  \ two adds are faster than shifting
   AX PUSH  BX PUSH  NEXT  \ and push d2
END-CODE

CODE D2/  ( d1 -- d2 )  \ divide d1 by 2 by sign-filled bit shift right
   BX POP  AX POP  \ bx:ax = d1
   BX SAR  AX RCR  \ double SAR
   AX PUSH  BX PUSH  NEXT  \ and push d2
END-CODE

: D<  ( d1 d2 -- flag )  \ true if d1 < d2
   ROT 2DUP = IF  2DROP U<  EXIT  THEN  \ high cells equal, what about lows?
   2SWAP 2DROP > ;  \ otherwise compare highs

: D=  ( d1 d2 -- flag )  \ true if d1 = d2
   D- D0= ;  \ if difference is zero they are equal

: D>S  ( d -- n )  \ convert double to single
   DROP ;  \ this version does no range checking

: DMAX  ( d1 d2 -- d3 )  \ d3 is the greater of d1 and d2
   2OVER 2OVER D< IF  2SWAP  THEN 2DROP ;

: DMIN  ( d1 d2 -- d3 )  \ d3 is the lesser of d1 and d2
   2OVER 2OVER D< 0= IF  2SWAP  THEN 2DROP ;

( multiply d1 by n1 giving a triple-cell intermediate, then divide by
  +n2, a positive single-cell, to get the quotient d2, remainder n3;
  M*/MOD uses symmetric division in which the quotient and remainder
  have the same sign )
CODE M*/MOD  ( d1 n1 +n2 -- n3 d2 )
   BX POP  CX POP  \ bx = +n2, cx = n1
   DI POP  AX POP  \ di:ax = d1
   DI DX MOV  CX DX XOR  DX PUSH  \ save sign of result (d2)
   BX PUSH  \ save divisor +n2
   CX CX OR  1 L JNS  CX NEG  \ get cx = abs(n1)
1 L:
   DI DI OR  2 L JNS  \ get di:ax = abs(d1)
   AX NEG  0 # DI ADC  DI NEG
2 L:
   CX MUL  \ lower partial product in dx:ax
   AX DI XCHG  \ now in dx:di, and ax = high cell of d1
   DX BX MOV  \ now in bx:di (so we can use dx:ax for another MUL)
   CX MUL  \ low intermediate in bx:di, high in dx:ax
   BX AX ADD  0 # DX ADC  \ add to get triple intermediate in dx:ax:di
   BX POP  BX DIV  \ divide by +n2
   AX DI XCHG  \ high cell of quotient in di (no overflow checking!)
   BX DIV  \ now quotient in di:ax
   CX POP  CX CX OR  3 L JNS  \ put correct sign on result
   AX NEG  0 # DI ADC  DI NEG  \ negate quotient di:ax if needed
   DX NEG  \ negate remainder dx if needed
3 L:
   DX PUSH  \ push remainder (n3)
   AX PUSH  DI PUSH  NEXT  \ push quotient (d2)
END-CODE

( multiply d1 by n1 giving a triple-cell intermediate, then divide by
  +n2, a positive single-cell, to get the quotient d2 )
: M*/  ( d1 n1 +n2 -- d2 )
   M*/MOD ROT DROP ;

: M+  ( d1 n -- d2 )  \ add n to d1 giving d2
   S>D D+ ;

\ 2ROT and DU< are in the Double Number Extensions wordset

: 2ROT  ( d1 d2 d3 -- d2 d3 d1 )  \ rotate third double d1 to the top
   2>R 2SWAP  2R> 2SWAP ;

: DU<  ( ud1 ud2 -- flag )  \ true if ud1 < ud2
   ROT 2DUP = IF  2DROP U<  EXIT  THEN  \ high cells equal, what about lows?
   2SWAP 2DROP U> ;  \ otherwise compare highs

BASE !  \ restore BASE
