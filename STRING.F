\ String handling words for Third

BASE @ HEX  \ do our work in hex

\ adjust length of string to exclude spaces on the end of the string
CODE -TRAILING  ( c-addr u1 -- c-addr u2 )
   BX POP  DI POP  BX BX OR  1 L JZ  \ di = c-addr, bx = u1
2 L:
   -1 [BX+DI] AL MOV  \ get a byte from end of string
   020 # AL CMP  1 L JNE  \ stop if we've hit a non-space char
   BX DEC  2 L JNZ  \ and stop if we've reduced the string to zilch
1 L:
   DI PUSH  BX PUSH  NEXT  \ push new address and count
END-CODE

( beh note: personally I don't like CMOVE or CMOVE> and recommend
  using MOVE and FILL for what they were made for, don't use CMOVE
  for a character fill! )

\ move u chars from c-addr1 to c-addr2 starting from lowest addresses
CODE CMOVE  ( c-addr1 c-addr2 u -- )
   CX POP  DI POP  \ cx = u, di = destination addr
   SI DX MOV  SI POP  \ save si, si = source addr
   DS PUSH  ES POP  \ make sure es=ds
   REP BYTE MOVS  \ move char-by-char from low to high
   DX SI MOV  NEXT  \ restore si
END-CODE

\ move u chars from c-addr1 to c-addr2 starting from highest addresses
CODE CMOVE>  ( c-addr1 c-addr2 u -- )
   CX POP  DI POP  \ cx = u, di = destination addr
   SI DX MOV  SI POP  \ save si, si = source addr
   DS PUSH  ES POP  \ make sure es=ds
   CX SI ADD  SI DEC  CX DI ADD  DI DEC  \ start high and move down
   STD  REP BYTE MOVS  CLD  \ move char-by-char from high to low
   DX SI MOV  NEXT  \ restore si
END-CODE

: BLANK  ( c-addr u -- )  \ fill character string c-addr u with spaces
   BL FILL ;

: SLITERAL  ( c-addr1 u -- )  \ compile string c-addr1 u
   POSTPONE (S" S, ; IMMEDIATE  \ E( -- c-a2 u ) return compiled string addr

( search string c-addr1 u1 for substring c-addr2 u2, if flag is true
  a match was found at c-addr3 with u3 chars remaining, if flag is
  false c-addr3 = c-addr1 and u3 = u1; SEARCH assumes u2 <= u1 )
: SEARCH  ( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag )
   2>R  2DUP  \ substring on R: and duplicate c-addr1 u1
   DUP IF  \ make sure c-addr1 u1 is not zero length
      BEGIN  OVER 2R@ S=  \ substring at this position?
         0= IF  2R> 2DROP  2SWAP 2DROP  TRUE EXIT  THEN  \ yep, found a match
         DUP R@ - WHILE  \ loop while we still have room to compare substring
         1 /STRING  \ move main string along a char
      REPEAT
   THEN  2R> 2DROP  2DROP  FALSE ;  \ no match, return original string

BASE !  \ restore previous BASE
