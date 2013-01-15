\ Third v0.3 - A small Forth compiler by Benjamin Hoyt

.( Metacompiling the Third kernel ... )

WARNINGS @  FALSE WARNINGS !  \ turn redefinition warnings off
META[  \ begin metacompilation, base is DECIMAL

\ Equates for metacompilation

H#   80 EQU =IMMED  \ immediate flag bit (in count byte of header)
H#   1F EQU =MAXN  \ maximum length of name in dictionary
H# FFFE EQU =RP0  \ bottom of return stack
H# FF00 EQU =SP0  \ bottom of data stack
H# FE00 EQU =CX0  \ CONTEXT0 (search order grows down from here)
H# FDF0 EQU =MP0  \ malloc area grows downward from here

\ Bootup sequence

PROC BOOT  ( -- )  \ sets up stacks and executes COLD
   0 # AX MOV  \ address of COLD, to be patched before image is saved
   =RP0 # BP MOV  \ setup BP = return stack pointer
   =SP0 # SP MOV  \ setup SP = data stack pointer
   AX JMP  \ go to COLD
END-CODE

\ Primitives used by compiling and defining words

DUAL (:  ( -- \ -- return-addr )  \ nest into a colon definition
   2 # BP SUB  SI 0 [BP] MOV  \ push current SI on return stack
   SI POP  NEXT  \ load new SI (instruction pointer)
END-CODE

DUAL (VAR  ( -- a-addr )  \ push address of following data
   NEXT  \ a-addr is already pushed by CALL (VAR
END-CODE

DUAL (CONST  ( -- x )  \ push a constant's value onto the stack
   BX POP  0 [BX] BX MOV  BX PUSH  NEXT  \ fetch and push data cell
END-CODE

DUAL (DEFER  ( i*x -- j*x )  \ execute DEFERred word's execution token
   BX POP  0 [BX] JMP  \ jump to contents of DEFER's data cell
END-CODE

CODE EXIT  ( -- \ return-addr -- )  \ exit from a colon definition
   0 [BP] SI MOV  2 # BP ADD  NEXT  \ pop return-addr into SI
END-CODE

CODE (LIT  ( -- x )  \ push an inline literal
   WORD LODS  AX PUSH  NEXT  \ fetch cell at SI and push
END-CODE

CODE (S"  ( -- c-addr u )  \ push addr and count of inline counted string
   AX AX SUB  BYTE LODS  SI PUSH  AX PUSH  AX SI ADD  NEXT
END-CODE

CODE (ELSE  ( -- )  \ branch to inline address unconditionally
   0 [SI] SI MOV  NEXT
END-CODE

CODE (IF  ( flag -- )  \ branch to inline address if flag nonzero
   AX POP  AX AX OR  ' (ELSE JZ  2 # SI ADD  NEXT
END-CODE

CODE (DO  ( limit start -- \ -- loop-sys )  \ setup DO from start..limit-1
   BX POP  CX POP  \ bx = start, cx = limit
LABEL >DO  \ used by (?DO
   SP BP XCHG  WORD LODS  AX PUSH  \ push end-of-loop address on R:
   H# 8000 # CX ADD  CX PUSH  \ push adjusted limit on R:
   CX BX SUB  BX PUSH  \ adjusted start on R:
   SP BP XCHG  NEXT  \ restore stack pointers
END-CODE

CODE (?DO  ( limit start -- \ -- loop-sys | )  \ setup DO if limit <> start
   BX POP  CX POP  CX BX CMP  >DO JNE  ' (ELSE # JMP
END-CODE

CODE (LOOP  ( -- \ loop-sys -- | loop-sys )  \ loop to DO if limit not hit
   WORD 0 [BP] INC  ' (ELSE JNO  \ count overflowed?
LABEL >LOOP  \ used by (+LOOP
   2 # SI ADD  6 # BP ADD  NEXT  \ get rid of loop info
END-CODE

CODE (+LOOP  ( n -- \ loop-sys -- | loop-sys )  \ add n to counter and (LOOP
   AX POP  AX 0 [BP] ADD  >LOOP JO  ' (ELSE # JMP
END-CODE

CODE LEAVE  ( -- \ loop-sys -- )  \ unloop and leave DO..LOOP
   4 [BP] SI MOV  \ si -> end of loop
LABEL >LEAVE  \ used by UNLOOP
   6 # BP ADD  NEXT  \ kill loop parameters from return stack
END-CODE

CODE UNLOOP  ( -- \ loop-sys -- )  \ unloop from DO..LOOP without leaving
   >LEAVE # JMP
END-CODE

CODE I  ( -- n )  \ push innermost loop counter onto stack
   0 [BP] AX MOV  2 [BP] AX ADD  AX PUSH  NEXT
END-CODE

CODE J  ( -- n )  \ push next inner loop counter onto stack
   6 [BP] AX MOV  8 [BP] AX ADD  AX PUSH  NEXT
END-CODE

\ Miscellaneous constants

   -1 CONSTANT -1  \ these decrease code size somewhat
	0 CONSTANT 0
	1 CONSTANT 1
   32 CONSTANT BL  \ space character, BLank
   13 CONSTANT cCR  \ code for CR
	9 CONSTANT cTAB  \ code for tab
	8 CONSTANT cBKSP  \ code for backspace
=MAXN CONSTANT MAXN  \ maximum length name in dictionary

\ Data stack operations

=SP0 CONSTANT SP0  \ bottom of data stack

CODE SP!  ( a-addr -- )  \ set stack pointer to aligned address
   SP POP  NEXT
END-CODE

CODE SP@  ( -- a-addr )  \ return stack pointer (before pushing a-addr)
   SP AX MOV  AX PUSH  NEXT  \ SP PUSH doesn't work on old 8086s
END-CODE

CODE DROP  ( a -- )  \ discard cell on top of stack (TOS)
   AX POP  NEXT
END-CODE

CODE DUP  ( a -- a a )  \ duplicate TOS
   AX POP  AX PUSH AX PUSH  NEXT
END-CODE

CODE SWAP  ( a b -- b a )  \ swap top two stack cells
   BX POP AX POP  BX PUSH AX PUSH  NEXT
END-CODE

CODE NIP  ( a b -- b )  \ discard second cell on stack
   BX POP AX POP  BX PUSH  NEXT  \ same as "SWAP DROP"
END-CODE

CODE OVER  ( a b -- a b a )  \ pull second cell over TOS
   SP BX MOV  \ use bx to index stack instead of push/pop
   WORD 2 [BX] PUSH  NEXT
END-CODE

CODE TUCK  ( a b -- b a b )  \ tuck TOS under second on stack
   BX POP AX POP  BX PUSH  AX PUSH BX PUSH  NEXT  \ same as "SWAP OVER"
END-CODE

CODE ?DUP  ( a -- a a | 0 )  \ duplicate TOS if it's nonzero
   AX POP  AX AX OR  \ setup flags
   1 L JZ  AX PUSH  \ nonzero, push x back twice
1 L:
   AX PUSH  NEXT
END-CODE

CODE ROT  ( a b c -- b c a )  \ rotate third item to top of stack
   CX POP BX POP AX POP  BX PUSH CX PUSH AX PUSH  NEXT
END-CODE

CODE 2DROP  ( a b -- )  \ discard top cell pair
   BX POP  AX POP  NEXT  \ could use "4 # SP ADD" here
END-CODE

CODE 2DUP  ( a b -- a b a b )  \ duplicate top cell pair
   SP BX MOV  \ use bx to index stack
   WORD 2 [BX] PUSH  \ duplicate a
   WORD 0 [BX] PUSH  NEXT  \ duplicate b
END-CODE

CODE 2SWAP  ( a b c d -- c d a b )  \ swap top two cell pairs
   DX POP  CX POP  BX POP  AX POP  \ pop 'em all
   CX PUSH DX PUSH AX PUSH BX PUSH  NEXT  \ push 'em back swapped
END-CODE

CODE 2OVER  ( a b c d -- a b c d a b )  \ pull second cell pair over first
   SP BX MOV  \ use bx to index stack
   WORD 6 [BX] PUSH  \ pull a over
   WORD 4 [BX] PUSH  NEXT  \ pull b over
END-CODE

CODE PICK  ( xu .. x1 x0 u -- xu .. x1 x0 xu )  \ pick uth item to top
   BX POP  BX BX ADD  SP BX ADD  0 [BX] BX MOV  BX PUSH  NEXT
END-CODE

CODE DEPTH  ( -- n )  \ return # of cells on stack (before pushing n)
   =SP0 # AX MOV  SP AX SUB  AX SAR  AX PUSH  NEXT
END-CODE

\ Return stack operations

=RP0 CONSTANT RP0  \ bottom of return stack

CODE RP!  ( a-addr -- )  \ set return stack pointer to aligned address
   BP POP  NEXT  \ BP is our return stack pointer
END-CODE

CODE RP@  ( -- a-addr )  \ fetch return stack pointer
   BP PUSH  NEXT
END-CODE

CODE >R  ( a -- \ -- a )  \ move cell to return stack
   2 # BP SUB   WORD 0 [BP] POP  NEXT
END-CODE

CODE R>  ( -- a \ a -- )  \ move cell from return stack
   WORD 0 [BP] PUSH  2 # BP ADD  NEXT
END-CODE

CODE R@  ( -- a \ a -- a )  \ copy cell from return stack
   WORD 0 [BP] PUSH  NEXT
END-CODE

CODE 2>R  ( a b -- \ -- a b )  \ move cell pair to return stack
   4 # BP SUB  \ the words 2>R 2R> 2R@ are not "nice" to code in high level
   WORD 0 [BP] POP  WORD 2 [BP] POP  NEXT
END-CODE

CODE 2R>  ( -- a b \ a b -- )  \ move cell pair from return stack
   WORD 2 [BP] PUSH  WORD 0 [BP] PUSH  4 # BP ADD  NEXT
END-CODE

CODE 2R@  ( -- a b \ a b -- a b )  \ copy cell pair from return stack
   WORD 2 [BP] PUSH  WORD 0 [BP] PUSH  NEXT
END-CODE

\ Arithmetic operations

CODE S>D  ( n -- d )  \ convert signed single to signed double
   AX POP  CWD  AX PUSH DX PUSH  NEXT  \ high cell on top
END-CODE

CODE +  ( a b -- a+b )  \ add a to b giving sum
   BX POP  AX POP  BX AX ADD  AX PUSH  NEXT
END-CODE

CODE -  ( a b -- a-b )  \ subtract b from a giving difference
   BX POP  AX POP  BX AX SUB  AX PUSH  NEXT
END-CODE

CODE 1+  ( n -- n+1 )  \ add one to n
   AX POP  AX INC  AX PUSH  NEXT  \ same as "1 +"
END-CODE

CODE 1-  ( n -- n-1 )  \ subtract one from n
   AX POP  AX DEC  AX PUSH  NEXT
END-CODE

CODE 2*  ( n -- n*2 )  \ multiply n by 2 (bitwise left shift)
   AX POP  AX AX ADD  AX PUSH  NEXT  \ possibly faster than a SHL
END-CODE

CODE 2/  ( n -- n/2 )  \ signed divide n by 2 (arithmetic right shift)
   AX POP  AX SAR  AX PUSH  NEXT
END-CODE

CODE UM*  ( u1 u2 -- ud )  \ unsigned multiply producing double
   BX POP  AX POP  BX MUL  AX PUSH DX PUSH  NEXT  \ dx is high cell
END-CODE

CODE M*  ( n1 n2 -- d )  \ signed multiply producing double
   BX POP  AX POP  BX IMUL  AX PUSH DX PUSH  NEXT
END-CODE

CODE UM/MOD  ( ud u -- rem quo )  \ unsigned divide double by single
   BX POP  DX POP AX POP  BX DIV  DX PUSH  AX PUSH  NEXT  \ dx is remainder
END-CODE

CODE SM/REM  ( d n -- rem quo )  \ signed symmetric divide double by single
   BX POP  DX POP AX POP  BX IDIV  DX PUSH  AX PUSH  NEXT
END-CODE

: *  ( a b -- a*b )  \ multiply a by b giving product
   UM* DROP ;

: /MOD  ( n1 n2 -- rem quo )  \ signed divide n1 by n2 with remainder
   >R  S>D  R> SM/REM ;  \ Third's standard divisions are symmetric

: MOD  ( n1 n2 -- rem )  \ signed divide n1 by n2 giving remainder
   /MOD DROP ;

: /  ( n1 n2 -- quo )  \ signed divide n1 by n2 giving quotient
   /MOD NIP ;

: */MOD  ( n1 n2 n3 -- rem quo )  \ divide double n1*n2 by n3 with remainder
   >R  M*  R> SM/REM ;

: */  ( n1 n2 n3 -- quo )  \ divide double n1*n2 by n3 giving quotient
   */MOD NIP ;

CODE NEGATE  ( n -- -n )  \ negate n (ie., subtract it from 0)
   AX POP  AX NEG  AX PUSH  NEXT  \ same as "0 SWAP -"
END-CODE

CODE ABS  ( n -- u )  \ return the absolute value of n
   AX POP  AX AX OR  1 L JGE  AX NEG  \ negate n if it's negative
1 L:
   AX PUSH  NEXT  \ ABS is same as "DUP 0< IF  NEGATE  THEN"
END-CODE

CODE D+  ( d1 d2 -- d1+d2 )  \ add doubles giving double sum
   DX POP CX POP  BX POP AX POP  \ dx:cx = d2, bx:ax = d1
   CX AX ADD  \ add low cells
   DX BX ADC  \ add high cells with carry from above
   AX PUSH  BX PUSH  NEXT  \ push sum bx:ax
END-CODE

CODE DNEGATE  ( d -- -d )  \ negate double d
   BX POP AX POP  AX NEG  \ negate low
   0 # BX ADC  BX NEG  \ negate high with carry
   AX PUSH BX PUSH  NEXT  \ push result
END-CODE

CODE CELL+  ( a-addr -- a-addr' )  \ move address to next cell
   AX POP  2 # AX ADD  AX PUSH  NEXT  \ cell is two bytes in Third
END-CODE

CODE CELLS  ( n -- bytes )  \ return size in bytes of n cells
   AX POP  AX AX ADD  AX PUSH  NEXT  \ same as 2* in Third
END-CODE

CODE CHAR+  ( c-addr -- c-addr' )  \ move address to next character
   AX POP  AX INC  AX PUSH  NEXT  \ char is one byte in Third
END-CODE

CODE CHARS  ( n -- bytes )  \ return size in bytes of n characters
   NEXT
END-CODE IMMEDIATE  \ do-nothing in Third

\ Comparison and bit operators

CODE <  ( n1 n2 -- flag )  \ true if n1 < n2
   BX POP  AX POP  CX CX SUB  \ assume flag (cx) is false for now
   BX AX CMP  1 L JGE  CX DEC  \ n1 < n2 so flag = true = -1
1 L:
   CX PUSH  NEXT  \ push flag
END-CODE

CODE =  ( a b -- flag )  \ true if a = b
   BX POP  AX POP  CX CX SUB
   BX AX CMP  1 L JNE  CX DEC  \ a = b so flag = true
1 L:
   CX PUSH  NEXT
END-CODE

CODE U<  ( u1 u2 -- flag )  \ true if u1 < u2
   BX POP  AX POP  CX CX SUB
   BX AX CMP  1 L JAE  CX DEC  \ u1 < u2 so flag = true
1 L:
   CX PUSH  NEXT
END-CODE

: >  ( n1 n2 -- flag )  \ true if n1 > n2
   SWAP < ;

: 0<  ( n -- flag )  \ true if n < 0
   0 < ;

: FM/MOD  ( d n -- rem quo )  \ signed floored divide double by single
   DUP >R  DUP 0< IF  NEGATE  >R DNEGATE R>  THEN
   OVER 0< IF  TUCK + SWAP  THEN
   UM/MOD  R> 0< IF  SWAP NEGATE SWAP  THEN ;

: DABS  ( d -- ud )  \ return absolute value of signed double
   DUP 0< IF  DNEGATE  THEN ;

: 0=  ( n -- flag )  \ true if n = 0
   0 = ;

: WITHIN  ( n|u base limit -- flag )  \ true if base <= n|u < limit
   OVER - >R  -  R> U< ;

CODE MAX  ( n1 n2 -- max )  \ return greater of n1 and n2
   BX POP  AX POP  BX AX CMP  1 L JGE  BX AX MOV  \ greater in ax
1 L:
   AX PUSH  NEXT
END-CODE

CODE MIN  ( n1 n2 -- min )  \ return lesser of n1 and n2
   BX POP  AX POP  BX AX CMP  1 L JL  BX AX MOV  \ lesser in ax
1 L:
   AX PUSH  NEXT
END-CODE

CODE AND  ( a b -- c )  \ return bitwise AND of a and b
   BX POP  AX POP  BX AX AND  AX PUSH  NEXT
END-CODE

CODE OR  ( a b -- c )  \ return bitwise inclusive OR of a and b
   BX POP  AX POP  BX AX OR  AX PUSH  NEXT
END-CODE

CODE XOR  ( a b -- c )  \ return bitwise exclusive OR of a and b
   BX POP  AX POP  BX AX XOR  AX PUSH  NEXT
END-CODE

CODE INVERT  ( a -- b )  \ invert all bits in a giving b
   AX POP  AX NOT  AX PUSH  NEXT
END-CODE

CODE LSHIFT  ( a u -- b )  \ bitwise shift a left u bits giving b
   CX POP  AX POP  AX CL SHL  AX PUSH  NEXT
END-CODE

CODE RSHIFT  ( a u -- b )  \ bitwise shift a right u bits giving b
   CX POP  AX POP  AX CL SHR  AX PUSH  NEXT
END-CODE

\ Words that play with memory

CODE C!  ( char c-addr -- )  \ store char (byte) at a-addr
   BX POP  AX POP  AL 0 [BX] MOV  NEXT
END-CODE

CODE C@  ( c-addr -- char )  \ fetch and zero-extend char from given a-addr
   BX POP  AX AX SUB  0 [BX] AL MOV  AX PUSH  NEXT
END-CODE

CODE !  ( x a-addr -- )  \ store x at given address in memory
   BX POP  WORD 0 [BX] POP  NEXT
END-CODE

CODE @  ( a-addr -- x )  \ fetch x from a-addr
   BX POP   WORD 0 [BX] PUSH  NEXT
END-CODE

CODE +!  ( n a-addr -- )  \ add n to contents of cell at a-addr
   BX POP  AX POP  AX 0 [BX] ADD  NEXT
END-CODE

: 2!  ( a b a-addr -- )  \ store b at a-addr and a in next cell
   TUCK !  CELL+ ! ;

: 2@  ( a-addr -- a b )  \ fetch b from a-addr and a from next cell
   DUP CELL+ @  SWAP @ ;

CODE ALIGNED  ( addr -- a-addr )  \ align given address giving a-addr
   NEXT
END-CODE IMMEDIATE  \ we don't need (want) alignment

CODE /STRING  ( c-addr u n -- c-addr+n u-n )  \ move string along n chars
   CX POP  BX POP  AX POP  \ ax = string addr, bx = string length, cx = n
   CX AX ADD  CX BX SUB  \ add n to addr, subtract from length
   AX PUSH  BX PUSH  NEXT  \ push new string descriptor
END-CODE

CODE COUNT  ( c-addr -- c-addr+1 u )  \ return string from counted string
   BX POP  AX AX SUB  0 [BX] AL MOV  \ fetch character count
   BX INC  BX PUSH  AX PUSH  NEXT  \ push new address and count
END-CODE

CODE >UPPER  ( char -- char' )  \ convert char to upper case
   AX POP  \ al = char
   CHAR a # AL CMP  1 L JB  \ only convert if 'a' <= char <= 'z'
   CHAR z # AL CMP  1 L JA
   32 # AL SUB  \ char is lowercase, change to upper
1 L:
   AX PUSH  NEXT  \ push new char
END-CODE

CODE DETAB  ( c-addr u -- )  \ convert control chars in string to spaces
   CX POP  DI POP  1 L JCXZ  \ test for zero-length string
2 L:
   32 # BYTE 0 [DI] CMP  3 L JAE  \ low ASCII char here?
   32 # BYTE 0 [DI] MOV  \ yep, replace with space
3 L:
   DI INC  2 L LOOP  \ loop through all chars in string
1 L:
   NEXT
END-CODE

CODE FILL  ( c-addr u char -- )  \ fill u chars at c-addr with char
   AX POP  CX POP  DI POP
   DS PUSH  ES POP  REP BYTE STOS  NEXT  \ make sure es=ds for STOS
END-CODE

: ERASE  ( addr u -- )  \ zero u bytes at addr
   0 FILL ;  \ byte = char in Third

CODE MOVE  ( src dest u -- )  \ move u bytes from src to dest
   CX POP  DI POP  \ cx = u, di = dest
   SI DX MOV  SI POP  \ save si in dx, si = src
   DI SI CMP  1 L JB  \ if src < dest then move backwards
2 L:
   DS PUSH  ES POP  REP BYTE MOVS  \ make sure es=ds and move!
   CLD  DX SI MOV  NEXT  \ make sure DF clear and si restored
1 L:
   STD  \ set DF so MOVS moves back to avoid overlap problem
   CX SI ADD  SI DEC  CX DI ADD  DI DEC  \ adjust addresses
   2 L # JMP  \ now do the move
END-CODE

: PLACE  ( c-addr u dest -- )  \ place string to dest as counted string
   2DUP 2>R  CHAR+ SWAP CHARS MOVE  2R> C! ;  \ move first in case of overlap

CODE EXECUTE  ( i*x xt -- j*x )  \ execute given eXecution Token
   AX POP  AX JMP  \ jump straight to xt
END-CODE

VARIABLE MPTR  \ malloc pointer, init'ed in COLD

: MFREE  ( u -- )  \ free u bytes of memory on the "malloc stack"
   MPTR +! ;

: MALLOC  ( u -- addr )  \ allocate u bytes on "malloc stack"
   NEGATE MFREE  MPTR @ ;

CODE REPCMPSB  ( -- )  \ case sensitive REP CMPSB, not a Forth word
   DS PUSH  ES POP  REP BYTE CMPS  RET  \ make sure es=ds and go for it
END-CODE

CODE REPICMPSB  ( -- )  \ case insensitive REP CMPSB, not a Forth word
   CX CX OR  4 L JZ  \ do nothing if CX=0 (but still setup flags)
1 L:
   BYTE LODS  \ get a char into al and increment pointer (SI)
   CHAR a # AL CMP  2 L JB  \ if char is lowercase then convert to upper
   CHAR z # AL CMP  2 L JA  32 # AL SUB
2 L:
   0 [DI] AH MOV  DI INC  \ ah = char from dest
   CHAR a # AH CMP  3 L JB  \ convert to upper if it's lower
   CHAR z # AH CMP  3 L JA  32 # AH SUB
3 L:
   AL AH CMP  1 L LOOPE  \ keep looping if chars are equal
4 L:
   RET
END-CODE

VARIABLE SCMP  ' REPICMPSB SCMP !  \ string compare (REP CMPSB) vector

: SENSE  ( -- )  \ change to case sensitive mode
   ['] REPCMPSB SCMP ! ;

: NONSENSE  ( -- )  \ change to case nonsense-itive mode :-)
   ['] REPICMPSB SCMP ! ;

: SENSITIVE  ( -- flag )  \ return true if we're in case SENSE mode
   ['] REPCMPSB  SCMP @ = ;

CODE S=  ( src dest u -- -1 | 0 | 1 )  \ compare string "src u" with "dest u"
   CX POP  \ result is 0 if equal, -1 if src < dest, 1 if src > dest
   DI POP  SI DX MOV  SI POP  \ si = src, di = dest (save si first)
   -1 # BX MOV  \ assume flag is -1
   SCMP [] CALL  \ REP CMPSB (case sensitive or otherwise)
   2 L JL  \ src < dest
   1 L JE  \ src = dest
   BX INC  \ src > dest
1 L:
   BX INC
2 L:
   DX SI MOV  BX PUSH  NEXT  \ restore si and push flag
END-CODE

VARIABLE DP  \ dictionary pointer (ie., HERE)

: HERE  ( -- addr )  \ return the data space (dictionary) pointer
   DP @ ;

: PAD  ( -- addr )  \ return address of a scratch pad
   HERE D# 256 + ;  \ PAD is at HERE+256

: QAD  ( -- addr )  \ return address of a second scratch pad
   HERE D# 512 + ;  \ QAD is at PAD+256

: RAD  ( -- addr )  \ return address of a third scratch pad
   HERE D# 768 + ;  \ RAD is at QAD+256

: UNUSED  ( -- u )  \ return # of unused bytes of data space
   MPTR @ HERE - ;

\ Far memory and port fetches and stores

CODE CS@  ( -- seg )  \ return Third code segment
   CS PUSH  NEXT
END-CODE

CODE LC!  ( char seg ofs -- )  \ store char (byte) at seg:ofs
   BX POP  ES POP  AX POP  AL ES: 0 [BX] MOV  NEXT
END-CODE

CODE LC@  ( seg ofs -- char )  \ fetch and zero-extend char from seg:ofs
   BX POP  ES POP  AX AX SUB  ES: 0 [BX] AL MOV  AX PUSH  NEXT
END-CODE

CODE L!  ( cell seg ofs -- )  \ store cell (word) at seg:ofs
   BX POP  ES POP  WORD ES: 0 [BX] POP  NEXT
END-CODE

CODE L@  ( seg ofs -- cell )  \ fetch cell from seg:ofs
   BX POP  ES POP  WORD ES: 0 [BX] PUSH  NEXT
END-CODE

CODE LFILL  ( seg ofs u char -- )  \ fill u chars at seg:ofs with char
   AX POP  CX POP  DI POP  ES POP  REP BYTE STOS  NEXT
END-CODE

\ move u bytes from src-seg:src-ofs to dest-seg:dest-ofs, no overlap check
CODE LMOVE  ( src-seg src-ofs dest-seg dest-ofs u -- )
   CX POP  DI POP ES POP  \ cx = count, es:di -> dest
   SI DX MOV  SI POP DS POP  \ save SI, ds:si -> src (DS changes here!)
   REP BYTE MOVS  \ move the whole intersegment lot :-)
   CS PUSH DS POP  DX SI MOV  NEXT  \ restore DS and SI
END-CODE

CODE PC!  ( char port -- )  \ write char (byte) to given port
   DX POP  AX POP  DX AL OUT  NEXT
END-CODE

CODE PC@  ( port -- char )  \ read char from port
   DX POP  AX AX SUB  DX AL IN  AX PUSH  NEXT
END-CODE

CODE P!  ( cell port -- )  \ write cell to port
   DX POP  AX POP  DX AX OUT  NEXT
END-CODE

CODE P@  ( port -- cell )  \ read cell from port
   DX POP  DX AX IN  AX PUSH  NEXT
END-CODE

\ Display input/output

DEFER TYPE  ( c-addr u -- )  \ display string c-addr u, vectored

CODE DTYPE  ( c-addr u -- )  \ TYPE a string through DOS standard output
   CX POP  DX POP  1 # BX MOV  \ bx = 1 = standard output file handle
   H# 40 # AH MOV  H# 21 INT  NEXT  \ write cx bytes from ds:dx
END-CODE

' DTYPE IS TYPE  \ TYPE initially uses DOS

: (."  ( -- )  \ display inline string, runtime for ."
   R> COUNT 2DUP + >R  TYPE ;

: EMIT  ( char -- )  \ display char on screen
   SP@ 1 TYPE  DROP ;  \ address of char on the stack :-)

CREATE CR/LF  2 C, 13 C, 10 C,  \ counted string containing CR,LF

: CR  ( -- )  \ move display cursor to start of next line, carriage return
   CR/LF COUNT TYPE ;  \ display CR,LF

: SPACE  ( -- )  \ display one space
   BL EMIT ;

: SPACES  ( n -- )  \ if n > 0 display n spaces
   0 MAX  0 ?DO  SPACE  LOOP ;

DEFER KEY  ( -- char )  \ wait for and return one keyboard character

CODE DKEY  ( -- char )  \ wait for key using DOS
   8 # AH MOV  H# 21 INT  \ DOS read key function
   AH AH SUB  AX PUSH  NEXT  \ high byte = 0
END-CODE

' DKEY IS KEY  \ KEY initially uses DOS

\ receive at most +n1 chars to c-addr, +n2 is # of chars actually received
DEFER ACCEPT  ( c-addr +n1 -- +n2 )

: KACCEPT  ( c-addr +n1 -- +n2 )  \ KEY version of ACCEPT
   TUCK >R  BEGIN  OVER WHILE  \ loop while countdown <> 0
	  KEY  DUP cCR = IF  \ enter key?
		 2DROP  R> SWAP - EXIT  THEN  \ yep, calculate +n2 and exit
	  DUP cBKSP = IF  \ backspace?
		 DROP  OVER R@ XOR IF  1 /STRING  \ yep, remove char from buffer
			cBKSP EMIT  SPACE  cBKSP EMIT  THEN  \ and erase it on screen
		 ELSE  DUP EMIT  \ normal char, display it
			OVER C!  -1 /STRING  \ and chuck it into buffer
		 THEN
   REPEAT  2DROP R> ;  \ +n2 = +n1

' KACCEPT IS ACCEPT  \ initially use KEY version of ACCEPT

\ Input buffer parsing

CREATE SRC  \ two cell variable: length, address of input buffer
   0 , 0 ,

CREATE PARSED  \ two cell variable: length, address of last PARSEd string
   0 , 0 ,

VARIABLE >IN  \ offset (number of chars) into parse area

: SOURCE  ( -- c-addr u )  \ return whole input buffer as a string
   SRC 2@ ;

: SOURCE!  ( c-addr u -- )  \ set source string and set >IN to zero
   2DUP DETAB  SRC 2!  0 >IN ! ;

CODE SKIP  ( char "<chars>" -- )  \ parse and discard chars in input buffer
   AX POP  \ al = char
   SRC [] CX MOV  >IN [] CX SUB  \ cx = # chars left in parse area
   1 L JZ  CX DX MOV  \ input buffer empty? no, save dx in cx
   SRC 2 + [] DI MOV  >IN [] DI ADD  \ di -> current pos in parse area
   DS PUSH ES POP  REPE BYTE SCAS  \ scan while equal to char
   2 L JE  CX INC  \ don't skip a non-char!
2 L:
   CX DX SUB  DX >IN [] ADD  \ adjust >IN byte # chars skipped
1 L:
   NEXT
END-CODE

\ parse till char is found, return ccc as string c-addr u
CODE PARSE  ( char "ccc<char>" -- c-addr u )
   AX POP  \ al = char
   SRC [] CX MOV  >IN [] CX SUB  CX DX MOV  \ cx = dx = # chars left
   SRC 2 + [] DI MOV  >IN [] DI ADD  \ di -> current pos in parse area
   DI PUSH  DI PARSED 2 + [] MOV  \ push c-addr, store it in PARSED
   DS PUSH ES POP  REPNE BYTE SCAS  \ scan while not equal to char
   PUSHF  CX DX SUB  DX >IN [] ADD  \ adjust >IN
   POPF  1 L JNE  DX DEC  \ don't include delimiter in count
1 L:
   DX PUSH  DX PARSED [] MOV  NEXT  \ push u, store it in PARSED
END-CODE

: PARSE-WORD  ( "<spaces>name" -- c-addr u )  \ parse name bounded by spaces
   BL SKIP BL PARSE ;  \ skip spaces then parse

\ parse ccc bounded by char(s), return as counted string
: WORD  ( char "<chars>ccc<char>" -- c-addr )
   DUP SKIP PARSE  HERE PLACE  \ parse and place at here
   HERE  BL OVER COUNT + C! ;  \ store BL at end of string

: \  ( "ccc<eol>" -- )  \ parse and discard till end of line
   SOURCE >IN !  DROP ; IMMEDIATE  \ set >IN to end of input buffer

\ Number output conversion

VARIABLE BASE  10 BASE !  \ number conversion radix
VARIABLE HLD  \ HOLD buffer pointer

: DECIMAL  ( -- )  \ set conversion radix to decimal
   D# 10 BASE ! ;

: HEX  ( -- )  \ set conversion radix to hexadecimal
   D# 16 BASE ! ;

: HOLD  ( char -- )  \ add one character to the HOLD buffer
   -1 HLD +!  HLD @ C! ;  \ make space and store char

: <#  ( -- )  \ begin number conversion sequence
   PAD HLD ! ;  \ buffer grows down from, but doesn't touch, PAD

: #>  ( ud -- c-addr u )  \ end conversion and return buffer as string
   2DROP  \ drop ud
   HLD @  PAD OVER - ;  \ c-addr = HLD, calculate u

: #  ( ud1 -- ud2 )  \ convert one digit from ud1 with BASE and HOLD it
   0 BASE @ UM/MOD >R  \ divide high cell of ud1 by BASE
   BASE @ UM/MOD SWAP  \ then low cell, remainder is digit to convert
   D# 9 OVER <  D# 7 AND +  [CHAR] 0 + HOLD  \ convert to character and HOLD
   R> ;  \ high cell of ud2 was on R:

: #S  ( ud -- 0. )  \ convert and HOLD digits of ud till it's zero
   BEGIN  #  2DUP OR 0= UNTIL ;

: SIGN  ( n -- )  \ add '-' sign to HOLD buffer if n is negative
   0< IF  [CHAR] - HOLD  THEN ;

: RSPACES  ( c-addr u n -- )  \ display n-u spaces and then string c-addr u
   OVER - SPACES  TYPE ;

: UD.R  ( ud n -- )  \ display ud in right aligned field n wide
   >R  <# #S #>  R> RSPACES ;

: D.R  ( d n -- )  \ display d in right aligned field n wide
   >R TUCK DABS  \ tuck away sign of d, convert its absolute value
   <# #S ROT SIGN #>  R> RSPACES ;  \ convert, add sign, and display

: U.R  ( u n -- )  \ display u in right aligned field n wide
   0 SWAP UD.R ;

: .R  ( n1 n2 -- )  \ display n1 in right aligned field n2 wide
   >R  S>D  R> D.R ;

: UD.  ( ud -- )  \ display ud followed by a space
   0 UD.R SPACE ;

: D.  ( d -- )  \ display d followed by a space
   0 D.R SPACE ;

: U.  ( u -- )  \ display u followed by a space
   0 UD. ;

: .  ( n -- )  \ display n followed by a space
   S>D D. ;

: .D  ( n -- )  \ display n in decimal without changing BASE
   BASE @ DECIMAL  SWAP .  BASE ! ;  \ save/restore BASE

\ Exception handling

\ throw error messages as counted strings (only those we need)
LABEL thr001 ," ABORTed!"
LABEL thr004 ," Stack underflow!"
LABEL thr013 ," Undefined word!"
LABEL thr015 ," Invalid FORGET!"
LABEL thr016 ," No name given!"
LABEL thr022 ," Unbalanced stack!"

\ DOS error messages (only those we need) used also by THROW
LABEL dos257 ," File not found!"
LABEL dos258 ," Path not found!"
LABEL dos260 ," File access denied!"
LABEL dos261 ," Invalid file handle!"
LABEL dos270 ," Invalid drive!"

CREATE THROWS  \ pointers to counted string THROW error messages
   thr001 ,  \   -1 ABORT
		0 ,  \   -2 ABORT"
		0 ,  \   -3 stack overflow
   thr004 ,  \   -4 stack underflow
		0 ,  \   -5 return stack overflow
		0 ,  \   -6 return stack underflow
		0 ,  \   -7 do-loops nested too deeply
		0 ,  \   -8 dictionary overflow
		0 ,  \   -9 invalid memory address
		0 ,  \  -10 division by zero
		0 ,  \  -11 result out of range
		0 ,  \  -12 argument type mismatch
   thr013 ,  \  -13 undefined word
		0 ,  \  -14 interpreting a compile-only word
   thr015 ,  \  -15 invalid FORGET
   thr016 ,  \  -16 attempt to use zero-length string as a name
		0 ,  \  -17 pictured numeric output string overflow
		0 ,  \  -18 parsed string overflow
		0 ,  \  -19 definition name too long
		0 ,  \  -20 write to a read-only location
		0 ,  \  -21 unsupported operation
   thr022 ,  \  -22 control structure mismatch
		0 ,  \  -23 address alignment exception
		0 ,  \  -24 invalid numeric argument
		0 ,  \  -25 return stack imbalance
		0 ,  \  -26 loop parameters unavailable
		0 ,  \  -27 invalid recursion
		0 ,  \  -28 user interrupt
		0 ,  \  -29 compiler nesting
		0 ,  \  -30 obsolescent feature
		0 ,  \  -31 >BODY used on non-CREATEd definition
		0 ,  \  -32 invalid name argument
		0 ,  \  -33 block read exception
		0 ,  \  -34 block write exception
		0 ,  \  -35 invalid block number
		0 ,  \  -36 invalid file position
		0 ,  \  -37 file I/O exception
		0 ,  \  -38 non-existent file
		0 ,  \  -39 unexpected end of file
		0 ,  \  -40 invalid BASE for floating point
		0 ,  \  -41 loss of precision
		0 ,  \  -42 floating point divide by zero
		0 ,  \  -43 floating point result out of range
		0 ,  \  -44 floating point stack overflow
		0 ,  \  -45 floating point stack underflow
		0 ,  \  -46 floating point invalid argument
		0 ,  \  -47 compilation wordlist deleted
		0 ,  \  -48 invalid POSTPONE
		0 ,  \  -49 search-order overflow
		0 ,  \  -50 search-order underflow
		0 ,  \  -51 compilation word list changed
		0 ,  \  -52 control-flow stack underflow
		0 ,  \  -53 exception stack overflow
		0 ,  \  -54 floating point underflow
		0 ,  \  -55 floating point unidentified fault
		0 ,  \  -56 QUIT
		0 ,  \  -57 exception in sending or receiving a character
		0 ,  \  -58 [IF] [ELSE] or [THEN] exception

		\ THROW codes -59 through -255 are reserved by ANS
		HERE  256 59 - CELLS  DUP ALLOT  0 FILL

		\ DOS error codes are moved from 1 and above to -256 and below
		0 , 	\ -256 (DOS  1) invalid DOS function number
   dos257 , 	\ -257 (DOS  2) file not found
   dos258 , 	\ -258 (DOS  3) path not found
		0 , 	\ -259 (DOS  4) too many open files
   dos260 , 	\ -260 (DOS  5) file access denied
   dos261 , 	\ -261 (DOS  6) invalid file handle
		0 , 	\ -262 (DOS  7) memory control blocks destroyed
		0 , 	\ -263 (DOS  8) not enough memory
		0 , 	\ -264 (DOS  9) invalid memory block address
		0 , 	\ -265 (DOS 10) invalid environment
		0 , 	\ -266 (DOS 11) invalid format
		0 , 	\ -267 (DOS 12) invalid file access code
		0 , 	\ -268 (DOS 13) invalid data
		0 , 	\ -269 (DOS 14) reserved by DOS
   dos270 , 	\ -270 (DOS 15) invalid drive
		0 , 	\ -271 (DOS 16) cannot remove current directory
		0 , 	\ -272 (DOS 17) not same device
		0 , 	\ -273 (DOS 18) no more files
		0 , 	\ -274 (DOS 19) disk is write protected
		0 , 	\ -275 (DOS 20) unknown unit
		0 , 	\ -276 (DOS 21) drive not ready
		0 , 	\ -277 (DOS 22) unknown command
		0 , 	\ -278 (DOS 23) CRC error
		0 , 	\ -279 (DOS 24) bad structure length
		0 , 	\ -280 (DOS 25) seek error
		0 , 	\ -281 (DOS 26) unknown media type
		0 , 	\ -282 (DOS 27) sector not found
		0 , 	\ -283 (DOS 28) printer out of paper
		0 , 	\ -284 (DOS 29) write fault
		0 , 	\ -285 (DOS 30) read fault
		0 , 	\ -286 (DOS 31) general failure
		0 , 	\ -287 (DOS 32) sharing violation
		0 , 	\ -288 (DOS 33) lock violation
		0 , 	\ -289 (DOS 34) invalid disk change
		0 , 	\ -290 (DOS 35) FCB not available
		0 , 	\ -291 (DOS 36) sharing buffer overflow
		0 , 	\ -292 (DOS 37) code page mismatch
		0 , 	\ -293 (DOS 38) cannot complete file operation
		0 , 	\ -294 (DOS 39) not enough disk space

VARIABLE FRAME  \ THROW frame address (return stack ptr) of last CATCHed word
VARIABLE ABORT"MSG  \ ptr to counted string of message -2 THROW is to display
VARIABLE LINE#  \ current line # of file if SOURCE-ID is a fileid

: CATCH  ( i*x xt -- j*x 0 | i*x n )  \ push exception frame and execute xt
   SP@ >R  FRAME @ >R  \ save data stack ptr and current frame
   RP@ FRAME !  \ this frame -> FRAME
   EXECUTE  \ execute the word xt
   R> FRAME !  \ nothing THROWn, restore FRAME
   R> DROP  0 ;  \ SP is the same, 0 = no error

: ERR-WORD  ( -- )  \ display the error word by putting ^^^s under it
   CR  PARSED 2@ 1 MAX  D# 256 MIN  >R  \ length of word, between 1 and 256
   SOURCE DROP -  D# 256 MIN  SPACES  \ spaces to get us out, less than 256
   QAD R@ [CHAR] ^ FILL  \ correct # of ^s for length of word
   QAD R> TYPE ;  \ blat 'em out

: FLINE  ( -- )  \ display current line and line # if INCLUDEing
   LINE# @ IF  CR  SOURCE TYPE  THEN  \ show line only if LINE# <> 0
   ERR-WORD CR  \ display error word in all cases
   LINE# @ ?DUP IF  ." Line # " .D  THEN ;  \ LINE# in decimal if INCLUDEing

: SQUIT  ( i*x -- \ j*x -- )  \ empty stacks and QUIT
   SP0 SP!  \ empty data stack
   [ LABEL PATCH-QUIT  0 , ] ;  \ forward reference to QUIT, to be patched l8r

: THROW-MSG  ( n -- a u )  \ return internal error message for throw code n
   DUP D# -2 = IF
	  DROP  ABORT"MSG @ COUNT  \ -2 = ABORT" (return user string)
   ELSE
	  DUP D# -294 0 WITHIN IF
		 NEGATE 1- CELLS  THROWS + @  \ fetch counted string ptr (or zero)
		 ?DUP IF  COUNT  ELSE  S" "  THEN  \ return string
	  ELSE  DROP S" "  THEN
   THEN ;

: THROW  ( k*x n -- k*x | i*x n )  \ THROW to last exception frame if n <> 0
   ?DUP IF  \ do nothing if n = 0
	  FRAME @ ?DUP IF  \ restore condition and control
		 RP!  \ restore return stack depth
		 R> FRAME !  \ restore FRAME
		 R> SWAP >R SP!  \ restore data stack depth
		 DROP  \ drop xt from CATCH
		 R> EXIT  \ restore control
	  THEN  \ FRAME = 0, no previous CATCH, display message and interpret
	  DUP INVERT IF  \ -1 = ABORT (don't display anything)
		 FLINE  \ display line and LINE#
		 DUP  THROW-MSG DUP IF  TYPE  \ display error message
		 ELSE  2DROP  ." Unknown exception # " .D
		 THEN
	  THEN  SQUIT  \ empty stack and QUIT (interpret)
   THEN ;

: NAME  ( "<spaces>name" -- c-addr u )  \ PARSE-WORD, abort if nothing parsed
   PARSE-WORD  DUP 0= D# -16 AND THROW ;

: ABORT  ( i*x -- \ i*x -- )  \ empty data stack and QUIT
   -1 THROW ;  \ -1 THROW is ABORT

: (ABORT"  ( x -- )  \ if x is nonzero THROW -2, which displays string, ABORTs
   IF  R> ABORT"MSG !  D# -2 THROW  \ ABORT"MSG -> inline string
   THEN  R> COUNT + >R ;  \ x = 0, skip inline string and continue

\ Number input conversion

\ convert and add digits from string to ud1 till we hit unconvertable char
: >NUMBER  ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
   BEGIN   DUP WHILE  \ u1 = zero?
	  >R DUP >R  C@ >UPPER  \ fetch a char and make sure it's uppercase
	  DUP [CHAR] : [CHAR] A WITHIN  0= WHILE  \ exit if non-digit
	  [CHAR] 0 -  \ partially convert to binary
	  D# 9 OVER <  D# 7 AND -  \ convert hex digits
	  DUP BASE @ U< WHILE  \ exit if non-digit
	  SWAP BASE @ UM* DROP  ROT BASE @ UM* D+  \ ud' = ud*base + digit
	  R> R> 1 /STRING  \ move string along
   REPEAT THEN  DROP R> R>  THEN ;

: ?DEF  ( x -- )  \ abort with "undefined" message if x is nonzero
   0= INVERT  D# -13 AND THROW ;  \ -13 is "Undefined word!"

\ convert string to binary single or double (trailing .), abort if non-number
: NUMBER?  ( c-addr u -- x false | xd true )
   0 0 2SWAP  \ set up for >NUMBER
   OVER C@ [CHAR] - =  DUP >R  \ first char a '-' sign? (flag on R:)
   IF  1 /STRING  THEN  \ yep, move string along one
   >NUMBER ?DUP IF  \ convert to binary, is it single or double?
	  1- ?DEF  \ no good if more than one char left
	  C@ [CHAR] . XOR ?DEF  \ postfixed '.' means number is double
	  R> IF  DNEGATE  THEN  -1 EXIT  \ chuck on the sign and exit
   ELSE  2DROP R> IF  NEGATE  THEN THEN  0 ;  \ single number

: NUMBERI  ( c-addr u -- x | xd )  \ interpret string as double or single #
   NUMBER? DROP ;

\ Searching and search order routines

=CX0 CONSTANT CONTEXT0  \ search order grows down from here
VARIABLE CONTEXT  \ pointer to top item on context (search order) stack
VARIABLE CURRENT  \ wid (wordlist id) of compilation wordlist

CREATE FORTH-WORDLIST  ( -- wid )  \ return wid of main Forth wordlist
   0 ,  \ chain of words linked in wordlist (patched by metacompiler)
   0 ,  \ link to previous wordlist (none)
   0 ,  \ pointer to FORTH header for display

VARIABLE WID-LINK  \ chain of all wordlists defined
FORTH-WORDLIST WID-LINK !  \ only FORTH-WORDLIST in chain now

LABEL >FORTH  \ pointer to FORTH header

: FORTH  ( -- )  \ replace top wordlist in search order with FORTH-WORDLIST
   FORTH-WORDLIST CONTEXT @ ! ;

>FORTH FORTH-WORDLIST 2 CELLS + !  \ patch header pointer in FORTH-WORDLIST

: GET-CURRENT  ( -- wid )  \ return wid of compilation wordlist
   CURRENT @ ;

: SET-CURRENT  ( wid -- )  \ set compilation wordlist to wid
   CURRENT ! ;

: GET-ORDER  ( -- widn ... wid1 n )  \ get wordlists on search order and #
   0 CONTEXT0 BEGIN
	  DUP CONTEXT @ - WHILE  \ loop while our pointer <> CONTEXT
	  D# 2 - DUP @ ROT ROT  \ decrement pointer, fetch one wid
	  SWAP 1+ SWAP  \ increment count (n)
   REPEAT  DROP ;

: SET-ORDER  ( widn ... wid1 n -- )  \ set search order, wid1 searched first
   DUP 0< IF  DROP FORTH-WORDLIST 1  THEN  \ n = -1 means minimum order
   CONTEXT0 OVER CELLS -  DUP CONTEXT !  \ make space on context
   SWAP 0 ?DO  TUCK !  CELL+  LOOP  DROP ;  \ store the wids

: ONLY  ( -- )  \ set the minimum search order
   -1 SET-ORDER ;

: DEFINITIONS  ( -- )  \ make top wordlist in context the compilation wordlist
   CONTEXT @ @ SET-CURRENT ;

\ search for name c-addr u in wid; return not found, xt immediate, xt normal
CODE SWID  ( c-addr u wid -- c-addr u 0 | xt 1 | xt -1 )
   BX POP  CX POP  SI DX MOV  SI POP  \ bx = wid, cx = u, si -> name
2 L:
   0 [BX] BX MOV  \ bx -> previous entry in name chain
   BX BX OR  1 L JZ  \ end of chain, exit
   2 [BX] AL MOV  MAXN # AL AND  \ al = dict name count (mask off flag bits)
   AL CL CMP  2 L JNE  \ loop if lengths not equal
   3 [BX] DI LEA  \ di -> dict name
   SI PUSH  CX PUSH  \ save c-addr u
   WORD SCMP [] CALL  \ REP BYTE CMPS (case sensitive or otherwise)
   CX POP  SI POP  2 L JNE  \ restore c-addr u, loop if strings not equal
   -1 # AX MOV  2 [BX] DI MOV  \ ax = flag, di = count/flags
   =IMMED # DI TEST  4 L JZ  2 # AX ADD  \ immediate, flag = 1
4 L:
   MAXN # DI AND  3 [BX+DI] BX LEA  \ bx = execution token (xt)
   BX PUSH  AX PUSH  3 L # JMP  \ push xt 1 or xt -1 and exit
1 L:
   SI PUSH  CX PUSH  0 # PUSH  \ not found, push c-addr u 0
3 L:
   DX SI MOV  NEXT  \ restore SI and exit
END-CODE

: SALL  ( c-addr u -- c-addr u 0 | xt 1 | xt -1 )  \ search whole context
   CONTEXT @ BEGIN
	  DUP CONTEXT0 U< WHILE  \ stop if at bottom of search order stack
	  DUP >R @ SWID  \ search one wordlist
	  ?DUP IF  R> DROP EXIT  THEN  \ found it?
	  R> CELL+  \ no, search next wordlist
   REPEAT  DROP 0 ;  \ not found in any wordlist

: FIND  ( c-addr -- c-addr 0 | xt 1 | xt -1 )  \ search for counted string
   COUNT SALL ?DUP 0= IF  DROP 1- 0  THEN ;

\ Compiling words

VARIABLE WARNINGS  -1 WARNINGS !  \ "do-you-want-warnings?" flag
VARIABLE STATE  \ nonzero if compiling, zero if interpreting
VARIABLE CSP  \ check stack pointer
VARIABLE LAST  \ pointer to most recently created header

CODE ALIGN  ( -- )  \ align the data space pointer
   NEXT
END-CODE IMMEDIATE  \ let's make it a no-op

: ALLOT  ( n -- )  \ allot n bytes of data space
   DP +! ;

: ,  ( x -- )  \ compile cell x to data space
   HERE !  [ 1 CELLS ] LITERAL ALLOT ;

: COMPILE,  ( xt -- )  \ compile execution token into code space
   , ;  \ same as , in Third (direct threaded)

: C,  ( char -- )  \ compile char to data space
   HERE C!  [ 1 CHARS ] LITERAL ALLOT ;

: COMPILE  ( -- )  \ compile next inline word into code space
   R> DUP CELL+ >R  \ don't execute next word
   @ COMPILE, ;  \ instead fetch and COMPILE, it

: POSTPONE  ( "<spaces>name" -- )  \ compile compilation semantics of name
   NAME SALL DUP 0= ?DEF  \ parse name, get it's xt
   0< IF  POSTPONE COMPILE  THEN  \ "name" is non-immediate
   COMPILE, ; IMMEDIATE  \ compile xt

: LITERAL  ( x -- )  \ compile cell x as inline literal
   POSTPONE (LIT , ; IMMEDIATE  \ E( -- x )

: NUMBER,  ( c-addr u -- )  \ compile string as inline literal
   NUMBER? IF  SWAP POSTPONE LITERAL  THEN  \ double E( -- xd )
   POSTPONE LITERAL ;  \ single E( -- x )

: !CSP  ( -- )  \ save stack depth
   SP@ CSP ! ;

: ?CSP  ( -- )  \ check stack depth
   SP@ CSP @ =  \ THROW if CSP is not equal to SP@
   0= D# -22 AND THROW ;  \ -22 is "Unbalanced stack!"

: S,  ( c-addr u -- )  \ compile string as counted string
   HERE  OVER 1+ ALLOT  PLACE ;

: HEADER,  ( "<spaces>name" -- )  \ compile header for name, don't reveal
   NAME  MAXN MIN  \ parse name, truncate length
   WARNINGS @ IF  \ are WARNINGS on?
	  2DUP SALL IF  \ yep, if name's already defined, display warning
		 DUP FLINE ." Warning: redefined word. "  THEN 2DROP  THEN
   HERE LAST !  \ update LAST
   GET-CURRENT @ ,  \ link to previous word in CURRENT wordlist
   S, ;  \ compile name as counted string

: REVEAL  ( -- )  \ reveal most recently defined word
   LAST @ ?DUP IF  \ LAST = 0 means last word was :NONAMEd
	  GET-CURRENT !  \ update chain in current wordlist
   THEN ;

: REL,  ( xt -- )  \ compile xt as Intel relative (from HERE) address
   HERE CELL+ - , ;

: CALL,  ( xt -- )  \ compile xt as "CALL xt" instruction
   H# E8 C, REL, ;  \ $E8 is opcode for "CALL rel16"

: [  ( -- )  \ set interpretation state IMMEDIATEly
   0 STATE ! ; IMMEDIATE

: ]  ( -- )  \ set compilation state
   -1 STATE ! ;

: :  ( "<spaces>name" -- )  \ begin colon definition "name"
   HEADER,  !CSP  ]  \ compile header, store stack pointer, begin compiling
   [ (: ] LITERAL CALL, ;  \ compile "CALL (:" instruction

: ;  ( -- )  \ end colon definition, allowing it to be found
   ?CSP  POSTPONE [  \ check stack, begin interpreting
   POSTPONE EXIT  REVEAL ; IMMEDIATE  \ put in an EXIT and reveal

: IMMEDIATE  ( -- )  \ make most recent definition immediate
   LAST @ CELL+  DUP C@  \ fetch last word's flags byte
   [ =IMMED ] LITERAL OR SWAP C! ;  \ set IMMEDIATE bit and re-store

\ File handling words

0 CONSTANT R/O  \ read-only file access method (fam)
1 CONSTANT W/O  \ write-only fam
2 CONSTANT R/W  \ read/write fam

CODE BIN  ( fam -- fam' )  \ modify fam to select BINary file access
   NEXT
END-CODE IMMEDIATE  \ do-nothing in Third (ie., under DOS)

CODE BYE  ( -- )  \ exit to DOS (put here for lack of a better place)
   H# 4C00 # AX MOV  H# 21 INT  \ exit with error level 0
END-CODE

CODE DOS  ( dx cx bx ax -- 'ax 'dx carry-flag )  \ generic DOS call for files
   AX POP  BX POP  CX POP  DX POP  \ pop and set registers
   H# 21 INT  \ DOS interrupt
   BX BX SBB  \ setup bx = carry flag
   AX PUSH  DX PUSH  BX PUSH  NEXT  \ push results
END-CODE

CODE FATTRIB  ( dx -- cx ax flag )  \ DOS routine for FILE-STATUS
   DX POP  \ dx -> ASCIIZ filename
   H# 4300 # AX MOV  H# 21 INT  \ get cx = file attributes
   BX BX SBB  CX PUSH  AX PUSH  BX PUSH  NEXT  \ push stuff
END-CODE

CODE FRENAME  ( dx di -- ax flag )  \ DOS routine for RENAME-FILE
   DI POP  DX POP  \ dx -> old name, di -> new name
   DS PUSH  ES POP  \ make sure es = ds
   H# 56 # AH MOV  H# 21 INT  \ rename file
   BX BX SBB  AX PUSH  BX PUSH  NEXT  \ push results
END-CODE

: >IOR  ( ax flag -- ax ior )  \ convert ax and carry flag to ax, I/O result
   IF  DUP  D# -255 SWAP -  EXIT  \ from DOS 1...39 to Third -256...-294
   THEN  0 ;  \ if flag is false, ior = 0

: >ASCIIZ  ( c-addr u -- asciiz-addr )  \ convert string to ASCIIZ, give addr
   QAD  2DUP +  0 SWAP C!  \ store NULL byte on end of string
   SWAP MOVE  QAD ;  \ move chars and return the ASCIIZ addr

: OPEN-FILE  ( c-addr u fam -- fileid ior )  \ open file named c-addr u
   >R  >ASCIIZ  0  0  \ filename -> ASCIIZ
   R> H# 3D00 OR  DOS  NIP >IOR ;  \ open file and return fileid ior

: CLOSE-FILE  ( fileid -- ior )  \ close file identified by fileid
   >R 0 0 R> H# 3E00  DOS  NIP >IOR NIP ;

: CREATE-FILE  ( c-addr u fam -- fileid ior )  \ create file named c-addr u
   >R  2DUP >ASCIIZ  H# 20 0 H# 3C00  DOS NIP  \ create the file
   >IOR ?DUP IF  2SWAP 2DROP  R> DROP EXIT  THEN  \ exit on error
   CLOSE-FILE DROP  R> OPEN-FILE ;  \ now re-open file with fam

: DELETE-FILE  ( c-addr u -- ior )  \ delete file named c-addr u
   >ASCIIZ 0 0 H# 4100  DOS  NIP >IOR NIP ;

: FPOS  ( d where fileid -- ud ior )  \ set relative file position d
   SWAP H# 4200 OR  \ where: =0 start of file, =1 from current, =2 from EOF
   DOS  \ move the file position, ud is new absolute position
   DUP IF  NIP >IOR  TUCK  THEN ;  \ ud is undefined if ior <> 0

: FILE-POSITION  ( fileid -- ud ior )  \ return fileid's current position ud
   >R 0 0 1 R> FPOS ;  \ set 0 0 from current position to get position

: REPOSITION-FILE  ( ud fileid -- ior )  \ set fileid's position to ud
   0 SWAP FPOS  NIP NIP ;  \ set from start of file

: FILE-SIZE  ( fileid -- ud ior )  \ return fileid's size in characters
   DUP >R  FILE-POSITION ?DUP IF  R> DROP EXIT  THEN  \ save current pos
   0 0 D# 2 R@ FPOS  DROP  \ find file size (ior for this is ignored! :-)
   2SWAP R> REPOSITION-FILE ;  \ restore old file position

: WRITE-FILE  ( c-addr u fileid -- ior )  \ write u chars from c-addr to fileid
   H# 4000 DOS  NIP >IOR NIP ;

: RESIZE-FILE  ( ud fileid -- ior )  \ resize file to ud chars
   DUP >R  REPOSITION-FILE ?DUP IF  R> DROP EXIT  THEN  \ set file pos to ud
   0 0 R> WRITE-FILE ;  \ writing zero bytes truncates to this length in DOS

: WRITE-LINE  ( c-addr u fileid -- ior )  \ write c-addr u following by EOL
   DUP >R  WRITE-FILE ?DUP IF  R> DROP EXIT  THEN  \ write line
   CR/LF COUNT R> WRITE-FILE ;  \ then write CR,LF

: READ-FILE  ( c-addr u1 fileid -- u2 ior )  \ read max u1 chars from fileid
   H# 3F00 DOS  NIP >IOR ;  \ u2 is the # of chars actually read

\ LENGTH and READ-LINE are supposed to handle DOS (CR/LF) and Unix LF
\ end-of-line sequences properly, but don't count on good behaviour if both
\ types are in the same file!

\ LENGTH will include the CR in the length if the string has just a CR on the
\ very end (if your buffer isn't quite big enough for a coming LF, say)

CODE LENGTH  ( c-addr u -- length #delims )  \ find length of line in c-addr u
   CX POP  DI POP  CX DX MOV  \ cx,dx = u, di = c-addr
   DS PUSH ES POP  10 # AL MOV  \ look for line feed character
   REPNE BYTE SCAS  1 L JNE  \ scan until LF hit, if any
   2 # DX CMP  2 L JB  \ string less than 2 chars long?
   DX DEC  CX DX SUB  3 L JZ  \ only LF on the line?
   13 # BYTE -2 [DI] CMP  3 L JNE  \ CR before the line feed?
   DX DEC  DX PUSH  2 # PUSH  NEXT  \ exclude 2nd delimiter char from length
3 L:
   DX PUSH  1 # PUSH  NEXT
2 L:
   0 # PUSH  DX PUSH  NEXT  \ length = 0, delim = 0 or 1
1 L:
   DX PUSH  0 # PUSH  NEXT  \ length = u, no delimiter
END-CODE

: READ-LINE  ( c-addr u1 fileid -- u2 flag ior )  \ read line max u1 from file
   >R  2DUP CELL+  R@ READ-FILE  \ try to read u1+2 chars (to get CR,LF)
   ?DUP IF  R> DROP NIP EXIT  THEN  \ read error occured?
   ?DUP IF  \ end of file (ie., zero bytes read?)
	  SWAP >R  TUCK LENGTH  \ find length of line and # of delimiters
	  SWAP DUP R@ > IF  \ if line too long then length=u1, delims=0
		 2DROP 0 R>
	  ELSE  R> DROP  THEN  \ else drop r1
	  >R - R> TUCK -  \ calculate # of chars to backtrack (length+delims)
	  NEGATE S>D 1 R> FPOS  \ move file's pos back that far
	  NIP NIP  -1 SWAP  EXIT  \ return line length, true, and ior
   THEN  2DROP 0 0 0  R> DROP ;  \ u2 = flag = ior = 0

: FLUSH-FILE  ( fileid -- ior )  \ flush file and update directory
   0 0 ROT H# 4500 DOS  \ get duplicate file handle
   NIP >IOR ?DUP IF  NIP EXIT  THEN  \ error?
   CLOSE-FILE ;  \ close the duplicate to flush

: FILE-STATUS  ( c-addr u -- x ior )  \ return status about file named c-addr u
   >ASCIIZ FATTRIB >IOR NIP ;  \ x = DOS file attributes

: RENAME-FILE  ( c-addr1 u1 c-addr2 u2 -- ior )  \ rename file str1 to str2
   2>R  >ASCIIZ  2R>  \ convert c-addr1 u1 to ASCIIZ
   RAD  2DUP +  0 SWAP C!  SWAP MOVE  \ store NULL on end and copy string
   RAD FRENAME >IOR NIP ;  \ call DOS to rename

: SAVED  ( c-addr u -- )  \ save the Third system as file named c-addr u
   W/O CREATE-FILE THROW  DUP  \ create the file
   D# 256 HERE OVER -  ROT WRITE-FILE THROW  \ write bytes from 256 to HERE
   CLOSE-FILE THROW ;  \ and close the file

: SAVE  ( "name" -- )  \ parse name and save
   NAME SAVED ;

\ Main Forth interpreter

VARIABLE SID  \ source identification cell (ie., SOURCE-ID)
VARIABLE IBUF  \ pointer to the input buffer
256 CONSTANT MAX-LINE  \ maximum line length for REFILL etc
MAX-LINE 2 + CONSTANT FAX-LINE  \ # chars for READ-LINE buffer must be +2

: SOURCE-ID  ( -- 0 | -1 | fileid )  \ return the source identification
   SID @ ;  \ 0 = text, -1 = EVALUATE, fileid = text file

: REFILL  ( -- flag )  \ refill input source from input device, true = ok
   SOURCE-ID -1 = IF  0 EXIT  THEN  \ if EVALUATEing return false
   IBUF @ DUP MAX-LINE  SOURCE-ID IF  \ reading from text file?
	  SOURCE-ID READ-LINE THROW  \ yep, read one line
	  0= IF  2DROP  0 EXIT  THEN  \ end of file?
	  1 LINE# +!  \ nope, increment line number
   ELSE  ACCEPT  \ reading from keyboard
   THEN  SOURCE!  -1 ;  \ set SOURCE >IN and exit

: (  ( "ccc<paren>" -- )  \ parse, and discard till ending ')' found
   SOURCE-ID 0 > IF  \ SOURCE-ID > 0 if including from file
	  BEGIN  \ including from text file, REFILL till we find a ')'
		 >IN @  [CHAR] ) PARSE NIP  \ parse for a ')'
		 >IN @ ROT - = WHILE  \ did we find one?
		 REFILL 0= IF  EXIT  THEN  \ no, refill and loop (exit if EOF)
	  REPEAT
   ELSE  [CHAR] ) PARSE 2DROP  THEN ; IMMEDIATE  \ not a file, use normal (

: .(  ( "ccc<paren>" -- )  \ display string ccc then and there
   [CHAR] ) PARSE  TYPE ; IMMEDIATE  \ it's an immediate word

: ?STACK  ( -- )  \ THROW error if the stack is underflowing
   DEPTH 0< D# -4 AND THROW ;  \ -4 is "Stack underflow!"

CREATE DO-WORD  \ table of execution tokens of what to do with a parsed word
   ' COMPILE, ,  \ non-immediate, compiling
   ' EXECUTE ,   \ non-immediate, interpreting
   ' NUMBER, ,   \ word not found, compiling
   ' NUMBERI ,   \ word not found, interpreting
   ' EXECUTE ,   \ immediate, compiling
   ' EXECUTE ,   \ immediate, interpreting

: INTERPRET  ( i*x -- j*x )  \ interpret the parse area (input buffer)
   BEGIN  PARSE-WORD DUP WHILE  \ parse till empty
	  SALL  \ now we have on stack: xt -1 | c-addr u 0 | xt 1
	  1+ 2*  \ 0 = non-immed, 2 = not found, 4 = immed
	  STATE @ 1+  \ 0 = compiling, 1 = interpreting
	  + CELLS  \ add the above two together to get offset into DO-WORD
	  DO-WORD + @ EXECUTE  ?STACK  \ execute handler and check stack
   REPEAT  2DROP ;

DEFER PROMPT  \ prompt displayer

: .OK  ( -- )  \ display "ok" if interpreting
   STATE @ 0= IF  ." ok"  THEN ;  \ STATE = 0 means interpreting

' .OK IS PROMPT  \ default prompt is just "ok"

: INIT  ( -- )  \ initialise QUIT for interpreting
   POSTPONE [  0 SID !  0 LINE# ! ;  \ set required variables

: QUIT  ( -- \ i*x -- )  \ empty return stack and interpret from keyboard
   RP0 RP!  INIT  \ initialise everything
   BEGIN  CR  \ this nice CR here is apparently non-Standard :-)
	  REFILL DROP  SPACE  \ REFILL input buffer
	  INTERPRET  PROMPT  \ interpret and show prompt
   AGAIN ;  \ loop forever... :-)

' QUIT PATCH-QUIT !  \ patch QUIT forward reference in SQUIT

: SAVE-INPUT  ( -- xn ... x1 n )  \ return current input source, n cells
   PARSED 2@  \ a u of last parsed string
   IBUF @  SOURCE-ID  >IN @  \ IBUF SOURCE-ID and >IN
   LINE# @  SOURCE  \ LINE# and SOURCE (two cells)
   D# 8 ;  \ total 8 cells on stack

: RESTORE-INPUT  ( xn ... x1 n -- flag )  \ restore input source, true = error
   DUP D# 8 = IF  \ right # of cells to restore?
	  DROP SRC 2!  \ restore SOURCE (two cells)
	  LINE# !  >IN !  SID !  IBUF !  \ restore LINE# >IN SOURCE-ID and IBUF
	  PARSED 2!  \ restore the two cells of PARSED
	  0 EXIT  \ success!
   THEN  0 ?DO  DROP  LOOP  -1 ;  \ n <> 8, drop n items

: EVALUATE  ( i*x c-addr u -- j*x )  \ interpret string c-addr u
   SAVE-INPUT >R  \ save input source
   2>R 2>R 2>R 2>R  \ we can do this, we know n = 8
   SOURCE!  -1 SID !  \ SOURCE-ID is -1 when EVALUATEing
   INTERPRET  \ interpret the string
   2R> 2R> 2R> 2R>  R> RESTORE-INPUT DROP ;  \ restore input source

\ Nested file inclusion

: INCLUDE-FILE  ( i*x fileid -- j*x )  \ include file, set SOURCE-ID to fileid
   SAVE-INPUT >R  2>R 2>R 2>R 2>R  \ we can do this, we know there are 8 cells
   SID !  0 LINE# !  \ store fileid in SOURCE-ID, reset LINE#
   FAX-LINE MALLOC IBUF !  \ allocate another IBUF
   BEGIN  REFILL WHILE  \ read lines until EOF
	  ['] INTERPRET CATCH ?DUP  \ CATCH the interpreter
	  IF  FAX-LINE MFREE  THROW  THEN  \ error, free line buffer and reTHROW
   REPEAT  FAX-LINE MFREE  \ free up IBUF
   SOURCE-ID CLOSE-FILE THROW  \ ANS says this must close file
   2R> 2R> 2R> 2R>  R> RESTORE-INPUT DROP ;  \ restore input source

: INCLUDED  ( i*x c-addr u -- j*x )  \ include named file
   R/O OPEN-FILE THROW  INCLUDE-FILE ;  \ open file and include

: INCLUDE  ( i*x "name" -- j*x )  \ parse name and include
   NAME INCLUDED ;

\ Conditional clauses

: >MARK  ( -- orig )  \ mark a forward branch
   HERE  0 , ;  \ origin of forward branch, allot space for it

: IF  ( -- orig )  \ begin an IF..THEN or IF..ELSE..THEN clause
   POSTPONE (IF  \ (IF branches to ELSE or THEN at runtime if TOS = 0
   >MARK ; IMMEDIATE

: THEN  ( orig -- )  \ end an IF..THEN or IF..ELSE..THEN clause
   HERE SWAP ! ; IMMEDIATE  \ resolve marked forward branch

: ELSE  ( orig1 -- orig2 )  \ begin an ELSE..THEN clause
   POSTPONE (ELSE  >MARK  \ (ELSE is an unconditional branch to THEN
   SWAP  POSTPONE THEN ; IMMEDIATE  \ resolve IF orig

\ Looping structures

: BEGIN  ( -- dest )  \ begin a BEGIN..UNTIL or BEGIN..WHILE..REPEAT loop
   HERE ; IMMEDIATE  \ mark destination of loop

: UNTIL  ( dest -- )  \ end a BEGIN..UNTIL loop
   POSTPONE (IF , ; IMMEDIATE  \ branch to BEGIN if TOS = 0 at runtime

: WHILE  ( dest -- orig dest )  \ begin a WHILE..REPEAT clause
   POSTPONE IF  SWAP ; IMMEDIATE  \ jump to after REPEAT if TOS = 0 at runtime

: AGAIN  ( dest -- )  \ end a BEGIN..AGAIN loop
   POSTPONE (ELSE , ; IMMEDIATE  \ unconditional branch to BEGIN at runtime

: REPEAT  ( orig dest -- )  \ end a BEGIN..WHILE..REPEAT loop
   POSTPONE AGAIN  \ unconditional branch to BEGIN
   POSTPONE THEN ; IMMEDIATE  \ resolve WHILE branch

: DO  ( -- loop-sys )  \ begin a DO..LOOP construct
   POSTPONE (DO  >MARK  \ resolved by LOOP
   HERE ; IMMEDIATE  \ beginning of loop address

: ?DO  ( -- loop-sys )  \ begin a ?DO..LOOP construct
   POSTPONE (?DO  >MARK  \ resolved by LOOP
   HERE ; IMMEDIATE  \ beginning of loop address

: LOOP  ( loop-sys -- )  \ end a DO..LOOP construct
   POSTPONE (LOOP ,  \ branch to DO if loop counter not overflowed at runtime
   POSTPONE THEN ; IMMEDIATE  \ resolve "end-of-loop" address

: +LOOP  ( loop-sys -- )  \ end a DO..+LOOP construct
   POSTPONE (+LOOP ,  \ branch to DO if loop counter not overflowed at runtime
   POSTPONE THEN ; IMMEDIATE  \ resolve "end-of-loop" address

\ Words needed for compiling words

: >NAME  ( hp -- addr u )  \ return name string given header pointer
   CELL+ COUNT  MAXN AND ;  \ be sure to mask off flags in count byte

: >XT  ( hp -- xt )  \ return execution token given header pointer
   >NAME + ;  \ code for word directly follows name string

: '  ( "name" -- xt )  \ return name's execution token
   NAME SALL 0= ?DEF ;  \ search CONTEXT stack, abort if undefined

: CHAR  ( "name" -- char )  \ return first character of "name"
   NAME DROP C@ ;

: REL!  ( addr dest -- )  \ resolve Intel relative branch
   TUCK  CELL+ -  SWAP ! ;  \ store relative for jump to addr at dest

: >BODY  ( xt -- a-addr )  \ return CREATEd xt's data address
   D# 3 + ;  \ skip 3-byte CALL instruction

\ Compiling words
\ Note: execution time (ie., run-time) stack is given by E( .. -- .. )

: [']  ( "name" -- )  \ return name's execution token at runtime
   ' POSTPONE LITERAL ; IMMEDIATE  \ E( -- xt )

: LAST-XT  ( -- xt )  \ return xt of most recently defined word
   LAST @ >XT ;

: RECURSE  ( -- )  \ recurse to current word at runtime
   LAST-XT COMPILE, ; IMMEDIATE  \ E( i*x -- j*x )

: [CHAR]  ( "name" -- )  \ return first char of name at runtime
   CHAR POSTPONE LITERAL ; IMMEDIATE  \ E( -- char )

: ,"  ( "ccc<quote>" -- )  \ parse string ccc ended with " and compile it
   [CHAR] " PARSE  S, ;

: ."  ( "str" -- )  \ parse str and display it at runtime
   POSTPONE (."  ," ; IMMEDIATE  \ E( -- )

: ABORT"  ( "str" -- )  \ abort and display str if TOS <> 0 at runtime
   POSTPONE (ABORT"  ," ; IMMEDIATE  \ E( flag -- )

: S"  ( "str" -- c-addr u | )  \ parse and return string c-addr u, start-smart
   STATE @ IF  POSTPONE (S"  ,"  \ compiling, E( -- c-addr u )
   ELSE  [CHAR] " PARSE  HERE PLACE  HERE COUNT  \ interpreting
   THEN ; IMMEDIATE

: (DOES>  ( -- )  \ patch last CREATEd word to execute code following DOES>
   R>  LAST-XT 1+ REL! ;  \ patch "CALL (VAR" to "CALL stuff-after-DOES>"

: DOES>  ( -- )  \ give last CREATEd word a runtime definition
   POSTPONE (DOES>  ['] (: CALL, ; IMMEDIATE  \ E( -- )

\ Defining words

: HEADER  ( "name" -- )  \ create header for "name"
   HEADER, REVEAL ;  \ allow name to be found in dictionary

: CONSTANT  ( x "name" -- )  \ create constant which will return x
   HEADER  ['] (CONST CALL,  , ;  \ "name" will put x on stack when executed

: CREATE  ( "name" -- )  \ create word which will return its data address
   HEADER  ['] (VAR CALL, ;  \ "name" will put a-addr on stack when executed

: VARIABLE  ( "name" -- )  \ create 1-cell variable which returns its address
   CREATE  0 , ;  \ "name" will put a-addr on stack when executed

\ Search order and vocabularies

: WORDLIST  ( -- wid )  \ create a new, empty wordlist
   HERE  \ the wid, a pointer to the wordlist structure
   0 ,  \ NULL chain means no words in chain
   WID-LINK @ ,  DUP WID-LINK !  \ link to previous wordlist
   0 , ;  \ points to vocabulary's header (initially NULL)

: VOCABULARY  ( "name" -- )  \ create a named vocabulary
   HERE  \ vocabulary's header pointer
   CREATE WORDLIST  D# 4 + !  \ create wordlist, store header pointer
   DOES>  ( -- )  \ when executed it replaces first wid in search order
   CONTEXT @ ! ;

: SEARCH-WORDLIST  ( c-addr u wid -- 0 | xt 1 | xt -1 )
   SWID ?DUP 0= IF  2DROP 0  THEN ;

: ALSO  ( -- )  \ duplicate top wordlist on the search order stack
   GET-ORDER  OVER SWAP 1+ SET-ORDER ;

: PREVIOUS  ( -- )  \ drop a wordlist from the search order stack
   GET-ORDER  NIP 1-  SET-ORDER ;

: .NAME  ( hp -- )  \ display name of word given a header pointer
   >NAME TYPE  SPACE ;

: .WID  ( wid -- )  \ display name (if any) of vocabulary identified by wid
   D# 4 + @ ?DUP IF  .NAME  ELSE  ." ??? "  THEN ;

: ORDER  ( -- )  \ display search order and compilation wordlists
   GET-ORDER  0 ?DO  .WID  LOOP  \ display names of wids on search order
   ." / "  GET-CURRENT .WID ;  \ display compilation wordlist

\ Startup words

DEFER BOOTER  ' (VAR IS BOOTER  \ word to execute at COLD, (VAR is a no-op

: THIRD  ( -- )  \ display info about Third
   ." Third v0.3 - A small Forth compiler by Benjamin Hoyt - 07feb2000" ;

CODE SMALLER  ( -- ax carry-flag )  \ free up all memory but Third's 64k
   H# 4A # AH MOV  4096 # BX MOV  \ resize memory block to 64k (4k paras)
   DS PUSH ES POP  H# 21 INT  \ do the DOS call (make sure es=ds)
   BX BX SBB  AX PUSH  BX PUSH  NEXT  \ ax = ior, flag = true if error (cf set)
END-CODE

: COLD  ( -- )  \ cold startup, gets Forth going in startup
   [ =MP0 ] LITERAL MPTR !  DECIMAL  0 FRAME !  \ init MPTR BASE FRAME
   ONLY  DEFINITIONS  \ init search order and compilation wordlist
   MAX-LINE MALLOC IBUF !  \ allocate mem for TIB
   INIT  \ initialise interpreter
   SMALLER >IOR THROW DROP  \ shrink ourselves, THROWing any error
   BOOTER  \ execute the BOOTER (for turnkey applications)
   H# 80 COUNT EVALUATE  \ interpret the DOS command line
   THIRD  QUIT ;  \ display Third stuff and QUIT (begin interpreting)

CR .( Saving target system as Kernel.COM ... )

]META Kernel.COM  \ end metacompilation and save target
WARNINGS !  \ restore WARNINGS

CR .( Success!) CR
