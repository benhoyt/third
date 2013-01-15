\ An 8086 assembler for Third

BASE @  DECIMAL  \ save BASE and switch to DECIMAL
WARNINGS @  FALSE WARNINGS !  \ turn off redefinition warnings

\ General and housekeeping words

VOCABULARY ASSEMBLER  \ ASSEMBLER vocab to avoid name clashes

ALSO ASSEMBLER  DEFINITIONS  \ compile into assembler vocabulary

8 BASE !  \ octal is a great BASE for ModR/M fields

: REGS  ( #regs mode "names" -- )  \ create registers with modes
   SWAP 0 DO  DUP I  11 * SWAP  1000 * OR  CONSTANT  LOOP  DROP ;

\ Register names: They are simply specialised CONSTANTs

10 0 REGS  AL  CL  DL  BL  AH  CH  DH  BH
10 1 REGS  AX  CX  DX  BX  SP  BP  SI  DI
10 2 REGS  [BX+SI] [BX+DI] [BP+SI] [BP+DI] [SI] [DI] [BP] [BX]
 4 3 REGS  ES  CS  SS  DS
 2 4 REGS  #   []  \ immediate, direct memory

\ Defer the target store, fetch, and compile words for metacompiling

DEFER THERE  ' HERE IS THERE  \ use defaults
DEFER TC@    ' C@   IS TC@
DEFER TC!    ' C!   IS TC!
DEFER TC,    ' C,   IS TC,
DEFER T,     '  ,   IS T,

\ Addressing modes

: MD  ( mode -- )  \ define mode tester word
   CREATE  1000 * ,
   DOES>  ( op -- t=op-is-mode )  @  SWAP 7000 AND  = ;

1 MD R16?  \ 16 bit reg
2 MD MEM?  \ register indirect
3 MD SEG?  \ segment register

: REG?  ( op -- t=op-is-reg )  \ is operand a general register?
   7000 AND  2000 < ;

: BIG?  ( x -- t=16big )  \ is x > signed 8 bits?
   -200 200 WITHIN  0= ;

: OFFSET  ( dest src -- relofs )  \ calculate relative offset and check range
   1+ -  DUP BIG? ABORT" Branch too far!" ;

: RLOW  ( op -- op-low-3-bits )  \ return R/M field of operand
   007 AND ;

: RMID  ( op -- op-mid-3-bits )  \ return reg/opcode field of operand
   070 AND ;

VARIABLE SIZE  \ true to force 16 bits

: BYTE  ( -- )  \ set 8 bit (byte) operand
   0 SIZE ! ;

: WORD  ( -- )  \ set (16 bit) word operand
   -1 SIZE ! ;

WORD  \ default to 16 bits

: OP,  ( x1 x2 -- )  \ OR and compile the byte
   OR TC, ;

: W,  ( op reg -- )  \ compile op with low bit set if reg is word
   R16?  1 AND  OP, ;

: SIZE,  ( op -- )  \ compile op with low bit set if SIZE is 16 bit
   SIZE @  1 AND  OP, ;

: B/W,  ( x flag -- )  \ compile x as byte, or word if flag is TRUE
   IF  T,  ELSE  TC,  THEN ;

\ Addressing assembling

: RR,  ( rlow rmid -- )  \ compile ModR/M byte with two regs
   RMID  SWAP RLOW  OR  300 OP, ;

: MEM,  ( disp mem rmid -- )  \ compile ModR/M byte, possible displacement
   RMID  OVER [] =
   IF  6 OP,  DROP  T,  \ 16 bit direct address
   ELSE  OVER RLOW OR  ROT ROT  [BP] =  OVER 0= AND
      IF  SWAP  100 OP,  TC,  \ 0 [BP] requires offset
      ELSE  SWAP  OVER BIG?
         IF  200 OP, T,  ELSE  OVER  \ 16 bit offset
            IF  100 OP, TC,  \ 8 bit offset
            ELSE  OP,  \ zero offset
   THEN THEN THEN THEN ;

: WMEM,  ( disp mem reg op )  \ compile ModR/M dependant on op's size
   OVER W,  MEM, ;

: R/M,  ( reg reg | disp mem reg -- )  \ compile reg,reg or mem,reg
   OVER REG? IF  RR,  ELSE  MEM,  THEN ;

: SR/M,  ( reg rmid opc | disp mem rmid opc -- )  \ dependant on SIZE
   2 PICK  DUP REG?  \ opc is an opcode
   IF  W, RR,  ELSE  DROP SIZE,  MEM,  THEN WORD ;

: WR/M,  ( reg reg opc | disp mem reg opc -- )  \ dependant on operand size
   2 PICK DUP REG? IF  W, RR,  ELSE  DROP OVER W,  MEM,  THEN WORD ;

\ Defining words to generate opcodes

HEX  \ switch to HEX for opcodes

: CC,  ( char "name" -- )  \ create and compile char
   CREATE C, ;

: SINGLE  ( opc "inst" -- )  \ all single byte instructions
   CC,  DOES>  ( -- )  C@ TC, ;

: AAMD  ( opc "inst" -- )  \ AAM and AAD
   CC,  DOES>  ( -- )  C@ TC,  0A TC, ;

: JCC  ( opc "inst" -- )  \ 8-bit offset branch instructions
   CC,  DOES>  ( dest -- )  C@ TC,  THERE OFFSET  TC, ;

: EFF  ( opc "inst" -- )  \ load EFFective address/segment instructions
   CC,  DOES>  ( mem reg -- )  C@ TC,  MEM, ;

: STR  ( opc "inst" -- )  \ string instructions
   CC,  DOES>  ( -- )  C@ SIZE,  WORD ;

: UNARY  ( opc "inst" -- )  \ unary (one) operand instructions like MUL
   CC,  DOES>  ( r/m -- )  C@  0F6 SR/M, ;

: PORT  ( opc "inst -- )  \ port instructions like IN OUT
   CC,  DOES>  ( #/dx al/ax -- )  C@  SWAP R16?  1 AND  OR
   SWAP # = IF  TC,  ELSE  8 OR  THEN TC, ;

: INCDEC  ( opc "inst" -- )  \ INC and DEC instructions
   CC,  DOES>  ( r/m -- )  C@  OVER R16?
   IF  040 OR  SWAP RLOW OP,  ELSE  0FE SR/M,  THEN ;

: SHIFT  ( opc "inst" -- )  \ bit shift and rotate
   CC,  DOES>  ( dest | dest cl -- )  C@  \ no CL mean shift dest,1
   OVER CL = IF  NIP 0D2  ELSE  0D0  THEN SR/M, ;

: BRANCH  ( opc "inst" -- )  \ CALL and JuMP instructions
   CC,  DOES>  ( #/reg/mem -- )  C@  OVER # =
   IF  NIP SWAP  THERE - 2 -  TUCK
      BIG? 0=  OVER 1 AND ( jmp? )  AND
      IF  2 OP,  TC,  ELSE  TC,  1- T,  THEN
   ELSE  0FF TC,  1 AND  1+ 010 * ( /2=call /4=jmp )
      R/M,  THEN ;

: STACK  ( opc3 opc2 opc1 "inst" -- )  \ PUSH and POP instructions
   CC, C, C,  DOES>  ( dest -- )  OVER # =  \ immediate?
   IF  2DROP DUP BIG? IF  068 TC, T,  ELSE  06A TC, TC,  THEN
   ELSE  OVER REG? IF  C@  SWAP RLOW OP,  \ register
      ELSE  1+ OVER SEG? IF  C@ RLOW  SWAP RMID  OP,  \ segreg
      ELSE  COUNT SWAP  C@ TC,  MEM,  \ memory
   THEN THEN THEN ;

: 8*  ( x -- x<<3 )  \ shift x left three bits, filling with zeroes
   3 LSHIFT ;

: BINARY  ( opc "inst" -- )  \ binary (two) operand instructions like ADD
   CC,  DOES>  ( src dest -- )  C@ >R  DUP REG?  \ dest a reg?
   IF  OVER # =  ( immediate? )
      IF  NIP  DUP RLOW 0=  ( ax/al? )
         IF  R> 8* 4 +  OVER W,  R16? B/W,  \ accum, #
         ELSE  OVER BIG? 0=  OVER R16?  AND
            IF  083 TC,  R> 8* RR,  TC,  \ sign extended immed8 to reg16
            ELSE  080 OVER W,  DUP R> 8* RR,  R16? B/W,  \ reg, #
         THEN THEN
      ELSE  R> 8* 2 + WR/M,  THEN  \ reg, r/m
   ELSE  ( dest=memory )  ROT  DUP REG?
      IF  R> 8* WMEM,  \ mem, reg
      ELSE  DROP  2 PICK BIG? 0=  \ mem, #
         2 AND  080 OR  SIZE,
         R> 8* MEM,  DUP BIG? SIZE @ AND  B/W,  WORD
   THEN THEN ;

\ Miscellaneous instructions

: TEST  ( src dest -- )  \ bit TEST instruction
   DUP REG?  \ dest a reg?
   IF  OVER REG?  \ src a reg too?
      IF  084 OVER W,  SWAP RR,  \ reg, reg
      ELSE  OVER DUP MEM?  SWAP [] =  OR
        IF  084 WMEM,  \ reg, mem
        ELSE  ( immediate )  NIP  DUP RLOW 0=  \ ax or al?
          IF  0A8 OVER W,  \ shorter instruction for accum, #
          ELSE  0F6 OVER W,  DUP RLOW  0C0 OP,  \ reg, #
          THEN  R16? B/W,  THEN THEN
   ELSE  ( dest=memory )  ROT  DUP REG?
      IF  084 WMEM,  \ mem, reg
      ELSE  DROP  0F6 SIZE,  0 MEM,  SIZE @ B/W,  WORD  \ mem, #
   THEN THEN ;

: INT  ( int# -- )  \ INTerrupt instruction, use INT3 for opcode 0CC
   0CD TC,  TC, ;

: +RET  ( #bytes -- )  \ RETurn and pop bytes instruction "RET 8"
   0C2 TC,  T, ;

: XCHG  ( r/m1 r/m2 -- )  \ eXCHanGe instruction
   DUP REG?
   IF  OVER REG?  OVER AX =  AND
      IF  DROP  RLOW  090 OP,  \ ax, reg
      ELSE  OVER AX =
         IF  NIP   RLOW  090 OP,  \ reg, ax
         ELSE  086 WR/M,  THEN THEN  \ reg, mem
   ELSE  ROT  086 WR/M,  THEN ;  \ mem, reg (assembled same as reg,mem)

: MOV  ( src dest -- )  \ MOVe instruction
   DUP SEG?
   IF  08E TC,  R/M,  \ seg, r/m
   ELSE  DUP REG?
      IF  OVER [] =  OVER RLOW 0=  AND  \ direct -> accum?
         IF  0A0 SWAP W,  DROP  T,  \ ax/al, [addr]
         ELSE  OVER SEG?
            IF  08C TC,  SWAP RR,  \ reg, seg
            ELSE  OVER # =  \ immediate?  reg, #
               IF  NIP DUP R16?  SWAP RLOW  OVER 8 AND OR  0B0 OP,  B/W,
               ELSE  08A OVER W,  R/M,  THEN THEN THEN  \ reg, mem
      ELSE  ( dest=mem )  ROT DUP SEG?
         IF  08C TC,  MEM,  \ mem, seg
         ELSE  DUP # =  \ immediate?
            IF  DROP 0C6 SIZE,  0 MEM,  SIZE @ B/W,  \ mem, #
            ELSE  OVER [] =  OVER RLOW 0=  AND  \ accum -> direct?
               IF  0A2 SWAP W,  DROP  T,  \ [addr], ax/al
               ELSE  088 OVER W,  R/M,  \ mem, reg
   THEN THEN THEN THEN THEN  WORD ;

\ Local label handling

DECIMAL
10 CONSTANT /LNEST  \ # of labels per label nest
4 CONSTANT #NESTS  \ maximum # of nests (for macros)
/LNEST #NESTS * CONSTANT #LABELS  \ total maximum # of local labels
HEX

: ARRAY  ( #entries -- )  \ create a cell-wide array
   CREATE  CELLS ALLOT
   DOES>  ( cell# -- a-addr )  SWAP CELLS + ;

VARIABLE +LNEST  \ nesting offset
#LABELS ARRAY LVALUE  \ dest addr for local label or 0
#LABELS ARRAY LLINK  \ linked list heads of unresolved labels

: LNEST  ( -- )  \ nest into next set of local labels
   +LNEST @ /LNEST +  DUP #LABELS U< 0= ABORT" Too many label nests!"
   DUP LVALUE  /LNEST CELLS  ERASE  \ clear this section out
   DUP LLINK   /LNEST CELLS  ERASE  +LNEST ! ;  \ set new +LNEST

: LINIT  ( -- )  \ initialise local label arrays
   /LNEST NEGATE +LNEST !  LNEST ;

: LCHECK  ( -- )  \ make sure all labels are resolved
   +LNEST @ /LNEST  OVER + SWAP  \ check current nesting level only
   DO  I LLINK @ ABORT" Label not resolved!"  LOOP ;

: LBACK  ( -- )  \ un-LNEST to previous set of local labels
   LCHECK  /LNEST NEGATE +LNEST +! ;

: LINDEX  ( label# -- index )  \ convert label# to index into label array
   1-  DUP /LNEST U< 0= ABORT" Not a label!"  +LNEST @ + ;

: L:  ( label# -- )  \ define a numbered local label (one-based)
   LINDEX  DUP LVALUE @ ABORT" Duplicate label!"  \ can't redefine label
   THERE  OVER LLINK @ ?DUP IF  \ any unresolved references?
      BEGIN  DUP TC@ >R  \ save offset to next reference
         2DUP  OFFSET OVER TC!  \ resolve this reference
         R@ 0100 -  +  \ go to next reference
      R> 0= UNTIL  DROP  \ loop till end of list
      OVER LLINK  0 SWAP !  \ clear unresolved list
   THEN  SWAP LVALUE ! ;  \ store label address

: L  ( label# -- addr )  \ reference a local label (one-based)
   LINDEX  DUP LVALUE @ ?DUP
   IF  NIP  \ already resolved, just return value
   ELSE  DUP LLINK @  \ fetch link
      ?DUP 0= IF  THERE 1+  THEN  \ first reference?
      1+ THERE 1+  \ link to previous label
      ROT LLINK !  \ store current label at head of list
   THEN ;

: END-CODE  ( -- )  \ end assembler definition
   PREVIOUS  ?CSP  LCHECK  REVEAL ;

\ The defining word instructions defined...
\ Missing instructions: ESC HLT LOCK and FAR instructions
\ Do not use LOOP XOR AND etc in Forth with ASSEMBLER vocabulary

026 SINGLE ES:  \ segment override prefixes
02E SINGLE CS:
036 SINGLE SS:
03E SINGLE DS:

0F3 SINGLE REP  0F3 SINGLE REPE  \ other instruction prefixes
0F2 SINGLE REPNE

037 SINGLE AAA  \ single byte, no-opcode instructions
03F SINGLE AAS
098 SINGLE CBW
0F8 SINGLE CLC
0FC SINGLE CLD
0FA SINGLE CLI
0F5 SINGLE CMC
099 SINGLE CWD
027 SINGLE DAA
02F SINGLE DAS
09F SINGLE LAHF
090 SINGLE NOP
0CC SINGLE INT3
0CE SINGLE INTO
0CF SINGLE IRET
09D SINGLE POPF
09C SINGLE PUSHF
0C3 SINGLE RET
09E SINGLE SAHF
0F9 SINGLE STC
0FD SINGLE STD
0FB SINGLE STI
09B SINGLE WAIT
0D7 SINGLE XLAT

0D5 AAMD AAD  \ ASCII adjust after Division and Multiplication
0D4 AAMD AAM

070 JCC JO  \ 8 bit offset branch instructions
071 JCC JNO
072 JCC JB  072 JCC JC
073 JCC JAE  073 JCC JNC
074 JCC JE  074 JCC JZ
075 JCC JNE  075 JCC JNZ
076 JCC JBE
077 JCC JA
078 JCC JS
079 JCC JNS
07A JCC JP
07B JCC JNP
07C JCC JL
07D JCC JGE
07E JCC JLE
07F JCC JG
0E3 JCC JCXZ
0E2 JCC LOOP
0E1 JCC LOOPE
0E0 JCC LOOPNE

0C5 EFF LDS  \ load effective address instructions
08D EFF LEA
0C4 EFF LES

0A6 STR CMPS  \ string instructions
0AC STR LODS
0A4 STR MOVS
0AE STR SCAS
0AA STR STOS
06D STR INS
06F STR OUTS

030 UNARY DIV  \ unary operand instructions
038 UNARY IDIV
028 UNARY IMUL
020 UNARY MUL
018 UNARY NEG
010 UNARY NOT

0E4 PORT IN  \ port instructions
0E6 PORT OUT

008 INCDEC DEC  \ INCrement and DECrement instructions
000 INCDEC INC

010 SHIFT RCL  \ bit shift and rotate instructions
018 SHIFT RCR
000 SHIFT ROL
008 SHIFT ROR
038 SHIFT SAR
020 SHIFT SHL
028 SHIFT SHR

0E8 BRANCH CALL  \ CALL and JuMP instructions
0E9 BRANCH JMP

08F 007 058 STACK POP  \ PUSH and POP stack instructions
0FF 036 050 STACK PUSH

002 BINARY ADC  \ binary operand instructions
000 BINARY ADD
004 BINARY AND
007 BINARY CMP
001 BINARY OR
003 BINARY SBB
005 BINARY SUB
006 BINARY XOR

\ Assembler macros etc

FALSE VALUE MACRO?  \ are we compiling a macro?

: EXIT  ( -- )  \ redefine EXIT to work while defining macros
   MACRO? IF  POSTPONE LBACK  THEN  \ in a macro, compile LBACK
   POSTPONE EXIT ; IMMEDIATE  \ as well as old EXIT

: END-MACRO  ( saved-current -- )  \ end assembler macro
   FALSE TO MACRO?  POSTPONE LBACK  POSTPONE ;  \ tidy up macro
   PREVIOUS SET-CURRENT ; IMMEDIATE  \ restore compilation wordlist

PREVIOUS DEFINITIONS  ALSO ASSEMBLER  \ CODE must be in FORTH vocabulary

: CODE  ( "name" -- )  \ create header and begin assembler definition
   HEADER,  !CSP  LINIT  ALSO ASSEMBLER ;

: ;CODE  ( -- )  \ change last CREATEd word's code to execute the following
   ?CSP !CSP  POSTPONE (DOES>  \ check stack, change CREATE's code
   POSTPONE [  \ switch to interpreting state
   LINIT  ALSO ASSEMBLER ; IMMEDIATE  \ initialise assembler

: MACRO  ( "name" -- saved-current )  \ begin assembler macro
   ALSO ASSEMBLER  GET-CURRENT DEFINITIONS  \ define macro in ASSEMBLER
   :  POSTPONE LNEST  TRUE TO MACRO? ;

MACRO NEXT  ( -- )  \ compile code to end a CODE word
   WORD LODS  AX JMP
END-MACRO

PREVIOUS  \ restore context

WARNINGS !  BASE !  \ restore WARNINGS and BASE
