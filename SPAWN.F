\ DOS spawn routines to execute child programs

BASE @  HEX  \ make sure we're in HEX

CREATE SPARAMS  ( -- a-addr )  \ spawn parameter block
   0 ,  \ seg of environment (zero means same env as parent)
   0 , CS@ ,  \ seg:ofs of command tail
   5C , CS@ ,  \ seg:ofs of first FCB
   6C , CS@ ,  \ seg:ofs of second FCB

VARIABLE SSP  \ saved SP for spawn

( Spawn and execute program named in ASCIIZ string at seg:prog with
  command line tail at cmd-line. error-level is the spawned program's
  return code and ior is the I/O result for the actual spawn. )
CODE DSPAWN  ( seg prog cmd-line -- error-level ax carry-flag )
   CS SPARAMS 4 + [] MOV  \ set CS references in SPARAMS
   CS SPARAMS 8 + [] MOV  CS SPARAMS 12 + [] MOV

   WORD SPARAMS 2 + [] POP  \ set command tail pointer in SPARAMS
   DS PUSH ES POP  SPARAMS # BX MOV  \ ES:BX points to the param block
   DX POP  DS POP  \ DS:DX points to ASCIIZ program name

   SI PUSH  BP PUSH  \ save SI and BP (other regs no worries)
   SP CS: SSP [] MOV  \ save stack pointer
   4B00 # AX MOV  21 INT  \ spawn the thing!
   CLI  CS DX MOV  DX SS MOV  CS: SSP [] SP MOV  STI  \ restore stack seg:ofs
   DX DS MOV  CLD  \ restore DS, make sure direction flag is clear
   BP POP  SI POP  \ restore other regs for Third

   AX CX MOV  \ CX = ax (DOS error code)
   BX BX SBB  1 L JNZ  \ BX = carry-flag, exit now if error
   4D # AH MOV  21 INT  \ get child processes error code
1 L:
   AX PUSH  CX PUSH  BX PUSH  NEXT  \ push error-level ax carry-flag
END-CODE

: ESEG  ( -- seg )  \ return environment segment
   2C @ ;

: ENV-C@  ( e-addr -- char )  \ fetch char from environment segment
   ESEG SWAP LC@ ;

: ASCIIZ>  ( e-addr -- e-addr u )  \ get length of ASCIIZ string in ESEG
   DUP  BEGIN  DUP ENV-C@ WHILE  1+  REPEAT  OVER - ;

( Get contents of DOS environment variable c-addr1 u1, return
  environment ASCIIZ ptr e-addr and true if found, else false.
  c-addr1 u1 must be uppercase and have "=" sign like "COMSPEC=". )
: DOS-ENV  ( c-addr1 u1 -- e-addr flag )  \ e-addr is in ESEG
   0 BEGIN  DUP ENV-C@ 0= IF  2DROP FALSE EXIT  THEN  \ not found?
      >R  ESEG R@ CS@  RAD  4 PICK LMOVE  \ move to CS:RAD for S=
      2DUP RAD SWAP  S= WHILE  \ is this it?
      R>  ASCIIZ> + 1+  \ nope, move to next variable
   REPEAT  NIP  R> +  TRUE ;  \ found it

DECIMAL  \ base changed to DECIMAL here

: CMD-TAIL  ( c-addr u -- cmd-tail )  \ convert string to command tail ptr
   RAD  DUP >R PLACE  13 R@ COUNT + C!  R> ;  \ place at RAD with CR on end

( Spawn program c-addr1 u1 with command line c-addr2 u2. c-addr1 u1
  must be a full path with filename and extension. Return error-level
  of child process and I/O result of spawn itself. )
: SPAWN  ( c-addr1 u1 c-addr2 u2 -- error-level ior )
   CMD-TAIL >R  >ASCIIZ CS@ SWAP  R> DSPAWN  >IOR NIP ;

: SHELL  ( c-addr u -- error-level ior )  \ shell to COMSPEC with command tail
   2>R  ESEG S" COMSPEC=" DOS-ENV  0= ABORT" No COMSPEC!"  \ get COMSPEC
   2R> CMD-TAIL DSPAWN  >IOR NIP ;  \ spawn to it with command line

BASE !  \ restore BASE
