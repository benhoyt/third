\ Third makefile for the Big.COM system

   .( Creating fully fledged Third system Big.COM ... )
CR .(   CORE extension and miscellaneous words ... )  INCLUDE Extend.F
CR .(   8086 reverse polish notation assembler ... )  INCLUDE ASM86.F
CR .(   Facility and terminal functions ... )         INCLUDE Facility.F
CR .(   Double number handling ... )                  INCLUDE Double.F
CR .(   String manipulation ... )                     INCLUDE String.F
CR .(   DOS shell and spawn routines ... )            INCLUDE Spawn.F
CR .(   SEE decompiler ... )                          INCLUDE See.F
CR .(   Quick reference help ... )                    INCLUDE Quick.F
CR .(   Obselete words for completeness ... )         INCLUDE NoNeed.F
CR .(   Environment query routine ... )               INCLUDE Environ.F
CR .( Saving system as Big.COM ... )  HERE $ 0100 - U. .( bytes)
DEPTH [IF]  -22 THROW  [THEN]  CR .( Success!) CR
