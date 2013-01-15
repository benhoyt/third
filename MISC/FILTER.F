\ Silly little filter

decimal  \ make sure we're in DECIMAL

1 constant eof-ior  \ our ior for end-of-file
variable buffer  \ storage for read and write

: read  ( -- char )  \ read a char from STDIN, throw on end-of-file
   buffer 1 0 read-file throw  \ read char to buffer
   0= eof-ior and throw  \ check for EOF
   buffer c@ ;  \ char on stack

: write  ( char -- )  \ write a char to STDOUT
   buffer c!  buffer 1 1 write-file throw ;

: one  ( char -- char' )  \ process one char
   >upper ;

: process  ( -- )  \ process stdin > stdout
   begin  read one write  again ;

: filter  ( -- )  \ filter STDIN to STDOUT
   chained booter  \ execute upon startup to make turnkey application
   ['] process catch drop  \ process it till EOF
   bye ;  \ exit back to DOS

save filter.com  bye  \ save as runnable file and exit
