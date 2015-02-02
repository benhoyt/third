Third, a small Forth compiler for 8086 DOS
==========================================

**Historical note:** I wrote this in 1998 (when I was 16), so I'm just putting
it up here as a backup and for "historical interest". It's still a pretty good
show-case for how simple a self-hosting
[Forth](http://en.wikipedia.org/wiki/Forth_%28programming_language%29)
compiler can be.

I wrote it more or less all myself (though parts of the assembler were pretty
much copied, I think :-), but it's loosely based on
[eForth](http://www.calcentral.com/~forth/forth/) and the Forth compilers my
dad wrote.

It still works on current 32-bit Windows systems, but 64-bit Windows systems
don't run 16-bit DOS programs anymore, so for those you'll need to use
[DOSBox](http://www.dosbox.com/) or something similar.

All the documentation that follows is original.


Quick start
===========

To jump straight into using Third, execute the batch file MBIG MBER
MQUICK in that order. Then you can run Big to use Third, or Ber to
use Third with Berwyn Hoyt's extensions. Type BYE once you're in
Third to exit back to DOS. To load a Forth file (source code) type
"INCLUDE filename.f".

General information
===================

Third was designed and coded by Benjamin Hoyt, with complements to
the following people:

    Charles Moore           inventing Forth
    Bruce Hoyt              coding FE (Forth Editor)
    Berwyn Hoyt             bug reports and help to make Third clean
    Bill Muench             eForth
    Wonyong Koh             hForth
    Tom Zimmer              F-PC
    Anton Ertl              gForth
    ANS committee           creating the ANS Standard

Third tries hard, most of the time, to be a reasonable 16 bit Forth
compiler for DOS PCs. It was originally coded to be a very small
Forth as a learning tool for others, but this idea vanished when
Berwyn and I decided to use it for a cross compiler for the Motorola
68HC12 CPU. It is now a somewhat stable ANS Forth system with various
features.

Excepting ENVIRONMENT? Third is a full CORE system just with the bare
Kernel.COM. With Big.F loaded (to make Big.COM) Third is a ANS system
with the following wordsets and their extensions implemented:

    CORE DOUBLE EXCEPTION FACILITY FILE TOOLS SEARCH STRING

Missing are BLOCK FLOATING LOCALS MEMORY. Even though the BLOCK
wordset is supposed to be implemented if the FILE wordset is, I can't
see much use for it at the moment. When I need it, I'll code it.
FLOATING will probably never come into the picture. I've never needed
or liked LOCALS (to say the least). MEMORY words seem a bit pointless
for a small 16 bit Forth with only a 64k address space.

Third also has a clean metacompiler (thanks to Bill Muench's eForth
metacompiler), an 8086 assembler, a nice DOS shell routine, a simple
decompiler, a fairly nice Quick reference help, and reasonable docs.

Third dictionary structure
==========================

Third's dictionary is a structure of names and other information
chained together in a linked list. Each "record" in the linked
structure contains information about a Forth "word" (don't confuse
with the CPU-word type meaning). Each record in the linked list
has a pointer to the previously defined word, whose link points to
the word defined before that, whose link points to ... and so it goes
till the link contains a zero (end of linked list).

A wordlist is a list of words chained together in the above way.

Each word in Third's dictionary is composed of three items: the link
to the previously defined word, a count and flags byte, and a name
for use by the Forth dictionary searcher. Directly following the name
string in memory is the machine code for that word.

The link is simply a 16 bit "cell" (a cell is the basic unit of
information in any Forth system) which contains a pointer, or zero.

The count and flags byte is divided into two sections. The five low
bits is the count: the number of characters in the following Forth
name, which can be a maximum of 31, for obvious reasons. The three
high bits in this byte are "flags", of which only the highest bit
(bit 7 of the byte) is presently used, for the IMMEDIATE flag.

The Forth virtual machine
=========================

This virtual machine is sort of like a very fast emulated CPU. Its
instructions are the CODEd words. NEXT is the instruction processor.
SI points to the current "instruction", and acts like a CPU
instruction pointer. Each instruction is a pointer to 80x86 code to
execute this instruction, so to execute the instruction we simply
load a 16 bit word from SI and jump to this address.

Like all Forths, Third has two stacks: the data stack and the return
stack. It keeps function call return addresses (and at times, various
other stuff) on the return stack. The return stack is addressed using
BP as a stack pointer. The data stack, referenced by SP using PUSH
and POP, holds all function parameters and return values. Yes, with
this lovely setup you can more than one return value (unlike some
languages we know of :-).

The Third virtual machine uses 80x86 CPU registers as follows:

    registers       use
    -------------------
    AX              scratch, used for LODSW in NEXT
    BX              scratch, used for referencing memory and the like
    CX              scratch, used for counting and loops
    DX              scratch, used for division and multiplication
    DI              scratch, used for string instructions
    SI              the virtual machine instruction pointer
    BP              the virtual machine return stack pointer
    SP              the virtual machine data stack pointer
    CS              80x86 code segment register (never changes)
    DS              data segment, must stay the same as CS
    SS              stack segment, must stay the same as CS
    ES              scratch segment register
    flags           direction flag is expected to always be clear (CLD)

Memory usage
============

Third uses one full 80x86 64k segment which is divides up as follows:

    base    limit   description
    ---------------------------
    0000    0100    256 byte DOS program segment prefix (PSP)
    0100    HERE    Forth dictionary: code, name, and data spaces
    HERE    FDF0    free space (used for PAD buffers and MALLOC)
    FDF0    FE00    8 element search order (CONTEXT) stack
    FE00    FF00    128 cell data stack (SP stack)
    FF00    FFFE    127 cell return stack (BP stack)
    FFFE    10000   two bytes unused memory :-)

Stack diagrams
==============

Stack diagrams or pictures are expressed with the incoming parameters
on the left of the "--" and outgoing return values on the right. The
item on the top of the stack (abbreviated TOS) is always the left
most item in the stack picture.

A '|' (vertical bar) character in a stack picture means "or this" and
is used when a word has varying parameters or return values. A ".."
(two dots) sequence means "many items could be here".

Possible double quotes before the "--" tells what (if any) input
buffer parsing the word does. Parsed text abbreviations are given in
the following table:

    abbreviation    description
    ---------------------------
    <char>          a delimiting character marking end of string being parsed
    <chars>         zero or more occurences of the character char
    <space>         a delimiting space character (or any whitespace character)
    <spaces>        zero or more consecutive spaces (or any whitespace)
    <quote>         a delimiting double quote '"'
    <paren>         a delimiting right parenthesis ')'
    <eol>           an implied delimiter marking the end of a line
    ccc             a parsed sequence of arbitrary chars excluding any delimiter
    name            a token bounded by space, same as ccc<space> or ccc<eol>

Some examples of stack diagrams follow:

    word            stack picture
    -----------------------------
    MY-WORD         stack-before "parsing" -- stack-after
    ROT             x1 x2 x3 -- x2 x3 x1
    ?DUP            x -- 0 | x x
    PARSE           char "ccc<char>" -- c-addr u
    RESTORE-INPUT   xn .. x1 n -- flag

The meaning of the various stack picture data types are as follows:

    symbol          data type                               # of cells on stack
    ---------------------------------------------------------------------------
    flag            boolean flag                            1
    true            true flag                               1
    false           false flag                              1
    xt              execution token                         1
    addr            address                                 1
    a-addr          cell aligned address                    1
    c-addr          character aligned address               1
    ofs             offset into segment (Third specific)    1
    seg             segment pointer (Third specific)        1
    ior             I/O result                              1
    fam             file access method                      1
    fileid          file identifier                         1
    wid             wordlist identifier                     1
    char            character                               1
    n               signed single-cell integer              1
    +n              non-negative single-cell integer        1
    u               unsigned single-cell integer            1
    n|u             signed or unsigned single-cell integer  1
    x               unspecified cell                        1
    d               signed double-cell integer              2
    +d              non-negative double-cell integer        2
    ud              unsigned double-cell integer            2
    d|ud            signed or unsigned double-cell integer  2
    xd              unspecified cell pair                   2
    colon-sys       colon definition compilation            implementation defined
    do-sys          DO...LOOP structures                    implementation defined
    case-sys        CASE...ENDCASE structure                implementation defined
    of-sys          OF...ENDOF structure                    implementation defined
    orig            control-flow origins                    implementation defined
    dest            control-flow destinations               implementation defined
    loop-sys        DO...LOOP control parameters            implementation defined
    nest-sys        definition calls                        implementation defined
    i*x, j*x, k*x   indeterminate # of stack items          0 or more

An execution token is a Forth word identifier that can be EXECUTEd.
In Third, an execution token is a pointer to the word's machine code
(a colon definition xt is a pointer to a CALL instruction).

seg and ofs items are used by Third's far memory operators to make
use of the Intel segment and offset architechture.

An I/O result (used mainly by FILE words) is zero if the operation
was successful. If the operation was not successful, the ior is an
implementation defined nonzero value that can be THROWn. An end of
file condition (for instance, in READ-FILE) gives a zero ior.

In Third a fileid is the same as the DOS file handle but the Standard
specifies that fileid's are implementation dependant.

A wid is a wordlist identifier and contains information about the
corresponding wordlist. In Third a wid is a pointer to a three cell
structure containing a search chain pointer, the link to the
previously defined wordlist, and a pointer to the wordlist's header.

Implementation defined options
==============================

Addresses need not be cell aligned for @ and ! to function properly.

EMIT (using the default EMIT that sends to DOS standard output)
displays all characters 32 (hex 20) to 126 (hex 7E) using the ASCII
character set. Values above this range are displayed using the
extended ASCII character set standard to IBM compatibles. Values
below this range which are not control characters are also displayed
in standard IBM PC fashion. Control characters are handled as shown
in the following table:

    value   description
    -------------------
    07      sounds a beep and displays nothing
    08      moves the text cursor left one character
    09      displays enough spaces to position the cursor at the next tab stop
    0A      moves the cursor down one line, scrolling the screen if necessary
    0D      moves the cursor to the start of the current line

Although ACCEPT is deferred (and EXPECT calls ACCEPT), the default
version exits when the Enter key is pressed and removes the last
character (from the buffer and the screen) when Backspace is pressed.

A character is eight bits wide and can contain a value in the range
0 to 255.

Addresses need not be character aligned for C@ and C! to function
properly.

Third recognises no extended (eg., accented international characters)
characters as upper or lower case letters.

REFILL converts tab characters to spaces before interpreting. Parsing
routines (PARSE and SKIP) recognise only the space character 32 as a
space delimiter.

The control flow stack is kept on the data stack and items are always
one cell wide.

Binary digits from 0 to 9 are converted to the characters '0' to '9'.
Binary digits from 10 to 35 are converted to the characters
'A' to 'Z'. Binary digits from 36 onwards are converted to the
characters from 91 (that is, '[') onwards.

The display is not altered in any way after input terminates in
ACCEPT or EXPECT. The cursor position is not changed.

ABORT displays nothing. ABORT" and other THROW values show the word
in the source which caused the error and display the error message.
If a text file was being interpreted the line number is given. If an
unknown error (unknown THROW #) is encountered the error message is
"Unknown exception # nnn" where nnn is the unknown THROW number.

The Enter key terminates line input from the user input device.

The maximum size of a counted string is 255 chars excluding count.

The maximum size of a PARSEd string is pretty much infinite. :-)

The maximum size of a definition name is 31 characters.

The maximum string length for ENVIRONMENT? is 31 characters.

User input device can be redirected as DOS allows.

User output device can be redirected as DOS allows.

The logical code, data, and name spaces are all organised in one
physical segment addressed by HERE.

An address unit (byte) is eight bits wide.

Two's complement integer arithmetic is used.

The ranges for various integer data types are given in the following
table:

    data type       allowable range
    -------------------------------
    +n              0 to 32,767
    n               -32,768 to 32,767
    u               0 to 65,535
    +d              0 to 2,147,483,647
    d               -2,147,483,648 to 2,147,483,647
    ud              0 to 4,294,967,295

Third has no read-only data space regions.

The buffer used by WORD (at HERE) can hold one count character, 254
parsed characters, and one space character. PAD is 256 characters
beyond HERE.

A cell is sixteen bits wide.

The keyboard terminal input buffer is 256 characters in size.

The pictured numeric output string buffer is at least 80 characters
in size.

The size of the scratch PAD is 256 characters.

Third is case insensitive by default but can be switched to a case
sensitive system by executing the word SENSE and back by executing
the work NONSENSE. SENSITIVE returns a true flag if the system is in
case sensitive mode.

The system prompt when no error has occured is simply "ok".

The default type of division rounding used is symmetric.

The value of STATE when true is a Standard true flag, ie., a cell
with all bits set.

The values returned when arithmetic operations have overflowed are
the correct values mod MAX-U. In effect, Third uses modulo arithmetic
with a divisor of 65,536.

The current definition cannot be found in the dictionary after DOES>,
but only after the final ; (semicolon).

The system values used by CATCH and THROW are the Standard ones from
-1 to -58 and system I/O results from -256 to -294. The user should
not have to worry about this and should only use positive values.

The encoding of keyboard events as returned by EKEY are simply the
16-bit values the BIOS keyboard interrupt (hex 16) functions give.

The duration of a system clock tick for MS is 55 milliseconds.

A R/O fam is 0, a W/O fam is 1, and a R/W fam is 2. BIN leaves these
values unchanged as DOS doesn't have a READ-LINE function.

The file line terminators accepted by READ-LINE are either an LF
character (hex 0A) by itself or a CR,LF sequence (hex 0D,0A). This
allows for both Unix-style and DOS text files, respectively.
WRITE-LINE writes the DOS CR,LF sequence.

Standard DOS filenames are used for the file words. Case does not
matter. No wildcards are accepted. Filenames of the following formats
are recognised (where "filename" may include an extension):

    filename
    d:filename
    d:\path\filename

Bits in cell x returned FILE-STATUS are as follows:

    bit     description
    -------------------
    0       set if file is read-only (protected)
    1       set if file is hidden
    2       set if file is a system file
    3-4     unused
    5       set if file is an archive file
    6-15    unused

After an exception during interpretation of a text file, the file
is closed and the line buffer is deallocated. The exception is then
re-THROWn.

I/O result (ior) values are simply the DOS error code values
subtracted from -255. This gives a range from -256 downward.

The maximum nesting depth for INCLUDE-FILE is probably about 8. There
is no actual maximum number of nests, but when the return stack is
full, you're a goner. :-)

The maximum length line for READ-LINE and interpretation and parsing
of text files is basically unlimited except by memory restrictions.

The BLOCK wordset is not implemented so no block mapping is done.

Only one string buffer is provided for S" (have fun with words like
RENAME-FILE :-). This string buffer is 256 bytes in size.

Ending the sequence of assembler instructions following CODE or ;CODE
is done with END-CODE.

Processing assembler instruction following CODE or ;CODE is normal
interpreted Forth source code.

The full SEARCH order wordsets are implemented given ASSEMBLER and
EDITOR expected results.

SEE decompiles and displays most things, excepting conditional
constructs, as they were typed in. LITERALs, POSTPONEd words,
DO...LOOPs, CREATE DOES> words, and strings (S" ABORT" etc.) are
decompiled pretty much as they were compiled. Conditional clauses and
structures like BEGIN...UNTIL are not simple to reproduce from
executable code. Cconditional branches (IF, WHILE, UNTIL, etc.) are
decompiled as "IF+offset" for forward branches or "IF-offset" for
backward branches. Unconditional branches are decompiled with
"ELSE+offset" and "ELSE-offset" similarly. A colon definition would
be SEEn as in the example below:

    : SPACES  0 MAX 0 ?DO SPACE LOOP ;

The ; is followed by IMMEDIATE if the word is immediate. Note that
;CODE in a word to be SEEn is not respected (that is, won't work :-).
Also, SEE cannot differentiate between an EXIT and a ; and stops
decompiling as soon as it hits any EXIT.

Anything CREATEd, including VARIABLEs, are decompiled as in the
following:

    CREATE STATE

Any VALUE or CONSTANT is displayed as in the example below:

    32 VALUE BL

A DEFERred word is displayed along with the word it is currently
deferred to. An amiguous condition exists if the word it points to
was defined with :NONAME. A DEFERred word is SEEn as in the example
below:

    DEFER TYPE IS DTYPE

The maximum number of word lists allowed in the search order is
eight.

The minimum search order (as with "-1 SET-ORDER" or ONLY) is the
FORTH-WORDLIST. ONLY is equivalent to "FORTH-WORDLIST 1 SET-ORDER".

Action taken upon ambiguous conditions
======================================

When an interpreted name is neither a valid definition nor a valid
number the interpreter aborts with an appropriate error message.

When a definition name exceeds the maximum length allowed it is
truncated to 31 characters.

Third might crash if values are stored in the code or name space.

No type checking is performed.

When a user uses ' on a definition with undefined interpretation
semantics ' returns an xt of that word's compilation semantics.

Division by zero causes Third to exit to DOS with a "Divide Error"
message.

Data stack or return stack overflow: probable crash.

Insufficient space for loop control parameters: probable crash.

Insufficent space in the dictionary: probable crash.

When a word with undefined interpretation semantics is interpreted
Third executes that word's compilation semantics.

The input buffer and string literal regions are read/write.

Overflow of a pictured numeric output string: possible crash.

Parsed string overflow (doesn't happen): minutely possible crash.

Result out of range in * or / etc.: good old mod MAX-U arithmetic
takes place.

Reading values from an empty return stack is near impossible. Reading
values from an empty data stack gives undefined values. Data stack
underflow is checked for by the interpreter after each word is
interpreted.

Using a zero length string as a name causes the interpreter to abort
with an appropriate error message.

\>IN greater than the size of the input buffer: probably a -13 THROW.

RECURSE appearing after DOES>: probable crash when word is executed.

RESTORE-INPUT input source different from current input source: input
source will probably change, minutely possible crash.

Data space containing de-ALLOTed definitions: probably crash.

Data space read/write with incorrect alignment: Nothing. Third
doesn't worry about alignment.

Data space pointer not properly aligned: Nothing. Third doesn't
worry about alignment.

Less than u+2 stack items with PICK and ROLL: 1 PICK is an OVER.
0 PICK is a DUP. 1 ROLL is a SWAP. 0 ROLL is a do-nothing.

Loop control parameters not available: wonky results.

Most recent definition does not have a name (for IMMEDIATE): probably
nothing, possible wonky happenings.

Name not defined by VALUE used by TO: Third's TO works with VALUES,
CREATEd words, VARIABLEs, VALUEs, DEFERs, but probably messes up a
CODE or colon definition.

Name not found (with ' ['] POSTPONE etc.): A -13 THROW.

Parameters are not of same type (DO ?DO WITHIN etc.): funny things.

POSTPONE or [COMPILE] applied to TO: works fine as you would expect.

String longer than counted string returned by WORD: PAD overwritten.

When u is greater than or equal to the number of bits in a cell for
LSHIFT or RSHIFT: unexpected results.

\>BODY or DOES> applied to a word not defined with CREATE: DOES> will
probably work on CONSTANTs, VARIABLEs, DEFERs, but do funny things
with colon definitions. >BODY works well on CREATEd words, VARIABLEs,
VALUEs, and DEFERs.

\# #S HOLD or SIGN used outside a <# ... #> sequence: you can expect
weird happenings if you do weird things like this! :-)

When d is outside the range of n in D>S: result is truncated.

AT-XY can always be performed on the user output device.

Positioning a file outside the file boundaries causes no error.
Reading from the file after this does not result in an error but zero
characters are read. Writing to the file after this extends the
length of the file but the unwritten information in the file is
undefined.

Attempting to read from file positions not yet written to (for
example, creating a new file and immediately executing READ-FILE)
does not cause an error, but zero bytes are read.

An invalid fileid passed to INCLUDE-FILE THROWs an appropriate
exception when REFILL is first executed (in INCLUDE-FILE).

When an I/O exception is encountered reading or closing a text file
(in INCLUDE-FILE or INCLUDED) an appropriate exception is THROWn.

When a name file passed to INCLUDED cannot be opened the appropriate
exception is THROWn.

Since the BLOCK wordset is not implemented requesting an unmapped
block number does not occur. (Strange, that. :-) Neither does using
SOURCE-ID when BLK is nonzero.

Deleting the with MARKER or FORGET has no effect until you try to
compile a definition when the system will probably crash.

A possible crash will occur if there are less than u+1 items on the
control flow stack (data stack) when CS-PICK or CS-ROLL are executed.

When the name parsed by FORGET cannot be found -13 is THROWn.

;CODE used on a non-CREATEd definition will actually work.

POSTPONE applied to [IF] seems to work seemlessly. :-)

If the end of the input file is reached before a matching [ELSE] or
[THEN] is encountered an un-nesting from all levels of [IF] occurs.

Removing a needed definition with MARKER or FORGET will probably
cause a crash.

Changing the compilation wordlist while compiling a definition will
probably make the definition unable to be found.

If FIND (or similar) is executed when the search order is empty, no
words will be found. If the search order is empty while interpreting,
(and kids, don't try this at home!) only numbers will be accepted.

Too many wordlists in the search order may cause a crash.

Other information
=================

No non-Standard Third words use the scratch PAD. No Standard words do
either.

Available terminal facilities: AT-XY KEY? PAGE EKEY EKEY>CHAR EKEY?
EMIT? MS and TIME&DATE.

Available data space for programs: given by UNUSED.

Return stack space available: 127 cells.

Data stack space available: 128 cells.

Dictionary space used by Third system: given by "HERE 256 -".
