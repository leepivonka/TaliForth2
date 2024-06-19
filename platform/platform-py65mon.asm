        .cpu "6502"        ; 6502 processor (Tali will run on NMOS 6502)
			; still needs ROR
        .enc "none"        ; No special text encoding (eg. ASCII)

; I/O facilities are handled in these separate kernel files because of their
; hardware dependencies. See docs/memorymap.txt for a discussion of Tali's
; memory layout.


; MEMORY MAP OF RAM

; Drawing is not only very ugly, but also not to scale. See the manual for
; details on the memory map. Note that some of the values are hard-coded in
; the testing routines, especially the size of the input history buffer, the
; offset for PAD, and the total RAM size. If these are changed, the tests will
; have to be changed as well

* = $0
 .dsection zp ; zero page, writale, uninitialized
 .cwarn *>$80, "zero page ends >$80
;    $0000  +-------------------+
;           |  ZP varliables    |
;DStack:    +-------------------+
;           |                   |
;           +~~~~~~~~~~~~~~~~~~~+  <-- dsp
;           |                   |
;           |  ^  Data Stack    |
;           |  |                |
;           +-------------------+  DStack0
;           | FP stack arrays   |
;           |                   |
;           |   (Reserved for   |
;           |      kernel)      |
;           |                   |
;           +-------------------+
RStack = $0100 ; begin of 6502 Return stack
;           |                   |
;           |  ^  Return Stack  |  <-- CPU S register
;           |  |                |
rsp0      = $ff		; initial Return Stack Pointer (6502 stack)

* = $200
 .dsection bss ;writable, uninitialized
 .cwarn * > $c00, "bss ends >$c00"

;    $0200  +-------------------+
;           |  |                |
;           |  v  Input Buffer  |
;           |                   |
;           | more variables    |
;           |                   |
;           +-------------------+  cp0
;           |  |                |
;           |  v  Dictionary    |
;           |       (RAM)       |
;           |                   |
;   (...)   ~~~~~~~~~~~~~~~~~~~~~  <-- cp
;           |                   |
;           |                   |
;           |                   |
cp_end = $8000 ; LastRAM byte available for Data
;           +-------------------+

 * = $8000
 .dsection code ;read-only, initialized
 .cwarn * > $e000, "code ends >$e000"



; HARD PHYSICAL ADDRESSES

; Some of these are somewhat silly for the 6502, where for example
; the location of the Zero Page is fixed by hardware. However, we keep
; these for easier comparisons with Liara Forth's structure and to
; help people new to these things.


; SOFT PHYSICAL ADDRESSES

; Tali currently doesn't have separate user variables for multitasking.
; We do have limited zero-page space.
; Many Forth variables are pushed up to user0 in non-zp memory.

PadOffset = 84	; offset from CP to PAD (holds number strings)
		;  Must be at least 84 bytes in size (says ANS).

MAX_LINE_LENGTH  = 79      ; output line size

DDim = 20	; # of cells in data stack
FDim = 10	; # of entries in floating-point stack

DoStkDim = 8	; # of entries in Do stack



; OPTIONAL WORDSETS

; Tali Forth 2 is a bit of a beast, expecting about 24K of ROM space.
; For some applications, the user might not need certain words and would
; prefer to have the memory back instead.  Remove any of the items in
; TALI_OPTIONAL_WORDS to remove the associated words when Tali is
; assembled.  If TALI_OPTIONAL_WORDS is not defined in your platform file,
; you will get all of the words.

;TALI_OPTIONAL_WORDS := [ "fp","fpe","fpieee","fptrancendentals","ed", "editor", "ramdrive", "block", "environment?", "assembler", "wordlist" ]


; TALI_OPTION_CR_EOL sets the character(s) that are printed by the word
; CR in order to move the cursor to the next line.  The default is "lf"
; for a line feed character (#10).  "cr" will use a carriage return (#13).
; Having both will use a carriage return followed by a line feed.  This
; only affects output.  Either CR or LF can be used to terminate lines
; on the input.

TALI_OPTION_CR_EOL := [ "lf" ]
;TALI_OPTION_CR_EOL := [ "cr" ]
;TALI_OPTION_CR_EOL := [ "cr" "lf" ]

 .section bss	; 1st part of bss space
bsize     = $ff		; size of input/output buffers
buffer0: .fill bsize	; input buffer
 .endsection bss


; Make sure the above options are set BEFORE this include.

.include "../taliforth.asm" ; zero page variables, definitions

 .section bss	; last part of bss space
hist_buff: .fill 8*128	; Input History for ACCEPT
cp0:  ; start of RAM dictionary
 .endsection bss

 .cwarn *>$e000 ; Tali code overflowed 24k

;==== C65 simulator detail ===============================================================

;Magic IO
; c65 provides a magic IO block that spans a 22 byte range and is normally based at $f000.
; Use -m to change the base address.
; This supports a number of IO functions:
;
;		   Addr    Name    Description
;
c65_putc	= $f001 ;  putc    Write here to send the byte to stdout
c65_kbhit	= $f003 ;  kbit    Return non-zero if key ready to fetch with getc
c65_getc	= $f004 ;  getc    Non-blocking read from stdin, returns 0 if no byte ready
;
c65_start	= $f006 ;  start   Reading here starts the cycle counter
c65_stop	= $f007 ;  stop    Reading here stops the cycle counter
c65_cycles	= $f008 ; cycles  Current 32 bit cycle count in NUXI order

c65_blkio	= $f010 ;  blkio   Write here to execute a block IO action (see below)
c65_status	= $f011 ;  status  Read block IO status here
c65_blknum	= $f012 ; blknum  word, Block number to read/write
c65_buffer	= $f014 ; buffer  word, Start of 1024 byte memory buffer to read/write

;Block IO
; The base address (default $f010) is the first byte of a six byte interface:
;
; offset  name    I/O description
; 0       action  I   initiate IO action (set other params first)
; 1       status  O   returns 0 on success and 0xff otherwise
; 2-3     blknum  I   0-indexed low-endian block to read or write
; 4-5     bufptr  I   low-endian pointer to 1024 byte buffer to r/w

; To initiate a block IO operation, set the blknum and bufptr parameters and then write the action code
; to the base address. The status value is returned.
; Four actions are currently supported:
;   status (0): query blkio status: sets status to 0x0 if enabled, 0xff otherwise
;   read (1): read the 1024 byte block @ blknum to bufptr
;   write (2): write 1024 bytes from bufptr to the block @ blknum
; Note that an external blockfile must be specified with the -b ... option to enable block IO.
;  The file is simply a binary file with block k mapped to offset k*1024 through (k+1)*1024-1. The
;  two-byte blknum supports a maximum addressable file size of 64Mb.
;  A portable (cross-platform) check for blkio availability is:
;
; write 1 to status
; write 0 to action
; test if status is now 0
; You can boot from a blkio file by adding the following snippet to the end of forth_code/user_words.fs:

;	\ if blkio is available and block 0 starts with the bytes 'TF'
;	\ `evaluate` the remainder of block 0 as a zero-terminated string
;	\ Requires the word asciiz> ( addr -- addr n )
;
;	: blkrw ( blk buf action -- )
;	    -rot $c014 ! $c012 ! $c010 c!
;	;
;	:noname
;	    1 $c011 c! 0 $c010 c! $c011 c@ 0= if  \ blkio available?
;	        0 $1000 1 blkrw
;	        $1000 @ $4654 = if                \ starts with magic "TF" ?
;	            $1002 asciiz> evaluate else   \ run the block
;	            ." bad boot block" CR
;	        then else
;	        ." no block device" CR
;	    then
;	; execute



; =====================================================================
; FINALLY

; Of the 32 KiB we use, 24 KiB are reserved for Tali (from $8000 to $DFFF)
; and the last eight (from $E000 to $FFFF) are left for whatever the user
; wants to use them for.


; Default kernel file for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Sam Colwell
; First version: 19. Jan 2014
; This version: 31. May 2024
;
; This section attempts to isolate the hardware-dependent parts of Tali
; Forth 2 to make it easier for people to port it to their own machines.
; Ideally, you shouldn't have to touch any other files. There are three
; routines and one string that must be present for Tali to run:
;
;       kernel_init - Initialize the low-level hardware
;       kernel_getc - Get single character in A from the keyboard (blocks)
;       kernel_putc - Prints the character in A to the screen
;       s_kernel_id - The zero-terminated string printed at boot
;
; This default version Tali ships with is written for the py65mon machine
; monitor (see docs/MANUAL.md for details).

; The main file of Tali got us to $e000. However, py65mon by default puts
; the basic I/O routines at the beginning of $f000. We don't want to change
; that because it would make using it out of the box harder, so we just
; advance past the virtual hardware addresses.
 .section code
* = $f100

; All vectors currently end up in the same place - we restart the system
; hard. If you want to use them on actual hardware, you'll have to redirect
; them all.
v_nmi:
v_reset:
v_irq:
kernel_init:
        ; """Initialize the hardware. This is called with a JMP and not
        ; a JSR because we don't have anything set up for that yet. With
        ; py65mon, of course, this is really easy. -- At the end, we JMP
        ; back to the label forth to start the Forth system.
        ; """
                ; Since the default case for Tali is the py65mon emulator, we
                ; have no use for interrupts. If you are going to include
                ; them in your system in any way, you're going to have to
                ; do it from scratch. Sorry.
                sei             ; Disable interrupts

                ; We've successfully set everything up, so print the kernel
                ; string
                ldx #0
-               lda s_kernel_id,x
                beq _done
                jsr kernel_putc
                inx
                bne -
_done:

		bit c65_start		; start the cycle counter

                jmp forth

kernel_getc:
        ; """Get a single character from the keyboard. By default, py65mon
        ; is set to $f004, which we just keep. Note that py65mon's getc routine
        ; is non-blocking, so it will return '00' even if no key has been
        ; pressed. We turn this into a blocking version by waiting for a
        ; non-zero character.
        ; """
_loop:
                inc RndState+0	; randomize
                lda c65_getc
                beq _loop
                rts


kernel_havekey: ; a key ready?
		lda c65_kbhit
		rts


kernel_putc:
        ; """Print a single character to the console. By default, py65mon
        ; is set to $f001, which we just keep.
        ; """
                sta c65_putc
                rts


platform_bye:
                brk

				
platform_CCAt: ; ( -- ud )  Fetch CPU cycle counter
		dex			; allot DStack space
		dex
		dex
		dex
		bit c65_stop		; stop the cycle counter so we can read it
		lda c65_cycles+0	; cycles  Current 32 bit cycle count in NUXI order
		sta DStack+0,x
		lda c65_cycles+1
		sta DStack+1,x
		lda c65_cycles+2
		sta DStack+2,x
		lda c65_cycles+3
		sta DStack+3,x
		bit c65_start		; restart the cycle counter		

                rts


Platform_Block_Read: ; ( addr u -- )  Read a block from storage
		jsr Platform_BlockParms
		lda #1			; read
		sta c65_blkio
		lda c65_status
		beq +
		lda #$100+err_BlockRead
		jsr ThrowA
+		rts

Platform_BlockParms: ; ( addr u -- )
		jsr PopYA		; set block #
		sta c65_blknum+0
		sty c65_blknum+1
		jsr PopYA		; set buffer addr
		sta c65_buffer+0
		sty c65_buffer+1
		lda #$ff		; in case c65 isn't talking
		sta c65_status
		rts

Platform_Block_Write: ; ( addr u -- )  Write a block to storage
		jsr Platform_BlockParms
		lda #2			; write
		sta c65_blkio
		lda c65_status
		beq +
		lda #$100+err_BlockWrite
		jsr ThrowA
+		rts


; Leave the following string as the last entry in the kernel routine so it
; is easier to see where the kernel ends in hex dumps. This string is
; displayed after a successful boot
s_kernel_id:
        .text "Tali Forth 2 remix default kernel for py65mon (31 May 2024)", AscLF, 0


 .cerror *<c65_putc ; code overflowed into C65 I/O area 

; Add the interrupt vectors
* = $fffa

.word v_nmi
.word v_reset
.word v_irq
 .endsection code

; END

