	.cpu "6502"	; 6502 processor (Tali will compile on older 6502)
			;   still needs ROR
	.enc "none"	; No special text encoding (just plain ASCII)


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
 .dsection zp ; zero page, writable, uninitialized
 .cwarn * > $80, "zero page ends >$80"
;		+-------------------+
;		|  zp variables     |
;		|  Data stack[]	    |
;		| FP stack arrays   |
;		|		    |
;		|   unused	    |
;		|		    |
;		+-------------------+
RStack = $0100	; begin of 6502 Return Stack
;		|                   |
;		|  ^  Return Stack  |  <-- CPU S register
;		|  |                |
rsp0      = $ff		; initial Return Stack Pointer (6502 stack)

* = $200
 .dsection bss ;writable, unitialized
 .cwarn * > $c00, "bss ends >$c00"
;		+-------------------+
;		| input buffer	    |
;		| more variables    |
;		|                   |
;		+-------------------+
;cp0:		|  |                |
;		|  v  Dictionary    |
;		|       (RAM)       |
;		|                   |
;   (...)	~~~~~~~~~~~~~~~~~~~~~  <-- cp
;		|                   |
;		|                   |
;		|                   |
;		|                   |
cp_end    = $8000	; Last RAM byte available for code
;		+-------------------+

 * = $8000
 .dsection code ;read-only, initialized
 .cwarn * > $e000, "code ends >$c000"





; HARD PHYSICAL ADDRESSES

; Some of these are somewhat silly for the 6502, where for example
; the location of the Zero Page is fixed by hardware. However, we keep
; these for easier comparisons with Liara Forth's structure and to
; help people new to these things.


; SOFT PHYSICAL ADDRESSES

; Tali currently doesn't have separate user variables for multitasking.
; We do have limited zero-page space, so
; many Forth variables are moved to user0 in non-zp memory.

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

;TALI_OPTIONAL_WORDS := ["fp","fpe", "fpieee","fptrancendentals", "ed", "editor", "ramdrive", "block", "environment?", "assembler", "wordlist" ]


; TALI_OPTION_CR_EOL sets the character(s) that are printed by the word
; CR in order to move the cursor to the next line.  The default is "lf"
; for a line feed character (#10).  "cr" will use a carriage return (#13).
; Having both will use a carriage return followed by a line feed.  This
; only affects output.  Either CR or LF can be used to terminate lines
; on the input.

;TALI_OPTION_CR_EOL := [ "lf" ]
;TALI_OPTION_CR_EOL := [ "cr" ]
TALI_OPTION_CR_EOL := [ "cr", "lf" ]

 .section bss	; 1st part of bss space
bsize     = $ff		; size of input/output buffers
buffer0: .fill bsize	; input buffer
 .endsection bss

; Make sure the above options are set BEFORE this include.

.include "../taliforth.asm" ; guts of Tali Forth

 .section bss	; last part of bss space
hist_buff: .fill 8*128	; Input History for ACCEPT
cp0:  ; start of RAM dictionary
 .endsection bss

; =====================================================================

; Default kernel file for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Sam Colwell
; First version: 19. Jan 2014
; This version: 04. Dec 2022
;
; This version is for the 65816S simulator.
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

; The main file of Tali got us to $e000. However, py65mon by default puts
; the basic I/O routines at the beginning of $f000. We don't want to change
; that because it would make using it out of the box harder, so we just
; advance past the virtual hardware addresses.
 .section code
* = $f010

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
                jmp Cold

kernel_getc: ; """Get a single character from the keyboard to A.
  ; By default, py65mon
        ; is set to $f004, which we just keep. Note that py65mon's getc routine
        ; is non-blocking, so it will return '00' even if no key has been
        ; pressed. We turn this into a blocking version by waiting for a
        ; non-zero character.
        ; """
_loop:
;                lda $f004
;               beq _loop
;              rts
-		inc RndState+0	; randomize
		.byte $22	; jsl GET_BYTE_FROM_PC
		.word $e033
		.byte 0
		bcs -
		rts


kernel_putc:
        ; """Print a single character to the console. By default, py65mon
        ; is set to $f001, which we just keep.
        ; """
;                sta $f001
;                rts
-		.byte $22	; jsl SEND_BYTE_TO_PC
		.word $e063
		.byte 0
		bcs -
		rts


platform_bye:
                brk


platform_CCAt: ; ( -- ud )  Fetch CPU cycle counter
		dex
		dex
		dex
		dex
		.byte $02,$F4,DStack ; cop $f4	get 65816s simulator cycle count in DStack,x
		rts


; Leave the following string as the last entry in the kernel routine so it
; is easier to see where the kernel ends in hex dumps. This string is
; displayed after a successful boot
s_kernel_id:
        .text "Tali Forth 2 kernel for 65816s (31. May 2024)", AscCR,AscLF, 0


; Add the interrupt vectors
* = $fffa

.word v_nmi
.word v_reset
.word v_irq
 .endsection code

; END

