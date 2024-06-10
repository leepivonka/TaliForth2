; Programmer  : Sam Colwell
; File        : platform-neo6502.asm
; Date        : 2024-02
; Description : A platform file for the Neo6502 board.
; use "make taliforth-neo6502.bin" from main Tali directory to generate
; binary.  For the simulator (which starts in Neo6502 BASIC) place the
; binary file in the "storage" folder and use the following commands to
; load and start Tali Forth 2.
;
; load "taliforth-neo6502.bin", $a000
; sys $a000
; 
; simulator commandline:
; neo taliforth-neo6502.bin@a000 run@a000


        ; 65C02 processor (Tali will not compile on older 6502)
        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"

        ; Where to start Tali Forth 2 in ROM (or RAM if loading it) 
        * = $a000

; I/O facilities are handled in these separate kernel files because of their
; hardware dependencies. See docs/memorymap.txt for a discussion of Tali's
; memory layout.


; MEMORY MAP OF RAM

; Drawing is not only very ugly, but also not to scale. See the manual for
; details on the memory map. Note that some of the values are hard-coded in
; the testing routines, especially the size of the input history buffer, the
; offset for PAD, and the total RAM size. If these are changed, the tests will
; have to be changed as well


;    $0000  +-------------------+  ram_start, zpage, user0
;           |  User varliables  |
;           +-------------------+
;           |                   |
;           |                   |
;           +~~~~~~~~~~~~~~~~~~~+  <-- dsp
;           |                   |
;           |  ^  Data Stack    |
;           |  |                |
;    $0078  +-------------------+  dsp0, stack
;           |                   |
;           |   (Reserved for   |
;           |      kernel)      |
;           |                   |
;    $0100  +-------------------+
;           |                   |
;           |  ^  Return Stack  |  <-- rsp
;           |  |                |
;    $0200  +-------------------+  rsp0, buffer, buffer0
;           |  |                |
;           |  v  Input Buffer  |
;           |                   |
;    $0300  +-------------------+  cp0
;           |  |                |
;           |  v  Dictionary    |
;           |       (RAM)       |
;           |                   |
;   (...)   ~~~~~~~~~~~~~~~~~~~~~  <-- cp
;           |                   |
;           |                   |
;           |                   |
;           |                   |
;           |                   |
;           |                   |
;    $9C00  +-------------------+  hist_buff, cp_end
;           |   Input History   |
;           |    for ACCEPT     |
;           |  8x128B buffers   |
;    $9fff  +-------------------+  ram_end


; HARD PHYSICAL ADDRESSES

; Some of these are somewhat silly for the 65c02, where for example
; the location of the Zero Page is fixed by hardware. However, we keep
; these for easier comparisons with Liara Forth's structure and to
; help people new to these things.

ram_start = $0000          ; start of installed 32 KiB of RAM
ram_end   = $a000-1        ; end of installed RAM
zpage     = ram_start      ; begin of Zero Page ($0000-$00ff)
zpage_end = $7F            ; end of Zero Page used ($0000-$007f)	
stack0    = $0100          ; begin of Return Stack ($0100-$01ff)
hist_buff = ram_end-$03ff  ; begin of history buffers


; SOFT PHYSICAL ADDRESSES

; Tali currently doesn't have separate user variables for multitasking. To
; prepare for this, though, we've already named the location of the user
; variables user0. Note cp0 starts one byte further down so that it currently
; has the address $300 and not $2FF. This avoids crossing the page boundry when
; accessing the user table, which would cost an extra cycle.

user0     = zpage            ; user and system variables
rsp0      = $ff              ; initial Return Stack Pointer (65c02 stack)
bsize     = $ff              ; size of input/output buffers
buffer0   = stack0+$100      ; input buffer ($0200-$027f)
cp0       = buffer0+bsize+1  ; Dictionary starts after last buffer
cp_end    = hist_buff        ; Last RAM byte available for code
padoffset = $ff              ; offset from CP to PAD (holds number strings)


; OPTIONAL WORDSETS

; Tali Forth 2 is a bit of a beast, expecting about 24K of ROM space.
; For some applications, the user might not need certain words and would
; prefer to have the memory back instead.  Remove any of the items in
; TALI_OPTIONAL_WORDS to remove the associated words when Tali is
; assembled.  If TALI_OPTIONAL_WORDS is not defined in your platform file,
; you will get all of the words.

TALI_OPTIONAL_WORDS := [ "ed", "editor", "ramdrive", "block", "environment?", "assembler", "wordlist" ]

; "ed" is a string editor. (~1.5K)
; "editor" is a block editor. (~0.25K)
;     The EDITOR-WORDLIST will also be removed.
; "ramdrive" is for testing block words without a block device. (~0.3K)
; "block" is the optional BLOCK words. (~1.4K)
; "environment?" is the ENVIRONMENT? word.  While this is a core word
;     for ANS-2012, it uses a lot of strings and therefore takes up a lot
;     of memory. (~0.2K)
; "assembler" is an assembler. (~3.2K)
;     The ASSEMBLER-WORDLIST will also be removed.
; "wordlist" is for the optional SEARCH-ORDER words (eg. wordlists)
;     Note: Without "wordlist", you will not be able to use any words from
;     the EDITOR or ASSEMBLER wordlists (they should probably be disabled
;     by also removing "editor" and "assembler"), and all new words will
;     be compiled into the FORTH wordlist. (~0.9K)


; TALI_OPTION_CR_EOL sets the character(s) that are printed by the word
; CR in order to move the cursor to the next line.  The default is "lf"
; for a line feed character (#10).  "cr" will use a carriage return (#13).
; Having both will use a carriage return followed by a line feed.  This
; only affects output.  Either CR or LF can be used to terminate lines
; on the input.

;TALI_OPTION_CR_EOL := [ "lf" ]
; Neo6502 uses CR
TALI_OPTION_CR_EOL := [ "cr" ]
;TALI_OPTION_CR_EOL := [ "cr" "lf" ]



; Put the kernel init first (at $8000)

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
kernel_init:
        ; """Initialize the hardware. This is called with a JMP and not
        ; a JSR because we don't have anything set up for that yet. With
        ; py65mon, of course, this is really easy. -- At the end, we JMP
        ; back to the label forth to start the Forth system.
        ; """

                ; Nothing special to set up here.

                ; We've successfully set everything up, so print the kernel
                ; string
                ldx #0
-               lda s_kernel_id,x
                beq _done
                jsr kernel_putc
                inx
                bra -
_done:
                jmp forth


; Map kernel_getc and kernel_putc to Neo6502 API routines.
ReadCharacter = $ffee
WriteCharacter = $fff1
kernel_getc = ReadCharacter
kernel_putc = WriteCharacter



platform_bye:
                brk


; Make sure the options are set BEFORE this include.

; Put the guts of Tali Forth 2 here.
.include "../taliforth.asm" ; zero page variables, definitions

; Leave the following string as the last entry in the kernel routine so it
; is easier to see where the kernel ends in hex dumps. This string is
; displayed after a successful boot
s_kernel_id:
        .text "Tali Forth 2 kernel for Neo6502 (2024-02-06)", AscLF, 0



; END

