        ; 65C02 processor (Tali will not compile on older 6502)
        .cpu "65c02"
        ; No special text encoding (eg. ASCII)
        .enc "none"

; This file is based upon the steckschwein version which in turn seems to
; be based on the py65mon platform. Thanks for the good groundwork.

; Sorbus does not utilize a load address.
; Just build with:
;   make taliforth-sorbus.bin
; and rename the file to tali4th2.sx4
* = $0400   ; start of SX4 file

; from the Sorbus memory map
BANK       = $DF00      ; select bank to use
krn_chrout = $FF03      ; output a character to UART
krn_getkey = $FF00      ; read character from UART, C=1, Z=1, A=0: no data

kernel_init:
        ; """Initialize the hardware.
        ; There is really not a lot to do as we use the Sorbus kernel which
        ; already has done all the dirty work for us. Also there is not much
        ; to setup.
        ; We put this right before including the "real" taliforth as
        ; kernel_init is not called through any vector. Also, we save a few
        ; bytes as we need no jmp to kernel_init and no jmp to Forth.
        ; """

                ; We've successfully set everything up, so print the kernel
                ; string
                brk                 ; software interrupt
                .byte $0b           ; to copy BIOS from ROM to RAM
                stz BANK            ; switch to RAM

                ldx #$00
-               lda s_kernel_id,x
                beq _done
                jsr kernel_putc     ; print kernel id message
                inx
                bra -
_done:
                ;jmp forth


; I/O facilities are handled in the separate kernel files because of their
; hardware dependencies. See docs/memorymap.txt for a discussion of Tali's
; memory layout.


; MEMORY MAP

; Drawing is not only very ugly, but also not to scale. See the manual for
; details on the memory map. Note that some of the values are hard-coded in
; the testing routines, especially the size of the input history buffer, the
; offset for PAD, and the total RAM size. If these are changed, the tests will
; have to be changed as well


;    $0000  +-------------------+
;           |  (Reserved for    |
;           | Sorbus kernel,I/O)|
;    $0010  +-------------------+  ram_start, zpage, user0
;           |  user varliables  |
;           +-------------------+
;           |                   |
;           |                   |
;           +~~~~~~~~~~~~~~~~~~~+  <-- dsp
;           |                   |
;           |  ^  data stack    |
;           |  |                |
;    $0078  +-------------------+  dsp0, stack
;           |                   |
;           |     (unused)      |
;           |                   |
;    $0100  +-------------------+
;           |                   |
;           |  ^  return stack  |  <-- rsp
;           |  |                |
;    $0200  +-------------------+  rsp0
;           |                   |
;           | (disk buffer for  |
;           |  saving only)     |
;           |                   |
;    $0300  +-------------------+
;           |                   |
;           | (disk buffer for  |
;           |  all access)      |
;           |                   |
;    $0360  +-------------------+
;           |  |                |
;           |  v  Input Buffer  |
;           |                   |
;    $0400  +-------------------+
;           |                   |
;           |  Forth program    |
;           |       code        |
;           |                   |
;   ~$6000  +-------------------+  cp0
;           |  |                |
;           |  v  dictionary    |
;           |       (RAM)       |
;           |                   |
;   (...)   ~~~~~~~~~~~~~~~~~~~~~  <-- cp
;           |                   |
;           |                   |
;           |                   |
;           |                   |
;           |                   |
;           |                   |
;    $D000  +-------------------+  ram_end, cp_end
;           |                   |
;           |   hardware I/O    |
;           |                   |
;    $E000  +-------------------+  hist_buff
;           |   input history   |
;           |    for ACCEPT     |
;           |  8x128B buffers   |
;    $E400  +-------------------+  buffer, buffer0
;           |                   |
;           |     free RAM      |
;           |   (any ideas?)    |
;           |                   |
;    $FF00  +-------------------+
;           |                   |
;           |   I/O functions   |
;           |       (BIOS)      |
;           |                   |
;    $FFFF  +-------------------+


; HARD PHYSICAL ADDRESSES

; Some of these are somewhat silly for the 65c02, where for example
; the location of the zero page is fixed by hardware. However, we keep
; these for easier comparisons with Liara Forth's structure and to
; help people new to these things.

ram_start = $0010          ; start of installed 32 KiB of RAM
ram_end   = $D000-1        ; end of installed RAM
zpage     = ram_start      ; begin of zero page ($0010-$00ff)
zpage_end = $FF            ; end of zero page used ($0010-$00ff)
stack0    = $0100          ; begin of return stack ($0100-$01ff)
hist_buff = $E000          ; begin of "RAM under kernel"


; SOFT PHYSICAL ADDRESSES

; Tali currently doesn't have separate user variables for multitasking. To
; prepare for this, though, we've already named the location of the user
; variables user0.

user0     = zpage          ; user and system variables
rsp0      = $ff            ; initial Return Stack Pointer (65c02 stack)
bsize     = $ff            ; size of input/output buffers
buffer0   = hist_buff+$400 ; input buffer ($E400-$E4ff (?))
cp0       = tali_end       ; Dictionary starts after code
cp_end    = ram_end        ; Last RAM byte available for code
padoffset = $ff            ; offset from CP to PAD (holds number strings)


.include "../taliforth.asm" ; zero page variables, definitions

; =====================================================================

; Default kernel file for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; First version: 19. Jan 2014
; This version: 18. Feb 2018
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


; routine called when leaving Tali Forth 2: just call the reset vector.
platform_bye:
        jmp ($fffc)


kernel_getc:
        ; """Get a single character from the keyboard.
        ; krn_getkey does not block and uses the carry flag to signal
        ; if there was a byte. We need a small wrapper routine.
        ; """

-       jsr krn_getkey
        bcs -
        rts


; we need no wrapper routine but alias krn_chrout as kernel_putc as they are compatible
kernel_putc = krn_chrout


; Leave the following string as the last entry in the kernel routine so it
; is easier to see where the kernel ends in hex dumps. This string is
; displayed after a successful boot
s_kernel_id:
        .text AscLF, "Tali Forth 2 default kernel for Sorbus (2024-01-09)"
        .text AscLF, 0

tali_end:

; END
