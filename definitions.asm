; Definitions for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; First version: 01. Apr 2016 (Liara Forth)
; This version: 24. Dec 2018 forked

; This file is included by taliforth.asm. These are the general
; definitions; platform-specific definitions such as the
; memory map are kept in the platform folder.


 .text ZeroPage ; ZERO PAGE ADDRESSES/VARIABLES ---------------------------------
        .org user0

; These are kept at the top of Zero Page, with the most important variables at
; the top because the Data Stack grows towards this area from dsp0: If there is
; an overflow, the lower, less important variables will be clobbered first,
; giving the system a chance to recover. In other words, they are part of the
; floodplain.
; This layout duplicated in cold_zp_table.

 .space cp      2          ; Compiler Pointer
 .space dp      2          ; Dictionary Pointer
 .space workword 2         ; nt (not xt!) of word being compiled, except in
                           ; a :NONAME declared word (see status)
; The four variables insrc, cib, ciblen, and toin must stay together in this
; sequence for the words INPUT>R and R>INPUT to work correctly.
 .space insrc   2          ; input Source for SOURCE-ID
 .space cib     2          ; address of current input buffer
 .space ciblen  2          ; length of current input buffer
 .space toin    2          ; pointer to CIB (>IN in Forth)
 .space tmp0    2          ; temporary storage
 .space output  2          ; vector for EMIT
 .space input   2          ; vector for KEY
 .space havekey 2          ; vector for KEY?
 .space state   2          ; STATE: -1 compile, 0 interpret
 .space base    2          ; number radix, default 10
 .space nc_limit 2         ; limit for Native Compile size
 .space uf_strip 2         ; flag to strip underflow detection code
 .space up      2          ; User Pointer (Address of user variables)
 .space status  2          ; internal status information
                           ; (used by : :NONAME ; ACCEPT)
                           ; Bit 7 = Redefined word message postpone
                           ;         When set before calling CREATE, it will
                           ;         not print the "redefined xxxx" message if
                           ;         the word exists.  Instead, this bit will
                           ;         be reused and after CREATE has run, it will
                           ;         be set if the word was redefined and 0 if
                           ;         not.
                           ;         This bit should be 0 when not in use.
                           ; Bit 6 = 1 for normal ":" definitions
                           ;         WORKWORD contains nt of word being compiled
                           ;       = 0 for :NONAME definitions
                           ;         WORKWORD contains xt of word being compiled
                           ; Bit 5 = 1 for NUMBER returning a double word
                           ;       = 0 for NUMBER returning a single word
                           ; Bit 3 = 1 makes CTRL-n recall current history
                           ;       = 0 CTRL-n recalls previous history
                           ; Bit 2 = Current history buffer msb
                           ; Bit 1 = Current history buffer (0-7, wraps)
                           ; Bit 0 = Current history buffer lsb
                           ; status+1 is used by ACCEPT to hold history lengths.
        ; cold_zp_table initialize ends here
 .space tmp4    2          ; temporary storage
 .space tmp1    2          ; temporary storage
 .space tmp2    2          ; temporary storage
 .space tmp3    2          ; temporary storage (especially for print)
 .space tmpdsp  2          ; temporary DSP (X) storage (two bytes)
 .space tmptos  2          ; temporary TOS storage
 .space editor1 2          ; temporary for editors
 .space editor2 2          ; temporary for editors
 .space editor3 2          ; temporary for editors
 .alias leave_anchor editor1 ; head of LEAVE resolve chain, used by DO LEAVE LOOP
 .space tohold  2          ; pointer for formatted output
 .space scratch 8          ; 8 byte scratchpad (see UM/MOD)

 .space dsp     56        ; Data stack, 28 16-bit cells
dsp0:                     ; Empty value of Data Stack Pointer


 .text UserVariables ; User Variables:------------------------------------------
 ; This layout duplicated in cold_user_table.
        .org 0
; Block variables
 .space blk_offset 2        ; BLK
 .space scr_offset 2        ; SCR

; Wordlists
 .space current_offset 1    ; CURRENT  (Compilation wordlist)
 .space num_wordlists_offset 1 ; #WORDLISTS
 .alias max_wordlists 12   ; Maximum number of wordlists supported
                           ; 4 Tali built-ins + 8 user wordlists
 .space wordlists_offset 24 ;2*max_wordlists  ; WORDLISTS (cells)
                            ;             (FORTH, EDITOR, ASSEMBLER, ROOT, +8 more)
 .space num_order_offset 1 ; #ORDER (Number of wordlists in search order)
 .space search_order_offset 9 ; SEARCH-ORDER (bytes)
                           ; Allowing for 9 to keep offsets even.

; Buffer variables
 .space blkbuffer_offset 2  ; Address of buffer
 .space buffblocknum_offset 2   ; Block number current in buffer
 .space buffstatus_offset 2   ; Status of buffer (bit 0 = used, bit 1 = dirty)
; Block I/O vectors
 .space blockread_offset  2   ; Vector to block reading routine
 .space blockwrite_offset 2   ; Vector to block writing routine
 .space uv_ramdrive 2      ; ramdrive buffer ptr

 uv_dim: ; dimension of UserVariable block
 .text


; ASCII CHARACTERS

.alias AscCC   $03  ; break (CTRL-c)
.alias AscBELL $07  ; bell sound
.alias AscBS   $08  ; backspace
.alias AscTAB  $09  ; horz tab
.alias AscLF   $0a  ; line feed
.alias AscCR   $0d  ; carriage return
.alias AscESC  $1b  ; escape
.alias AscSP   $20  ; space
.alias AscDEL  $7f  ; delete (CTRL-h)
.alias AscCP   $10  ; CTRL-p (used to recall previous input history)
.alias AscCN   $0e  ; CTRL-n (used to recall next input history)


; VARIOUS

.alias MAX_LINE_LENGTH  79      ; assumes 80 character lines

; END
