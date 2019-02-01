; List of Strings for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; First version: 01. Apr 2016 (for Liara Forth)
; This version: 10. Oct 2018

; This file is included by taliforth.asm

; ## GENERAL STRINGS
; All general strings must be zero-terminated, names start with "s_",
; aliases with "str_"

string0: ; base of string list
s_ok:            .byte " ok", 0          ; note space at beginning, assumed to be at offset 0
s_compile:       .byte " compiled", 0    ; note space at beginning
s_redefined:     .byte "redefined ", 0   ; note space at end

s_wid_assembler: .byte "Assembler ",0    ; Wordlist ID 2, note space at end
s_wid_editor:    .byte "Editor ",0       ; Wordlist ID 3, note space at end
s_wid_forth:     .byte "Forth ",0        ; Wordlist ID 4, note space at end
s_wid_root:      .byte "Root ",0         ; Wordlist ID 5, note space at end

s_see_flags:     .byte "flags (CO AN IM NN UF HC R6): ",0
s_see_nt:        .byte "nt: ",0
s_see_xt:        .byte "xt: ",0
s_see_size:      .byte "size (decimal): ",0


; ## ERROR STRINGS
; All error strings must be zero-terminated,
; aliases with "err_"
; NOTE: If the string texts are changed, the test suite must be as well

.alias err_allot           0
.alias err_badsource       1
.alias err_compileonly     2
.alias err_defer           3
.alias err_divzero         4
.alias err_noname          5
.alias err_refill          6
.alias err_state           7
.alias err_syntax          8
.alias err_underflow       9
.alias err_negallot        10
.alias err_wordlist        11
.alias err_blockwords      12

.scope
error_table:
        .word _allot, _badsource, _compileonly, _defer  ;  0-3
        .word _divzero, _noname, _refill, _state        ;  4-7
        .word _syntax, _underflow, _negallot, _wordlist ;  8-11
        .word _blockwords                               ; 12

_allot:       .byte "ALLOT using all available memory", 0
_badsource:   .byte "Illegal SOURCE-ID during REFILL", 0
_compileonly: .byte "Interpreting a compile-only word", 0
_defer:       .byte "DEFERed word not defined yet", 0
_divzero:     .byte "Division by zero", 0
_noname:      .byte "Parsing failure", 0
_refill:      .byte "QUIT could not get input (REFILL returned -1)", 0
_state:       .byte "Already in compile mode", 0
_syntax:      .byte "Undefined word", 0
_underflow:   .byte "Stack underflow", 0
_negallot:    .byte "Max memory freed with ALLOT", 0
_wordlist:    .byte "No wordlists available", 0
_blockwords:  .byte "Please assign vectors BLOCK-READ-VECTOR and BLOCK-WRITE-VECTOR",0
.scend

; ## ENVIRONMENT STRINGS

; These are used by the ENVIRONMENT? word and stored in the old string
; format: Length byte first, then the string itself that is not rpt. not
; zero-terminated. Note these are uppercase by ANS defintion.
; All start with "envs_".

; These return a single-cell number
envs_cs:        .byte 15, "/COUNTED-STRING"
envs_hold:      .byte 5, "/HOLD"
envs_pad:       .byte 4, "/PAD"
envs_aub:       .byte 17, "ADDRESS-UNIT-BITS"
envs_floored:   .byte 7, "FLOORED"
envs_max_char:  .byte 8, "MAX-CHAR"
envs_max_n:     .byte 5, "MAX-N"
envs_max_u:     .byte 5, "MAX-U"
envs_rsc:       .byte 18, "RETURN-STACK-CELLS"
envs_sc:        .byte 11, "STACK-CELLS"
envs_wl:        .byte 9, "WORDLISTS"
; These return a double-cell number
envs_max_d:     .byte 5, "MAX-D"
envs_max_ud:    .byte 6, "MAX-UD"
