; Low-level Forth word routines
; Tali Forth 2 for the 65c02
; Scot W. Stevenson <scot.stevenson@gmail.com>
; First version: 19. Jan 2014
; This version: 24. Dec 2018 forked

; This list is ordered alphabetically by the names of the words, not their
; strings (so "!" is sorted as "STORE"). However, we start off with COLD,
; ABORT, and QUIT as the natural start sequence. Each word has two special
; status lines that begins with "; ## ", which allows auto-generation of the
; WORDLIST.md file and other entries in the docs folder. Status entries are:
;
;       TBA --> fragment --> coded --> tested --> auto
;
; "Auto" means that the word is automatically tested by the test suite (good),
; "tested" means that it was tested by hand in some way (okay), "coded" means
; it hasn't been tested at all (bad). See the test suite for more details.


; ## COLD ( -- ) "Reset the Forth system"
; ## "cold"  tested  Tali Forth
;       """Reset the Forth system. Does not restart the kernel,
;       use the 65c02 reset for that. Flows into ABORT.
;       """
xt_cold:
                cld

                ; Load all of the important zero page variables from ROM
                ; This loop loads them back to front.
                ; We can use X here because Tali hasn't started
                ; using the stack yet.
                ldx #cold_zp_table_end-cold_zp_table-1
*               lda cold_zp_table,x
                sta 0,x
                dex
                bpl -

                ; Initialize 65c02 stack (Return Stack)
                ldx #rsp0
                txs

                ; Clear Data Stack. This is repeated in ABORT, but this way we
                ; can load high-level words with EVALUATE
                ldx #dsp0

                ; Initialize the user variables.
                ; Like the zero page variables, these are initialized
                ; back to front.
                ldy #cold_user_table_end-cold_user_table-1
*               lda cold_user_table,y
                sta (up),y
                dey
                bpl -

                jsr xt_cr

                ; Define high-level words in forth_words.asm via EVALUATE.  If
                ; you do not have any high-level words, this part can be
                ; commented out.
                lda #<forth_words_start                ; start address goes NOS
                ldy #>forth_words_start
                jsr PsuYA
                lda #<[forth_words_end-forth_words_start] ; length goes TOS
                ldy #>[forth_words_end-forth_words_start]
                jsr PsuYA
                jsr xt_evaluate

                ; Now define any user words via EVALUATE. If you do not have
                ; any user-defined words, this part can be commented out as
                ; well.
                lda #<user_words_start                ; start address goes NOS
                ldy #>user_words_start
                jsr PsuYA
                lda #<[user_words_end-user_words_start] ; length goes TOS
                ldy #>[user_words_end-user_words_start]
                jsr PsuYA
                jsr xt_evaluate

                ; Initialize all of the history buffers by putting a zero in
                ; each length byte.
                stz hist_buff
                stz hist_buff+$80
                stz hist_buff+$100
                stz hist_buff+$180
                stz hist_buff+$200
                stz hist_buff+$280
                stz hist_buff+$300
                stz hist_buff+$380

                ; fall through to ABORT


; ## ABORT ( -- ) "Reset the Data Stack and restart the CLI"
; ## "abort"  tested  ANS core
        ; """https://forth-standard.org/standard/core/ABORT
        ; Clear Data Stack and continue into QUIT. We can jump here via
        ; subroutine if we want to because we are going to reset the 65c02's
        ; stack pointer (the Return Stack) anyway during QUIT. Note we don't
        ; actually delete the stuff on the Data Stack.
        ; """
xt_abort:       ldx #dsp0
                ; fall through to QUIT


; ## QUIT ( -- ) "Reset the input and get new input"
; ## "quit"  tested  ANS core
        ; """https://forth-standard.org/standard/core/QUIT
        ; Rest the input and start command loop
        ; """
.scope
xt_quit:
                ; Clear the Return Stack. This is a little screwed up
                ; because the 65c02 can only set the Return Stack via X,
                ; which is our Data Stack pointer. The ANS specification
                ; demands, however, that ABORT reset the Data Stack pointer
                txa             ; Save the DSP that we just defined
                ldx #rsp0
                txs
                tax             ; Restore the DSP. Dude, seriously.

                ; SOURCE-ID is zero (keyboard input)
                stz insrc
                stz insrc+1

                ; STATE is zero (interpret, not compile)
                stz state
                stz state+1
_get_line:
                lda #<buffer0   ; input buffer, this is paranoid
                sta cib
                lda #>buffer0
                sta cib+1

                ; Size of current input buffer (CIB) is zero
                stz ciblen
                stz ciblen+1

                ; accept a line from the current import source
                jsr xt_refill           ; ( -- f )

                ; test flag: LSB of TOS
                lda 0,x
                bne _success

                ; If REFILL returned a FALSE flag, something went wrong and we
                ; need to print an error message and reset the machine. We
                ; don't need to save TOS because we're going to clobber it
                ; anyway when we go back to ABORT.
                lda #err_refill
                jmp error

_success:
                ; Assume we have successfully accepted a string of input from
                ; a source, with address cib and length of input in ciblen. We
                ; arrive here still with the TRUE flag from REFILL as TOS
                inx                     ; drop
                inx

                ; Main compile/execute routine
                jsr interpret

                ; Test for Data Stack underflow. Tali Forth does not check for
                ; overflow because it is so rare
                cpx #dsp0+1
                bcc _stack_ok           ; DSP must always be smaller (!) than DSP0

                jmp underflow_error

_stack_ok:
                ; Display system prompt if all went well. If we're interpreting,
                ; this is " ok", if we're compiling, it's " compiled".  Note
                ; space at beginning of the string.
                ldy state
                beq _print          ; assumes s_ok-string0 is 0
                ldy #s_compile-string0
_print:
                jsr print_string_Y

                ; Awesome line, everybody! Now get the next one
                bra _get_line

z_cold:
z_abort:
z_quit:         ; no RTS required
.scend


; This table holds all of the initial values for the variables in zero page.
; This table is used by COLD.
; This duplicates the ZeroPage layout in definitions.asm .
cold_zp_table:
        .word cp0+256+1024      ; cp moved to make room for user vars (uv_dim) and block buffer.
        .word dictionary_start  ; dp
        .word 0                 ; workword
        .word 0                 ; insrc (SOURCE-ID is 0 for keyboard)
        .word buffer0           ; cib
        .word 0                 ; ciblen
        .word 0                 ; toin
        .word 0                 ; ip
        .word kernel_putc       ; output
        .word kernel_getc       ; input
        .word 0                 ; havekey
        .word 0                 ; state (0 = interpret)
        .word 10                ; base
        .word 12                ; nc-limit
        .word 0                 ; uf_strip (off by default)
        .word cp0               ; up (user vars put right at beginning of available RAM)
        .word 0                 ; status
cold_zp_table_end:
; No further ZP variables are initialized.
; The variables past this point are all temporaries.

; This table holds the inital values for the user variables.  This table is
; used by COLD.
; This duplicates the UserVariables layout in definitions.asm .
cold_user_table:
        .word 0                         ; BLK
        .word 0                         ; SCR
        .byte 0                         ; CURRENT = FORTH-WORDLIST
        .byte 4                         ; #WORDLISTS (FORTH EDITOR ASSEMBLER ROOT)
        .word dictionary_start          ; FORTH-WORDLIST
        .word editor_dictionary_start   ; EDITOR-WORDLIST
        .word assembler_dictionary_start ; ASSEMBLER-WORDLIST
        .word root_dictionary_start     ; ROOT-WORDLIST
        .word 0,0,0,0,0,0,0,0           ; User wordlists
        .byte 1                         ; #ORDER
        .byte 0,0,0,0,0,0,0,0,0         ; search-order
        .word cp0+256                   ; Address of buffer (right after USER vars)
        .word 0                         ; block in buffer
        .word 0                         ; buffer status (not in use)
        .word xt_block_word_error       ; block-read vector
        .word xt_block_word_error       ; block-write vector
cold_user_table_end:



; ## ABORT_QUOTE ( "string" -- ) "If flag TOS is true, ABORT with message"
; ## "abort""  tested  ANS core
        ; """https://forth-standard.org/standard/core/ABORTq
        ; Abort and print a string.
        ; """
.scope
xt_abort_quote:
                ; compile run-time part
                stz tmp2    ; don't process escapes
                ldy #>abort_quote_runtime
                lda #<abort_quote_runtime
                jmp cmpl_s_quote_YA
z_abort_quote:
.scend

abort_quote_runtime:
        ; """Runtime aspect of ABORT_QUOTE"""
.scope
        ; xt_abort_quote compiled code of the form:
        ;       jsr abort_quote_runtime
        ;       jmp +
        ;       .byte "string"
        ;   *
                ; We arrive here with ( f ) & return_addr
                lda 0,x         ; test f
                ora 1,x
                bne +           ; if TRUE, abort
                ; We;re not aborting!
                inx             ; DROP f
                inx
                rts             ; Return after the "jsr abort_quote_runtime".
                                ; We'll then execute the "jmp +" to jump
                                ; around the string & we're done.

*               ; We're aborting!
                ; We can use the return_addr & the "jmp +" operand
                ; to calc the addr & u of the string.
                pla             ; get return_addr
                ply
                phy
                pha
                sta tmp1
                sty tmp1+1
                                ; ( f )
                clc             ; get string addr
                adc #4
                sta 0,x
                bcc +
                iny
*               sty 1,x
                                ; ( addr )
                dex             ; get string length
                dex
                sec
                ldy #2
                lda (tmp1),y
                sbc 2,x
                sta 0,x
                iny
                lda (tmp1),y
                sbc 3,x
                sta 1,x
                                ; ( addr u )
                jsr xt_type
                jsr xt_cr       ; We follow Gforth
                                ; in going to a new line after the string
                jmp xt_abort    ; not JSR, so never come back
.scend


; ## ABS ( n -- u ) "Return absolute value of a number"
; ## "abs"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ABS
        ; Return the absolute value of a number.
        ; """
.scope
xt_abs:
                jsr underflow_1
abs_nouf:
                lda 1,x         ; MSB
                bpl +           ; positive number, easy money!
                jsr negate_nouf ; negative: calculate 0 - n
*
z_abs:          rts
.scend


; ## ACCEPT ( addr n -- n ) "Receive a string of characters from the keyboard"
; ## "accept"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ACCEPT
        ; Receive a string of at most n1 characters, placing them at
        ; addr. Return the actual number of characters as n2. Characters
        ; are echoed as they are received. ACCEPT is called by REFILL in
        ; modern Forths.
        ; """
.scope
xt_accept:
                jsr underflow_2

                lda 0,x         ; number of chars to get in tmp2 ...
                inx
                inx
                sta tmp2
                                ; ... but we only accept max 255 chars

                lda 0,x         ; address of buffer is NOS, to tmp1
                ldy 1,x
                sta tmp1
                sty tmp1+1

                ldy #0

                lda tmp2        ; Abort if we were asked to receive 0 chars
                beq _buffer_full

                ; Select the next history buffer
                ; Clear bit 3 first (so overflow from bit 2 to 3 is OK)
                lda status
                and #$f7
                ; Increment the buffer number (overflow from 7 to 0 OK)
                inc
                ; Set bit 3 for detecting if CTRL-n has been pressed
                ; the first time.  This bit will be cleared on the
                ; first CTRL-n or CTRL-p received and won't be used to
                ; calculate the history buffer offset.
                ora #$08
                sta status

_loop:
                ; Out of the box, py65mon catches some CTRL sequences such as
                ; CTRL-c. We also don't need to check for CTRL-l because a
                ; vt100 terminal clears the screen automatically.

                ; This is the internal version of KEY without all the mucking
                ; about with the Data Stack while still using the input vector
                jsr key_a

                ; We quit on both line feed and carriage return
                cmp #AscLF
                beq _eol
                cmp #AscCR
                beq _eol

                ; BACKSPACES and DEL do the same thing for the moment
                cmp #AscBS
                beq _backspace
                cmp #AscDEL
                beq _backspace

                ; Check for CTRL-p and CTRL-n to recall input history
                cmp #AscCP
                beq _ctrl_p
                cmp #AscCN
                beq _ctrl_n

                ; That's enough for now. Save and echo character.
                sta (tmp1),y
                iny

                jsr emit_a

                cpy tmp2        ; reached character limit?
                bne _loop       ; fall through if buffer limit reached
                bra _buffer_full

_eol:
                jsr xt_space    ; print final space
_buffer_full:
                ; REFILL updates ciblen and toin, we don't need to do it here
                sty 0,x         ; Y contains number of chars accepted already
                stz 1,x         ; we only accept 256 chars

                jmp _done

_backspace:     ; Handle backspace and delete key, which currently do the same
                ; thing
                tya             ; buffer empty?
                bne +

                jsr xt_bell     ; complain and don't delete beyond the start of line
                bra _loop
*
                dey
                lda #AscBS      ; move back one
                jsr emit_a
                jsr xt_space    ; print a space (rubout)
                lda #AscBS      ; move back over space
                jsr emit_a

                bra _loop

_ctrl_p:
                ; CTRL-p was pressed.  Recall the previous input buffer.
                ; Select the previous buffer
                lda status
                ; Check for 0 (need to wrap back to 7)
                bit #7
                bne _ctrl_p_dec
                ; We need to wrap back to 7.
                ora #7
                sta status
                bra _recall_history
_ctrl_p_dec:
                ; It's safe to decrement the buffer index directly.
                dec status
                bra _recall_history

_ctrl_n:
                ; CTRL-n was pressed.  Recall the next input buffer.

                ; Select the next buffer

                ; Check bit 3.
                ; If it's set, this is the first time CTRL-n has been
                ; pressed and we should select the CURRENT history buffer.
                lda #$8
                bit status
                bne _recall_history
                ; This isn't the first time CTRL-n has been pressed,
                ; select the next history buffer.
                ; Clear bit 3 first (so overflow is OK)
                lda status
                and #$f7
                ; Increment the buffer number (overflow from 7 to 0 OK)
                inc
                ; Bit 3 (if it got set by going from buffer 7 to 0) will
                ; be cleared below.
                sta status
                ; Falls through to _recall_history

_recall_history:
                ; Clear bit 3 (first time ctrl-n recall) bit in status
                lda #%00001000
                trb status

                jsr _total_recall

                ; tmp3 now has the address of the previous history buffer.
                ; First byte of buffer is length.

                ; Clear the line by sending CR, Y spaces, then CR.
                jsr _emit_cr
_input_clear:
                tya
                beq _input_cleared
                jsr xt_space
                dey
                bra _input_clear
_input_cleared:
                jsr _emit_cr

                ; Save the history length byte
                lda (tmp3)
                sta status+1

                ; Increment the tmp3 pointer so we can use ,y addressing
                ; on both tmp1 (the input buffer) and tmp3 (the history
                ; buffer)
                inc tmp3
                bne +           ; Increment the upper byte on carry.
                inc tmp3+1
*

                ; Copy the history buffer into the input buffer,
                ; sending the characters to the output as we go.
                jsr _emit_cr
_history_loop:
                ; See if we have reached the end of the history buffer.
                cpy status+1
                bne +
                jmp _loop       ; Needs a long jump
*
                ; See if we have reached the end of the input buffer.
                ; (only comparing to lower byte as we currently limit
                ; to 255 characters max)
                cpy tmp2
                beq _hist_filled_buffer

                ; Copy a character and echo.
                lda (tmp3),y
                sta (tmp1),y
                jsr emit_a

                ; Move to the next character.
                iny

                bra _history_loop

_hist_filled_buffer:
                ; We don't want a history recall to EOL our buffer,
                ; so back up one character and return to editing.
                dey
                jmp _loop
_done:
                ; Copy the input buffer into the currently
                ; selected history buffer.
                jsr _total_recall
                sta status+1

                ; Also save it in the first buffer byte.
                sta (tmp3)

                ; Move path the count to the data bytes
                inc tmp3
                bne +           ; Increment the upper byte on carry.
                inc tmp3+1
*
                ; Copy the characters from the input buffer to the
                ; history buffer.
                ldy status+1
                bra _save_history_2
_save_history_loop:
                dey
                lda (tmp1),y
                sta (tmp3),y
 _save_history_2:
                tya
                bne _save_history_loop

z_accept:       rts


_total_recall: ; """Internal subroutine for ACCEPT tath recalls history entry"""

                ; Generate the address of the buffer in tmp3.
                ; This is a bit annoying as some bits go into each byte.
                ; .....xxx gets put into address like ......xx x.......
                lda status
                ror
                and #3
                clc
                adc #>hist_buff
                sta tmp3+1

                lda status
                ror             ; Rotate through carry into msb.
                ror
                and #$80
                clc
                adc #<hist_buff
                sta tmp3
                bcc +           ; Increment the upper byte on carry.
                inc tmp3+1
*
                ; Save the current length of the input buffer in
                ; histinfo+! temporarily.  Reduce to 127 if larger.
                tya
                cmp #$80
                bcc +
                lda #$7f
*
                rts


_emit_cr:       lda #AscCR
                jmp emit_a

.scend


; ## ACTION_OF ( "name" -- xt ) "Get named deferred word's xt"
; ## "action-of"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/ACTION-OF"""
.scope
xt_action_of:
                ; This is a state aware word with differet behavior
                ; when used while compiling vs interpreting.
                ; Check STATE
                lda state
                ora state+1
                beq _interpreting
_compiling:
                ; Run ['] to compile the xt of the next word
                ; as a literal.
                jsr xt_bracket_tick

                ; Postpone DEFER@ by compiling a JSR to it.
                ldy #>xt_defer_fetch
                lda #<xt_defer_fetch
                jmp cmpl_subroutine

_interpreting:
                jsr xt_tick
                jmp xt_defer_fetch
z_action_of:
.scend


; ## AGAIN ( addr -- ) "Code backwards branch to address left by BEGIN"
; ## "again"  tested  ANS core ext
        ; """https://forth-standard.org/standard/core/AGAIN"""
.scope
xt_again:
                jsr underflow_1

                ; Compile a JMP. We use JMP instead of BRA
                ; so we have the range and don't have to calculate the
                ; offset.
                jsr PluYA
                jmp cmpl_jump
z_again:
.scend


; ## ALIGN ( -- ) "Make sure CP is aligned on word size"
; ## "align"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ALIGN
        ; On a 8-bit machine, this does nothing. ALIGNED uses
        ; this routine as well, and also does nothing
        ; """
; ## ALIGNED ( addr -- addr ) "Return the first aligned address"
; ## "aligned"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ALIGNED"""
.scope
xt_align:
xt_aligned:
z_align:
z_aligned:      rts
.scend


; ## ALLOT ( n -- ) "Reserve or release memory"
; ## "allot"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ALLOT
        ; Reserve a certain number of bytes (not cells) or release them.
        ; If n = 0, do nothing. If n is negative, release n bytes, but only
        ; to the beginning of the Dictionary. If n is positive (the most
        ; common case), reserve n bytes, but not past the end of the
        ; Dictionary. See http://forth-standard.org/standard/core/ALLOT
        ; """
.scope
xt_allot:
                jsr underflow_1
allot_nouf:
                clc             ; move cp
                lda cp
                adc 0,x
                sta cp
                lda cp+1
                adc 1,x
                sta cp+1

                ; Releasing memory is going to be a very rare operation,
                ; so we check for it at the beginning and try to make
                ; the most common case as fast as possible
                ldy 1,x
                bmi _release

                ; Common case: We are reserving memory, not releasing it

                ; Wait, did we just grant more space than we have?  This is
                ; a check we only do here, not for other situations like cmpl_a
                ; where smaller amounts are reserved.
                ldy #<cp_end
                bcs _alloc_err          ; if moving cp wrapped, it's bad
                cpy cp
                lda #>cp_end
                sbc cp+1
                bcs _done               ; we're fine.

_alloc_err:     ; Oops, that was too much, we're beyond the end of
                ; legal Dictionary RAM. Reduce to max memory and report
                ; an error
                sty cp                  ; still #<cp_end
                lda #>cp_end
                sta cp+1

                lda #err_allot
                jmp error

_release:
        ; The ANS standard doesn't really say what to do if too much
                ; memory is freed ("negatively alloted"). In fact, there isn't
                ; even an official test. Gforth is little help either. The good
                ; news is, this is going to be a rare case. We want to use as
                ; few bytes as possible.

                ; What we do is let the user free anything up to the beginning
                ; of the RAM area assigned to the Dicionary (CP0), but at
                ; their own risk. This means that the Dictionary pointer DP
                ; might end up pointing to garbage. However, an attempt to
                ; free more than RAM than CP0 will lead to CP being set to CP0,
                ; the DP pointing to the last word in RAM (should be DROP) and
                ; an error message. We arrive here with ( n ) which is
                ; negative.

                bcc _nega_err       ; if moving cp wrapped, we're in trouble
                lda cp
                cmp #<cp0
                lda cp+1
                sbc #>cp0
                bcs _done           ; if cp>=cp0, we're OK

_nega_err:      ; Yep, we're in trouble. Set CP to CP0, set DP to the first
                ; word in ROM (should be DROP), and abort with an error
                lda #<cp0
                sta cp
                lda #>cp0
                sta cp+1

                lda #<dictionary_start
                sta dp
                lda #>dictionary_start
                sta dp+1

                lda #err_negallot
                jmp error

_done:
                inx            ; DROP n
                inx
z_allot:
                rts
.scend


; ## ALLOW_NATIVE ( -- ) "Flag last word to allow native compiling"
; ## "allow-native"  auto  Tali Forth
xt_allow_native:
                jsr current_to_dp
                ldy #nt_status
                lda (dp),y
                and #$ff-NN-AN  ; AN and NN flag is clear.
                sta (dp),y
z_allow_native:
                rts


; ## ALSO ( -- ) "Make room in the search order for another wordlist"
; ## "also"  auto  ANS search ext
        ; """http://forth-standard.org/standard/search/ALSO"""
xt_also:
                jsr xt_get_order
                jsr xt_over
                jsr xt_swap
                jsr xt_one_plus
                jmp xt_set_order
z_also:


; ## ALWAYS_NATIVE ( -- ) "Flag last word as always natively compiled"
; ## "always-native"  auto  Tali Forth
xt_always_native:
                jsr current_to_dp
                ldy #nt_status
                lda (dp),y
                ora #AN         ; Make sure AN flag is set
                and #$ff-NN     ; and NN flag is clear.
                sta (dp),y
z_always_native:
                rts


; ## AND ( n n -- n ) "Logically AND TOS and NOS"
; ## "and"  auto  ANS core
        ; """https://forth-standard.org/standard/core/AND"""
xt_and:
                jsr underflow_2

                lda 0,x
                and 2,x
                sta 2,x

                lda 1,x
                and 3,x
                sta 3,x

                inx
                inx

z_and:          rts


; ## ASSEMBLER_WORDLIST ( -- u ) "WID for the Assembler wordlist"
; ## "assembler-wordlist"  tested  Tali Assembler
        ; """ Commonly used like 'assembler-wordlist >order' to add the
        ; assembler words to the search order so they can be used.
        ; See the tutorial o Wordlists and the Search Order for
        ; more information.
        ; """
xt_assembler_wordlist:
                lda #2             ; The WID for the Assembler is 2.
                jmp PsuZA
z_assembler_wordlist:



; ## AT_XY ( n m -- ) "Move cursor to position given"
; ## "at-xy"  tested  ANS facility
        ; """https://forth-standard.org/standard/facility/AT-XY
        ; On an ANSI compatible terminal, place cursor at row n colum m.
        ; ANSI code is ESC[<n>;<m>H

        ; Do not use U. to print the numbers because the
        ; trailing space will not work with xterm
        ; """
xt_at_xy:
                jsr underflow_2
at_xy_nouf:
                lda #AscESC
                jsr emit_a
                lda #$5B        ; ASCII for "["
                jsr emit_a
                jsr print_u
                lda #$3B        ; ASCII for ";"
                jsr emit_a
                jsr print_u
                lda #'H
                jmp emit_a
z_at_xy:


; ## BACKSLASH ( -- ) "Ignore rest of line"
; ## "\"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/bs"""
xt_backslash:
                lda ciblen
                sta toin
                lda ciblen+1
                sta toin+1

z_backslash:    rts


; ## BASE ( -- addr ) "Push address of radix base to stack"
; ## "base"  auto  ANS core
        ; """https://forth-standard.org/standard/core/BASE"""
xt_base:
                lda #base
                jmp PsuZA
z_base:


; ## BEGIN ( -- addr ) "Mark entry point for loop"
; ## "begin"  auto  ANS core
        ; """https://forth-standard.org/standard/core/BEGIN
        ;
        ; This is just an immediate version of here which could just
        ; as well be coded in Forth as
        ;       : BEGIN HERE ; IMMEDIATE COMPILE-ONLY
        ; Since this is a compiling word, we don't care that much about
        ; about speed
        ; """
.scope
xt_begin:
                jmp xt_here
z_begin:
.scend


; ## BELL ( -- ) "Emit ASCII BELL"
; ## "bell"  tested  Tali Forth
xt_bell:
                lda #7          ; ASCII value for BELl
                jmp emit_a
z_bell:


; ## BL ( -- c ) "Push ASCII value of SPACE to stack"
; ## "bl"  auto  ANS core
        ; """https://forth-standard.org/standard/core/BL"""
.scope
xt_bl:
                lda #AscSP
                jmp PsuZA
z_bl:
.scend

; ## BLK ( -- addr ) "Push address of block being interpreted"
; ## "block"  auto  ANS block
        ; """https://forth-standard.org/standard/block/BLK"""
xt_blk:
                lda #blk_offset ; BLK is at UP + blk_offset

Psu_up_plus_A:
                dex
                dex
                clc
                adc up
                sta 0,x
                lda up+1
                adc #0          ; Adding carry
                sta 1,x

z_blk:          rts


; ## BLKBUFFER ( -- addr ) "Push address of block buffer"
; ## "blkbuffer"  auto  Tali block
xt_blkbuffer:
                ldy #blkbuffer_offset  ; blkbuffer address is at UP + blkbuffer_offset.

Psu_up_Y:       ; Unlike some of the other user variables, we actually
                ; want to push the address stored here, which will
                ; point to somewhere outside of the user variables.
                dex
                dex
                ; Put the address on the stack.
                lda (up),y
                sta 0,x
                iny             ; Move along to the next byte
                lda (up),y
                sta 1,x

z_blkbuffer:    rts


; ## BLOCK ( u -- a-addr ) "Fetch a block into a buffer"
; ## "block"  auto  ANS block
        ; """https://forth-standard.org/standard/block/BLOCK"""
.scope
xt_block:
                ; See if the block requested is the same as the one
                ; we currently have in the buffer.
                ; Check the LSB.

                ldy #buffblocknum_offset
                lda (up),y
                cmp 0,x
                bne _not_in_buffer
                ; Check the MSB.
                iny
                lda (up),y
                cmp 1,x
                bne _not_in_buffer

                ; The block is in the buffer. See if the buffer is in use.
                ldy #buffstatus_offset
                lda (up),y
                and #1          ; Check the in-use flag (bit 0)
                bne _done       ; It's already in the buffer and in use.
                                ; _done will replace the block# with the
                                ; buffer address.

_not_in_buffer:

                ; Check the buffer status
                ldy #buffstatus_offset
                lda (up),y      ; Only bits 0 and 1 are used, so only
                cmp #3          ; LSB is needed.
                bne _buffer_available ; Unused or not dirty = available

                ; We need to save the block.
                jsr xt_blkbuffer
                jsr xt_buffblocknum
                jsr xt_fetch
                jsr xt_block_write

_buffer_available:
                ; Save the block number.
                ldy #buffblocknum_offset
                lda 0,x
                sta (up),y
                iny
                lda 1,x
                sta (up),y

                ; Get the requested block.
                jsr xt_blkbuffer
                jsr xt_swap
                jsr xt_block_read

                ; Mark the buffer as clean and in-use.
                lda #1
                ldy #buffstatus_offset
                sta (up),y

                ; Make room on the stack for the return address.
                dex
                dex

_done:
                ; It's in the buffer.  Return the buffer address.
                ldy #blkbuffer_offset
                lda (up),y
                sta 0,x
                iny
                lda (up),y
                sta 1,x


z_block:        rts
.scend


; RAMDRIVE ( -- addr ) "Push ptr to ramdrive buffer"
xt_ramdrive:
                ldy #uv_ramdrive
                bra Psu_up_Y
z_ramdrive:


; ## BLOCK_RAMDRIVE_INIT ( u -- ) "Create a ramdrive for blocks"
; ## "block-ramdrive-init"  auto  Tali block
        ; """Create a RAM drive, with the given number of
        ; blocks, in the dictionary along with setting up the block words to
        ; use it.  The read/write routines do not provide bounds checking.
        ; Expected use: '4 block-ramdrive-init' ( to create blocks 0-3 )
        ; """
.scope
xt_block_ramdrive_init:
                jsr underflow_1

                lda cp                  ; remember start of ramdrive buffer
                ldy #uv_ramdrive
                sta (up),y
                lda cp+1
                iny
                sta (up),y

                lda 0,x                 ; Calculate how many bytes are needed for numblocks blocks
                asl
                asl
                sta 1,x
                stz 0,x
                jsr xt_allot            ; Create ramdrive

                ldy #blockread_offset   ; replace read vector
                lda #<_block_read_ramdrive
                sta (up),y
                iny
                lda #>_block_read_ramdrive
                sta (up),y

                ldy #blockwrite_offset  ; replace write vector
                lda #<_block_write_ramdrive
                sta (up),y
                iny
                lda #>_block_write_ramdrive
                sta (up),y

z_block_ramdrive_init: rts

        ; These routines just copy between the buffer and the ramdrive blocks
_block_read_ramdrive:           ; ( addr u -- )
                jsr _block_u
                                ; ( addr u*1024+ramdrive )
                jsr xt_swap
                                ; ( u*1024+ramdrive addr )
_block_mov:     lda #<1024
                ldy #>1024
                jsr PsuYA
                jmp xt_move

_block_write_ramdrive: ; ( addr u -- )
                jsr _block_u
                                        ; ( addr u*1024+ramdrive )
                bra _block_mov

_block_u:       lda 0,x         ; 1024 *
                asl
                asl
                ldy #uv_ramdrive+1
                adc (up),y              ; ramdrive @ +
                sta 1,x
                dey
                lda (up),y
                sta 0,x
                rts

.scend


; ## BLOCK_READ ( addr u -- ) "Read a block from storage (deferred word)"
; ## "block-read"  auto  Tali block
        ; """BLOCK-READ is a vectored word that the user needs to override
        ; with their own version to read a block from storage.
        ; The stack parameters are ( buffer_address block# -- ).
        ; """
xt_block_read:
                ; Execute the BLOCK-READ-VECTOR
                ldy #blockread_offset

Jmp_ind_up_plus_Y:
                lda (up),y
                sta tmp1
                iny
                lda (up),y
                sta tmp1+1

                jmp (tmp1)

z_block_read:   ; No RTS needed


; ## BLOCK_READ_VECTOR ( -- addr ) "Address of the block-read vector"
; ## "block-read-vector"  auto  Tali block
        ; """BLOCK-READ is a vectored word that the user needs to override
        ; with their own version to read a block from storage.
        ; This word gives the address of the vector so it can be replaced.
        ; """
xt_block_read_vector:
                lda #blockread_offset  ; Get the BLOCK-READ-VECTOR address
                jmp Psu_up_plus_A
z_block_read_vector:


; This is the default error message the vectored words BLOCK-READ and
; BLOCK-WRITE start with.  This word is not included in the dictionary.
xt_block_word_error:
                lda #err_blockwords
                jmp error
z_block_word_error:

; ## BLOCK_WRITE ( addr u -- ) "Write a block to storage (deferred word)"
; ## "block-write"  auto  Tali block
        ; """BLOCK-WRITE is a vectored word that the user needs to override
        ; with their own version to write a block to storage.
        ; The stack parameters are ( buffer_address block# -- ).
        ; """
xt_block_write:
                ; Execute the BLOCK-READ-VECTOR
                ldy #blockwrite_offset
                bra Jmp_ind_up_plus_Y

z_block_write:  ; No RTS needed


; ## BLOCK_WRITE_VECTOR ( -- addr ) "Address of the block-write vector"
; ## "block-write-vector"  auto  Tali block
        ; """BLOCK-WRITE is a vectored word that the user needs to override
        ; with their own version to write a block to storage.
        ; This word gives the address of the vector so it can be replaced.
        ; """
xt_block_write_vector:
                lda #blockwrite_offset  ; Get the BLOCK-WRITE-VECTOR address
                jmp Psu_up_plus_A
z_block_write_vector:


; ## BOUNDS ( addr u -- addr+u addr ) "Prepare address for looping"
; ## "bounds"  auto  Gforth
        ; """http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Memory-Blocks.html
        ; Given a string, return the correct Data Stack parameters for
        ; a DO/LOOP loop; over its characters. This is realized as
        ; OVER + SWAP in Forth, but we do it a lot faster in assembler
        ; """
xt_bounds:
                jsr underflow_2

                clc
                lda 0,x                 ; LSB u
                ldy 2,x                 ; LSB addr
                adc 2,x
                sta 2,x                 ; LSB addr+u
                sty 0,x

                lda 1,x                 ; MSB u
                ldy 3,x                 ; MSB addr
                adc 3,x
                sta 3,x                 ; MSB addr+u
                sty 1,x

z_bounds:       rts


; ## BRACKET_CHAR ( "c" -- ) "Compile character"
; ## "[char]"  auto  ANS core
        ; """https://forth-standard.org/standard/core/BracketCHAR
        ; Compile the ASCII value of a character as a literal. This is an
        ; immediate, compile-only word.
        ;
        ; A definition given in
        ; http://forth-standard.org/standard/implement is
        ; : [CHAR]  CHAR POSTPONE LITERAL ; IMMEDIATE
        ; """
xt_bracket_char:
                jsr xt_char
                jmp xt_literal
z_bracket_char:


; ## BRACKET_TICK ( -- ) "Store xt of following word during compilation"
; ## "[']"  auto  ANS core
        ; """https://forth-standard.org/standard/core/BracketTick"""
xt_bracket_tick:
                jsr xt_tick
                jmp xt_literal
z_bracket_tick:



; ## BUFFBLOCKNUM ( -- addr ) "Push address of variable holding block in buffer"
; ## "buffblocknum"  auto  Tali block
xt_buffblocknum:
                lda #buffblocknum_offset  ; BUFFBLOCKNUM is at UP + buffblocknum_offset
                jmp Psu_up_plus_A
z_buffblocknum:


; ## BUFFER ( u -- a-addr ) "Get a buffer for a block"
; ## "buffer"  auto  ANS block
        ; """https://forth-standard.org/standard/block/BUFFER"""
.scope
xt_buffer:
                ; Check the buffer status
                ldy #buffstatus_offset
                lda (up),y      ; Only bits 0 and 1 are used, so only
                cmp #3          ; LSB is needed.
                bne _buffer_available ; Unused or not dirty = available

                ; We need to save the block.
                jsr xt_blkbuffer
                jsr xt_buffblocknum
                jsr xt_fetch
                jsr xt_block_write

_buffer_available:
                ; Save the block number.
                ldy #buffblocknum_offset
                lda 0,x
                sta (up),y
                iny
                lda 1,x
                sta (up),y

                ; Mark the buffer as clean and in-use.
                lda #1
                ldy #buffstatus_offset
                sta (up),y

_done:
                ; Return the buffer address.
                ldy #blkbuffer_offset

                lda (up),y
                sta 0,x
                iny
                lda (up),y
                sta 1,x

z_buffer:       rts
.scend



; ## BUFFER_COLON ( u "<name>" -- ; -- addr ) "Create an uninitialized buffer"
; ## "buffer:"  auto  ANS core ext
                ; """https://forth-standard.org/standard/core/BUFFERColon
                ; Create a buffer of size u that puts its address on the stack
                ; when its name is used.
                ; """
xt_buffer_colon:
                jsr xt_create
                jmp xt_allot
z_buffer_colon:


; ## BUFFSTATUS ( -- addr ) "Push address of variable holding buffer status"
; ## "buffstatus"  auto  Tali block
xt_buffstatus:
                lda #buffstatus_offset  ; BUFFSTATUS is at UP + buffstatus_offset
                jmp Psu_up_plus_A
z_buffstatus:


; ## BYE ( -- ) "Break"
; ## "bye"  tested  ANS tools ext
        ; """https://forth-standard.org/standard/tools/BYE"""
.scope
xt_bye:
                brk
z_bye:          rts             ; never reached
.scend


; ## C_COMMA ( c -- ) "Store one byte/char in the Dictionary"
; ## "c,"  auto  ANS core
; TODO make sure we haven't allocated more than we have
        ; """https://forth-standard.org/standard/core/CComma"""
.scope
xt_c_comma:
                jsr underflow_1

                lda 0,x
                inx
                inx
                jsr cmpl_a

z_c_comma:      rts
.scend


; ## C_FETCH ( addr -- c ) "Get a character/byte from given address"
; ## "c@"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CFetch"""
xt_c_fetch:
                jsr underflow_1

                lda (0,x)
                sta 0,x
                stz 1,x

z_c_fetch:      rts


; ## C_STORE ( c addr -- ) "Store character at address given"
; ## "c!"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CStore
        ; """
xt_c_store:
                jsr underflow_2

                lda 2,x
                sta (0,x)

                inx
                inx
                inx
                inx

z_c_store:      rts


; ## CASE (C: -- 0) ( -- ) "Conditional flow control"
; ## "case"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/CASE
        ;
        ; This is a dummy header, CASE shares the actual code with ZERO.
        ; """


; ## CELL_PLUS ( u -- u ) "Add cell size in bytes"
; ## "cell+"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CELLPlus
        ; Add the number of bytes ("address units") that one cell needs.
        ; Since this is an 8 bit machine with 16 bit cells, we add two bytes.
        ; """
.scope
xt_cell_plus:
                jsr underflow_1

                clc
                lda 0,x
                adc #2
                sta 0,x
                bcc +
                inc 1,x
*
z_cell_plus:    rts
.scend


; ## CELLS ( u -- u ) "Convert cells to size in bytes"
; ## "cells"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CELLS
        ;
        ; Dummy entry for the CELLS word, the code is the same as for
        ; 2*, which is where the header directs us to
        ; """


; ## CHAR ( "c" -- u ) "Convert character to ASCII value"
; ## "char"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CHAR"""
.scope
xt_char:
                ; get character from string, returns ( addr u )
                jsr xt_parse_name

                ; if we got back a zero, we have a problem
                lda 0,x
                ora 1,x
                bne _not_empty

                lda #err_noname
                jmp error

_not_empty:
                inx             ; drop number of characters, leave addr
                inx

                lda (0,x)       ; get character (equivalent to C@)

                sta 0,x
                stz 1,x         ; MSB is always zero

z_char:         rts
.scend


; ## CHAR_PLUS ( addr -- addr+1 ) "Add the size of a character unit to address"
; ## "char+"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CHARPlus
        ;
        ; This is a dummy entry, the code is shared with ONE_PLUS
        ; """


; ## CHARS ( n -- n ) "Number of bytes that n chars need"
; ## "chars"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CHARS
        ; Return how many address units n chars are. Since this is an 8 bit
        ; machine, this does absolutely nothing and is included for
        ; compatibility with other Forth versions
        ; """
.scope
xt_chars:
                ; Checking for underflow seems a bit stupid because this
                ; routine does nothing on this machine. However, the user
                ; should be warned that there is something wrong with the
                ; code if this occurs.
                jsr underflow_1

z_chars:        rts
.scend

; ## CLEAVE ( addr u -- addr2 u2 addr1 u1 ) "Split off word from string"
; ## "cleave"  auto  Tali Forth

        ; """Given a range of memory with words delimited by whitespace,return
        ; the first word at the top of the stack and the rest of the word
        ; following it.
        ;
        ; Example:
        ; s" w1 w2 w3" cleave  -> "w2 w3" "w1"
        ; s" w1" cleave        -> "" "w1"
        ;
        ; Since it will be used in loops a lot, we want it to work in pure
        ; assembler and be as fast as we can make it. Calls PARSE-NAME so we
        ; strip leading delimiters.
        ; """
.scope
xt_cleave:
                jsr underflow_2

                ; We arrive here with ( addr u ). We need to strip any leading
                ; spaces by hand: PARSE-NAME does do that, but it doesn't
                ; remember how many spaces were stripped. This means we can't
                ; calculate the length of the remainder. Fortunately, Tali
                ; Forth has just the word we need for this:
                jsr minus_leading_nouf  ; ( addr u )

                ; The main part we can turn over to PARSE-NAME, except that we
                ; have a string ( addr u ) and not stuff in the input buffer.
                ; We get around this by cheating: We place ( addr u ) in the
                ; input buffer and then call PARSE-NAME.
                jsr xt_input_to_r       ; save old imput state

                lda 0,x         ; u is new ciblen
                sta ciblen
                lda 1,x
                sta ciblen+1

                lda 2,x         ; addr is new cib
                sta cib
                lda 3,x
                sta cib+1

                stz toin        ; >IN pointer is zero
                stz toin+1

                ; PARSE-NAME gives us back the substring of the first word
                jsr xt_parse_name       ; ( addr u addr-s u-s )

                ; If we were given an empty string, then we're done. It's the
                ; resposibility of the user to catch this as a sign to end the
                ; any loop
                lda 0,x
                ora 1,x
                beq _done

                ; Now we have to adjust the original string
                lda 4,x         ; LSB of original u
                sec
                sbc 0,x
                sta 4,x

                lda 5,x         ; MSB of original u
                sbc 1,x
                sta 5,x

                lda 6,x         ; LSB of original addr
                clc
                adc 0,x
                sta 6,x

                lda 7,x         ; MSB of original addr
                adc 1,x
                sta 7,x

                ; There is one small problem: PARSE-NAME will probably have
                ; left the string with the rest of the words with leading
                ; delimiters. We use our magic -LEADING again
                jsr two_swap_nouf       ; ( addr-s u-s addr u )
                jsr minus_leading_nouf
                jsr two_swap_nouf       ; ( addr u addr-s u-s )
_done:
                ; Restore input
                jsr xt_r_to_input

z_cleave:       rts
.scend


; ## CMOVE ( addr1 addr2 u -- ) "Copy bytes going from low to high"
; ## "cmove"  auto  ANS string
        ; """https://forth-standard.org/standard/string/CMOVE
        ; Copy u bytes from addr1 to addr2, going low to high (addr2 is
        ; larger than addr1). Based on code in Leventhal, Lance A.
        ; "6502 Assembly Language Routines", p. 201, where it is called
        ; "move left".
        ;
        ; There are no official tests for this word.
        ; """
.scope
xt_cmove:
                jsr underflow_3

                ; move destination address to where we can work with it
                lda 2,x
                sta tmp2        ; use tmp2 because easier to remember
                lda 3,x
                sta tmp2+1

                ; move source address to where we can work with it
                lda 4,x
                sta tmp1        ; use tmp1 because easier to remember
                lda 5,x
                sta tmp1+1

                ldy #0
                lda 1,x         ; number of whole pages to move
                beq _dopartial
_page:
                lda (tmp1),y
                sta (tmp2),y
                iny
                bne _page

                inc tmp1+1
                inc tmp2+1
                dec 1,x
                bne _page
_dopartial:
                lda 0,x         ; length of last page
                beq _done
_partial:
                lda (tmp1),y
                sta (tmp2),y
                iny

                dec 0,x
                bne _partial

_done:          ; clear the stack
                txa
                clc
                adc #6
                tax

z_cmove:        rts
.scend


; ## CMOVE_UP ( add1 add2 u -- ) "Copy bytes from high to low"
; ## "cmove>"  auto  ANS string
        ; """https://forth-standard.org/standard/string/CMOVEtop
        ; Based on code in Leventhal, Lance A. "6502 Assembly Language
        ; Routines", p. 201, where it is called "move right".
        ;
        ; There are no official tests for this word.
        ; """
.scope
xt_cmove_up:
                jsr underflow_3

                ; move destination address to where we can work with it
                lda 2,x
                sta tmp2        ; use tmp2 because easier to remember
                lda 3,x
                clc
                adc 1,x
                sta tmp2+1      ; point to last page of destination

                ; move source address to where we can work with it
                lda 4,x
                sta tmp1        ; use tmp1 because easier to remember
                lda 5,x
                clc
                adc 1,x
                sta tmp1+1      ; point to last page of source
                inc 1,x         ; allows us to use bne with dec 1,x below

                ; Move the last partial page first
                ldy 0,x         ; length of last page
                beq _nopartial
_outerloop:
                dey
                beq _finishpage
_innerloop:
                lda (tmp1),y
                sta (tmp2),y
                dey
                bne _innerloop
_finishpage:
                lda (tmp1)      ; handle y = 0 separately
                sta (tmp2)
_nopartial:
                dec tmp1+1      ; back up to previous pages
                dec tmp2+1
                dec 1,x
                bne _outerloop
_done:
                ; clear up the stack and leave
                txa
                clc
                adc #6
                tax

z_cmove_up:     rts
.scend


; ## COLON ( "name" -- ) "Start compilation of a new word"
; ## ":"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Colon
        ;
        ; Use the CREATE routine and fill in the rest by hand.
        ; """
.scope
xt_colon:
                ; If we're already in the compile state, complain
                ; and quit
                lda state
                ora state+1
                beq +

                lda #err_state
                jmp error
*
                ; switch to compile state
                dec state
                dec state+1
                ; Set bit 6 in status to tell ";" and RECURSE this is a normal
                ; word.
                lda #%01000000
                tsb status

                ; CREATE is going to change the DP to point to the new word's
                ; header. While this is fine for (say) variables, it would mean
                ; that FIND-NAME etc would find a half-finished word when
                ; looking in the Dictionary. To prevent this, we save the old
                ; version of DP and restore it later. The new DP is placed in
                ; the variable WORKWORD until we're finished with a SEMICOLON.
                jsr current_to_dp
                lda dp+1            ; CREATE uses a lot of variables
                pha
                lda dp
                pha

                ; Tell create not to print warning for duplicate name.
                lda #%10000000
                tsb status

                lda #NN
                jsr cmpl_WordHeaderA

                ; Get the nt (not the xt!) of the new word as described above.
                ; Only COLON, SEMICOLON and RECURSE get to access WORKWORD
                jsr current_to_dp   ; This might be able to be omitted
                lda dp
                sta workword
                lda dp+1
                sta workword+1

                ; Restore original DP
                pla
                sta dp
                pla
                sta dp+1
                jsr dp_to_current

_done:
z_colon:        rts
.scend

; ## COLON_NONAME ( -- ) "Start compilation of a new word""
; ## ":NONAME"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ColonNONAME
        ; Compile a word with no nt.  ";" will put its xt on the stack.
        ; """
.scope
xt_colon_noname:
                ; If we're already in the compile state, complain
                ; and quit
                lda state
                ora state+1
                beq +

                lda #err_state
                jmp error
*
                ; switch to compile state
                dec state
                dec state+1
                ; Clear bit 6 in status to tell ";" and RECURSE this is
                ; a :NONAME word.
                lda #%01000000
                trb status

                ; Put cp (the xt for this word) in WORKWORD.
                ; The flag above lets both ";" and RECURSE know that is is an
                ; xt instead of an nt and they will modify their behavior.
                lda cp
                sta workword
                lda cp+1
                sta workword+1
_done:
z_colon_noname: rts
.scend


; ## COMMA ( n -- ) "Allot and store one cell in memory"
; ## ","  auto  ANS core
        ; """https://forth-standard.org/standard/core/Comma
        ; Store TOS at current place in memory.
        ;
        ; Since this an eight-bit machine, we can ignore all alignment issues.
        ; """
.scope
xt_comma:
                jsr underflow_1

                lda 0,x
                ldy 1,x
                inx
                inx
                jmp cmpl_word
z_comma:
.scend


; ## COMPARE ( addr1 u1 addr2 u2 -- -1 | 0 | 1) "Compare two strings"
; ## "compare"   auto  ANS string
        ; """https://forth-standard.org/standard/string/COMPARE
        ; Compare string1 (denoted by addr1 u1) to string2 (denoted by
        ; addr2 u2).  Return -1 if string1 < string2, 0 if string1 = string2
        ; and 1 if string1 > string2 (ASCIIbetical comparison).  A string
        ; that entirely matches the beginning of the other string, but is
        ; shorter, is considered less than the longer string.
        ; """
.scope
xt_compare:
                jsr underflow_4

                ; Load the two string addresses into tmp1 and tmp2.
                lda 2,x
                sta tmp2
                lda 3,x
                sta tmp2+1
                lda 6,x
                sta tmp1
                lda 7,x
                sta tmp1+1

                lda 0,x        ; copy string2 length LSB
                sta 0+tmp3
                lda 4,x        ; copy string1 length LSB
                sta 1+tmp3

                ldy #0
_a:
                cpy 1+tmp3    ; Check string1 length LSB
                beq _1_len
_1_ok:

                cpy 0+tmp3    ; Check string2 length LSB
                beq _2_len
_2_ok:

                ; Both strings have at least one letter left.
                ; Check the letters against each other.
                lda (tmp1),y
                cmp (tmp2),y
                bne _ne
                iny
                bne _a
                inc 1+tmp1
                inc 1+tmp2
                bra _a

_2_len:         dec 1,x        ; check string2 length MSB (limits to 32k)
                bpl _2_ok
                bra _greater    ; Str2 empty first

_1_len:         dec 5,x        ; check string1 length LSB (limits to 32k)
                bpl _1_ok
                ; String 1 is out of letters. Check string 2.
                cpy 0+tmp3
                bne _less
                lda 1,x
                beq _equal      ; Both out of letters
                ; Falls into less (str1 is out but str2 has more)
_less:
                ; Return -1
                lda #$FF
                sta 6,x
                sta 7,x
                bra _done
_equal:
                ; Return 0
                stz 6,x
                stz 7,x
                bra _done

_ne:            bcc _less

_greater:
                ; Return 1
                lda #1
                sta 6,x
                stz 7,x
                ; Falls into _done
_done:
                ; Remove all but the result from the stack.
                txa
                clc
                adc #6
                tax

z_compare:      rts
.scend


; ## COMPILE_COMMA ( xt -- ) "Compile xt"
; ## "compile,"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/COMPILEComma
        ; Compile the given xt in the current word definition. It is an
        ; error if we are not in the compile state. Because we are using
        ; subroutine threading, we can't use , (COMMA) to compile new words
        ; the traditional way. By default, native compiled is allowed, unless
        ; there is a NN (Never Native) flag associated. If not, we use the
        ; value NC_LIMIT (from definitions.asm) to decide if the code
        ; is too large to be natively coded: If the size is larger than
        ; NC_LIMIT, we silently use subroutine coding. If the AN (Always
        ; Native) flag is set, the word is always natively compiled.
        ; """
.scope
xt_compile_comma:
                jsr underflow_1

                ; See if this is an Always Native (AN) word by checking the
                ; AN flag. We need nt for this. First, save a copy of xt to
                ; the Return Stack
                lda 1,x                 ; MSB
                pha
                lda 0,x
                pha                     ; LSB

                jsr xt_int_to_name      ; ( xt -- nt )

                ; put nt away for safe keeping
                lda 0,x
                sta tmptos
                lda 1,x
                sta tmptos+1
                beq _jsr_normal         ; No nt found.  Just compile as a JSR.

                jsr xt_wordsize         ; ( nt -- u )

                ; adjust wordsize for native compile
                lda uf_strip            ; strip underflow check?
                beq _uf_end
                ldy #nt_status
                lda (tmptos),y
                and #UF
                beq _uf_end
                lda #3
_uf_end:        sta tmp1

                ldy #nt_status          ; strip return stack pop & push?
                lda (tmptos),y
                and #R6
                beq _r6_end
                lda #6
_r6_end:        sta tmp1+1
                clc
                adc tmp1
                sta tmp1
                adc tmp1+1

                eor #$ff                ;adjust length
                sec
                adc 0,x
                sta 0,x
                bcs +
                dec 1,x
*

                ldy #nt_status
                lda (tmptos),y
                bit #AN                 ; Always Native?
                bne _compile_as_code    ; We're natively compiling no matter what.
                bit #NN                 ; Never Native?
                bne _compile_as_jsr

                ; We get to choose based on length
                lda 1,x                 ; if it's >=256 bytes, just jsr
                bne _jsr_normal
                lda nc_limit+1
                bne _compile_as_code
                lda nc_limit            ; Allow native compiling for <= limit.
                cmp 0,x
                bcs _compile_as_code

_compile_as_jsr:
                ldy #nt_status
                lda (tmptos),y
                bit #R6
                bne _jsr_normal
                bit #UF
                beq _jsr_normal
                ldy uf_strip
                beq _jsr_normal

                pla                     ; skip jsr underflow_check
                ply
                clc
                adc #3
                bcc +
                iny
*               bra _jsr_5

_jsr_normal:    ; Compile xt as a subroutine call
                pla             ; LSB
                ply             ; MSB
_jsr_5:         inx                     ; drop wordsize
                inx
                jmp cmpl_subroutine

_compile_as_code:
                ; We arrive here with the length of the word's code TOS and
                ; xt on top of the Return Stack. MOVE will need ( xt cp u )
                ; on the data stack
                                        ; ( -- u )
                jsr xt_here
                                        ; ( -- u cp )
                jsr xt_over
                                        ; ( -- u cp u )
                pla                     ; get xt
                ply
                clc                     ; add tmp1
                adc tmp1
                bcc +
                iny
*               sta 4,x
                sty 5,x
                                        ; ( -- xt cp u )

                ; Adjust CP.
                jsr dup_nouf
                jsr allot_nouf

                ; Enough of this, let's move those bytes already! We have
                ; ( xt cp u ) on the stack at this point
                jmp xt_move

z_compile_comma:
.scend


; ## COMPILE_ONLY ( -- ) "Mark most recent word as COMPILE-ONLY"
; ## "compile-only"  tested  Tali Forth
        ; """Set the Compile Only flag (CO) of the most recently defined
        ; word.
        ;
        ; The alternative way to do this is to define a word
        ; ?COMPILE that makes sure  we're in compile mode
        ; """
.scope
xt_compile_only:
                lda #CO
                jmp status_or_A
z_compile_only:
.scend


; ## CONSTANT ( n "name" -- ) "Define a constant"
; ## "constant"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CONSTANT
        ;
        ; Forth equivalent is  CREATE , DOES> @  but we do
        ; more in assembler and let CREATE do the heavy lifting.
        ; See http://www.bradrodriguez.com/papers/moving3.htm for
        ; a primer on how this works in various Forths. This is the
        ; same code as VALUE in our case.
        ; """
xt_value:
xt_constant:
                jsr underflow_1

                lda #NN                     ; compile word header
                jsr cmpl_WordHeaderA

                jsr cmpl_loadYAImmed        ; complie code to load n into YA

                lda #<PsuYA                 ; compile jmp PsuYA
                ldy #>PsuYA
                jsr cmpl_jump

                jsr adjust_z                ; update word size

z_value:
z_constant:     rts


; ## COUNT ( c-addr -- addr u ) "Convert character string to normal format"
; ## "count"  auto  ANS core
        ; """https://forth-standard.org/standard/core/COUNT
        ; Convert old-style character string to address-length pair. Note
        ; that the length of the string c-addr is stored in character length
        ; (8 bit), not cell length (16 bit). This is rarely used these days,
        ; though COUNT can also be used to step through a string character by
        ; character.
        ; """
xt_count:
                jsr underflow_1

                lda (0,x)       ; Get number of characters (255 max)

                ; move start address up by one
                inc 0,x         ; LSB
                bne +
                inc 1,x         ; MSB
*
                ; save number of characters to stack
                jmp PsuZA
z_count:


; ## CR ( -- ) "Print a line feed"
; ## "cr"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CR"""
xt_cr:
                lda #AscLF
                jmp emit_a
z_cr:


; ## CREATE ( "name" -- ) "Create Dictionary entry for 'name'"
; ## "create"  auto  ANS core
        ; """https://forth-standard.org/standard/core/CREATE
        ;
        ; See the drawing in headers.asm for details on the header
        ; """
.scope
xt_create:
                lda #<dovar
                ldy #>dovar

cmpl_createYA: ; CFA routine ptr in YA
                phy                     ; save CFA routine ptr
                pha

                ; By default, we set all new words
                ; to "never native", user will have to decide if they should
                ; be inlined
                ; Also, words defined by CREATE are marked in the header has
                ; having a Code Field Area (CFA), which is a bit tricky for
                ; Subroutine Threaded Code (STC). We do this so >BODY works
                ; correctly with DOES> and CREATE. See the discussion at
                ; http://forum.6502.org/viewtopic.php?f=9&t=5182 for details
                lda #NN+HC
                jsr cmpl_WordHeaderA

                pla                     ; build  jsr CFA
                ply
                jsr cmpl_subroutine

                jmp adjust_z
z_create:
.scend


; Build a new word header ready for code to be appended.
.scope
cmpl_WordHeaderA: ; new word status in A
                pha

                ; get string
                jsr xt_parse_name       ; ( addr u )

                ; if we were given an empty string, we complain and quit
                lda 0,x
                bne _got_name

                lda #err_noname
                jmp error

_got_name:
                ; We assume name length is < $ff-nt_name bytes.
                ; Larger values will cause errors or be ignored.

                ; Check to see if this name already exists.
                jsr xt_two_dup          ; ( addr u addr u )
                jsr xt_find_name        ; ( addr u flag ) (non-zero nt as flag)
                lda 0,x
                ora 1,x
                beq _new_name           ; We haven't seen this one before.

                ; This name already exists.  See if we are supposed to print
                ; the message for it.
                inx                     ; Drop flag (nt) from find-name.
                inx
                ; Check bit 7
                bit status
                bpl _redefined_name     ; Bit 7 is zero, so print the message.

                ; We aren't supposed to print the redefined message ourselves,
                ; but we should indicate that it is redefined (for ; to print
                ; later).
                lda #$80                ; Set bit 7 to indicate dup
                tsb status
                bra _process_name

_redefined_name:
                ; Print the message that the name is redefined.
                ldy #s_redefined-string0
                jsr print_string_no_lf_Y
                jsr xt_two_dup           ; ( addr u addr u )
                jsr xt_type
                jsr xt_space

                bra _process_name

_new_name:
                inx                     ; Drop flag (0) from find-name.
                inx
                lda #$80                ; Clear bit 0 of status to indicate new word.
                trb status
_process_name:
                lda 0,x
                sta tmp2                ; store length of string in tmp2

                ; remember the first free byte of memory as the start of
                ; the new word
                lda cp
                sta tmp1
                lda cp+1
                sta tmp1+1

                ; we need nt_name + the length of the string bytes for our new header.
                ; This is also the offset for the start of the code field (the
                ; xt_ label) so we need to remember it. Otherwise, we could
                ; just allot the space afterwards
                lda 0,x
                clc
                adc #nt_name
                sta tmp3                ; total header length

                ; We overwrite the length of the string returned by PARSE-NAME
                ; and then call ALLOT
                sta 0,x
                stz 1,x         ; max header size is 255 chars
                jsr allot_nouf  ; ( addr )

                ; Get the CURRENT dictionary pointer.
                jsr current_to_dp

                ; Now we walk through the header with Y as the index, adding
                ; information byte-by-byte

                ; HEADER BYTE 0: nt_length: Length of string
                lda tmp2
                sta (tmp1)

                ; HEADER BYTE 1: nt_status byte.
                pla
                ldy #1
                sta (tmp1),y
                iny

                ; HEADER BYTE 2,3: nt_next_nt: Next header. This is the current
                                ; last word in the Dictionary
                lda dp
                sta (tmp1),y
                iny
                lda dp+1
                sta (tmp1),y
                iny

                ; Interlude: Make old CP new DP (new start of Dictionary)
                lda tmp1+1
                sta dp+1
                lda tmp1
                sta dp

                ; HEADER BYTE 4,5: nt_xt: Start of the code field ("xt_" of this word).
                ; This begins after the header so we take the length of the
                ; header, which we cleverly saved in tmp3, and use it as an
                ; offset to the address of the start of the word. We come here
                ; with tmp1 in A
                clc
                adc tmp3        ; add total header length
                sta (tmp1),y
                iny

                lda tmp1+1
                adc #0          ; only need the carry
                sta (tmp1),y
                iny

                ; HEADER BYTE 6,7: nt_z: End of code ("z_" of this word).
                ; This can be modified later by adjust_z .
                lda cp
                sta (tmp1),y
                iny
                lda cp+1
                sta (tmp1),y
                iny

                ; HEADER BYTE 8: nt_name: Start of name string. The address is TOS, the
                ; length in tmp2. We subtract 8 from the address so we can
                ; use the same loop index, which is already 8 byte ahead at
                ; this point
                lda 0,x
                sec
                sbc #nt_name
                sta tmptos

                lda 1,x
                sbc #0          ; only need carry
                sta tmptos+1

_name_loop:
                lda (tmptos),y

;                ; Make sure it goes into the dictionary in lower case.
;                cmp #1+'Z
;                bcs _store_name
;                cmp #'A
;                bcc _store_name
;                ora #$20           ;  Make it lowercase.
;_store_name:

                sta (tmp1),y
                iny
                dec tmp2
                bne _name_loop

                ; Update the CURRENT wordlist with the new DP.
                ; We do this down here because this routine uses Y.
                jsr dp_to_current

                ; And we're done. Restore stack
                inx
                inx
                rts
.scend


adjust_z: ; Update the word-end pointer in the current word header
                jsr current_to_dp

                ldy #nt_z
                lda cp
                sta (dp),y
                iny
                lda cp+1
                sta (dp),y

                rts


; ## DABS ( d -- d ) "Return the absolute value of a double"
; ## "dabs"  auto  ANS double
        ; """https://forth-standard.org/standard/double/DABS"""
.scope
xt_dabs:
                jsr underflow_2 ; double number
dabs_nouf:
                lda 1,x         ; MSB of high cell
                bpl +           ; positive?
                jsr dnegate_nouf
*
z_dabs:         rts
.scend


; ## D_MINUS ( d d -- d ) "Subtract two double-celled numbers"
; ## "d-"  auto  ANS double
        ; """https://forth-standard.org/standard/double/DMinus"""
.scope
xt_d_minus:
                jsr underflow_4 ; two double numbers

                sec

                lda 6,x         ; LSB of lower word
                sbc 2,x
                sta 6,x

                lda 7,x         ; MSB of lower word
                sbc 3,x
                sta 7,x

                lda 4,x         ; LSB of upper word
                sbc 0,x
                sta 4,x

                lda 5,x         ; MSB of upper word
                sbc 1,x
                sta 5,x

                inx
                inx
                inx
                inx

z_d_minus:      rts
.scend


; ## D_PLUS ( d d -- d ) "Add two double-celled numbers"
; ## "d+"  auto  ANS double
        ; """https://forth-standard.org/standard/double/DPlus"""
.scope
xt_d_plus:
                jsr underflow_4 ; two double numbers

                clc
                lda 2,x         ; LSB of lower word
                adc 6,x
                sta 6,x

                lda 3,x         ; MSB of lower word
                adc 7,x
                sta 7,x

                lda 0,x         ; LSB of upper word
                adc 4,x
                sta 4,x

                lda 1,x         ; MSB of upper word
                adc 5,x
                sta 5,x

                inx
                inx
                inx
                inx

z_d_plus:       rts
.scend


; ## D_TO_S ( d -- n ) "Convert a double number to single"
; ## "d>s"  auto  ANS double
        ; """https://forth-standard.org/standard/double/DtoS
        ; Though this is basically just DROP, we keep it
        ; separate so we can test for underflow
        ; """
.scope
xt_d_to_s:
                jsr underflow_2

                inx
                inx

z_d_to_s:       rts
.scend


; ## DECIMAL ( -- ) "Change radix base to decimal"
; ## "decimal"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DECIMAL"""
xt_decimal:
                lda #10
                sta base
                stz base+1              ; paranoid

z_decimal:      rts


; ## DEFER ( "name" -- ) "Create a placeholder for words by name"
; ## "defer"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/DEFER
        ; Reserve an name that can be linked to various xt by IS.
        ;
        ; The ANSI reference implementation is
        ;       CREATE ['] ABORT , DOES> @ EXECUTE ;
        ; The 6502 knows this one as "jmp abs"
        ; so we don't need a CFA routine at all, just the 6502 instruction.
        ; DEFER@ & DEFER! know how to get & set this.
        ; We could pad this with a 2 byte do-nothing instruction (like cmp #)
        ; if we really want the part before the address to be 3 bytes long
        ; so >BODY could work.
.scope
xt_defer:
                lda #NN
                jsr cmpl_WordHeaderA

                ; As default, we include the error "Defer not defined"
                lda #<defer_error   ; LSB
                ldy #>defer_error   ; MSB
                jsr cmpl_jump

                jmp adjust_z    ; adjust header to correct length
z_defer:
.scend


; ## DEFER_FETCH ( xt1 -- xt2 ) "Get the current XT for a deferred word"
; ## "defer@"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/DEFERFetch"""
.scope
xt_defer_fetch:
                ; No underflow checking as @ does it.
                inc 0,x         ; 1+
                bne +
                inc 1,x
*
                jmp xt_fetch
z_defer_fetch:
.scend


; ## DEFER_STORE ( xt2 x1 -- ) "Set xt1 to execute xt2"
; ## "defer!"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/DEFERStore"""
.scope
xt_defer_store:
                ; No underflow checking as ! does it.
                inc 0,x         ; 1+
                bne +
                inc 1,x
*
                jmp xt_store
z_defer_store:
.scend


; ## DEFINITIONS ( -- ) "Make first wordlist in search order the current wordlist"
; ## "definitions" auto ANS search
xt_definitions:
                ldy #search_order_offset    ; Transfer byte variable
                lda (up),y                  ; SEARCH_ORDER[0] to
                ldy #current_offset         ; byte variable CURRENT.
                sta (up),y
z_definitions:  rts


; ## DEPTH ( -- u ) "Get number of cells (not bytes) used by stack"
; ## "depth"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DEPTH"""
xt_depth:
                lda #dsp0
                stx tmpdsp
                sec
                sbc tmpdsp

                ; divide by two because each cell is two bytes
                lsr

                dex             ; PsuZA
                dex
                sta 0,x
                stz 1,x

z_depth:        rts


; ## DIGIT_QUESTION ( char -- u f | char f ) "Convert ASCII char to number"
; ## "digit?"  auto  Tali Forth
        ; """Inspired by the pForth instruction DIGIT, see
        ; https://github.com/philburk/pforth/blob/master/fth/numberio.fth
        ; Rewritten from DIGIT>NUMBER in Tali Forth. Note in contrast to
        ; pForth, we get the base (radix) ourselves instead of having the
        ; user provide it. There is no standard name for this routine, which
        ; itself is not ANS; we use DIGIT? following pForth and Gforth.
        ; """
; .scope
; xt_digit_question:
                ; jsr underflow_1

                ; ldy #0                  ; default flag is failure

                ; lda 0,x                 ; Check the character.

                ; sec
                ; sbc #'0
                ; bcc _done               ; < '0' ?

                ; cmp #10
                ; bcc _checkbase          ; <= '9' ?

                ; and #$df                ; convert to uppercase
                ; sbc #7                  ; 'A'-'9'+1
                ; cmp #10                 ; < 'A' ?
                ; bcc _done

; _checkbase:     ; we have a number, now see if it falls inside the range
                ; ; provided by BASE
                ; cmp base
                ; bcs _done               ; already have false flag

                ; ; Found a legal number
                ; sta 0,x                 ; put number
                ; stz 1,x                 ; paranoid
                ; dey                     ; set success flag

; _done:          dex                     ; push flag
                ; dex
                ; sty 0,x
                ; sty 1,x

; z_digit_question:
                ; rts
; .scend

; ## DISASM ( addr u -- ) "Disassemble a block of memory"
; ## "disasm"  tested  Tali Forth
        ; """Convert a segment of memory to assembler output. This
        ; word is vectored so people can add their own disassembler.
        ; Natively, this produces Simpler Assembly Notation (SAN)
        ; code, see the section on The Disassembler in the manual and
        ; the file disassembler.asm for more details.
        ; """
xt_disasm:
                jsr underflow_2
                jmp disassembler
z_disasm:


; ## DNEGATE ( d -- d ) "Negate double cell number"
; ## "dnegate"  auto  ANS double
        ; """https://forth-standard.org/standard/double/DNEGATE"""
xt_dnegate:
                jsr underflow_2 ; double number
dnegate_nouf:
                ldy #0
                sec

                tya
                sbc 2,x         ; LSB of low cell
                sta 2,x

                tya
                sbc 3,x         ; MSB of low cell
                sta 3,x

                tya
                sbc 0,x         ; LSB of high cell
                sta 0,x

                tya
                sbc 1,x         ; MSB of high cell
                sta 1,x

z_dnegate:      rts


; ## QUESTION_DO ( limit start -- )(R: -- limit start) "Conditional loop start"
; ## "?do"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/qDO"""
xt_question_do:
                ; ?DO shares most of its code with DO. We use the tmp1 flag
                ; to mark which is which
                lda #$ff                ; -1 is ?DO, jump to common code
                bra do_common

; ## DO ( limit start -- )(R: -- limit start)  "Start a loop"
; ## "do"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DO
        ;
        ; Compile-time part of DO.
        ; See the Control Flow section of the manual for details.
        ;
        ; This may not be native compile. Don't check for a stack underflow
        ; """
.scope
xt_do:
                ; DO and ?DO share most of their code, use tmp1 as a flag.
                lda #0                  ; 0 is DO, drop through to DO_COMMON
do_common:
                pha                     ; save do/do? flag

                lda leave_anchor        ; save outer "DO"s value
                ldy leave_anchor+1
                jsr PsuYA
                stz leave_anchor        ; current value is end_of_list
                stz leave_anchor+1

                ; compile the (?DO) portion of ?DO if appropriate
                pla
                beq _compile_do

                ; We came from ?DO, so compile its runtime first. We do
                ; this with a quick loop because we know it has to be
                ; Always Native anyway
                ldy #0
*               lda question_do_runtime,y
                jsr cmpl_a
                iny
                cpy #question_do_runtime_end-question_do_runtime
                bne -
                jsr leave_jmp2          ; compile jmp abs

_compile_do:
                ; compile runtime part of DO.
                ldy #0
*               lda do_runtime,y
                jsr cmpl_a
                iny
                cpy #do_runtime_end-do_runtime
                bne -

                ; Save the start of the body of the loop code.
                jsr xt_here
z_question_do:
z_do:           rts
.scend

do_runtime:
        ; """Runtime routine for DO loop. Note that ANS loops quit when the
        ; boundry of limit-1 and limit is reached, a different mechanism than
        ; the FIG Forth loop (you can see which version you have by running
        ; a loop with start and limit as the same value, for instance
        ; 0 0 DO -- these will walk through the number space). We use a
        ; "fudge factor" for the limit that makes the Overflow Flag trip when
        ; it is reached; see http://forum.6502.org/viewtopic.php?f=9&t=2026
        ; for further discussion of this. The source given there for
        ; this idea is Laxen & Perry F83. -- This routine is called (DO)
        ; in some Forths. Usually, we would define this as a separate word
        ; and compile it with COMPILE, and the Always Native (AN) flag.
        ; However, we can do it faster if we just copy the bytes
        ; of this routine with a simple loop in DO.
        ; """
                ; First step: create fudge factor (FUFA) by subtracting the
                ; limit from $8000, the number that will trip the overflow
                ; flag
                sec
                lda #0
                sbc 2,x         ; LSB of limit
                tay             ; save FUFA for later use

                lda #$80
                sbc 3,x         ; MSB of limit
                sta 3,x         ; save FUFA for later use
                pha             ; push FUFA (replaces limit on R stack)
                phy

                ; Second step: index is FUFA plus original index
                clc
                tya             ; LSB of FUFA
                adc 0,x         ; add LSB of original index
                tay
                lda 1,x         ; MSB of orginal index
                adc 3,x         ; add MSB of FUFA

                inx             ; Clean the data stack
                inx
                inx
                inx

                pha             ; push index
                phy

do_runtime_end:

; Alternative:  bytes for each DO from 29 to 10, cycles from 58 to 82
; DO could generate
;               jsr do_runtime_1
;               pha                 ; push FUFA
;               phy
;               jsr do_runtime_2
;               pha                 ; push modified index
;               phy
;
; where the runtime routines are
;
;do_runtime_1:   ; First step: create fudge factor (FUFA) by subtracting the
;                ; limit from $8000, the number that will trip the overflow
;                ; flag
;                sec
;                lda #0
;                sbc 2,x         ; LSB of limit
;                tay             ; save FUFA for later use
;
;                lda #$80
;                sbc 3,x         ; MSB of limit
;                sta 3,x         ; save FUFA for later use
;                rts
;
;do_runtime_2:   ; Second step: index is FUFA plus original index
;                clc
;                tya             ; LSB of FUFA
;                adc 0,x         ; add LSB of original index
;                tay
;                lda 1,x         ; MSB of orginal index
;                adc 3,x         ; add MSB of FUFA
;
;                inx             ; Clean the data stack
;                inx
;                inx
;                inx
;                rts

question_do_runtime:
.scope
        ; """This is called (?DO) in some Forths. See the explanation at
        ; do_runtime for the background on this design
        ; """
                ; see if TOS and NOS are equal.
                lda 0,x
                cmp 2,x
                bne _qdo_do
                lda 1,x
                cmp 3,x
                bne _qdo_do

                inx             ; DROP index
                inx
                inx             ; DROP limit
                inx

 .alias _qdo_do ^+3
question_do_runtime_end:
.scend


; ## DOES ( -- ) "Add payload when defining new words"
; ## "does>"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DOES
        ; Create the payload for defining new defining words. See
        ; http://www.bradrodriguez.com/papers/moving3.htm and
        ; the Developer Guide for a discussion of
        ; DOES>'s internal workings.
        ; """
.scope
xt_does:
                ; compile a subroutine jump to runtime of DOES>
                ldy #>does_runtime
                lda #<does_runtime
                jsr cmpl_subroutine

                ; compile a subroutine jump to DODOES. In traditional
                ; terms, this is the Code Field Area (CFA) of the new
                ; word
                ldy #>dodoes
                lda #<dodoes
                jmp cmpl_subroutine
z_does:
.scend

does_runtime:
        ; """Runtime portion of DOES>. This replaces the subroutine jump
        ; to DOVAR that CREATE automatically encodes by a jump to the
        ; address that contains a subroutine jump to DODOES. We don't
        ; jump to DODOES directly because we need to work our magic with
        ; the return addresses. This routine is also known as "(DOES)" in
        ; other Forths
        ; """
.scope
                ; CREATE has also already modified the DP to point to the new
                ; word. We have no idea which instructions followed the CREATE
                ; command if there is a DOES> so the CP could point anywhere
                ; by now.
                jsr current_to_dp   ; Grab the DP from the CURRENT wordlist.
                ldy #nt_xt      ; tmp3 = xt of the latest word
                lda (dp),y
                sta tmp3
                iny
                lda (dp),y
                sta tmp3+1

                ; Get our caller's return addr
                pla             ; LSB
                inc             ; +1 to correct RTS addr
                bne +           ; add carry to MSB
                ply
                iny
                phy
*
                ; Replace the jsr DOVAR address with our own
                ldy #1          ; LSB
                sta (tmp3),y
                pla             ; MSB
                iny
                sta (tmp3),y

                ; Since we removed the return address that brought us here, we
                ; go back to whatever the main routine was. Otherwise, we we
                ; smash into the subroutine jump to DODOES.
                rts
.scend


; ## DOT ( u -- ) "Print TOS"
; ## "."  auto  ANS core
        ; """https://forth-standard.org/standard/core/d"""
.scope
xt_dot:
                jsr underflow_1

                jsr dup_nouf                    ; ( n n )
                jsr abs_nouf                    ; ( n u )
                jsr xt_zero                     ; ( n u 0 )
dot_common:     jsr xt_less_number_sign         ; ( n u 0 )
                jsr number_sign_s_nouf          ; ( n ud )
                jsr rot_nouf                    ; ( ud n )
                jsr sign_nouf                   ; ( ud )
dot_comm4:      jsr xt_number_sign_greater      ; ( addr u )
                jsr xt_type
                jmp xt_space
z_dot:
.scend


; ## DOT_PAREN ( -- ) "Print input up to close paren .( comment )"
; ## ".("  auto  ANS core
        ; """http://forth-standard.org/standard/core/Dotp"""
.scope
xt_dot_paren:
                ; Put a right paren on the stack.
                lda #41     ; Right parenthesis
                jsr PsuZA
                ; Call parse.
                jsr xt_parse
                ; Print the contents
                jsr xt_type
z_dot_paren:    rts
.scend


; ## DOT_QUOTE ( "string" -- ) "Print string from compiled word"
; ## ".""  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/Dotq
        ; Compile string that is printed during run time. ANS Forth wants
        ; this to be compile-only, even though everybody and their friend
        ; uses it for everything. We follow the book here, and recommend
        ; .( for general printing
        ; """
.scope
xt_dot_quote:
                ; we let S" do the heavy lifting. Since we're in
                ; compile mode, it will save the string and reproduce it
                ; during runtime
                jsr xt_s_quote

                ; We then let TYPE do the actual printing
                ldy #>xt_type
                lda #<xt_type
                jsr cmpl_subroutine

z_dot_quote:    rts
.scend


; ## DOT_R ( n u -- ) "Print NOS as unsigned number with TOS with"
; ## ".r"  tested  ANS core ext
        ; """https://forth-standard.org/standard/core/DotR
        ;
        ; Based on the Forth code
        ;  : .R  >R DUP ABS 0 <# #S ROT SIGN #> R> OVER - SPACES TYPE ;
        ; """
.scope
xt_dot_r:
                jsr underflow_2

                lda 0,x
                inx
                inx
                pha
                jsr dup_nouf
                jsr abs_nouf
                jsr xt_zero
dot_r_common:   jsr xt_less_number_sign
                jsr number_sign_s_nouf
                jsr rot_nouf
                jsr xt_sign
dot_r_comm2:    jsr xt_number_sign_greater
                pla
                sec
                sbc 0,x
                sta 0,x
                jsr xt_spaces
                jmp xt_type
z_dot_r:
.scend


; ## DOT_S ( -- ) "Print content of Data Stack"
; ## ".s"  tested  ANS tools
        ; """https://forth-standard.org/standard/tools/DotS
        ;
        ; Print content of Data Stack non-distructively. We follow the format
        ; of Gforth and print the number of elements first in brackets,
        ; followed by the Data Stack content (if any).
        ;
        ; Since this is for humans, we don't have to worry about speed.
        ; """
.scope
xt_dot_s:
                ; Print stack depth in brackets
                lda #$3c        ; ASCII for "<"
                jsr emit_a
                jsr xt_depth    ; ( -- u )
                jsr print_u     ; print unsigned number without the trailing space
                lda #$3e        ; ASCII for ">"
                jsr emit_a
                jsr xt_space

                ldy #dsp0       ; Y is our print ptr
_loop:          dey
                dey
                stx tmp1
                cpy tmp1
                bcc _done
                phy

                dex
                dex
                lda 1,y
                sta 1,x
                lda 0,y
                sta 0,x
                jsr xt_dot

                ply
                bra _loop
_done:
z_dot_s:        rts
.scend


; ## D_DOT ( d -- ) "Print double"
; ## "d."  tested  ANS double
        ; """http://forth-standard.org/standard/double/Dd"""
        ; From the Forth code:
        ; : D. TUCK DABS <# #S ROT SIGN #> TYPE SPACE ;
        ; """
.scope
xt_d_dot:
                jsr underflow_2

                jsr tuck_nouf
                jsr dabs_nouf
                jmp dot_common
z_d_dot:
.scend


; ## D_DOT_R ( d u -- ) "Print double right-justified u wide"
; ## "d.r"  tested  ANS double
        ; """http://forth-standard.org/standard/double/DDotR"""
.scope
xt_d_dot_r:
                jsr underflow_3

                ; From the forth code:
; : d.r >r tuck dabs <# #s rot sign #> r> over - spaces type ;
                lda 0,x
                inx
                inx
                pha
                jsr tuck_nouf
                jsr dabs_nouf
                bra dot_r_common
z_d_dot_r:
.scend


; ## DROP ( u -- ) "Pop top entry on Data Stack"
; ## "drop"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DROP"""
xt_drop:
                jsr underflow_1

                inx
                inx
z_drop:         rts


; ## DUMP ( addr u -- ) "Display a memory region"
; ## "dump"  tested  ANS tools
        ; """https://forth-standard.org/standard/tools/DUMP
        ;
        ; DUMP's exact output is defined as "implementation dependent".
        ; Uses TMP2
        ; """
.scope
xt_dump:
                jsr underflow_2
_row_loop:

                jsr xt_cr

                ; print address number
                lda 3,x
                jsr byte_to_ascii
                lda 2,x
                jsr byte_to_ascii

                jsr xt_space
                jsr xt_space

                ldy #0                  ; bytes in the row counter
_byte_loop:
                ; if there are zero bytes left to display, we're done
                lda 0,x
                ora 1,x
                beq _byte_end

                ; extra space after eight bytes
                cpy #8
                bne +
                jsr xt_space
*
                lda (2,x)               ; fetch the byte
                sta (cp),y              ; save for ASCII print later
                jsr byte_to_ascii
                jsr xt_space

                jsr decinc              ; decrement length, increment address

                iny
                cpy #16
                bcc _byte_loop          ; next byte
_byte_end:      sty tmp2

                ; Done with one line, print the ASCII version of these
                ; characters
                jsr xt_space

                ; Print the ASCII characters that we have saved from
                ; HERE (CP) to HERE plus whatever is in TMP2.
                ldy #0
_ascii_loop:    cpy tmp2
                bcs _ascii_end

                ; extra space after eight chars
                cpy #8
                bne +
                jsr xt_space
*
                lda (cp),y
                jsr is_printable
                bcs +
                lda #'.                 ; Print dot if not printable
*
                jsr emit_a

                iny
                bra _ascii_loop
_ascii_end:

                lda 0,x
                ora 1,x
                bne _row_loop

                jmp two_drop_nouf       ; one byte less than 4x INX
z_dump:
.scend


; ## DUP ( u -- u u ) "Duplicate TOS"
; ## "dup"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DUP"""
xt_dup:
                jsr underflow_1
dup_nouf:
                lda 0,x         ; LSB
                ldy 1,x         ; MSB

PsuYA:  ; push YA onto the param stack
                dex
                dex
                sta 0,x
                sty 1,x

z_dup:          rts


PluYA:  ; pop YA from the param stack
                lda 0,x
                ldy 1,x
                inx
                inx
                rts


; ## ED ( -- u ) "Line-based editor"
; ## "ed"  fragment  Tali Forth
        ; """Start the line-based editor ed6502. See separate file
        ; ed.asm or the manual for details.
        ; """
xt_ed:
                jsr ed6502      ; kept in separate file

z_ed:       rts


; ## EDITOR_WORDLIST ( -- u ) "WID for the Editor wordlist"
; ## "editor-wordlist"  tested  Tali Editor
        ; """ Commonly used like `editor-wordlist >order` to add the editor
        ; words to the search order so they can be used.  This will need
        ; to be done before any of the words marked "Tali Editor" can be
        ; used.  See the tutorial on Wordlists and the Search Order for
        ; more information.
        ;
        ; This is a dummy entry, the code is shared with ONE
        ; """


; ## ELSE (C: orig -- orig) ( -- ) "Conditional flow control"
; ## "else"  auto  ANS core
        ; """http://forth-standard.org/standard/core/ELSE
        ;
        ; The code is shared with ENDOF
        ; """
.scope
xt_else:
xt_endof:
                ; Put an unconditional branch.
                lda #$4c     ; opcode for jmp abs
                jsr cmpl_a
                ; Put the address of the branch address on the stack.
                jsr xt_here

                ; Use zero for the branch address for now.
                ; THEN will fill it in later.
                jsr cmpl_word0

                ; Get the address to jump to (just after the
                ; unconditional branch) for the IF to jump to
                ; when false.
                jsr xt_here
                jsr xt_rot

                ; Update the original if 0branch address.
                jmp xt_store
z_else:
z_endof:
.scend


; ## EMIT ( char -- ) "Print character to current output"
; ## "emit"  auto  ANS core
        ; """https://forth-standard.org/standard/core/EMIT
        ; Run-time default for EMIT. The user can revector this by changing
        ; the value of the OUTPUT variable. We ignore the MSB completely, and
        ; do not check to see if we have been given a valid ASCII character.
        ; Don't make this native compile
        ; """
.scope
xt_emit:
                jsr underflow_1

                lda 0,x
                inx
                inx

emit_a:
        ; We frequently want to print the character in A without fooling
        ; around with the Data Stack. This is emit_a's job, which still
        ; allows the output to be vectored. Call it with JSR as you
        ; would XT_EMIT
                jmp (output)            ; JSR/RTS

z_emit:         ; never reached
.scend


; ## EMPTY_BUFFERS ( -- ) "Empty all buffers without saving"
; ## "empty-buffers"  tested  ANS block ext
        ; """https://forth-standard.org/standard/block/EMPTY-BUFFERS"""
xt_empty_buffers:
                ; Set the buffer status to empty.
                ldy #buffstatus_offset
                lda #0
                sta (up),y      ; Only LSB is used.
z_empty_buffers:
                rts


; ## ENDCASE (C: case-sys -- ) ( x -- ) "Conditional flow control"
; ## "endcase"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/ENDCASE"""
.scope
xt_endcase:
                ; Postpone DROP to remove the item
                ; being checked.
                jsr cmpl_drop

                ; There are a number of address (of branches
                ; that need their jump addressed filled in with
                ; the address of right here).  Keep calling
                ; THEN to deal with them until we reach the
                ; 0 that CASE put on the stack at the beginning.
_endcase_loop:
                ; Check for 0 on the stack.
                lda 1,x
                beq _done
                jsr xt_then
                bra _endcase_loop
_done:
                ; Remove the 0 from the stack.
                inx
                inx
z_endcase:      rts
.scend


; ## ENDOF (C: case-sys1 of-sys1-- case-sys2) ( -- ) "Conditional flow control"
; ## "endof"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/ENDOF
        ; This is a dummy entry, the code is shared with ELSE
        ; """


; ## ENVIRONMENT_Q  ( addr u -- 0 | i*x true )  "Return system information"
; ## "environment?"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ENVIRONMENTq
        ; By ANS definition, we use upper-case strings here, see the
        ; string file for details. This can be realized as a high-level
        ; Forth word as
        ;
        ; : STRING_OF POSTPONE 2OVER POSTPONE COMPARE POSTPONE 0=
        ;    POSTPONE IF POSTPONE 2DROP ; IMMEDIATE COMPILE-ONLY
        ; HEX
        ; : ENVIRONMENT? ( C-ADDR U -- FALSE | I*X TRUE )
        ; CASE
        ; S" /COUNTED-STRING"    STRING_OF  7FFF TRUE ENDOF
        ; S" /HOLD"              STRING_OF    FF TRUE ENDOF
        ; S" /PAD"               STRING_OF    54 TRUE ENDOF ( 84 DECIMAL )
        ; S" ADDRESS-UNIT-BITS"  STRING_OF     8 TRUE ENDOF
        ; S" FLOORED"            STRING_OF FALSE TRUE ENDOF ( WE HAVE SYMMETRIC )
        ; S" MAX-CHAR"           STRING_OF   255 TRUE ENDOF
        ; S" MAX-D"              STRING_OF
                                     ; 7FFFFFFF. TRUE ENDOF
        ; S" MAX-N"              STRING_OF  7FFF TRUE ENDOF
        ; S" MAX-U"              STRING_OF  FFFF TRUE ENDOF
        ; S" MAX-UD"             STRING_OF
                                     ; FFFFFFFF. TRUE ENDOF
        ; S" RETURN-STACK-CELLS" STRING_OF    80 TRUE ENDOF
        ; S" STACK-CELLS"        STRING_OF    20 TRUE ENDOF ( FROM DEFINITIONS.ASM )
        ; ( DEFAULT ) 2DROP FALSE FALSE ( ONE FALSE WILL DROPPED BY ENDCASE )
        ; ENDCASE ;
        ;
        ; but that uses lots of memory and increases the start up time. This
        ; word is rarely used so we can try to keep it short at the expense
        ; of speed.
        ; """
.scope
xt_environment_q:
                jsr underflow_2

                ; This code is table-driven: We walk through the list of
                ; strings until we find one that matches, and then we take
                ; the equivalent data from the results table. This is made
                ; a bit harder by the fact that some of these return a
                ; double-cell number and some a single-cell one.

                ldy 3,x     ; setup tmp2 for string compare
                lda 2,x
                sta tmp2
                sty tmp2+1
                lda 0,x
                sta tmp3

                phx

                ; Search the table for a string match
                ldx #$ff                ; index for table
                stx tmp1                ; init entry #

_search_loop:   inx                     ; skip to next entry
                lda envs_tbl,x
                beq _no_match
                cmp #$20
                bcs _search_loop
                inc tmp1                ; increment entry #

                cmp tmp3                ; strings equal length?
                bne _search_loop

                ldy #0                  ; strings equal?
_compare_loop:  inx
                lda (tmp2),y
                cmp envs_tbl,x
                bne _search_loop
                iny
                cpy tmp3
                bcc _compare_loop

                ; found!
                plx
                ; We arrive here with ( addr u ) and know that we've found
                ; a match.
                lda tmp1                ; The index of the match is in A.

                ; See if this is a single-cell word.
                asl
                tay
                cmp #_double_start*2
                bcc _single

                ; This is a double-celled result, which means we have to
                ; fool around with the index some more.
                asl
                sbc #[_double_start*2]-1
                tay

                ; We also need a further cell on the stack
                dex                     ; ( addr u ? )
                dex

                lda _results+2,y
                sta 4,x
                lda _results+3,y
                sta 5,x                 ; ( res res ? )

_single:        ; Copy 1st cell of result
                lda _results+0,y
                sta 2,x
                lda _results+1,y
                sta 3,x

                lda #$ff
                sta 0,x
                sta 1,x                 ; ( res f )
                rts


_no_match:      ; Bummer, not found. We arrive here with
                ; ( addr u ) and need to return just a zero
                plx
                inx
                inx
                stz 0,x
                stz 1,x
z_environment_q: rts

; Tables for ENVIRONMENT?. We use two separate ones, one for the single-cell
; results and one for the double-celled results. The zero cell at the
; end of each table marks its, uh, end. The strings themselves are defined
; in strings.asm. Note if we add more entries to the single-cell table, we
; have to adapt the result code for double printout, where we subtract 22
; (two bytes each single-cell string and two bytes for the end-of-table
; marker 0000

_results: ; these are synchronized with envs_tbl
        ; single --------
        .word $7FFF     ; /COUNTED-STRING
        .word $00FF     ; /HOLD
        .word 84        ; /PAD (this is 84 decimal)
        .word 8         ; ADDRESS-UNIT-BITS (keep "$" to avoid octal!)
        .word 0         ; FLOORED ("FALSE", we have symmetric)
        .word $00FF     ; MAX-CHAR
        .word $7FFF     ; MAX-N
        .word $FFFF     ; MAX-U
        .word $80       ; RETURN-STACK-CELLS
        .word $20       ; STACK-CELLS (from definitions.asm)
        .word 9         ; WORDLISTS
 .alias _double_start [^-_results]/2
        .word $7FFF, $FFFF      ; MAX-D
        .word $FFFF, $FFFF      ; MAX-UD

.scend


; ## EQUAL ( n n -- f ) "See if TOS and NOS are equal"
; ## "="  auto  ANS core
        ; """https://forth-standard.org/standard/core/Equal"""
.scope
xt_equal:
                jsr underflow_2

                ldy #0                  ; assume FALSE

                lda 0,x                 ; LSB
                cmp 2,x
                bne _done

                lda 1,x                 ; MSB
                cmp 3,x
                bne _done

                dey                     ; change to TRUE

_done:          sty 2,x
                sty 3,x

                inx
                inx

z_equal:        rts
.scend


; ## BLANK ( addr u -- ) "Fill memory region with spaces"
; ## "blank"  auto  ANS string
        ; """https://forth-standard.org/standard/string/BLANK"""
xt_blank:
                jsr underflow_2

                ldy #AscSP
                bra fillY


; ## ERASE ( addr u -- ) "Fill memory region with zeros"
; ## "erase"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/ERASE
        ; Note that ERASE works with "address" units
        ; (bytes), not cells.
        ; """
.scope
xt_erase:
                jsr underflow_2

                ldy #0
                bra fillY


; ## FILL ( addr u char -- ) "Fill a memory region with a character"
; ## "fill"  auto  ANS core
        ; """https://forth-standard.org/standard/core/FILL
        ; Fill u bytes of memory with char starting at addr. Note that
        ; this works on bytes, not on cells. On an 8-bit machine such as the
        ; 65c02, this is a serious pain in the rear. It is not defined what
        ; happens when we reach the end of the address space
        ; """
xt_fill:
                jsr underflow_3

                ; We use Y to hold the character
                ldy 0,x
                inx
                inx
fillY:

                ; For more safety, we'll abort if we're writing outside of RAM
                sec
                lda #>ram_end
                sbc 3,x
                bcc _ram_err
                sbc 1,x
                bcc _ram_err
                sbc #>buffer0
                bcs _ram_ok
_ram_err:       lda #err_underflow  ; TODO: this could use it's own error code
                jsr error
_ram_ok:

                ; We use tmp1 to hold the address
                lda 2,x         ; LSB
                sta tmp1
                lda 3,x
                sta tmp1+1

                tya             ;byte to fill with now in A

                ldy 1,x
                beq _partial

                ldy #0
_page_loop:     sta (tmp1),y
                iny
                bne _page_loop
                inc tmp1+1
                dec 1,x
                bne _page_loop

_partial:       ldy 0,x
                beq _partial_done
_partial_loop:  dey
                sta (tmp1),y
                bne _partial_loop
_partial_done:

                jmp two_drop_nouf
z_blank:
z_erase:
z_fill:
.scend



; ## EXECUTE ( xt -- ) "Jump to word based on execution token"
; ## "execute"  auto  ANS core
        ; """https://forth-standard.org/standard/core/EXECUTE"""
xt_execute:
                jsr underflow_1
execute_nouf:
                lda 0,x
                ldy 1,x
                inx
                inx

execute_YA:     sta tmp0
                sty tmp0+1
                jmp (tmp0)
                ; we don't need a RTS here because we've JMPed to
                ; the word we're calling
z_execute:



; ## EXECUTE_PARSING ( addr u xt -- ) "Pass a string to a parsing word"
; ## "execute-parsing"  auto  Gforth
        ; """https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/The-Input-Stream.html
        ; Execute the parsing word defined by the execution token (xt) on the
        ; string as if it were passed on the command line. See the file
        ; tests/tali.fs for examples.
        ;
        ; Note that this word is coded completely
        ; different in its Gforth version, see the file execute-parsing.fs
        ; (in /usr/share/gforth/0.7.3/compat/ on Ubuntu 18.04 LTS) for details.
        ; """
xt_execute_parsing:
                jsr underflow_3

                jsr xt_input_to_r       ; save normal input for later
                jsr xt_not_rote         ; -ROT ( xt addr u )

                lda 0,x                 ; TOS is new ciblen
                sta ciblen
                lda 1,x
                sta ciblen+1

                lda 2,x                 ; NOS is new cib
                sta cib
                lda 3,x
                sta cib+1

                stz toin                ; Set >IN to zero
                stz toin+1

                jsr xt_two_drop         ; 2DROP ( xt )
                jsr xt_execute

                jsr xt_r_to_input

z_execute_parsing:
                rts


; ## EXIT ( -- ) "Return control to the calling word immediately"
; ## "exit"  auto  ANS core
        ; """https://forth-standard.org/standard/core/EXIT
        ; If we're in a loop, we need to UNLOOP first and get everything
        ; we we might have put on the Return Stack off as well. This should
        ; be natively compiled
        ; """
.scope
xt_exit:
                rts             ; keep before z_exit
z_exit:                         ; never reached
.scend

;
; ## FALSE ( -- f ) "Push flag FALSE to Data Stack"
; ## "false"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/FALSE"""
xt_false:
                dex
                dex
                stz 0,x
                stz 1,x

z_false:        rts


; ## FETCH ( addr -- n ) "Push cell content from memory to stack"
; ## "@"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Fetch"""
xt_fetch:
                jsr underflow_1
fetch_nouf:
                lda (0,x)               ; LSB
                tay
                inc 0,x
                bne +
                inc 1,x
*
                lda (0,x)               ; MSB
                sta 1,x
                sty 0,x

z_fetch:        rts


; ## FIND ( caddr -- addr 0 | xt 1 | xt -1 ) "Find word in Dictionary"
; ## "find"  auto  ANS core
        ; """https://forth-standard.org/standard/core/FIND
        ; Included for backwards compatibility only, because it still
        ; can be found in so may examples. It should, however, be replaced
        ; by FIND-NAME. Counted string either returns address with a FALSE
        ; flag if not found in the Dictionary, or the xt with a flag to
        ; indicate if this is immediate or not. FIND is a wrapper around
        ; FIND-NAME, we get this all over with as quickly as possible. See
        ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Word-Lists.html
        ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
        ; """
.scope
xt_find:
                jsr underflow_1

                ; Convert ancient-type counted string address to
                ; modern format
                jsr xt_count            ; ( caddr -- addr u )

                jsr find_common         ; ( addr u -- xt flag )
                bne _found
                ; No word found.

                ; The address needs to be restored.
                lda 2,x                 ; restore the addr from cout
                bne +
                dec 3,x
*               dec 2,x

_found:

z_find:         rts
.scend


; ## FIND_NAME ( addr u -- nt|0 ) "Get the name token of input word"
; ## "find-name"  auto  Gforth
.scope
xt_find_name:
        ; """www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
        ; Given a string, find the Name Token (nt) of a word or return
        ; zero if the word is not in the dictionary. We use this instead of
        ; ancient FIND to look up words in the Dictionary passed by
        ; PARSE-NAME. Note this returns the nt, not the xt of a word like
        ; FIND. To convert, use NAME>INT. This is a Gforth word. See
        ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
        ; FIND calls this word
        ; """
                jsr underflow_2

                jsr find_common     ; do the work

                inx                 ; DROP
                inx

                lda tmp1            ; return nt
                sta 0,x
                lda tmp1+1
                sta 1,x
z_find_name:    rts
.scend


find_common:
.scope
                ; Set up for traversing the wordlist search order.
                stz tmp3                ; Start at [0]
_wordlist_loop:
                lda tmp3
                ldy #num_order_offset   ; Compare to byte variable #ORDER
                cmp (up),y              ; Check to see if we are done
                beq _nomatch            ; We ran out of wordlists to search.

                jsr search_wordlist_A
                bne _done               ; if found, we're done

                ; Move on to the next wordlist in the search order.
                inc tmp3
                bra _wordlist_loop

_nomatch:       stz 2,x         ; failure flag
                stz 3,x
_done:          rts
.scend


; ## FLUSH ( -- ) "Save dirty buffers and empty buffers"
; ## "flush"  auto  ANS block
        ; """https://forth-standard.org/standard/block/FLUSH"""
xt_flush:
                jsr xt_save_buffers
                ; Set the buffer status to empty.
                ldy #buffstatus_offset
                lda #0
                sta (up),y      ; Only LSB is used.
z_flush:
                rts


; ## FM_SLASH_MOD ( d n1  -- rem n2 ) "Floored signed division"
; ## "fm/mod"  auto  ANS core
        ; """https://forth-standard.org/standard/core/FMDivMOD
        ; Note that by default, Tali Forth uses SM/REM for most things.
        ;
        ; There are various ways to realize this. We follow EForth with
        ;    DUP 0< DUP >R  IF NEGATE >R DNEGATE R> THEN >R DUP
        ;    0<  IF R@ + THEN  R> UM/MOD R> IF SWAP NEGATE SWAP THEN
        ; See (http://www.forth.org/eforth.html). However you can also
        ; go FM/MOD via SM/REM (http://www.figuk.plus.com/build/arith.htm):
        ;     DUP >R  SM/REM DUP 0< IF SWAP R> + SWAP 1+ ELSE  R> DROP THEN
        ; """
.scope
xt_fm_slash_mod:
                jsr underflow_3
fm_slash_mod_nouf:
                ; if sign of n1 is negative, negate both n1 and d
                lda 1,x         ; MSB of n1
                pha             ; save flag
                bpl +
                jsr negate_nouf ; NEGATE
                inx             ; pretend that we >R
                inx
                jsr dnegate_nouf ; DNEGATE
                dex             ; pretend that we R>
                dex
*
                ; if d is negative, add n1 to high cell of d
                lda 3,x         ; MSB of high word of d
                bpl +
                clc
                lda 0,x         ; LSB of n1
                adc 2,x         ; LSB of dh
                sta 2,x
                lda 1,x         ; MSB of n1
                adc 3,x         ; MSB of dh
                sta 3,x
*
                jsr um_slash_mod_nouf   ; ( d n1 -- rem n2 )

                ; if n was negative, negate the result
                pla
                beq +
                jsr negate_NOS_nouf
*
z_fm_slash_mod: rts
.scend



; ## FORTH ( -- ) "Replace first WID in search order with Forth-Wordlist"
; ## "forth"  auto  ANS search ext
        ; """https://forth-standard.org/standard/search/FORTH"""
xt_forth:
                lda #0          ; The WID for Forth is 0.
                ldy #search_order_offset
                sta (up),y
z_forth:
                rts


; This is a special jsr target to skip the zeroing of BLK at the beginning
; of evaluate.  It's used by LOAD to allow setting BLK while the block is
; being evaluated.  Evaluate's normal behavior is to zero BLK.
load_evaluate:
                ; Set a flag (using tmp1) to not zero BLK
                lda #$FF
                sta tmp1
                bra load_evaluate_start

; ## EVALUATE ( addr u -- ) "Execute a string"
; ## "evaluate"  auto  ANS core
        ; """https://forth-standard.org/standard/core/EVALUATE
        ; Set SOURCE-ID to -1, make addr u the input source, set >IN to zero.
        ; After processing the line, revert to old input source. We use this
        ; to compile high-level Forth words and user-defined words during
        ; start up and cold boot. In contrast to ACCEPT, we need to, uh,
        ; accept more than 255 characters here, even though it's a pain in
        ; the 8-bit.
        ; """
.scope
xt_evaluate:
                jsr underflow_2

                ; Clear the flag to zero BLK.  Only LOAD will set the flag,
                ; and will set the block number.
                stz tmp1

                ; If u is zero (which can happen a lot for the user-defined
                ; words), just leave again
                lda 0,x
                ora 1,x
                bne _got_work

                inx
                inx
                inx
                inx

                bra _done

; Special entry point for LOAD to bypass the zeroing of BLK.
load_evaluate_start:
_got_work:
                ; Save the current value of BLK on the return stack.
                ldy #blk_offset+1
                lda (up),y
                pha
                dey
                lda (up),y
                pha

                ; See if we should zero BLK.
                lda tmp1
                bne _nozero

                ; Set BLK to zero.
                ; lda #0        ; A is already zero from loading tmp1
                sta (up),y
                iny
                sta (up),y
_nozero:
                ; Save the input state to the Return Stack
                jsr xt_input_to_r

                ; set SOURCE-ID to -1
                lda #$ff
                sta insrc
                sta insrc+1

                ; set >IN to zero
                stz toin
                stz toin+1

                ; move TOS and NOS to input buffers
                lda 0,x
                sta ciblen
                lda 1,x
                sta ciblen+1

                lda 2,x
                sta cib
                lda 3,x
                sta cib+1

                inx             ; A clean stack is a clean mind
                inx
                inx
                inx

                jsr interpret   ; ( -- )

                ; restore variables
                jsr xt_r_to_input

                ; Restore BLK from the return stack.
                ldy #blk_offset
                pla
                sta (up),y
                iny
                pla
                sta (up),y
_done:
z_evaluate:     rts
.scend


; ## FORTH_WORDLIST ( -- u ) "WID for the Forth Wordlist"
; ## "forth-wordlist"  auto  ANS search
        ; """https://forth-standard.org/standard/search/FORTH-WORDLIST"""
        ; This is a dummy entry, the actual code is shared with ZERO.


; ## GET_CURRENT ( -- wid ) "Get the id of the compilation wordlist"
; ## "get-current" auto ANS search
        ; """https://forth-standard.org/standard/search/GET-CURRENT"""
.scope
xt_get_current:
                ; This is a little different than some of the variables
                ; in the user area as we want the value rather than
                ; the address.
                ldy #current_offset
                lda (up),y
                jmp PsuZA
z_get_current:
.scend

; ## GET_ORDER ( -- wid_n .. wid_1 n) "Get the current search order"
; ## "get-order" auto ANS search
        ; """https://forth-standard.org/standard/search/GET-ORDER"""
.scope
xt_get_order:
                ; Get #ORDER - the number of wordlists in the search order.
                ldy #num_order_offset
                lda (up),y
                beq _done       ; If zero, there are no wordlists.
_loop:          ; Count down towards the front of the list.
                ; By decrementing first, we also turn the length into an offset.
                dec             ; Count down by bytes.
                pha

                ; Get a pointer to the current wordlist, working back to front.
                clc
                adc #search_order_offset
                tay
                ; Put that wordlist id on the stack.
                lda (up),y
                jsr PsuZA

                ; See if that was the last one to process (first in the list).
                pla
                bne _loop
_done:
                ; Put the number of items on the stack.
                ldy #num_order_offset
                lda (up),y
                jsr PsuZA        ; We only support 8 wordlists.

z_get_order:    rts
.scend


; ## GREATER_THAN ( n n -- f ) "See if NOS is greater than TOS"
; ## ">"  auto  ANS core
        ; """https://forth-standard.org/standard/core/more"""
.scope
xt_greater_than:
                jsr underflow_2

                ldy #0          ; default false
                lda 0,x
                cmp 2,x
                lda 1,x
                sbc 3,x
                bvc +
                eor #$80
*               bpl +
                dey             ; true
*
                inx
                inx
                sty 0,x
                sty 1,x

z_greater_than: rts
.scend



; ## HERE ( -- addr ) "Put Compiler Pointer on Data Stack"
; ## "here"  auto  ANS core
        ; """https://forth-standard.org/standard/core/HERE
        ; This code is also used by the assembler directive ARROW
        ; ("->") though as immediate"""
xt_here:
xt_asm_arrow:
                lda cp
                ldy cp+1

                dex         ; PsuYA
                dex
                sta 0,x
                sty 1,x

z_asm_arrow:
z_here:         rts


; ## HEX ( -- ) "Change base radix to hexadecimal"
; ## "hex"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/HEX"""
xt_hex:
                lda #16
                sta base
                stz base+1              ; paranoid

z_hex:          rts


; ## HEXSTORE ( addr1 u1 addr2 -- u2 ) "Store a list of numbers"
; ## "hexstore"  auto  Tali
        ; """Given a string addr1 u1 with numbers in the current base seperated
        ; by spaces, store the numbers at the address addr2, returning the
        ; number of elements. Non-number elements are skipped, an zero-length
        ; string produces a zero output.
        ; """
.scope
xt_hexstore:
                jsr underflow_3

                jsr xt_dup              ; Save copy of original address
                jsr xt_two_to_r         ; ( addr1 u1 ) ( R: addr2 addr2 )

_loop:
                ; Loop until string is totally consumed
                lda 0,x
                ora 1,x
                beq _done

                jsr xt_cleave           ; ( addr1 u1 addr3 u3 ) ( R: addr2 addr2 )

                ; Prepare the conversion of the number.
                jsr xt_two_to_r
                jsr xt_zero_dot
                jsr xt_two_r_from       ; ( addr1 u1 0 0 addr3 u3 ) ( R: addr2 addr2 )
                jsr xt_to_number        ; ( addr1 u1 n n addr4 u4 ) ( R: addr2 addr2 )

                ; If u4 is not zero, we have leftover chars and have to do
                ; things differently
                lda 0,x
                ora 1,x
                bne _have_chars_left

                ; Normal case, this number is all done
                jsr xt_two_drop         ; ( addr1 u1 n n ) ( R: addr2 addr2 )
                jsr xt_d_to_s           ; ( addr1 u1 n ) ( R: addr2 addr2 )

                ; Store the new value
                jsr xt_r_fetch          ; ( addr1 u1 n addr2 ) ( R: addr2 addr2 )
                jsr xt_c_store          ; ( addr1 u1 ) ( R: addr2 addr2 )

                ; Increase counter
                pla
                ina
                bne +
                ply
                iny
                phy
*               pha                     ; ( addr1 u1 ) ( R: addr2+1 addr2 )
                bra _loop

_have_chars_left:
                ; Pathological case: Drop the rest of this number off the stack
                ; and continue with the next word. Doesn't print a warning. We
                ; need to drop four cells, that is, eight bytes
                txa
                clc
                adc #8
                tax

                bra _loop

_done:
                ; Clean up return stack and calculate number of chars stored
                inx
                inx
                inx
                inx                     ; 2DROP

                jsr xt_two_r_from       ; ( addr2+n addr2 )
                jsr swap_nouf
                jsr minus_nouf          ; ( n )

z_hexstore:     rts
.scend


; ## HOLD ( char -- ) "Insert character at current output"
; ## "hold"  auto  ANS core
        ; """https://forth-standard.org/standard/core/HOLD
        ; Insert a character at the current position of a pictured numeric
        ; output string on
        ; https://github.com/philburk/pforth/blob/master/fth/numberio.fth
        ; Forth code is : HOLD  -1 HLD +!  HLD @ C! ;  We use the the internal
        ; variable tohold instead of HLD.
        ; """
xt_hold:
                jsr underflow_1

                lda 0,x
                inx
                inx

hold_A:
                ldy tohold
                bne +
                dec tohold+1
*               dec tohold

                sta (tohold)

z_hold:         rts


; ## I ( -- n )(R: n -- n)  "Copy loop counter to stack"
; ## "i"  auto  ANS core
        ; """https://forth-standard.org/standard/core/I
        ; Note that this is not the same as R@ because we use a fudge
        ; factor for loop control; see the Control Flow section of the
        ; manual for details.
        ;
        ; This must always be compiled the same way (native or jsr)
        ; so we know the return stack layout.
        ; """
.scope
xt_i:
                ; Get the fudged index off of the top of the stack. It's
                ; easier to do math on the stack directly than to pop and
                ; push stuff around
                phx
                tsx

                sec
                lda $0102,x     ; LSB
                sbc $0104,x
                tay

                lda $0103,x     ; MSB
                sbc $0105,x

                plx

                dex
                dex
                sta 1,x         ; MSB of de-fudged index
                sty 0,x         ; LSB of de-fudged index

z_i:            rts
.scend


; ## IF (C: -- orig) (flag -- ) "Conditional flow control"
; ## "if"  auto  ANS core
        ; """http://forth-standard.org/standard/core/IF"""
.scope
xt_if:
                jsr cmpl_0branch   ; Compile a 0BRANCH

                ; Put the origination address on the stack for else/then
                jsr xt_here

                ; Stuff zero in for the branch address right now.
                ; THEN or ELSE will fix it later.
                jmp cmpl_word0
z_if:
.scend

.scope
cmpl_0branch:
                ldy #$100-[_0branch_runtime_end-_0branch_runtime]
_c2:            lda _0branch_runtime_end-$100,y
                jsr cmpl_a
                iny
                bne _c2
                rts


_0branch_runtime:
        ; """In some Forths, this is called (0BRANCH).
        ; """
                inx
                inx
                lda $fe,x       ; direct page wrapping won't work in 65816 native mode
                ora $ff,x
                bne ^+5
                .byte $4c       ; opcode for "jmp abs"
_0branch_runtime_end:
.scend


; ## IMMEDIATE ( -- ) "Mark most recent word as IMMEDIATE"
; ## "immediate"  auto  ANS core
        ; """https://forth-standard.org/standard/core/IMMEDIATE
        ; Make sure the most recently defined word is immediate. Will only
        ; affect the last word in the dictionary. Note that if the word is
        ; defined in ROM, this will have no affect, but will not produce an
        ; error message.
        ; """
xt_immediate:
                lda #IM
status_or_A: ; or A into status of current word
                pha
                jsr current_to_dp
                pla
                ldy #nt_status
                ora (dp),y
                sta (dp),y
z_immediate:    rts


; ## INPUT ( -- addr ) "Return address of input vector"
; ## "input" tested Tali Forth
.scope
xt_input:
                lda #input
                jmp PsuZA
z_input:
.scend


; ## INPUT_TO_R ( -- ) ( R: -- n n n n ) "Save input state to the Return Stack"
; ## "input>r"  tested  Tali Forth
        ; """Save the current input state as defined by insrc, cib, ciblen, and
        ; toin to the Return Stack. Used by EVALUTE.
        ;
        ; The naive way of doing
        ; this is to push each two-byte variable to the stack in the form of
        ;
        ;       lda insrc
        ;       pha
        ;       lda insrc+1
        ;       pha
        ;
        ; for a total of 24 byte of instruction in one direction and later
        ; a further 24 bytes to reverse the process. We shorten this at the
        ; cost of some speed by assuming the four variables are grouped
        ; together on the Zero Page and start with insrc (see definitions.asm
        ; for details). The reverse operation is r_to_input. These words must
    ; be flagged as Never Native. Uses tmp1
        ; """
.scope
xt_input_to_r:
                ; We arrive here with the return address on the top of the
                ; 65c02's stack. We need to move it out of the way first
                pla
                sta tmp1
                pla
                sta tmp1+1

                ; This assumes that insrc is the first of eight bytes and
                ; toin+1 the last in the sequence we want to save from the Zero
                ; Page.
                ldy #7
_loop:
                lda insrc,y     ; insrc+7 is toin+1
                pha
                dey
                bpl _loop

                ; Restore address for return jump
                lda tmp1+1
                pha
                lda tmp1
                pha

z_input_to_r:   rts
.scend


; ## INT_TO_NAME ( xt -- nt ) "Get name token from execution token"
; ## "int>name"  auto  Tali Forth
        ; """www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
        ; This is called >NAME in Gforth, but we change it to
        ; INT>NAME to match NAME>INT
        ; """
.scope
xt_int_to_name:
                jsr underflow_1

                ; Unfortunately, to find the header, we have to search
                ; all of the wordlists.

                ; (I'm assuming there is a reason this is avoiding tmp1)

                ; Search all of the wordlists in id order.
                lda #0
_wordlist_loop:
                ; A has the current wordlist id in it
                sta tmp3

                ; Get the DP for that wordlist.
                asl                     ; Turn offset into cells offset.
                adc #wordlists_offset+1
                tay
                lda (up),y              ; Get the DP for this wordlist
                pha
                dey
                lda (up),y
                bra _word_1

_word_next:     ; on to the next word in the chain.
                ldy #nt_next_nt+1
                lda (tmp2),y
                pha
                dey
                lda (tmp2),y
 _word_1:       sta tmp2
                pla
                sta tmp2+1
                beq _next_wordlist  ; end of word list?

                ldy #nt_xt      ; check LSB of xt in current nt
                lda (tmp2),y
                cmp 0,x
                bne _word_next
                iny             ; check MSB of xt in current nt
                lda (tmp2),y
                cmp 1,x
                bne _word_next
                bra _return     ; It's a match!  return nt

_next_wordlist: ; Move on to the next wordlist.
                lda tmp3
                inc
                cmp #max_wordlists
                bcc _wordlist_loop

                ; We didn't find it in any of the wordlists.

_return:        lda tmp2    ; replace TOS with nt (0 if not found)
                ldy tmp2+1
                sta 0,x
                sty 1,x
z_int_to_name:  rts
.scend


; ## INVERT ( n -- n ) "Complement of TOS"
; ## "invert"  auto  ANS core
        ; """https://forth-standard.org/standard/core/INVERT"""
xt_invert:
                jsr underflow_1

                lda #$FF
                eor 0,x         ; LSB
                sta 0,x

                lda #$FF
                eor 1,x         ; MSB
                sta 1,x

z_invert:       rts


; ## IS ( xt "name" -- ) "Set named word to execute xt"
; ## "is"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/IS"""
.scope
xt_is:
                ; This is a state aware word with differet behavior
                ; when used while compiling vs interpreting.
                ; Check STATE
                lda state
                ora state+1
                beq _interpreting
_compiling:
                ; Run ['] to compile the xt of the next word as a literal.
                ; as a literal.
                jsr xt_bracket_tick

                ; Postpone DEFER! by compiling a JSR to it.
                ldy #>xt_defer_store
                lda #<xt_defer_store
                jsr cmpl_subroutine
                bra _done
_interpreting:
                jsr xt_tick
                jsr xt_defer_store
_done:
z_is:           rts
.scend


; ## J ( -- n ) (R: n -- n ) "Copy second loop counter to stack"
; ## "j"  auto  ANS core
        ; """https://forth-standard.org/standard/core/J
        ; Copy second loop counter from Return Stack to stack. Note we use
        ; a fudge factor for loop control; see the Control Flow section of
        ; the manual for more details.
        ;
        ; This must always be compiled the same way (native or jsr)
        ; so we know the return stack layout.
        ; """
.scope
xt_j:

                ; Get the fudged index off from the stack. It's easier to
                ; do math on the stack directly than to pop and push stuff
                ; around
                phx
                tsx

                sec
                lda $0106,x     ; LSB
                sbc $0108,x
                tay

                lda $0107,x     ; MSB
                sbc $0109,x

                plx

                dex
                dex
                sta 1,x         ; MSB of de-fudged index
                sty 0,x         ; LSB of de-fudged index

z_j:            rts
.scend


; ## KEY ( -- char ) "Get one character from the input"
; ## "key"  tested  ANS core
xt_key:
        ; """https://forth-standard.org/standard/core/KEY
        ; Get a single character of input from the vectored
        ; input without echoing.
        ; """
                jsr key_a               ; returns char in A
                jmp PsuZA
z_key:

key_a:
        ; The 65c02 doesn't have a JSR (ADDR,X) instruction like the
        ; 65816, so we have to fake the indirect jump to vector it.
        ; This is depressingly slow. We use this routine internally
        ; to avoid manipulating the Data Stack when we just want a
        ; character
                jmp (input)             ; JSR/RTS



; ## LATESTNT ( -- nt ) "Push most recent nt to the stack"
; ## "latestnt"  auto  Tali Forth
        ; """www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
        ; The Gforth version of this word is called LATEST
        ; """
xt_latestnt:
                jsr current_to_dp

                dex
                dex
                lda dp
                sta 0,x
                lda dp+1
                sta 1,x

z_latestnt:     rts


; ## LATESTXT ( -- xt ) "Push most recent xt to the stack"
; ## "latestxt"  auto  Gforth
        ; """http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Anonymous-Definitions.html"""
xt_latestxt:
                jsr xt_latestnt         ; ( nt )
                jsr xt_name_to_int      ; ( xt )
z_latestxt:     rts


; ## LEAVE ( -- ) "Leave DO/LOOP construct"
; ## "leave"  auto  ANS core
        ; """https://forth-standard.org/standard/core/LEAVE
        ; Note that this does not work with anything but a DO/LOOP in
        ; contrast to other versions such as discussed at
        ; http://blogs.msdn.com/b/ashleyf/archive/2011/02/06/loopty-do-i-loop.aspx
        ;
        ;       : LEAVE POSTPONE BRANCH HERE SWAP 0 , ; IMMEDIATE COMPILE-ONLY
        ; See the Control Flow section in the manual for details of how this works.
        ; """
.scope
xt_leave:
                                        ; pop index & limit (compile UNLOOP)
                lda #$68                ; opcode for pla
                tay
                jsr cmpl_word
                lda #$68
                tay
                jsr cmpl_word

leave_jmp2:     ; build a jmp to end of the loop
                ; link the addr into the leave_anchor chain to fix later
                lda leave_anchor        ; insert previous reference
                ldy leave_anchor+1
                jsr cmpl_jump

                lda cp                  ; leave_anchor now points to this reference
                ldy cp+1
                sec
                sbc #2
                bcs +
                dey
*
                sta leave_anchor
                sty leave_anchor+1

z_leave:        rts
.scend


; ## LEFT_BRACKET ( -- ) "Enter interpretation state"
; ## "["  auto  ANS core
        ; """https://forth-standard.org/standard/core/Bracket
        ; This is an immediate and compile-only word
        ; """
xt_left_bracket:
                stz state
                stz state+1

z_left_bracket: rts


; ## LESS_NUMBER_SIGN ( -- ) "Start number conversion"
; ## "<#"  auto  ANS core
        ; """https://forth-standard.org/standard/core/num-start
        ; Start the process to create pictured numeric output.
        ;
        ; The new
        ; string is constructed from back to front, saving the new character
        ; at the beginning of the output string. Since we use PAD as a
        ; starting address and work backward (!), the string is constructed
        ; in the space between the end of the Dictionary (as defined by CP)
        ; and the PAD. This allows us to satisfy the ANS Forth condition that
        ; programs don't fool around with the PAD but still use its address.
        ; Based on pForth
        ; http://pforth.googlecode.com/svn/trunk/fth/numberio.fth
        ; pForth is in the pubic domain. Forth is : <# PAD HLD ! ; we use the
        ; internal variable tohold instead of HLD.
        ; """
xt_less_number_sign:
                lda cp
                clc
                adc #padoffset  ; assumes padoffset one byte in size
                sta tohold

                lda cp+1
                adc #0          ; only need carry
                sta tohold+1

z_less_number_sign:
                rts

; ## LESS_THAN ( n m -- f ) "Return true if NOS < TOS"
; ## "<"  auto  ANS core
        ; """https://forth-standard.org/standard/core/less"""
.scope
xt_less_than:
                jsr underflow_2

                ldy #0          ; default false

                lda 2,x
                cmp 0,x
                lda 3,x
                sbc 1,x
                bvc +
                eor #$80
*               bpl +

                dey                ; true
*

                inx
                inx
                sty 0,x
                sty 1,x

z_less_than:    rts
.scend


; ## LIST ( scr# -- ) "List the given screen"
; ## "list"  tested  ANS block ext
        ; """https://forth-standard.org/standard/block/LIST"""
.scope
xt_list:
                jsr underflow_1

                ; Save the screen number in SCR
                jsr xt_scr
                jsr xt_store
                ; Use L from the editor-wordlist to display the screen.
                jsr xt_editor_l
z_list:         rts
.scend


; ## LITERAL ( n -- ) "Store TOS to be push on stack during runtime"
; ## "literal"  auto  ANS core
        ; """https://forth-standard.org/standard/core/LITERAL
        ; Compile-only word to store TOS so that it is pushed on stack
        ; during runtime. This is a immediate, compile-only word.
        ;
        ; Note the cmpl_ routines use TMPTOS
        ; """
xt_literal:
                jsr underflow_1

                lda 1,x                 ; small constant?
                beq _1byte

                jsr cmpl_loadYAImmed

cmpl_PsuYA:     lda #$ca                ; opcode for dex
                tay
                jsr cmpl_word
                lda #$95                ; opcode for sta d,x
                ldy #0
                jsr cmpl_word
                lda #$94                ; opcode for sty d,x
                ldy #1
                jmp cmpl_word

_1byte: ; 1 byte zero-extended literal
                lda #$a9                ; opcode for lda #
                jsr cmpl_a
                jsr xt_c_comma

cmpl_PsuZA:     lda #$ca                ; opcode for dex
                tay
                jsr cmpl_word
                lda #$95                ; opcode for sta d,x
                ldy #0
                jsr cmpl_word
                lda #$74                ; opcode for stz d,x
                ldy #1
                jmp cmpl_word
z_literal:

cmpl_loadYAImmed:
                lda #$a9        ; build lda #<n
                ldy 0,x
                jsr cmpl_word
                lda #$a0        ; build ldy #>n
                ldy 1,x
                inx
                inx
                jmp cmpl_word



; ## LOAD ( scr# -- ) "Load the Forth code in a screen/block"
; ## "load"  auto  ANS block
        ; """https://forth-standard.org/standard/block/LOAD
        ;
        ; Note: LOAD current works because there is only one buffer.
        ; If/when multiple buffers are supported, we'll have to deal
        ; with the fact that it might re-load the old block into a
        ; different buffer.
        ; """
.scope
xt_load:
                jsr underflow_1

                ; Save the current value of BLK on the return stack.
                ldy #blk_offset+1
                lda (up),y
                pha
                dey
                lda (up),y
                pha
                ; Set BLK to the given block/screen number.
;                ldy #blk_offset    ; blk_offset already in y
                lda 0,x
                sta (up),y
                iny
                lda 1,x
                sta (up),y
                ; Load that block into a buffer
                jsr xt_block
                ; Put 1024 on the stack for the screen length.
                dex
                dex
                lda #4
                sta 1,x
                stz 0,x

                ; Jump to a special evluate target. This bypasses the underflow
                ; check and skips the zeroing of BLK.
                jsr load_evaluate

                ; Restore the value of BLK from before the LOAD command.
                ldy #blk_offset
                pla
                sta (up),y
                iny
                pla
                sta (up),y

                ; If BLK is not zero, read it back into the buffer.
                ; A still has MSB
                dey
                ora (up),y
                beq _done

                ; The block needs to be read back into the buffer.
                dex
                dex
                ldy #blk_offset
                lda (up),y
                sta 0,x
                iny
                lda (up),y
                sta 1,x
                jsr xt_block
                ; Drop the buffer address.
                inx
                inx

_done:
z_load:         rts
.scend


; ## LOOP ( -- ) "Finish loop construct"
; ## "loop"  auto  ANS core
        ; """https://forth-standard.org/standard/core/LOOP
        ; Compile-time part of LOOP. This does nothing more but push 1 on
        ; the stack and then call +LOOP.
        ;
        ; In Forth, this is
        ;       : LOOP  POSTPONE (LOOP) , POSTPONE UNLOOP ;
        ;       IMMEDIATE ; COMPILE-ONLY
        ; """
xt_loop:
.scope
                ; Compile the run-time part. We do this with a short loop
                ; and not a call to COMPILE, because it has to be natively
                ; coded & doesn't have a word header.
                ldy #0
*               lda _loop_runtime,y
                jsr cmpl_a
                iny
                cpy #_loop_runtime_end-_loop_runtime
                bcc -

                bra plus_loop_common
z_loop:

_loop_runtime:
        ; """Runtime compile for loop control.
        ; Note we use a fudge factor for
        ; loop  control so we can test with the Overflow Flag. See
        ; docs/loop.txt for details. The step value is TOS in the loop. This
        ; musst always be native compiled. In some Forths, this is a separate
        ; word called (LOOP)
        ; """
                clc
                pla             ; LSB of index
                adc #1          ; LSB of step
                tay             ; temporary storage of LSB

                pla             ; MSB of index
                adc #0          ; MSB of step
                pha             ; put MSB of index back on stack

                phy             ; put LSB of index back on stack

                ; if V flag is set, we're done looping and continue
                ; after the +LOOP instruction
                bvs _hack+3     ; skip over JMP instruction

_hack:          ; This is why this routine must be natively compiled: We
                ; compile the opcode for JMP here without an address to
                ; go to, which is added by the next next instruction of
                ; LOOP/+LOOP during compile time
                .byte $4c
_loop_runtime_end:
.scend


; ## PLUS_LOOP ( -- ) "Finish loop construct"
; ## "+loop"  auto  ANS core
        ; """https://forth-standard.org/standard/core/PlusLOOP
        ; Compile-time part of +LOOP, also used for LOOP. Is usually
        ;       : +LOOP POSTPONE (+LOOP) , POSTPONE UNLOOP ; IMMEDIATE
        ;       COMPILE-ONLY
        ; in Forth. LOOP uses this routine as well. We jump here with the
        ; address for looping as TOS and the address for aborting the loop
        ; (LEAVE) as the second double-byte entry on the Return Stack (see
        ; DO and the Control Flow section of the manual details).
        ; """
.scope
xt_plus_loop:
                ; Compile the run-time part. We do this with a short loop
                ; and not a call to COMPILE, because it has to be natively
                ; coded & doesn't have a word header.
                ldy #0
*               lda plus_loop_runtime,y
                jsr cmpl_a
                iny
                cpy #plus_loop_runtime_end-plus_loop_runtime
                bcc -

plus_loop_common:
                ; The address we need to loop back to is TOS. Store it so
                ; the runtime part of +LOOP jumps back up there
                jsr xt_comma

                ; Compile an UNLOOP for when we're all done.
                ldy #4
*               lda #$68                ; opcode for PLA
                jsr cmpl_a
                dey
                bne -

                ; Resolve all the LEAVE jmp addresses
                ldy #1
                lda leave_anchor+1      ; while(leave_anchor!=0) {
                beq _resolve9
_resolve1:      lda (leave_anchor),y    ;   save *leave_anchor
                pha
                lda (leave_anchor)
                pha
                lda cp                  ;   *leave_anchor=cp
                sta (leave_anchor)
                lda cp+1
                sta (leave_anchor),y
                pla                     ;   leave_anchor=saved
                sta leave_anchor
                pla
                sta leave_anchor+1
                bne _resolve1           ;   }
_resolve9:

                jsr PluYA               ; restore previous leave_anchor
                sta leave_anchor
                sty leave_anchor+1

z_plus_loop:    rts
.scend

plus_loop_runtime:
        ; """Runtime compile for loop control. This is used for both +LOOP and
        ; LOOP which are defined at high level. Note we use a fudge factor for
        ; loop  control so we can test with the Overflow Flag. See
        ; docs/loop.txt for details. The step value is TOS in the loop. This
        ; musst always be native compiled. In some Forths, this is a separate
        ; word called (+LOOP)
        ; """
.scope
                clc
                pla             ; LSB of index
                adc 0,x         ; LSB of step
                tay             ; temporary storage of LSB

                pla             ; MSB of index
                adc 1,x         ; MSB of step
                pha             ; put MSB of index back on stack

                phy             ; put LSB of index back on stack

                inx             ; dump step from TOS
                inx

                ; if V flag is set, we're done looping and continue
                ; after the +LOOP instruction
                bvs _hack+3     ; skip over JMP instruction

_hack:          ; This is why this routine must be natively compiled: We
                ; compile the opcode for JMP here without an address to
                ; go to, which is added by the next next instruction of
                ; LOOP/+LOOP during compile time
                .byte $4c
.scend
plus_loop_runtime_end:


; ## LSHIFT ( x u -- u ) "Shift TOS left"
; ## "lshift"  auto  ANS core
        ; """https://forth-standard.org/standard/core/LSHIFT"""
.scope
xt_lshift:
                jsr underflow_2

                lda 0,x
                inx
                inx

lshift_A:       tay
                beq _done
                lda 0,x
_loop:
                asl
                rol 1,x
                dey
                bne _loop
                sta 0,x
_done:
z_lshift:       rts
.scend


; ## M_STAR ( n n -- d ) "16 * 16 --> 32"
; ## "m*"  auto  ANS core
        ; """https://forth-standard.org/standard/core/MTimes
        ; Multiply two 16 bit numbers, producing a 32 bit result. All
        ; values are signed. Adapted from FIG Forth for Tali Forth.
        ; The original Forth is : M* OVER OVER XOR >R ABS SWAP ABS UM* R> D+- ;
        ; with  : D+- O< IF DNEGATE THEN ;
        ; """
.scope
xt_m_star:
                jsr underflow_2
m_star_nouf:
                ; figure out the sign
                lda 1,x         ; MSB of n1
                eor 3,x         ; MSB of n2

                ; UM* uses all kinds of temporary variables so we don't
                ; risk a conflict but just take the 1 cycle hit and
                ; use the stack
                pha

                ; get the absolute value of both numbers so we can feed
                ; them to UM*, which does the real work
                lda 1,x
                bpl +
                jsr negate_nouf
*
                lda 3,x
                bpl +
                jsr negate_NOS_nouf
*
                jsr um_star_nouf        ; ( d )

                ; handle the sign
                pla
                bpl +
                jmp dnegate_nouf
*
z_m_star:       rts
.scend


; ## MARKER ( "name" -- ) "Create a deletion boundry"
; ## "marker"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/MARKER
        ; This word replaces FORGET in earlier Forths. Old entries are not
        ; actually deleted, but merely overwritten by restoring CP and DP.
        ; Run the named word at a later time to restore all of the wordlists
        ; to their state when the word was created with marker.  Any words
        ; created after the marker (including the marker) will be forgotten.
        ;
        ; To do this, we want to end up with something that jumps to a
        ; run-time component with a link to the original CP and DP values:
        ;
        ;       jsr marker_runtime
        ;       <Original CP MSB>
        ;       <Original CP LSB>
        ;       <Original DP MSB> ( for CURRENT wordlist )
        ;       <Original DP LSB>
        ;       < USER variables from offset 4 to 39 >
        ;
        ;       The user variables include:
        ;       CURRENT (byte variable)
        ;       <All wordlists> (currently 12) (cell array)
        ;       <#ORDER> (byte variable)
        ;       <All search order> (currently 9) (byte array)
        ;
        ; This code uses tmp1 and tmp2
        ; """
.scope
xt_marker:
                ; Before we do anything, we need to save CP, which
                ; after all is the whole point of this operation. CREATE
                ; uses tmp1 and tmp2, so we take the speed hit and push stuff
                ; to the stack
                jsr current_to_dp
                lda dp+1
                pha
                lda dp
                pha

                lda cp+1
                pha
                lda cp
                pha

                ldy #>marker_runtime
                lda #<marker_runtime
                jsr cmpl_createYA

                ; Add original CP as payload
                pla                     ; LSB
                ply                     ; MSB
                jsr cmpl_word

                ; Add original DP as payload
                pla                     ; LSB
                ply                     ; MSB
                jsr cmpl_word

                ; Add the user variables for the wordlists and search order.
                ; We're compiling them in byte order.
                ldy #current_offset
_marker_loop:
                lda (up),y
                jsr cmpl_a
                iny
                cpy #num_order_offset   ; One past the end of the search order.
                bne _marker_loop

z_marker:       rts
.scend

.scope
marker_runtime:
        ; """Restore Dictionary and memory (DP and CP) to where the were
        ; when this marker was defined. We arrive here with the return
        ; address on the Return Stack in the usual 65c02 format
        ; """

                ; Get the address of the string address off the stack and
                ; increase by one because of the RTS mechanics
                pla
                ply
                inc
                bne +
                iny
*               sta tmp1        ; LSB of address
                sty tmp1+1      ; MSB of address

                ldy #0

                ; CP was stored first
                lda (tmp1),y
                sta cp
                iny
                lda (tmp1),y
                sta cp+1

                ; Next was DP
                iny
                lda (tmp1),y
                sta dp
                iny
                lda (tmp1),y
                sta dp+1

                ; Conveniently, the offset into both tmp1 and UP is 4
                ; to start restoring the wordlists and search order.
                ldy #current_offset     ; must be 4
_marker_restore_loop:
                ; Copy from the dictionary back on top of the wordlists
                ; and search order.
                lda (tmp1),y
                sta (up),y
                iny
                cpy #num_order_offset   ; One past the end of the search order.
                bne _marker_restore_loop

                jsr dp_to_current       ; Move the CURRENT DP back.
                ; The return instruction takes us back to the original caller
                rts
.scend


; ## MAX ( n n -- n ) "Keep larger of two numbers"
; ## "max"  auto  ANS core
        ; """https://forth-standard.org/standard/core/MAX
        ; Compare TOS and NOS and keep which one is larger. Adapted from
        ; Lance A. Leventhal "6502 Assembly Language Subroutines". Negative
        ; Flag indicates which number is larger. See also
        ; http://6502.org/tutorials/compare_instructions.html and
        ; http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        ; """
.scope
xt_max:
                jsr underflow_2

                ; Compare LSB. We do this first to set the carry flag
                lda 0,x         ; LSB of TOS
                cmp 2,x         ; LSB of NOS, this sets the carry

                lda 1,x         ; MSB of TOS
                sbc 3,x         ; MSB of NOS
                bvc _no_overflow

                ; handle overflow, because we use signed numbers
                eor #$80        ; complement negative flag
_no_overflow:
                ; if negative, NOS is larger and needs to be kept
                bmi _keep_nos

                ; move TOS to NOS
                lda 0,x
                sta 2,x
                lda 1,x
                sta 3,x
_keep_nos:
                inx
                inx

z_max:          rts
.scend


; ## MIN ( n n -- n ) "Keep smaller of two numbers"
; ## "min"  auto  ANS core
        ; """https://forth-standard.org/standard/core/MIN
        ; Adapted from Lance A. Leventhal "6502 Assembly Language
        ; Subroutines." Negative Flag indicateds which number is larger. See
        ; http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        ; """
.scope
xt_min:
                jsr underflow_2

                ; compare LSB. We do this first to set the carry flag
                lda 0,x         ; LSB of TOS
                cmp 2,x         ; LSB of NOS, this sets carry

                lda 1,x         ; MSB of TOS
                sbc 3,x         ; MSB of NOS
                bvc _no_overflow

                ; handle overflow because we use signed numbers
                eor #$80
_no_overflow:
                ; if negative, NOS is larger and needs to be dumped
                bpl _keep_nos

                ; move TOS to NOS
                lda 0,x
                sta 2,x
                lda 1,x
                sta 3,x
_keep_nos:
                inx
                inx

z_min:          rts
.scend


; ## MINUS ( n n -- n ) "Subtract TOS from NOS"
; ## "-"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Minus"""
xt_minus:
                jsr underflow_2
minus_nouf:
                sec
                lda 2,x         ; LSB
                sbc 0,x
                sta 2,x
                lda 3,x         ; MSB
                sbc 1,x
                sta 3,x

                inx
                inx

z_minus:        rts


; ## MINUS_LEADING ( addr1 u1 -- addr2 u2 ) "Remove leading spaces"
; ## "-leading"  auto  Tali String
        ; """Remove leading whitespace. This is the reverse of -TRAILING
        ; """
.scope
xt_minus_leading:
                jsr underflow_2
minus_leading_nouf:
                bra _chk

_fwd:           ; remove 1st char
                jsr xt_one              ; ( addr u 1 )
                jsr slash_string_nouf   ; ( addr+ u-1 )

_chk:           lda 0,x                 ; any chars left?
                ora 1,x
                beq _done

                lda (2,x)               ; is 1st char whitespace?
                jsr is_whitespace
                bcs _fwd

_done:
z_minus_leading:
                rts
.scend


; ## MINUS_TRAILING ( addr u1 -- addr u2 ) "Remove trailing spaces"
; ## "-trailing"  auto  ANS string
        ; """https://forth-standard.org/standard/string/MinusTRAILING
        ; Remove trailing spaces
        ; """
.scope
xt_minus_trailing:
                jsr underflow_2
minus_trailing_nouf:

                lda 2,x         ; tmp1 = addr + (u1 & $ff00)
                sta tmp1
                clc
                lda 3,x
                adc 1,x
                sta tmp1+1

                ldy 0,x         ; Y = u1 & $00ff

_back:          dey             ; move back 1
                cpy #$ff
                bne +
                dec 1,x
                bmi _done
                dec tmp1+1
*
                lda (tmp1),y    ; check char
                cmp #AscSP
                beq _back
_done:
                iny             ; move forward 1
                bne +
                inc 1,x
*               sty 0,x         ; save LSB of u2
z_minus_trailing:
                rts
.scend


; ## MOD ( n1 n2 -- n ) "Divide NOS by TOS and return the remainder"
; ## "mod"  auto  ANS core
        ; """https://forth-standard.org/standard/core/MOD
        ; """
xt_mod:
                jsr underflow_2

                jsr slash_mod_nouf
                inx             ; DROP quotient
                inx
z_mod:
                rts


; ## MOVE ( addr1 addr2 u -- ) "Copy bytes"
; ## "move"  auto  ANS core
        ; """https://forth-standard.org/standard/core/MOVE
        ; Copy u "address units" from addr1 to addr2. Since our address
        ; units are bytes, this is just a front-end for CMOVE and CMOVE>. This
        ; is actually the only one of these three words that is in the CORE
        ; set.
        ;
        ; This word must not be natively compiled
        ; """
.scope
xt_move:
                ; We let CMOVE and CMOVE> check if there is underflow or
                ; we've been told to copy zero bytes

                ; compare source & destination addresses
                lda 2,x                 ; LSB of addr2
                cmp 4,x                 ; LSB of addr1
                lda 3,x                 ; MSB of addr2
                sbc 5,x                 ; MSB of addr1
                bcs _to_move_up         ; we want CMOVE>
                jmp xt_cmove            ; JSR/RTS

_to_move_up:    jmp xt_cmove_up         ; JSR/RTS
z_move:
.scend


; ## NAME_TO_INT ( nt -- xt ) "Convert Name Token to Execute Token"
; ## "name>int"  tested  Gforth
; TODO deal with compile-only words
        ; """See
        ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
        ; """
; Also saves nt in tmp3.
.scope
xt_name_to_int:
                jsr underflow_1
name_to_int_nouf:
                lda 0,x
                ldy 1,x
                sta tmp3
                sty tmp3+1

                ldy #nt_xt
                lda (tmp3),y
                sta 0,x
                iny
                lda (tmp3),y
                sta 1,x

z_name_to_int:  rts
.scend


; ## NAME_TO_STRING ( nt -- addr u ) "Given a name token, return string of word"
; ## "name>string"  tested  Gforth
        ; """http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html"""
.scope
xt_name_to_string:
                jsr underflow_1

                dex
                dex

                ; the length of the string is at nt_lemgth in the
                ; header pointed to by nt
                lda (2,x)
                sta 0,x
                stz 1,x

                ; the string itself always starts nt_name bytes down in the word header
                lda 2,x         ; LSB
                clc
                adc #nt_name
                sta 2,x
                bcc +           ; MSB
                inc 3,x
*
z_name_to_string:
                rts
.scend

; ## NC_LIMIT ( -- addr ) "Return address where NC-LIMIT value is kept"
; ## "nc-limit"  tested  Tali Forth
.scope
xt_nc_limit:
                lda #nc_limit
                jmp PsuZA
z_nc_limit:
.scend


; ## NEGATE ( n -- n ) "Two's complement"
; ## "negate"  auto  ANS core
        ; """https://forth-standard.org/standard/core/NEGATE"""
xt_negate:
                jsr underflow_1
negate_nouf:
                lda #0
                sec
                sbc 0,x         ; LSB
                sta 0,x
                lda #0
                sbc 1,x         ; MSB
                sta 1,x

z_negate:       rts


negate_NOS_nouf:
                sec
                lda #0          ; LSB
                sbc 2,x
                sta 2,x
                lda #0          ; MSB
                sbc 3,x
                sta 3,x
                rts


; ## NEVER_NATIVE ( -- ) "Flag last word as never natively compiled"
; ## "never-native"  auto  Tali Forth
xt_never_native:
                jsr current_to_dp
                ldy #nt_status
                lda (dp),y
                ora #NN         ; Make sure NN flag is set
                and #$ff-AN     ; and AN flag is clear.
                sta (dp),y
z_never_native:
                rts


; ## NIP ( b a -- a ) "Delete NOS"
; ## "nip"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/NIP"""
xt_nip:
                jsr underflow_2
nip_nouf:
                lda 0,x         ; LSB
                sta 2,x
                lda 1,x         ; MSB
                sta 3,x
                inx
                inx

z_nip:          rts


; ## NOT_EQUALS ( n m -- f ) "Return a true flag if TOS != NOS"
; ## "<>"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/ne
        ;
        ; This is just a variant of EQUAL, we code it separately
        ; for speed.
        ; """
.scope
xt_not_equals:
                jsr underflow_2

                ldy #$ff                ; default is true

                lda 0,x                 ; LSB
                cmp 2,x
                bne _not_equal

                ; LSB is equal
                lda 1,x                 ; MSB
                cmp 3,x
                bne _not_equal

                iny                     ; false

_not_equal:
                inx
                inx
                sty 0,x
                sty 1,x

z_not_equals:   rts
.scend


; ## NOT_ROTE ( a b c -- c a b ) "Rotate upwards"
; ## "-rot"  auto  Gforth
        ; """http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Data-stack.html"""
.scope
xt_not_rote:
                jsr underflow_3
not_rote_nouf:
                ldy 1,x         ; MSB first
                lda 3,x
                sta 1,x
                lda 5,x
                sta 3,x
                sty 5,x

                ldy 0,x         ; LSB second
                lda 2,x
                sta 0,x
                lda 4,x
                sta 2,x
                sty 4,x

z_not_rote:     rts
.scend


; ## NUMBER ( addr u -- u | d ) "Convert a number string"
; ## "number"  auto  Tali Forth
        ; """Convert a number string to a double or single cell number. This
        ; is a wrapper for >NUMBER and follows the convention set out in the
        ; "Forth Programmer's Handbook" (Conklin & Rather) 3rd edition p. 87.
        ; Based in part on the "Starting Forth" code
        ; https://www.forth.com/starting-forth/10-input-output-operators/
        ; Gforth uses S>NUMBER? and S>UNUMBER? which return numbers and a flag
        ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Number-Conversion.html
        ; Another difference to Gforth is that we follow ANS Forth that the
        ; dot to signal a double cell number is required to be the last
        ; character of the string.
        ;
        ; Number calls >NUMBER which uses tmp1, tmp2, so we can't use them here, which is
        ; a pain.
        ;"""
.scope
xt_number:
                jsr underflow_2

                ; set up stack for >NUMBER, which means
                ; we have to go ( addr u --> ud addr u )
                jsr two_dup_nouf
                stz 4,x         ; clear ud
                stz 5,x
                stz 6,x
                stz 7,x

                lda base        ; save old base in case we change it
                pha

                stz tmpdsp+1    ; init to not negative
                lda #$20        ; init to single number
                trb status

                jsr _get        ; any chars left?
                beq _all_converted

                cmp #$2d        ; ASCII for "-"
                bne +
                inc tmpdsp+1    ; set negative flag
                jsr _next       ; forward to next char
                beq _all_converted
*
                cmp #'$         ; hex?
                bne +
                jsr xt_hex      ; change base to hex
                jsr _next       ; forward to next char
                beq _all_converted
*

                jsr xt_to_number        ; ( ud addr u -- ud addr u )

                jsr _get        ; any chars not converted?
                beq _all_converted

                cmp #'.         ; a dot indicating double_cell?
                bne _cvt_err
                lda #$20        ; set the "double" flag
                tsb status
                jsr _next       ; get next char
                beq _all_converted

_cvt_err:       ; Something went wrong, we still have characters left over,
                ; so we print an error and abort.
                lda #$3e        ; ASCII for ">"
                jsr emit_a
                jsr xt_type
                lda #$3c        ; ASCII for "<"
                jsr emit_a
                jsr xt_space

                lda #err_syntax
                jsr error

_all_converted:
                inx             ; drop the string info
                inx
                inx
                inx

                lda tmpdsp+1    ; did it have a "-"?
                beq +
                jsr dnegate_nouf
*

                ; We have a double-cell number on the Data Stack that might
                ; actually be single-cell
                lda #$20        ; flag for double
                bit status
                bne +

                inx             ; convert double to single
                inx

*
                pla             ; restore saved base
                sta base
z_number:       rts


_next:          ; step to next char
                inc 2,x         ; start one character later
                bne +
                inc 3,x
*
                dec 0,x         ; decrease string length by one

_get:           ; get current char
                lda (2,x)       ; get next char
                ldy 0,x         ; Z=no more chars
                rts
.scend


; ## NUMBER_SIGN ( ud -- ud ) "Add character to pictured output string"
; ## "#"  auto  ANS core
        ; """https://forth-standard.org/standard/core/num
        ; Add one char to the beginning of the pictured output string.
        ; """

xt_number_sign:
                jsr underflow_2         ; double number
number_sign_nouf:

                ; like UD/MOD, but byte divisor & remainder left in A
                lda #0
                ldy #33
                clc
_A:             rol
                cmp base
                bcc +
                sbc base
*               rol 2,x
                rol 3,x
                rol 0,x
                rol 1,x
                dey
                bne _A

                ; Convert the number that is left over to an ASCII character.
                cmp #10
                bcc +
                adc #6      ; 'A'-'0'-10-1
*               adc #$30    ; '0'

                jmp hold_A
z_number_sign:


; ## NUMBER_SIGN_GREATER ( d -- addr u ) "Finish pictured number conversion"
; ## "#>"  auto  ANS core
        ; """https://forth-standard.org/standard/core/num-end
        ; Finish conversion of pictured number string, putting address and
        ; length on the Data Stack.
        ;
        ; Original Forth is  2DROP HLD @ PAD OVER -
        ; Based on
        ; https://github.com/philburk/pforth/blob/master/fth/numberio.fth
        ; """
xt_number_sign_greater:
                jsr underflow_2         ; double number

                ; The start address lives in tohold
                lda tohold
                sta 0,x         ; LSB of tohold
                sta 2,x
                lda tohold+1
                sta 1,x         ; MSB of addr
                sta 3,x         ; ( addr addr )

                ; The length of the string is pad - addr
                jsr xt_pad      ; ( addr addr pad )

                sec
                lda 0,x         ; LSB of pad address
                sbc 2,x
                sta 2,x

                lda 1,x         ; MSB, which should always be zero
                sbc 3,x
                sta 3,x         ; ( addr u pad )

                inx
                inx

z_number_sign_greater:
                rts


; ## NUMBER_SIGN_S ( d -- addr u ) "Completely convert pictured output"
; ## "#s"  auto  ANS core
        ; """https://forth-standard.org/standard/core/numS
        ; Completely convert number for pictured numerical output.
        ;
        ; Based on
        ; https://github.com/philburk/pforth/blob/master/fth/system.fth
        ; Original Forth code  BEGIN # 2DUP OR 0= UNTIL
        ; """
.scope
xt_number_sign_s:
                jsr underflow_2
number_sign_s_nouf:
_loop:
                ; convert a single number ("#")
                jsr number_sign_nouf

                ; stop when double-celled number in TOS is zero:
                lda 0,x
                ora 1,x
                ora 2,x
                ora 3,x
                bne _loop

z_number_sign_s:
                rts
.scend


; ## OF (C: -- of-sys) (x1 x2 -- |x1) "Conditional flow control"
; ## "of"  auto  ANS core ext
        ; """http://forth-standard.org/standard/core/OF"""
.scope
xt_of:
                lda #<_runtime
                ldy #>_runtime
                jsr cmpl_subroutine

                lda #$f0
                ldy #3      ; beq *+5
                jsr cmpl_word

                lda #$4c    ; jmp abs
                jsr cmpl_a
                jsr xt_here ; Put the origination address on the stack for else/then
                jsr cmpl_word0  ; Stuff zero in for the branch address right now.

Z_of:           rts

_runtime:
                lda 0,x     ; pop OF value
                ldy 1,x
                inx
                inx

                cmp 0,x     ; compare with index
                bne _ne
                tya
                cmp 1,x
                bne _ne

                inx         ; drop index
                inx
                lda #0      ; return Z=1
_ne:            rts

.scend


; ## ONE ( -- n ) "Push the number 1 to the Data Stack"
; ## "1"  auto  Tali Forth
        ; """This is also the code for EDITOR-WORDLIST"""
xt_editor_wordlist:
xt_one:
                lda #1

PsuZA:  ; push A zero-extened onto the param stack
                dex
                dex
                sta 0,x
                stz 1,x

z_editor_wordlist:
z_one:
                rts


; ## ONE_MINUS ( u -- u-1 ) "Decrease TOS by one"
; ## "1-"  auto  ANS core
        ; """https://forth-standard.org/standard/core/OneMinus"""
.scope
xt_one_minus:
                jsr underflow_1
one_minus_nouf:
                lda 0,x
                bne +
                dec 1,x
*               dec 0,x

z_one_minus:    rts
.scend


xt_two_plus_nouf:
                inc 0,x
                bne one_plus_nouf
                inc 1,x
                ; fall thru to xt_one_plus


; ## ONE_PLUS ( u -- u+1 ) "Increase TOS by one"
; ## "1+"  auto  ANS core
        ; """https://forth-standard.org/standard/core/OnePlus
        ;
        ; Code is shared with CHAR-PLUS
        ; """
.scope
xt_char_plus:
xt_one_plus:
                jsr underflow_1
one_plus_nouf:
                inc 0,x
                bne _done
                inc 1,x
_done:
z_char_plus:
z_one_plus:     rts
.scend


; ## ONLY ( -- ) "Set search order to minimum wordlist"
; ## "only"  auto  ANS search ext
        ; """https://forth-standard.org/standard/search/ONLY"""
.scope
xt_only:
                ; Put -1 on data stack.
                dex
                dex
                lda #$FF
                sta 0,x
                sta 1,x
                ; Invoke set-order to set the minimum search order.
                jmp xt_set_order
z_only:
.scend


; ## OR ( m n -- n ) "Logically OR TOS and NOS"
; ## "or"  auto  ANS core
        ; """https://forth-standard.org/standard/core/OR"""
xt_or:
                jsr underflow_2

                lda 0,x
                ora 2,x
                sta 2,x

                lda 1,x
                ora 3,x
                sta 3,x

                inx
                inx

z_or:           rts


; ## ORDER ( -- ) "Print current word order list and current WID"
; ## "order"  auto  ANS core
        ; """https://forth-standard.org/standard/search/ORDER
        ; Note the search order is displayed from first search to last
        ; searched and is therefore exactly the reverse of the order in which
        ; Forth stacks are displayed.
        ;
        ; A Forth implementation of this word is:
        ;
        ;   : .wid ( wid -- )
        ;   dup 0=  if ." Forth "  drop    else
        ;   dup 1 = if ." Editor " drop    else
        ;   dup 2 = if ." Assembler " drop else
        ;   dup 3 = if ." Root " drop      else
        ;              . ( just print the number )
        ;   then then then then ;
        ;
        ; : ORDER ( -- )
        ;   cr get-order 0 ?do .wid loop
        ;   space space get-current .wid ;
        ;
        ; This is an interactive program, so speed
        ; is not as important as size. We assume we do not have more than 255
        ; wordlists.
        ; """
.scope
xt_order:
                jsr xt_cr
                jsr xt_get_order        ; ( wid_n ... wid_1 n )

                ; Paranoid: Check if there are no wordlists, a rather
                ; pathological case. this would mean ( 0 ) on the stack. In
                ; that case, we just drop n and run
                lda 0,x                 ; assumes no more than 255 wordlists
                beq _drop_done

_have_wordlists:
                ; We arrive here with the LSB of TOS in A, the number of WIDs
                ; on the stack
                tay
_loop:
                inx
                inx                     ; DROP, now ( wid_n ... wid_1 )
                lda 0,x

                phy
                jsr _print_wid_string   ; internal helper function
                ply

                dey
                bne _loop

                ; We've printed the wordlists, now we add the current wordlist.
                ; This follows the convention of Gforth
                jsr xt_space
                jsr xt_space
                jsr xt_get_current      ; ( wid )

                lda 0,x
                jsr _print_wid_string
                jsr xt_cr

_drop_done:
                inx
                inx
z_order:
                rts

_print_wid_string:
        ; """Helper function for ORDER: Given a WID in A, print the
        ; corresponding string. If there is no such word list defined, just
        ; print the number. Assumes we will not have more than 256 WIDs; also
        ; assumes we have just loaded A so Z reflects status of byte.  In
        ; theory, we could speed this up by having the WID be the same as the
        ; number of the strings. However, ORDER is used rather infrequently and
        ; this would make changes to the strings.asm file very dangerous, so we
        ; follow the slightly more complicated route with a translation table.
        ; """
                ; If the WID is larger than 3, we have no string avaliable and
                ; just print the number.
                ; See http://6502.org/tutorials/compare_instructions.html
                ; for details
                cmp #4
                bcc _output_string      ; less than 4, print a real string

                ; Our WID is not less than 4, that is, 4 or larger. We just
                ; print the number
                dex
                dex
                sta 0,x
                stz 1,x
                jmp xt_u_dot            ; JSR/RTS as this routine is not compiled

_output_string:
                ; Get the string number based on WID 0 to 3
                tay
                lda _wid_data,y

                ; Print without a line feed
                tay
                jmp print_string_no_lf_Y  ; JSR/RTS as this routine is not compiled

_wid_data:
        ; Table of string numbers (see strings.asm) indexed by the WID if
        ; less than 4.
        .byte s_wid_forth    -string0 ; WID 0: "Forth"
        .byte s_wid_editor   -string0 ; WID 1: "Editor"
        .byte s_wid_assembler-string0 ; WID 2: "Assembler"
        .byte s_wid_root     -string0 ; WID 3: "Root"
.scend


; ## OUTPUT ( -- addr ) "Return the address of the EMIT vector address"
; ## "output"  tested  Tali Forth
xt_output:
        ; """Return the address where the jump target for EMIT is stored (but
        ; not the vector itself). By default, this will hold the value of
        ; kernel_putc routine, but this can be changed by the user, hence this
        ; routine.
        ; """
                lda #output
                jmp PsuZA
z_output:


; ## OVER ( b a -- b a b ) "Copy NOS to TOS"
; ## "over"  auto  ANS core
        ; """https://forth-standard.org/standard/core/OVER"""
xt_over:
                jsr underflow_2
over_nouf:
                lda 2,x
                ldy 3,x

                dex         ; PsuYA
                dex
                sta 0,x
                sty 1,x

z_over:         rts


; ## PAD ( -- addr ) "Return address of user scratchpad"
; ## "pad"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/PAD
        ; Return address to a temporary area in free memory for user. Must
        ; be at least 84 bytes in size (says ANS). It is located relative to
        ; the compile area pointer (CP) and therefore varies in position.
        ; This area is reserved for the user and not used by the system
        ; """
xt_pad:
                dex
                dex

                lda cp
                clc
                adc #padoffset  ; assumes padoffset one byte in size
                sta 0,x

                lda cp+1
                adc #0          ; only need carry
                sta 1,x

z_pad:          rts


; ## PAGE ( -- ) "Clear the screen"
; ## "page"  tested  ANS facility
        ; """https://forth-standard.org/standard/facility/PAGE
        ; Clears a page if supported by ANS terminal codes. This is
        ; Clear Screen ("ESC[2J") plus moving the cursor to the top
        ; left of the screen
        ; """
xt_page:
                lda #AscESC
                jsr emit_a
                lda #$5B        ; ASCII for "["
                jsr emit_a
                lda #'2
                jsr emit_a
                lda #'J
                jsr emit_a

                ; move cursor to top left of screen
                jsr xt_zero_dot
                jsr xt_at_xy

z_page:         rts


; ## PAREN ( -- ) "Discard input up to close paren ( comment )"
; ## "("  auto  ANS core
        ; """http://forth-standard.org/standard/core/p"""
.scope
xt_paren:
                ; Put a right paren on the stack.
                lda #41     ; Right parenthesis
                jsr PsuZA
                ; Call parse.
                jsr xt_parse
                ; Throw away the result.
                inx
                inx
                inx
                inx
z_paren:        rts
.scend


; ## PARSE_NAME ( "name" -- addr u ) "Parse the input"
; ## "parse-name"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/PARSE-NAME
        ; Find next word in input string, skipping leading whitespace. This is
        ; a special form of PARSE and drops through to that word. See PARSE
        ; for more detail. We use this word internally for the interpreter
        ; because it is a lot easier to use. Reference implementations at
        ; http://forth-standard.org/standard/core/PARSE-NAME and
        ; http://www.forth200x.org/reference-implementations/parse-name.fs
        ; Roughly, the word is comparable to BL WORD COUNT. -- Note that
        ; though the ANS standard talks about skipping "spaces", whitespace
        ; is actually perfectly legal (see for example
        ; http://forth-standard.org/standard/usage#subsubsection.3.4.1.1).
        ; Otherwise, PARSE-NAME chokes on tabs.
        ; """
.scope
xt_parse_name:
                ; To enable the compilation of the high-level Forth words
                ; in forth-words.asm and user-words.asm at boot time,
                ; PARSE-NAME and PARSE must be able to deal with 16-bit string
                ; lengths. This is a pain on an 8-bit machine. The pointer
                ; to the current location is in toin (>IN). We need to check,
                ; worst case, the characters from cib+toin to cib+ciblen, and
                ; we can't just use Y as an index.


                lda cib                 ; tmp2 = cib + (ciblen & 0xff00)
                sta tmp2
                lda cib+1
                clc
                adc toin+1
                sta tmp2+1

                ldy toin                ; Y = (ciblen & 0x00ff)

_skip_loop:
                cpy ciblen              ; toin>=ciblen?
                lda toin+1
                sbc ciblen+1
                bcs _empty_line

                lda (tmp2),y            ; A=cib[toin]
                jsr is_whitespace
                bcc _char_found

                ; Char is still whitespace, continue
                iny
                bne _skip_loop
                inc tmp2+1
                inc toin+1
                bra _skip_loop

_empty_line:
                ; Neither the ANS Forth nor the Gforth documentation say
                ; what to return as an address if a string with only
                ; spaces is given. For speed reasons, we just return junk
                ; NOS, with the TOS zero as per standard
;                dex
;                dex
;                jsr xt_zero             ; TOS is zero
;
;                jmp z_parse_name        ; skip over PARSE

_char_found:
                sty toin

                ; prepare Data Stack for PARSE by adding space
                ; as the delimiter
                jsr xt_bl
.scend

; ## PARSE ( "name" c -- addr u ) "Parse input with delimiter character"
; ## "parse"  tested  ANS core ext
        ; """https://forth-standard.org/standard/core/PARSE
        ; Find word in input string delimited by character given. Do not
        ; skip leading delimiters -- this is the main difference to PARSE-NAME.
        ; PARSE and PARSE-NAME replace WORD in modern systems. ANS discussion
        ; http://www.forth200x.org/documents/html3/rationale.html#rat:core:PARSE
        ;
        ;     cib  cib+toin   cib+ciblen
        ;      v      v            v
        ;     |###################|
        ;
        ;     |------>|  toin (>IN)
        ;     |------------------->|  ciblen
        ;
        ; The input string is stored starting at the address in the Current
        ; Input Buffer (CIB), the length of which is in CIBLEN. While searching
        ; for the delimiter, TOIN (>IN) points to the where we currently are.
        ; Since PARSE does not skip leading delimiters, we assume we are on a
        ; useful string if there are any characters at all. As with
        ; PARSE-NAME, we must be able to handle strings with a length of
        ; 16-bit for EVALUATE, which is a pain on an 8-bit machine.
        ; """
.scope
xt_parse:
                jsr underflow_1

                ; prepare the Data Stack for the return value
                dex
                dex

                ; Save the delimiter in tmptos.
                lda 2,x
                sta tmptos

                ; Calculate the beginning of the string, which is also the
                ; address to return
                lda toin        ; 0,x = toin  ; 2,x = cib + toin
                sta 0,x
                clc
                adc cib
                sta 2,x
                lda toin+1
                sta 1,x
                adc cib+1
                sta 3,x

                lda cib         ; tmp2 = cib + (toin & 0xff00)
                sta tmp2
                clc
                lda cib+1
                adc toin+1
                sta tmp2+1
                ldy toin        ; Y = (toin & 0x00ff)

                ; Initialize the offset we use to adjust EOL or found delimiter
                stz tmptos+1
_loop:
                ; If we are at the end of the string, quit
                cpy ciblen
                lda toin+1
                sbc ciblen+1
                bcs _eol

                ; We have to do this the hard way. In fact, it's really
                ; hard since if we are dealing with a SPACE, the standard
                ; wants us to skip all whitespace, not just spaces. Otherwise,
                ; Tali would choke on tabs between words. For details, see
                ; http://forth-standard.org/standard/file#subsection.11.3.5
                ; In theory, we could make this faster by defining a delimiter
                ; that is 00 as the sign that we skip all whitespace, thereby
                ; avoiding having to test every time. However, somebody,
                ; somewhere might want to parse a zero-delimited list. Since
                ; any byte value could be chosen for that, we just test for
                ; a space every single time for the moment.
                lda tmptos
                cmp (tmp2),y
                beq _found_delimiter
                cmp #AscSP
                bne _next

                ; The delimiter is a space, so we're looking for all
                ; whitespace
                lda (tmp2),y
                jsr is_whitespace
                bcs _found_delimiter

_next:          ; Not a delimiter, next character
                iny
                bne _loop
                inc tmp2+1
                inc toin+1
                bra _loop

_found_delimiter:
                ; Increase the offset: If we've found a delimiter, we want
                ; TOIN to point to the character after it, not the delimiter
                ; itself
                inc tmptos+1
_eol:

                ; The length of the new string is toin-0,x
                tya
                sec
                sbc 0,x
                sta 0,x
                lda toin+1
                sbc 1,x
                sta 1,x

                ; Add in the delimiter
                tya
                clc
                adc tmptos+1
                sta toin
                bcc _done
                inc toin+1
_done:
z_parse_name:
z_parse:        rts
.scend


; ## PICK ( n n u -- n n n ) "Move element u of the stack to TOS"
; ## "pick"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/PICK
        ; Take the u-th element out of the stack and put it on TOS,
        ; overwriting the original TOS. 0 PICK is equivalent to DUP, 1 PICK to
        ; OVER. Note that using PICK is considered poor coding form. Also note
        ; that FIG Forth has a different behavior for PICK than ANS Forth.
        ; """
.scope
xt_pick:
                ; Checking for underflow is difficult because it depends on
                ; which element we want to grab. We could probably figure
                ; something out, but it wouldn't work with underflow stripping
                ; Since using PICK is considered poor form anyway, we just
                ; leave it as it is

                asl 0,x         ; we assume u < 128 (stack is small)
                txa
                adc 0,x
                tay

                lda 0002,y
                sta 0,x
                lda 0003,y
                sta 1,x

z_pick:         rts
.scend


; ## PLUS ( n n -- n ) "Add TOS and NOS"
; ## "+"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Plus"""
xt_plus:
                jsr underflow_2
plus_nouf:
                clc
                lda 0,x         ; LSB
                adc 2,x
                sta 2,x
                lda 1,x         ; MSB. No CLC, conserve carry bit
                adc 3,x
                sta 3,x

                inx
                inx

z_plus:         rts



; ## PLUS_STORE ( n addr -- ) "Add number to value at given address"
; ## "+!"  auto  ANS core
        ; """https://forth-standard.org/standard/core/PlusStore"""
xt_plus_store:
                jsr underflow_2

                clc             ; LSB
                lda (0,x)
                adc 2,x
                sta (0,x)

                inc 0,x         ; increment addr to MSB
                bne +
                inc 1,x
*
                lda (0,x)       ; MSB
                adc 3,x
                sta (0,x)

                inx             ; clean stack
                inx
                inx
                inx

z_plus_store:   rts


; ## ONE_PLUS_STORE ( n addr -- ) "Add 1 to value at given address"
; ## "1+!"  auto
xt_one_plus_store:
                jsr underflow_1

                lda (0,x)       ; LSB
                inc
                sta (0,x)
                bne _z

                inc 0,x         ; increment addr to MSB
                bne +
                inc 1,x
*
                lda (0,x)       ; MSB
                inc
                sta (0,x)
_z:
                inx             ; clean stack
                inx

z_one_plus_store: rts


; ## POSTPONE ( -- ) "Change IMMEDIATE status (it's complicated)"
; ## "postpone"  auto   ANS core
        ; """https://forth-standard.org/standard/core/POSTPONE
        ; Add the compilation behavior of a word to a new word at
        ; compile time. If the word that follows it is immediate, include
        ; it so that it will be compiled when the word being defined is
        ; itself used for a new word. Tricky, but very useful.
        ;
        ; Because POSTPONE expects a word (not an xt) in the input stream (not
        ; on the Data Stack). This means we cannot build words with
        ; "jsr xt_postpone, jsr <word>" directly.
        ; """
.scope
xt_postpone:
                jsr xt_parse_name               ; ( -- addr n )

                ; if there was no word provided, complain and quit
                lda 0,x
                ora 1,x
                beq _err1

                jsr xt_find_name                ; ( -- nt | 0 )

                ; if word not in Dictionary, complain and quit
                beq _err1

                ; keep a copy of nt for later
                lda 0,x
                sta tmp1
                lda 1,x
                sta tmp1+1

                ; We need the xt instead of the nt
                jsr name_to_int_nouf            ; ( nt -- xt )

                ; See if this is an immediate word. This is easier
                ; with nt than with xt.
                ldy #nt_status
                lda (tmp1),y
                and #IM         ; mask all but Intermediate flag
                beq _not_immediate

                ; we're immediate, so instead of executing it right now, we
                ; compile it. xt is TOS, so this is easy. The RTS at the end
                ; takes us back to the original caller
                jsr xt_compile_comma
                bra _done

_err1:          lda #err_noname
                jmp error

_not_immediate:
                ; This is not an immediate word, so we enact "deferred
                ; compilation" by including ' <NAME> COMPILE, which we do by
                ; compiling xt as a LITERAL, and
                ; a subroutine jump to COMPILE,
                jsr xt_literal

                ldy #>xt_compile_comma
                lda #<xt_compile_comma
                jsr cmpl_subroutine
_done:
z_postpone:     rts
.scend


; ## PREVIOUS ( -- ) "Remove the first wordlist in the search order"
; ## "previous"  auto  ANS search ext
        ; """http://forth-standard.org/standard/search/PREVIOUS"""
.scope
xt_previous:
                jsr xt_get_order
                jsr xt_nip
                jsr xt_one_minus
                jmp xt_set_order
z_previous:
.scend



; ## QUESTION ( addr -- ) "Print content of a variable"
; ## "?"  tested  ANS tools
        ; """https://forth-standard.org/standard/tools/q
        ;
        ; Only used interactively. Since humans are so slow, we
        ; save size and just go for the subroutine jumps
        ; """
xt_question:
                ; FETCH takes care of underflow check
                jsr xt_fetch
                jmp xt_dot
z_question:


; ## QUESTION_DUP ( n -- 0 | n n ) "Duplicate TOS non-zero"
; ## "?dup"  auto  ANS core
        ; """https://forth-standard.org/standard/core/qDUP"""
.scope
xt_question_dup:
                jsr underflow_1

                ; Check if TOS is zero
                lda 0,x
                ldy 1,x
                bne +
                cmp #0
                beq _done
*
                ; not zero, duplicate
                dex         ; PsuYA
                dex
                sta 0,x
                sty 1,x
_done:
z_question_dup: rts
.scend


; ## R_FETCH ( -- n ) "Get copy of top of Return Stack"
; ## "r@"  auto  ANS core
        ; """https://forth-standard.org/standard/core/RFetch
        ; This word is Compile Only in Tali Forth, though Gforth has it
        ; work normally as well
        ;
        ; An alternative way to write this word
        ; would be to access the elements on the stack directly like 2R@
        ; does, these versions should be compared at some point.
        ; """
xt_r_fetch:
                ; get the return address
                pla             ; LSB
                ply             ; MSB
                sta tmp1
                sty tmp1+1

                ; --- CUT FOR NATIVE COMPILE ---

                pla             ; get a copy of the actual top of Return Stack
                ply
                phy
                pha

                dex             ; PsuYA; push it on the param stack
                dex
                sta 0,x
                sty 1,x

                ; --- CUT FOR NATIVE COMPILE ---

                ; restore return value
                lda tmp1
                ldy tmp1+1
                phy
                pha

z_r_fetch:      rts


; ## R_FROM ( -- n )(R: n --) "Move top of Return Stack to TOS"
; ## "r>"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Rfrom
        ; Move Top of Return Stack to Top of Data Stack.
        ;
        ; We have to move
        ; the RTS address out of the way first. This word is handled
        ; differently for native and and subroutine compilation, see COMPILE,
        ; This is a compile-only word
        ; """

xt_r_from:
                ; Rescue the address of the return jump that is currently
                ; on top of the Return Stack. If this word is natively
                ; compiled, this is a total waste of time
                pla             ; LSB
                ply             ; MSB
                sta tmptos
                sty tmptos+1

                ; --- CUT FOR NATIVE CODING ---

                pla             ; LSB
                ply             ; MSB

                dex             ; PsuYA
                dex
                sta 0,x
                sty 1,x

                ; --- CUT FOR NATIVE CODING ---

                ; restore the return address
                lda tmptos
                ldy tmptos+1
                phy             ; MSB
                pha             ; LSB

z_r_from:       rts


; ## R_TO_INPUT ( -- ) ( R: n n n n -- ) "Restore input state from Return Stack"
; ## "r>input"  tested  Tali Forth
        ; """Restore the current input state as defined by insrc, cib, ciblen,
        ; and toin from the Return Stack.
        ;
        ; See INPUT_TO_R for a discussion of this word. Uses tmp1
        ; """
.scope
xt_r_to_input:

                ; We arrive here with the return address on the top of the
                ; 65c02's stack. We need to move it out of the way first
                pla
                sta tmp1
                pla
                sta tmp1+1

                ; This assumes that insrc is the first of eight bytes and
                ; toin+1 the last in the sequence we want to save from the Zero
                ; Page. Since we went in reverse order, insrc is now on the top
                ; of the Return Stack.
                ldy #0
_loop:
                pla
                sta insrc,y
                iny
                cpy #8
                bne _loop

                ; Restore address for return jump
                lda tmp1+1
                pha
                lda tmp1
                pha

z_r_to_input:   rts
.scend


; ## RECURSE ( -- ) "Copy recursive call to word being defined"
; ## "recurse"  auto  ANS core
        ; """https://forth-standard.org/standard/core/RECURSE
        ; """
.scope
xt_recurse:
                ; The whole routine amounts to compiling a reference to
                ; the word that is being compiled.

                ; we save the LSB and MSB of the xt of the word
                ; we are currently working on.  We first need to see if
                ; WORKWORD has the nt (: started the word) or the
                ; xt (:NONAME started the word).  Bit 6 in status tells us.
                bit status
                bvs _nt_in_workword

                ; This is a special :NONAME word.  Just copy the xt
                ; from WORKWORD into the dictionary.
                lda workword
                ldy workword+1
                bra _cmpl


_nt_in_workword:
                ; This is a regular : word, so the xt is four bytes down
                ; from the nt which we saved in WORKWORD. We could probably
                ; use NAME>INT here but this is simpler & faster.
                ldy #nt_xt+1
                lda (workword),y
                pha
                dey
                lda (workword),y
                ply

_cmpl:
                jsr cmpl_subroutine

z_recurse:      rts
.scend


; ## REFILL ( -- f ) "Refill the input buffer"
; ## "refill"  tested  ANS core ext
        ; """https://forth-standard.org/standard/core/REFILL
        ; Attempt to fill the input buffer from the input source, returning
        ; a true flag if successful. When the input source is the user input
        ; device, attempt to receive input into the terminal input buffer. If
        ; successful, make the result the input buffer, set >IN to zero, and
        ; return true. Receipt of a line containing no characters is considered
        ; successful. If there is no input available from the current input
        ; source, return false. When the input source is a string from EVALUATE,
        ; return false and perform no other action." See
        ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/The-Input-Stream.html
        ; and Conklin & Rather p. 156.  Note we dont have to care about blocks
        ; because REFILL is never used on blocks - Tali is able to evaluate the
        ; entire block as a 1024 byte string.
        ; """"
.scope
xt_refill:
                ; Get input source from SOURCE-ID.
                ; This is an optimized version of a subroutine jump to SOURCE-ID
                lda insrc               ; cheat: We only check LSB
                bne _src_not_kbd

                ; SOURCE-ID of zero means we're getting stuff from the keyboard
                ; with ACCEPT, which wants the address of the current input
                ; buffer NOS and the max number of characters to accept TOS
                dex
                dex
                dex
                dex

                lda cib                 ; address of CIB is NOS
                sta 2,x
                lda cib+1
                sta 3,x

                stz ciblen              ; go in with empty buffer
                stz ciblen+1

                lda #bsize              ; max number of chars is TOS
                sta 0,x
                stz 1,x                 ; cheat: We only accept max 255

                jsr xt_accept           ; ( addr n1 -- n2)

                ; ACCEPT returns the number of characters accepted, which
                ; belong in CIBLEN
                lda 0,x
                sta ciblen
                lda 1,x
                sta ciblen+1            ; though we only accept 255 chars

                ; make >IN point to beginning of buffer
                stz toin
                stz toin+1

                lda #$ff                ; overwrite with TRUE flag
                sta 0,x
                sta 1,x

                bra _done

_src_not_kbd:
                ; If SOURCE-ID doesn't return a zero, it must be a string in
                ; memory or a file (remember, TODO: no blocks in this version).
                ; If source is a string, we were given the flag -1 ($ffff)
                inc
                bne _src_not_string

                ; Simply return FALSE flag as per specification
                dex
                dex
                stz 0,x
                stz 1,x

                bra z_refill

_src_not_string:
                ; Since we don't have blocks, this must mean that we are trying
                ; to read from a file. However, we don't have files yet, so we
                ; report an error and jump to ABORT.
                lda #err_badsource
                jmp error

_done:
z_refill:       rts
.scend


; ## REPEAT (C: orig dest -- ) ( -- ) "Loop flow control"
; ## "repeat"  auto  ANS core
        ; """http://forth-standard.org/standard/core/REPEAT"""
.scope
xt_repeat:
                ; Run again first
                jsr xt_again

                ; Stuff HERE in for the branch address
                ; to get out of the loop
                jsr xt_here
                jsr xt_swap
                jsr xt_store

z_repeat:       rts
.scend


; ## RIGHT_BRACKET ( -- ) "Enter the compile state"
; ## "]"  auto  ANS core
        ; """https://forth-standard.org/standard/right-bracket
        ; This is an immediate word.
        ; """
xt_right_bracket:
                lda #$ff
                sta state
                sta state+1
z_right_bracket:
                rts


; ## ROOT_WORDLIST ( -- u ) "WID for the Root (minimal) wordlist"
; ## "root-wordlist"  tested  Tali Editor
xt_root_wordlist:
                lda #3          ; The WID for the Root wordlist is 3.
                jsr PsuZA
z_root_wordlist:
                rts


; ## ROT ( a b c -- b c a ) "Rotate first three stack entries downwards"
; ## "rot"  auto  ANS core
        ; """https://forth-standard.org/standard/core/ROT
        ; Remember "R for 'Revolution'" - the bottom entry comes out
        ; on top!
        ; """
.scope
xt_rot:
                jsr underflow_3
rot_nouf:
                ldy 5,x         ; MSB first
                lda 3,x
                sta 5,x
                lda 1,x
                sta 3,x
                sty 1,x

                ldy 4,x         ; LSB next
                lda 2,x
                sta 4,x
                lda 0,x
                sta 2,x
                sty 0,x

z_rot:          rts
.scend


; ## RSHIFT ( x u -- x ) "Shift TOS to the right"
; ## "rshift"  auto  ANS core
        ; """https://forth-standard.org/standard/core/RSHIFT"""
xt_rshift:
                jsr underflow_2

                ldy 0,x
                beq _done               ; if 0 shifts, quit

_loop:          lsr 3,x
                ror 2,x
                dey
                bne _loop
_done:
                inx
                inx

z_rshift:       rts


; ## S_BACKSLASH_QUOTE ( "string" -- )( -- addr u ) "Store string in memory"
; ## "s\""  auto  ANS core
        ; """https://forth-standard.org/standard/core/Seq
        ; Store address and length of string given, returning ( addr u ).
        ; ANS core claims this is compile-only, but the file set expands it
        ; to be interpreted, so it is a state-sensitive word, which in theory
        ; are evil. We follow general usage.  This is just like S" except
        ; that it allows for some special escaped characters.
        ; """
.scope
xt_s_backslash_quote:
                lda #$FF    ; handle escaped chars
                jmp s_quote_start
z_s_backslash_quote:
.scend


; ## SEARCH_WORDLIST ( caddr u wid -- 0 | xt 1 | xt -1) "Search for a word in a wordlist"
; ## "search_wordlist" auto ANS search
        ; """https://forth-standard.org/standard/search/SEARCH_WORDLIST"""
.scope
xt_search_wordlist:
                jsr underflow_3

                lda 0,x         ; pop wid
                inx
                inx

                jsr search_wordlist_A
                bne _done
                inx             ; return not_found
                inx
                stz 0,x
                stz 1,x
_done:
z_search_wordlist: rts
.scend


.scope
; Common routine to search a word list.
; In: A=wordlist index, stack contains addr u    ( name to search for )
; Out: Not found:  Z=1, stack unchanged, tmp1=0
;      Is  found:  Z=0, N=!immediate, stack contains xt flag, tmp1=nt
search_wordlist_A:
                clc             ; SEARCH-ORDER is array of bytes.
                adc #search_order_offset
                tay
                lda (up),y      ; Get the id byte, which is the offset
                                ; into the cell array WORDLISTS

                asl             ; tmp1 = nt of 1st word in list
                adc #wordlists_offset
                tay
                lda (up),y
                sta tmp1
                iny
                lda (up),y
                sta tmp1+1
                beq _nomatch    ; empty wordlist?

                lda 2,x         ; build addr_u ptr with same offset as
                sec             ; name in word header
                sbc #nt_name
                sta tmp2
                lda 3,x
                sbc #0
                sta tmp2+1

                bra _word_check

_word_next:     ; on to the next word header.
                ldy #nt_next_nt+1
                lda (tmp1),y
                pha
                dey
                lda (tmp1),y
                sta tmp1
                pla
                sta tmp1+1

                beq _nomatch    ; If we got a zero, we've walked the whole list

_word_check:
                lda 0,x         ; Are strings the same length?
                cmp (tmp1)      ;  (nt_length)
                bne _word_next

                ; Compare name characters. We go
                ; from back to front, because words like CELLS and CELL+ would
                ; take longer otherwise.

                adc #nt_name-1  ; Y indexes name in wordheader (carry set from cmp)
                tay
_char_next:     dey
                cpy #nt_name
                bcc _match

                lda (tmp1),y
                eor (tmp2),y
                beq _char_next  ; exact match?
                cmp #$20        ; more than possible case difference?
                bne _word_next
                lda (tmp1),y    ; is it A..Z or a..z?
                and #$df        ;   to uppercase
                sbc #'A         ;   carry set from cmp
                cmp #26         ;   'Z'+1-'A'
                bcc _char_next
                bra _word_next

_match:         ; We found a match!

                ldy #nt_xt       ; NOS = xt in wordheader
                lda (tmp1),y
                sta 2,x
                iny
                lda (tmp1),y
                sta 3,x

                ldy #nt_status
                lda (tmp1),y
                and #IM
                bne _immediate

                lda #$ff        ; We're not immediate, return -1
                sta 0,x
                sta 1,x
                rts             ; return Z=0, N=1

_immediate:     lda #1          ; We're immediate, return 1
                sta 0,x
                stz 1,x
                rts             ; return Z=0, N=0

_nomatch:       rts             ; return Z=1
.scend


; ## SEE ( "name" -- ) "Print information about a Forth word"
; ## "see" tested  ANS tools
        ; """https://forth-standard.org/standard/tools/SEE
        ; SEE takes the name of a word and prints its name token (nt),
        ; execution token (xt), size in bytes, flags used, and then dumps the
        ; code and disassembles it.
        ; """
.scope
xt_see:
                jsr xt_parse_name       ; ( addr u )
                jsr xt_find_name        ; ( nt | 0 )

                ; If we got back a zero we don't know that word and so we quit
                ; with an error
                lda 1,x
                bne +
                lda #err_noname
                jmp error
*
                jsr xt_cr

                ; We have a legal word, so let's get serious.
                lda base                ; Save the current number base
                pha
                jsr xt_hex              ; use hexadecimal instead.

                ldy #s_see_nt-string0
                jsr print_string_no_lf_Y

                jsr xt_dup              ; ( nt nt )
                jsr xt_u_dot
                jsr xt_space            ; ( nt )

                jsr xt_dup              ; ( nt nt )
                jsr name_to_int_nouf    ; ( nt xt )

                ldy #s_see_xt-string0
                jsr print_string_no_lf_Y

                jsr xt_dup              ; ( nt xt xt )
                jsr xt_u_dot
                jsr xt_cr               ; ( nt xt )

                ; We print letters for flags and then later follow it with 1 or
                ; 0 to mark if which flag is set
                ldy #s_see_flags-string0
                jsr print_string_no_lf_Y

                jsr xt_over             ; ( nt xt nt )
                jsr one_plus_nouf       ; ( nt xt nt+1 ) (nt_status)
                lda (0,x)
                inx
                inx                     ; ( nt xt )

                ; This is crude, but for the moment it is good enough
                ldy #7                  ; Not all bits are used
_flag_loop:
                pha
                and #%00000001
                ora #$30                ; ASCII "0"
                jsr emit_a
                jsr xt_space
                pla
                ror                     ; next flag
                dey
                bne _flag_loop

                jsr xt_cr

                ; Figure out the size
                ldy #s_see_size-string0
                jsr print_string_no_lf_Y

                jsr xt_swap             ; ( xt nt )
                jsr xt_wordsize         ; ( xt u )
                jsr xt_dup              ; ( xt u u ) for DUMP and DISASM
                jsr xt_decimal
                jsr xt_u_dot            ; ( xt u )
                jsr xt_hex
                jsr xt_cr

                ; Dump hex and disassemble
                jsr xt_two_dup          ; ( xt u xt u )
                jsr xt_dump
                jsr xt_cr
                jsr xt_disasm

                pla
                sta base

z_see:          rts
.scend


; ## SET_CURRENT ( wid -- ) "Set the compilation wordlist"
; ## "set-current" auto ANS search
        ; """https://forth-standard.org/standard/search/SET-CURRENT"""
.scope
xt_set_current:
                jsr underflow_1

                ; Save the value from the data stack.
                lda 0,x         ; pop
                inx
                inx
                ldy #current_offset
                sta (up),y      ; CURRENT is byte variable so only LSB is used.

z_set_current:  rts
.scend


; ## SET_ORDER ( wid_n .. wid_1 n -- ) "Set the current search order"
; ## "set-order" auto ANS search
        ; """https://forth-standard.org/standard/search/SET-ORDER"""
.scope
xt_set_order:
                ; Test for -1 TOS
                lda 1,x
                bpl _start

                ; There is a -1 TOS.  Replace it with the default
                ; search order, which is just the FORTH-WORDLIST.
                lda #3          ; ROOT-WORDLIST is 3
                sta 0,x
                stz 1,x
                lda #1          ; Count is 1.
                jsr PsuZA
                ; Continue processing with ( forth-wordlist 1 -- )

_start:


                ; Set #ORDER - the number of wordlists in the search order.
                lda 0,x         ; pop count
                inx
                inx
                ldy #num_order_offset
                sta (up),y      ; #ORDER is a byte variable.
                sta tmp1        ; Save a copy for zero check and looping.
                                ; Only the low byte is saved in tmp1 as
                                ; only 8 wordlists are allowed.

                tay             ; zero wordlists?
                beq _done

                ; Move the wordlist ids from the data stack to the search order.
                ldy #search_order_offset
_loop:
                ; Move one wordlist id over into the search order.
                lda 0,x         ; The search order is a byte array
                sta (up),y      ; so only save the LSB
                iny
                ; Remove it from the data stack.
                inx
                inx
                ; See if that was the last one to process (first in the list).
                dec tmp1
                bne _loop
_done:
z_set_order:    rts
.scend


; ## S_QUOTE ( "string" -- )( -- addr u ) "Store string in memory"
; ## "s""  auto  ANS core
        ; """https://forth-standard.org/standard/core/Sq
        ; Store address and length of string given, returning ( addr u ).
        ; ANS core claims this is compile-only, but the file set expands it
        ; to be interpreted, so it is a state-sensitive word, which in theory
        ; are evil. We follow general usage.
        ;
        ; Can also be realized as
        ;     : S" [CHAR] " PARSE POSTPONE SLITERAL ; IMMEDIATE
        ; but it is used so much we want it in code.
        ; """
.scope
xt_s_quote:
                ; tmp2 will be used to determine if we are handling
                ; escaped characters or not.  In this case, we are
                ; not, so set it to zero.
                lda #0
s_quote_start:  ; escaped char processing flag in A
                sta tmp2

                ; What happens next depends on the state (which is bad, but
                ; that's the way it works at the moment).
                lda state
                ora state+1             ; paranoid
                bne cmpl_s_quote

                ; We're interpretating
                jsr cmpl_string     ; save the string to a transient buffer

                ; Calculate the length of the string, which is the
                ; difference between cp and the address of the start
                ; of the string (currently saved on the stack).
                dex
                dex
                lda cp
                sec
                sbc 2,x
                sta 0,x         ; LSB
                lda cp+1
                sbc 3,x
                sta 1,x         ; MSB

                rts


cmpl_s_quote:   ; tmp2=process escapes

                lda #<sliteral_runtime2
                ldy #>sliteral_runtime2
cmpl_s_quote_YA: jsr cmpl_subroutine

                ; Put a jmp over the string data with address to be filled
                ; in later.
                jsr cmpl_jump

                jsr cmpl_string

                ; Update the address of the jump-over jmp instruction.
                ; First determine location of jmp instructions address.
                ; It should be 2 bytes before the start of the string.

                ; Compute it into tmp1, which is no longer being used.
                lda 0,x
                sec
                sbc #2
                sta tmp1
                lda 1,x
                sbc #0          ; Propagate borrow
                sta tmp1+1
                inx
                inx

                ; Update the address of the jump to HERE.
                lda cp
                sta (tmp1)
                ldy #1
                lda cp+1
                sta (tmp1),y

z_s_quote:      rts


cmpl_string:
                ; Save the current value of HERE on the data stack for the
                ; address of the string.
                jsr xt_here

                ; Start saving the string into the dictionary up to the
                ; ending double quote.
_regular:
                jsr _getchar

                ; Check if the current character is the end of the string.
                cmp #$22        ; ASCII for "
                beq _found_string_end1

                ; Check for the backslash to see if we should escape
                ; the next char.
                cmp #$5C    ; The backslash char
                beq _escape

_save_char:     ; compile this character into the dictionary
                jsr cmpl_a
                bra _regular

_found_string_end1: rts

_escape:        ldy tmp2            ; are we doing escapes?
                beq _save_char

                jsr _getchar
                tay

                lda #7              ; BEL
                cpy #'a
                beq _save_char

                lda #8              ; Backspace
                cpy #'b
                beq _save_char

                lda #27             ; ESC
                cpy #'e
                beq _save_char

                lda #12             ; FF
                cpy #'f
                beq _save_char

                lda #10             ; LF
                cpy #'l
                beq _save_char

                cpy #'m
                beq _escape_m

                lda #10             ; newline, impl. dependant, using LF
                cpy #'n
                beq _save_char

                lda #34             ; Double quote
                cpy #'q
                beq _save_char

                lda #13             ; CR
                cpy #'r
                beq _save_char

                lda #9              ; Horizontal TAB
                cpy #'t
                beq _save_char

                lda #11             ; Vertical TAB
                cpy #'v
                beq _save_char

                lda #0              ; NULL
                cpy #'z
                beq _save_char

                lda #34             ; Double quote
                cpy #$22
                beq _save_char

                cpy #'x
                beq _escape_x

                lda #92             ; Backslash
                cpy #$5C
                beq _save_char

                bra _regular

_escape_m:      ; CR/LF pair (ASCII values 13, 10)
                lda #13
                jsr cmpl_a
                lda #10
                bra _save_char

_escape_x:      ; We are in the middle of a \x sequence.

                ; First digit.
                jsr _gethex
                ; This is the upper nybble, so move it up.
                asl
                asl
                asl
                asl
                sta tmp3    ; Save it for later.

                ; We are on the second hex digit of a \x sequence.

                jsr _gethex
                ora tmp3

                jmp _save_char


; This is a helper function for s_backslash_quote to convert a character
; from ASCII to the corresponding hex value, eg 'F'->15
_gethex:
                jsr _getchar
                cmp #'A
                bcc _Digit
                ; It's A-F
                and #$DF                ; Make it uppercase.
                sbc #'7                 ; gives value 10 for 'A'
                rts

_Digit:         ; It's 0-9
                sec
                sbc #'0
                rts


; get a character, whatever it takes.
_getchar:
                ; Check to see if the input buffer is empty.
                lda toin                ; LSB
                cmp ciblen
                lda toin+1              ; MSB
                sbc ciblen+1
                bcc _input_ok           ; unsigned comparison

                ; Input buffer is empty.  Refill it.
                ; Refill calls accept, which uses tmp2 and tmp3.
                ; Save and restore them.
                lda tmp2
                pha
                lda tmp3    ; Only tmp3 used, so don't bother with tmp3+1
                pha
                jsr xt_refill           ; ( -- f )
                pla
                sta tmp3
                pla
                sta tmp2

                ; Check result of refill.
                lda 0,x
                ora 1,x
                bne +
                lda #err_refill       ; Something when wrong with refill.
                jmp error
*
                ; Remove the refill flag from the data stack.
                inx
                inx
                ; For refill success, jump back up to the empty check,
                ; just in case refill gave us an empty buffer
                ; (eg. empty/blank line of input)
                bra _getchar

_input_ok:
                ; There should be at least one valid char to use.
                ; Calculate it's address at CIB+TOIN into tmp1
                lda cib
                clc
                adc toin        ; LSB
                sta tmp1
                lda cib+1
                adc toin+1      ; MSB
                sta tmp1+1

                ; Get the character
                lda (tmp1)

                inc toin        ; Move on to the next character.
                bne +
                inc toin+1
*
                rts
.scend


; ## S_TO_D ( u -- d ) "Convert single cell number to double cell"
; ## "s>d"  auto  ANS core
        ; """https://forth-standard.org/standard/core/StoD"""
.scope
xt_s_to_d:
                jsr underflow_1

                ldy #0
                lda 1,x
                bpl +
                dey
*
                dex
                dex
                sty 0,x
                sty 1,x

z_s_to_d:       rts
.scend


; ## SAVE_BUFFERS ( -- ) "Save all dirty buffers to storage"
; ## "save-buffers"  tested  ANS block
        ; """https://forth-standard.org/standard/block/SAVE-BUFFERS"""
.scope
xt_save_buffers:
                ; Check the buffer status
                ldy #buffstatus_offset
                lda (up),y      ; Only bits 0 and 1 are used, so only
                cmp #3          ; LSB is needed.
                bne _done       ; Either not used or not dirty = done!

                ; We need to save the block.
                jsr xt_blkbuffer
                jsr xt_buffblocknum
                jsr xt_fetch
                jsr xt_block_write
                ; Mark the buffer as clean now.
                lda #1
                ldy #buffstatus_offset
                sta (up),y

_done:
z_save_buffers: rts
.scend


; ## SCR ( -- addr ) "Push address of variable holding last screen listed"
; ## "scr"  auto  ANS block ext
        ; """https://forth-standard.org/standard/block/SCR"""
xt_scr:
                lda #scr_offset ; SCR is at UP + scr_offset
                jsr Psu_up_plus_A
z_scr:          rts

; ## SEARCH ( addr1 u1 addr2 u2 -- addr3 u3 flag) "Search for a substring"
; ## "search"   auto  ANS string
        ; """https://forth-standard.org/standard/string/SEARCH
        ; Search for string2 (denoted by addr2 u2) in string1 (denoted by
        ; addr1 u1).  If a match is found the flag will be true and
        ; addr3 will have the address of the start of the match and u3 will have
        ; the number of characters remaining from the match point to the end
        ; of the original string1.  If a match is not found, the flag will be
        ; false and addr3 and u3 will be the original string1's addr1 and u1.
        ; """

.scope
xt_search:
                jsr underflow_4

                ; Put an offset (starting at zero) on the stack.
                jsr xt_zero
_search_loop:
                ; We stop (not found) when u2 + offset > u1
                ; Calculate u2+offset into tmp1
                clc
                lda 0,x
                adc 2,x
                sta tmp1
                lda 1,x
                adc 3,x
                ;sta tmp1+1
                ; Compare to u1
                ; Start with the high byte
                ;lda tmp1+1 ; A already has the upper half.
                cmp 7,x
                bcc _init_comparison ; Obviously less
                bne _not_found
                ; The upper address byte matched - check the lower byte
                ; Load u1 first so we can use just a carry to check.
                lda 6,x
                cmp tmp1
                bcs _init_comparison

_not_found:
                ; The substring isn't in the main string.
                lda #0          ; Return just the main string and a false flag.
                bra _done

_init_comparison:
                ; Use tmp1 to hold address in string 1.
                ; Use tmp2 to hold address in string 2.
                ; Use tmp3 to hold the number of characters left to check.

                ; Compute the starting address in string 1
                ; as addr1 + offset
                clc
                lda 8,x
                adc 0,x
                sta tmp1
                lda 9,x
                adc 1,x
                sta tmp1+1
                ; The starting address in string 2 is just addr2.
                lda 4,x
                sta tmp2
                lda 5,x
                sta tmp2+1
                ; The number of characters to check is u2.
                lda 2,x
                sta tmp3
                lda 3,x
                sta tmp3+1

_comparison_loop:
                ; Check to see if the current characters match.
                lda (tmp1)
                cmp (tmp2)
                beq _letters_match

                ; One of the letters didn't match.
                ; Increment the offset and try again.
                jsr one_plus_nouf
                bra _search_loop

_letters_match:

                ; The letters match.  Advance the pointers until the
                ; count reaches zero.
                inc tmp1
                bne +
                inc tmp1+1
*
                inc tmp2
                bne +
                inc tmp2+1
*
                ; Decrement the count of remaining letters to check.
                lda tmp3
                bne +
                dec tmp3+1
*
                dec tmp3
                ; Check if we've reached zero.
                lda tmp3
                ora tmp3+1
                bne _comparison_loop ; Check the next letter

                ; We've run out of letters and they all match!
                ; Return (addr1+offset) (u1-offset) true
                ; Add offset to addr1.
                clc
                lda 0,x
                adc 8,x
                sta 8,x
                lda 1,x
                adc 9,x
                sta 9,x
                ; Subtract offset from u1.
                sec
                lda 6,x
                sbc 0,x
                sta 6,x
                lda 7,x
                sbc 1,x
                sta 7,x

                lda #$FF    ; true flag.

_done:          inx             ; drop offset
                inx
                inx             ; drop u2
                inx
                sta 0,x         ; replace addr2 with flag.
                sta 1,x

z_search:       rts
.scend


; shorter version where string2 must by <256 bytes
;.scope
;xt_search:
;                jsr underflow_4
;
;  verify length <256 bytes
;
;               clc         ; tmp1 = addr1 + u1 - u2
;               lda 6,x     ; ( last char of string1 that could match +1 )
;               sta tmp2    ; tmp2 = addr1
;               adc 4,x
;               pha
;               lda 7,x
;               sta tmp2+1
;               adc 5,x
;               tay
;               pla
;               sec
;               sbc 0,x
;               sta tmp1
;               tya
;               sbc 1,x
;               sta tmp1+1
;
;               lda 2,x     ; tmp3 = addr2
;               sta tmp3
;               lda 3,x
;               sta tmp3+1
;
;_search_loop:
;                lda tmp2       ; We stop (not found) when tmp2>=tmp1
;                cmp tmp1
;                lda tmp2+1
;                sbc tmp1+1
;               bcs _not_found
;
;               ldy 0,x
;_char_1:       tya
;               beq _found
;               dey
;               lda (tmp2),y
;                cmp (tmp3),y
;                beq _char_1
;
;                ; One of the letters didn't match.
;                ; Increment the offset and try again.
;                inc tmp2
;               bne _search_loop
;               inc tmp2+1
;                bra _search_loop
;
;_not_found:     ; The substring isn't in the main string.
;                ; Return just the main string and a false flag.
;                lda #0
;                bra _done
;
;_found:         ; We've run out of letters and they all match!
;                ; Return (addr1+offset) (u1-offset) true
;                clc                ; u1 = addr1 + u1
;               lda 4,x
;               adc 6,x
;               pha
;               lda 5,x
;               adc 7,x
;               tay
;               pla             ;   - tmp2
;                sec
;               sbc tmp2
;               sta 4,x
;               tya
;                sbc tmp2+1
;                sta 5,x
;
;                lda tmp2       ; addr1=tmp2
;                sta 6,x
;                lda tmp2+1
;                sta 7,x
;
;                lda #$FF       ; true flag
;
;_done           inx             ; drop u2
;                inx
;                sta 0,x         ; Turn addr2 into a flag.
;                sta 1,x
;z_search:       rts
;.scend


; ## SEMICOLON ( -- ) "End compilation of new word"
; ## ";"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Semi
        ; End the compilation of a new word into the Dictionary.
        ;
        ; When we
        ; enter this, WORKWORD is pointing to the nt_ of this word in the
        ; Dictionary, DP to the previous word, and CP to the next free byte.
        ; A Forth definition would be (see "Starting Forth"):
        ; : POSTPONE EXIT  REVEAL POSTPONE ; [ ; IMMEDIATE  Following the
        ; practice of Gforth, we warn here if a word has been redefined.
        ; """
.scope
xt_semicolon:
                ; Check if this is a : word or a :NONAME word.
                bit status
                bvs _colonword

                ; This is a :NONAME word - just put an RTS on the end and
                ; the address (held in workword) on the stack.
                lda #$60                ; opcode for RTS
                jsr cmpl_a

                dex
                dex
                lda workword
                sta 0,x
                lda workword+1
                sta 1,x
                bra _semicolon_done

_colonword:
                ; CP is the address we use in the
                ; header as the end-of-compile address (z_word).
                ldy #nt_z
                lda cp
                sta (workword),y
                iny
                lda cp+1
                sta (workword),y

                ; Allocate one further byte and save the RTS instruction
                ; there
                lda #$60                ; opcode for RTS
                jsr cmpl_a

                ; Before we formally add the word to the Dictionary, we
                ; check to see if it is already present, and if yes, we
                ; warn the user.

                ; See if word already in Dictionary.
                ; (STATUS bit 7 will be high as CREATE already
                ;  checked for us.)
                bit status
                bpl _new_word   ; Bit 7 is clear = new word

                ; This word is already in the Dictionary, so we print a
                ; warning to the user.

                ldy #s_redefined-string0
                jsr print_string_no_lf_Y

                ; Eight bytes below WORKWORD is the actual beginning of
                ; the name string
                lda workword
                clc
                adc #nt_name
                dex
                dex
                sta 0,x
                lda workword+1
                adc #0                  ; only want carry
                sta 1,x

                ; WORKWORD points to the beginning of the head of our new
                ; word, where the first byte is the length of the string
                ; We can't use LATESTNT because we haven't added the new
                ; word to the Dictionary yet
                lda (workword)          ; nt_length
                jsr PsuZA

                ; Now we print the offending word.
                jsr xt_type
                jsr xt_space

                ; Clear bit 7 of status (so future words will print message
                ; by default)
                lda #$7F
                and status
                sta status

                ; Continue processing word.
_new_word:

                ; Let's get this over with. Save beginning of our word
                ; as new last word in the Dictionary
                lda workword
                sta dp
                lda workword+1
                sta dp+1
                jsr dp_to_current       ; Save the updated DP to the
                                        ; CURRENT wordlist.
_semicolon_done:
                ; Word definition complete. Return compile flag to zero
                ; to return to interpret mode
                stz state
                stz state+1

z_semicolon:    rts
.scend


; ## SIGN ( n -- ) "Add minus to pictured output"
; ## "sign"  auto  ANS core
        ; """https://forth-standard.org/standard/core/SIGN
        ;
        ; Code based on
        ; http://pforth.googlecode.com/svn/trunk/fth/numberio.fth
        ; Original Forth code is   0< IF ASCII - HOLD THEN
        ; """
.scope
xt_sign:
                jsr underflow_1
sign_nouf:
                lda 1,x         ; check MSB of TOS
                bpl _done
                lda #$2d        ; ASCII for "-"
                jsr hold_A
_done:          inx             ; DROP n
                inx
z_sign:         rts
.scend


; ## SLASH ( n1 n2 -- n ) "Divide NOS by TOS"
; ## "/"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Div
        ;
        ; Forth code is either  >R S>D R> FM/MOD SWAP DROP
        ; or >R S>D R> SM/REM SWAP DROP -- we use SM/REM in Tali Forth.
        ; """
.scope
xt_slash:
                jsr underflow_2
slash_nouf:
                jsr slash_mod_nouf
                jmp nip_nouf
z_slash:



; ## SLASH_MOD ( n1 n2 -- n3 n4 ) "Divide NOS by TOS with a remainder"
; ## "/mod"  auto  ANS core
        ; """https://forth-standard.org/standard/core/DivMOD
        ; """
xt_slash_mod:
                jsr underflow_2
slash_mod_nouf:
                ; convert NOS from s to d
                dex
                dex
                lda 2,x
                ldy 3,x
                sta 0,x
                sty 1,x

                ldy #0
                lda 5,x
                bpl +
                dey
*               sty 2,x
                sty 3,x

                jmp sm_slash_rem_nouf
z_slash_mod:
.scend



; ## SLASH_STRING ( addr u n -- addr u ) "Shorten string by n"
; ## "/string"  auto  ANS string
        ; """https://forth-standard.org/standard/string/DivSTRING
        ;
        ; Forth code is
        ; : /STRING ( ADDR U N -- ADDR U ) ROT OVER + ROT ROT - ;
        ; Put differently, we need to add TOS and 3OS, and subtract
        ; TOS from NOS, and then drop TOS
        ; """
.scope
xt_slash_string:
                jsr underflow_3
slash_string_nouf:
                clc             ; 3OS+=TOS
                lda 0,x
                adc 4,x
                sta 4,x
                lda 1,x
                adc 5,x
                sta 5,x

                sec             ; NOS-=TOS
                lda 2,x
                sbc 0,x
                sta 2,x
                lda 3,x
                sbc 1,x
                sta 3,x

                inx
                inx

z_slash_string: rts
.scend

; ## SLITERAL ( addr u -- )( -- addr u ) "Compile a string for runtime"
; ## "sliteral" auto  ANS string
        ; """https://forth-standard.org/standard/string/SLITERAL
        ; Add the runtime for an existing string.
        ; """
.scope
xt_sliteral:
                jsr underflow_2

                ; We can't assume that ( addr u ) of the current string is in
                ; a stable area (eg. already in the dictionary.)  Copy the
                ; string data into the dictionary using move.

                ldy #>sliteral_runtime2
                lda #<sliteral_runtime2
                jsr cmpl_subroutine

                ; Compile a jmp over the string data
                clc
                lda cp
                adc #3
                pha
                lda cp+1
                adc #0
                tay
                pla
                adc 0,x
                pha
                tya
                adc 1,x
                tay
                pla
                jsr cmpl_jump

                ; Compile the string data in.
_string_loop:   lda 0,x
                ora 1,x
                beq +

                lda (2,x)
                jsr cmpl_a

                jsr decinc
                bra _string_loop
_string_end:

z_sliteral:     rts
.scend

sliteral_runtime2:
.scope
        ; """Run time behaviour of SLITERAL:
        ; Push ( addr u ) of string to the Data Stack.
        ; This expects a jmp abs to follow the jsr sliteral_runtime2,
        ; then a jmp abs to after the end of string character data,
        ; then the string character data.
        ; """
                ; Get the address of the string address off the stack and
                ; increase by one because of the RTS mechanics
                pla
                ply
                phy
                pha
                sta tmp1        ; LSB of address
                sty tmp1+1      ; MSB of address

                dex             ; push addr = rtn+4
                dex
                clc
                adc #4
                sta 0,x
                tya
                adc #0
                sta 1,x

                dex             ; push u = next-addr
                dex
                ldy #2
                lda (tmp1),y
                sec
                sbc 2,x
                sta 0,x
                iny
                lda (tmp1),y
                sbc 3,x
                sta 1,x

                rts
.scend


; ## SM_SLASH_REM ( d n1 -- n2 n3 ) "Symmetic signed division"
; ## "sm/rem"  auto  ANS core
        ; """https://forth-standard.org/standard/core/SMDivREM
        ; Symmetic signed division. Compare FM/MOD. Based on F-PC 3.6
        ; by Ulrich Hoffmann. See http://www.xlerb.de/uho/ansi.seq
        ;
        ; Forth:
        ; OVER >R 2DUP XOR 0< >R ABS >R DABS R> UM/MOD R> ?NEGATE SWAP
        ; R> ?NEGATE SWAP
        ; """
.scope
xt_sm_slash_rem:
                jsr underflow_3 ; contains double number
sm_slash_rem_nouf:
                ; push MSB of high cell of d to Data Stack so we can check
                ; its sign later
                lda 3,x
                pha

                ; XOR the MSB of the high cell of d and n1 so we figure out
                ; its sign later as well
                eor 1,x
                pha

                ; Prepare division by getting absolute of n1 and d
                lda 1,x
                bpl +
                jsr negate_nouf
*
                lda 3,x
                bpl +
                inx             ; pretend we pushed n1 to R
                inx
                jsr dnegate_nouf
                dex
                dex
*
                jsr um_slash_mod_nouf   ; UM/MOD

                ; if the XOR compiled above is negative, negate the
                ; quotient (n3)
                pla
                bpl +
                jsr negate_nouf
*
                ; if d was negative, negate the remainder (n2)
                pla
                bpl +
                jsr negate_NOS_nouf
*
z_sm_slash_rem: rts
.scend


; ## SOURCE ( -- addr u ) "Return location and size of input buffer""
; ## "source"  auto  ANS core
        ; """https://forth-standard.org/standard/core/SOURCE"""
xt_source:
                ; add address
                dex
                dex
                lda cib
                sta 0,x
                lda cib+1
                sta 1,x

                ; add size
                dex
                dex
                lda ciblen
                sta 0,x
                lda ciblen+1
                sta 1,x

z_source:       rts


; ## SOURCE_ID ( -- n ) "Return source identifier"
; ## "source-id"  tested  ANS core ext
        ; """https://forth-standard.org/standard/core/SOURCE-ID
        ; Identify the input source unless it is a block (s. Conklin &
        ; Rather p. 156). Since we don't have blocks (yet), this will give
        ; the input source: 0 is keyboard, -1 (0ffff) is character string,
        ; and a text file gives the fileid.
        ; """
xt_source_id:
                dex
                dex

                lda insrc
                sta 0,x
                lda insrc+1
                sta 1,x

z_source_id:    rts


; ## SPACE ( -- ) "Print a single space"
; ## "space"  auto  ANS core
        ; """https://forth-standard.org/standard/core/SPACE"""
xt_space:
                lda #AscSP
                jsr emit_a

z_space:        rts


; ## SPACES ( u -- ) "Print a number of spaces"
; ## "spaces"  auto  ANS core
        ; """https://forth-standard.org/standard/core/SPACES"""
.scope
xt_spaces:
                jsr underflow_1

                ldy 0,x
                bra _b

_c:             dec 1,x
_a:             jsr xt_space
                dey
_b:             bne _a

                lda 1,x
                bne _c

                inx             ; drop
                inx

z_spaces:       rts
.scend


; ## STAR ( n n -- n ) "16*16 --> 16 "
; ## "*"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Times
        ; Multiply two signed 16 bit numbers, returning a 16 bit result.
        ; """
.scope
xt_star:
                jsr underflow_2

                jsr um_star_nouf    ; UM*
                inx                 ; DROP
                inx

z_star:         rts
.scend


; ## STAR_SLASH  ( n1 n2 n3 -- n4 ) "n1 * n2 / n3 -->  n"
; ## "*/"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TimesDiv
        ; Multiply n1 by n2 and divide by n3, returning the result
        ; without a remainder. This is */MOD without the mod.
        ;
        ; This word
        ; can be defined in Fort as : */  */MOD SWAP DROP ; which is
        ; pretty much what we do here
        ; """
xt_star_slash:
                ; We let */MOD check for underflow
                jsr xt_star_slash_mod
                jsr nip_nouf
z_star_slash:
                rts


; ## STAR_SLASH_MOD  ( n1 n2 n3 -- n4 n5 ) "n1 * n2 / n3 --> n-mod n"
; ## "*/mod"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TimesDivMOD
        ; Multiply n1 by n2 producing the intermediate double-cell result
        ; d. Divide d by n3 producing the single-cell remainder n4 and the
        ; single-cell quotient n5.
        ;
        ; In Forth, this is
        ; : */MOD  >R M* >R SM/REM ;  Note that */ accesses this routine.
        ; """
xt_star_slash_mod:
                jsr underflow_3

                jsr PluYA
                phy
                pha
                jsr m_star_nouf
                pla
                ply
                jsr PsuYA
                jsr sm_slash_rem_nouf

z_star_slash_mod:
                rts


; ## STATE ( -- addr ) "Return the address of compilation state flag"
; ## "state"  auto  ANS core
        ; """https://forth-standard.org/standard/core/STATE
        ; STATE is true when in compilation state, false otherwise. Note
        ; we do not return the state itself, but only the address where
        ; it lives. The state should not be changed directly by the user; see
        ; http://forth.sourceforge.net/standard/dpans/dpans6.htm#6.1.2250
        ; """
xt_state:       lda #state
                jsr PsuZA
z_state:        rts


; ## STORE ( n addr -- ) "Store TOS in memory"
; ## "!"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Store"""
.scope
xt_store:
                jsr underflow_2
store_nouf:
                lda 2,x         ; LSB
                sta (0,x)

                inc 0,x
                bne +
                inc 1,x
*
                lda 3,x         ; MSB
                sta (0,x)

                inx             ; 2DROP
                inx
                inx
                inx

z_store:        rts
.scend


; ## STRIP_UNDERFLOW ( -- addr ) "Return address where underflow status is kept"
; ## "strip-underflow"  tested  Tali Forth
        ; """STRIP_UNDERFLOW contains a flag that determines if underflow
        ; checking should be removed during the compilation of new words.
        ; Default is false.
        ; """
.scope
xt_strip_underflow:
                lda #uf_strip
                jmp PsuZA
z_strip_underflow:
.scend


; ## SWAP ( b a -- a b ) "Exchange TOS and NOS"
; ## "swap"  auto  ANS core
        ; """https://forth-standard.org/standard/core/SWAP"""
xt_swap:
.scope
                jsr underflow_2
swap_nouf:
                lda 0,x         ; LSB
                ldy 2,x
                sta 2,x
                sty 0,x

                lda 1,x         ; MSB
                ldy 3,x
                sta 3,x
                sty 1,x

z_swap:         rts
.scend


; ## SYM ( adr -- )  "Type symbolic version of adr, aka ID."
.scope
xt_sym:
                                        ; +4 = adr to look for
                ldy #4                  ; +2 = best_offset
                jsr PsuYA
                dex                     ; +0 = best_nt
                dex
                stz 1,x

                ldy #num_wordlists_offset ;for each wordlist
                lda (up),y
                asl
                adc #wordlists_offset
                tay
_wl_loop:       dey
                dey
                phy
                lda (up),y              ;  for(tmp1=wordlist->1st_nt; tmp1!=0; tmp1=tmp1->nt_next_word)
                sta tmp1
                iny
                lda (up),y
                sta tmp1+1
                beq _wl_next
_w_loop:
                sec                     ;    tmp2 = adr - word->xt
                lda 4,x
                ldy #nt_xt
                sbc (tmp1),y
                sta tmp2
                lda 5,x
                iny
                sbc (tmp1),y
                sta tmp2+1

                lda tmp2                ;    if(tmp2<best_offset) {
                cmp 2,x
                lda tmp2+1
                sbc 3,x
                bcs _w_skip

                lda tmp2                ;      best_offset=tmp2
                sta 2,x
                lda tmp2+1
                sta 3,x

                lda tmp1                ;      best_nt = tmp1
                sta 0,x
                lda tmp1+1
                sta 1,x
_w_skip:                                ;      }

_w_next:        ldy #nt_next_nt+1       ;    next word in wordlist
                lda (tmp1),y
                pha
                dey
                lda (tmp1),y
                sta tmp1
                pla
                sta tmp1+1
                bne _w_loop

_wl_next:       ply                     ;  next wordlist
                cpy #wordlists_offset+2
                bcs _wl_loop

                lda 1,x                 ; if(not valid best_nt) {
                bne _print
                inx                     ;   DROP best_nt
                inx
                inx                     ;   DROP best_offset
                inx
                bra _end                ;   }
_print:                                 ; else {
                ; type the results
                jsr xt_space
                lda #'{
                jsr emit_a

                jsr xt_name_to_string   ;   type(name>string(best_nt))
                jsr xt_type
                lda 0,x                 ;   if(best_offset!=0) {
                ora 1,x
                beq _no_offset
                jsr xt_space            ;     space
                lda #$2b  ; '+          ;     emit('+')
                jsr emit_a
                jsr print_u             ;     print_u(best_offset)
                bra _offset_end ;     }
_no_offset:                             ;   else {
                inx                             ;     drop
                inx
_offset_end:                    ;     }
                lda #'}                 ;   emit('}')
                jsr emit_a
_end:                                   ;   }
                inx                     ; DROP adr
                inx
z_sym:          rts
.scend


; ## THEN (C: orig -- ) ( -- ) "Conditional flow control"
; ## "then"  auto  ANS core
        ; """http://forth-standard.org/standard/core/THEN"""
.scope
xt_then:
                ; Get the address to jump to.
                jsr xt_here

                ; Stuff HERE in for the branch address back
                ; at the IF or ELSE (origination address is on stack).
                jsr xt_swap
                jsr xt_store

z_then:         rts
.scend


; ## THRU ( scr# scr# -- ) "Load screens in the given range"
; ## "list"  tested  ANS block ext
        ; """https://forth-standard.org/standard/block/THRU"""
.scope
xt_thru:
                jsr underflow_2

                ; We need to loop here, and can't use the data stack
                ; because the LOADed screens might use it.  We'll
                ; need to use the same trick that DO loops use, holding
                ; the limit and current index on the return stack.

                jsr xt_to_r     ; Move ending screen number to the return stack
                bra _load

_next:          inc 0,x             ; step to next screen
                bne +
                inc 1,x
*
_load:          lda 0,x             ; Copy the current screen #
                ldy 1,x
                phy
                pha

                jsr xt_load         ; Load this screen.

                pla                 ; Get the current screen #
                ply
                jsr PsuYA

                pla                 ; YA = ending screen #
                ply
                phy
                pha

                clc                 ; current screen # > ending screen # ?
                sbc 0,x
                tya
                sbc 1,x
                bcs _next

                ply                 ; drop ending screen #
                pla
                inx                 ; drop current screen #
                inx

z_thru:         rts
.scend


; ## TICK ( "name" -- xt ) "Return a word's execution token (xt)"
; ## "'"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Tick"""
.scope
xt_tick:
                jsr xt_parse_name       ; ( -- addr u )

                ; if we got a zero, there was a problem getting the
                ; name of the word
                lda 0,x
                ora 1,x
                bne +

                lda #err_noname
                jmp error
*
                jsr xt_find_name        ; ( addr u -- nt )

                ; If we didn't find the word in the Dictionary, abort
                lda 0,x
                ora 1,x
                bne +

                lda #err_syntax
                jmp error
*
                jmp name_to_int_nouf    ; ( nt -- xt )

z_tick:
.scend


; ## TO ( n "name" -- ) or ( "name") "Change a value"
; ## "to"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/TO
        ; Gives a new value to a, uh, VALUE.
        ;
        ; One possible forth
        ; implementation is ' >BODY !  but given the problems we have
        ; with >BODY on STC Forths, we do this the hard way.  Since
        ; Tali Forth uses the same code for CONSTANTs and VALUEs, you
        ; could use this to redefine a CONSTANT, but that is a no-no.
        ;
        ; Note that the standard has different behaviors for TO depending
        ; on the state (https://forth-standard.org/standard/core/TO).
        ; This makes TO state-dependent (which is bad) and also rather
        ; complex (see the Gforth implementation for comparison). This
        ; word may not be natively compiled and must be immediate. Frankly,
        ; it would have made more sense to have two words for this.
        ; """
.scope
xt_to:
                ; One way or the other, we need the xt of the word
                ; after this one. At this point, we don't know if we
                ; are interpreted or compile, so we don't know if there
                ; is a value n on the stack
                jsr xt_tick             ; ( [n] xt )

;               lda (0,x)               ; TODO: verify it's a VALUE
;               cmp #$a9
;               beq _value
;               lda #???_not_a_value
;               jmp error
;_value:
                ; Now it gets ugly. See which state we are in
                lda state
                ora state+1
                beq _interpret

                ; Well, we're compiling.

                ; TODO: someday we'll need to look for 2VALUE

                ; We want compile code that takes the number that is
                ; TOS and saves it in the VALUE xt xt we were just given.
                ; So we want to compile this routine:
                ;
                ;       lda 0,x                 - B5 00
                ;       ldy 1,x                 - B4 01
                ;       inx                     - E8
                ;       inx                     - E8
                ;       sta <xt+1>              - 8D LSB MSB
                ;       sty <xt+3>              - 8C LSB MSB
                ;
                ; which at least is nice and short.

                ldy #$00                ; Code for LDA 0,X
                lda #$B5
                jsr cmpl_word
                ldy #$01                ; Code for LDY 1,X
                lda #$B4
                jsr cmpl_word
                jsr cmpl_drop

                jsr xt_one_plus         ; step xt to VALUE's n LSB
                lda #$8D                ; Code for STA abs
                jsr cmpl_a
                ldy 1,x                 ; MSB goes in Y
                lda 0,x
                jsr cmpl_word

                jsr xt_two_plus_nouf    ; step xt to VALUE's n MSB
                lda #$8C                ; Code for STY abs
                jsr cmpl_a
                jmp xt_comma            ; MSB goes in Y

_interpret:
                ; We're interpreting, so we arrive here with n
                ; & VALUE xt on the stack.

                jsr PluYA               ; save VALUE ptr
                sta tmp1
                sty tmp1+1

                jsr underflow_1

                ; Store n into the VALUE
                lda 0,x                 ; MSB
                ldy #1
                sta (tmp1),y
                lda 1,x                 ; MSB
                ldy #3
                sta (tmp1),y

                inx                     ; DROP n
                inx
z_to:           rts
.scend


; ## TO_BODY ( xt -- addr ) "Return a word's Code Field Area (CFA)"
; ## ">body"  auto  ANS core
        ; """https://forth-standard.org/standard/core/toBODY
        ; Given a word's execution token (xt), return the address of the
        ; start of that word's parameter field (PFA). This is defined as the
        ; address that HERE would return right after CREATE.
        ;
        ; This is a
        ; difficult word for STC Forths, because most words don't actually
        ; have a Code Field Area (CFA) to skip. We solve this by having CREATE
        ; add a flag, "has CFA" (HC), in the header so >BODY know to skip
        ; the subroutine jumps to DOVAR, or DODOES
        ; """
.scope
xt_to_body:
                jsr underflow_1

                ; Ideally, xt already points to the CFA. We just need to check
                ; the HC flag for special cases
                jsr xt_dup              ; ( xt xt )
                jsr xt_int_to_name      ; ( xt nt )

                ; The status byte is nt_status
                inc 0,x
                bne +
                inc 1,x
*
                lda (0,x)               ; get status byte
                inx
                inx
                and #HC
                beq _no_cfa

                ; We've got a DOVAR, DODOES or whatever,
                ; so we add three to xt, which is NOS
                clc
                lda 0,x         ; LSB
                adc #3
                sta 0,x
                bcc +
                inc 1,x         ; MSB
*
_no_cfa:
z_to_body:      rts
.scend


; ## TO_IN ( -- addr ) "Return address of the input pointer"
; ## ">in"  auto  ANS core
xt_to_in:
                lda #toin
                jmp PsuZA
z_to_in:


; ## TO_NUMBER ( ud addr u -- ud addr u ) "Convert a number"
; ## ">number"  auto  ANS core
        ; """https://forth-standard.org/standard/core/toNUMBER
        ; Convert a string to a double number.
        ; We arrive here from NUMBER which has
        ; made sure that we don't have to deal with a sign and we don't have
        ; to deal with a dot as a last character that signalizes double -
        ; this should be a pure number string.
        ; """

.scope
xt_to_number:
                jsr underflow_4

                lda 0,x                 ; zero length?
                beq _done

_loop:          lda (2,x)               ; get next char
                sec                     ; convert to digit value
                sbc #$30                ;  '0'
                bcc _done
                cmp #10
                bcc _digit_ok
                sbc #7                  ;  'A'-'9'-1
                cmp #10
                bcc _done
                cmp #$20+10-1           ;  'a'-'A'+10-1
                bcc _digit_ok
                sbc #$20                ;  'a'-'A'
_digit_ok:      cmp base
                bcs _done
                ; Conversion was successful.
                pha                     ; save digit value

                ; multiply ud by base
                lda 4,x                 ; tmp = ud
                sta tmp1
                lda 5,x
                sta tmp1+1
                lda 6,x
                sta tmp2
                lda 7,x
                sta tmp2+1
                stz 4,x                 ; ud = 0
                stz 5,x
                stz 6,x
                stz 7,x
                lda base                ; get multiplier
                bra _mult_8
_mult_2:        tay                     ; save multiplier
                clc                     ; ud += tmp
                lda 6,x
                adc tmp2
                sta 6,x
                lda 7,x
                adc tmp2+1
                sta 7,x
                lda 4,x
                adc tmp1
                sta 4,x
                lda 5,x
                adc tmp1+1
                sta 5,x
                tya                     ; restore multiplier
_mult_4:        asl tmp2                ; tmp *= 2
                rol tmp2+1
                rol tmp1
                rol tmp1+1
_mult_8:        lsr                     ; shift multiplier
                bcs _mult_2
                bne _mult_4

                pla                     ; ud += digit value
                clc
                adc 6,x
                sta 6,x
                bcc +
                inc 7,x
                bne +
                inc 4,x
                bne +
                inc 5,x
*

                ; One character down. Move address up
                inc 2,x
                bne +
                inc 3,x
*
                ; Decrease counter
                dec 0,x
                bne _loop

_done:          ; Counter has reached zero or we have an error.

z_to_number:    rts
.scend


; ## TO_ORDER ( wid -- ) "Add wordlist at beginning of search order"
; ## ">order"  tested  Gforth search
        ; """https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Word-Lists.html"""
.scope
xt_to_order:
                ; Put the wid on the return stack for now.
                jsr PluYA
                pha

                ; Get the current search order.
                jsr xt_get_order

                ; Get back the wid and add it to the list.
                jsr dup_nouf
                inc 0,x
                pla
                sta 2,x

                ; Set the search order with the new list.
                jmp xt_set_order
z_to_order:
.scend


; ## TO_R ( n -- )(R: -- n) "Push TOS to the Return Stack"
; ## ">r"  auto  ANS core
        ; """https://forth-standard.org/standard/core/toR
        ; This word is handled differently for native and for
        ; subroutine coding, see COMPILE, . This is a compile-only
        ; word.
        ; """
xt_to_r:
                ; Save the return address. If this word is natively
                ; coded, this is a complete waste of cycles, but
                ; required for subroutine coding
                pla             ; LSB
                ply             ; MSB
                sta tmptos
                sty tmptos+1

                ; --- CUT HERE FOR NATIVE CODING ---

                ; We check for underflow in the second step, so we can
                ; strip off the stack thrashing for native compiling first
                jsr underflow_1

                ; now we can do the actual work
                lda 0,x
                ldy 1,x
                inx
                inx

                phy
                pha

                ; --- CUT HERE FOR NATIVE CODING ---

                ; restore return address
                lda tmptos
                ldy tmptos+1
                phy             ; MSB
                pha             ; LSB

z_to_r:         rts


; ## TRUE ( -- f ) "Push TRUE flag to Data Stack"
; ## "true"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/TRUE"""
xt_true:        lda #$ff
                dex
                dex
                sta 0,x
                sta 1,x

z_true:         rts


; ## TUCK ( b a -- a b a ) "Copy TOS below NOS"
; ## "tuck"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/TUCK"""
xt_tuck:
                jsr underflow_2
tuck_nouf:
                dex
                dex

                ldy 4,x         ; LSB
                lda 2,x
                sta 4,x
                sty 2,x
                sta 0,x

                ldy 5,x         ; MSB
                lda 3,x
                sta 5,x
                sty 3,x
                sta 1,x

z_tuck:         rts


; ## TWO_DROP ( n n -- ) "Drop TOS and NOS"
; ## "2drop"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoDROP"""
.scope
xt_two_drop:
                jsr underflow_2
two_drop_nouf:
                inx
                inx
                inx
                inx

z_two_drop:     rts
.scend


; ## TWO_DUP ( a b -- a b a b ) "Duplicate first two stack elements"
; ## "2dup"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoDUP"""
.scope
xt_two_dup:
                jsr underflow_2
two_dup_nouf:
                dex
                dex
                dex
                dex

                lda 4,x         ; TOS
                sta 0,x
                lda 5,x
                sta 1,x

                lda 6,x         ; NOS
                sta 2,x
                lda 7,x
                sta 3,x

z_two_dup:      rts
.scend


; ## TWO_FETCH ( addr -- n1 n2 ) "Fetch the cell pair n1 n2 stored at addr"
; ## "2@"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoFetch
        ; Note n2 stored at addr and n1 in the next cell -- in our case,
        ; the next 2 bytes. This is equvalent to  DUP CELL+ @ SWAP @
        ; """
.scope
xt_two_fetch:
                jsr underflow_1

                lda 0,x
                sta tmp1
                ldy 1,x
                sty tmp1+1

                dex             ; reuse one stack element
                dex

                lda (tmp1)      ; copy LSB
                sta 0,x
                ldy #1          ; copy next
                lda (tmp1),y
                sta 1,x
                iny             ; copy next
                lda (tmp1),y
                sta 2,x
                iny             ; copy next
                lda (tmp1),y
                sta 3,x

z_two_fetch:    rts
.scend

; ## TWO_OVER ( d1 d2 -- d1 d2 d1 ) "Copy double word NOS to TOS"
; ## "2over"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoOVER"""
.scope
xt_two_over:
                jsr underflow_4
two_over_nouf:
                dex
                dex
                dex
                dex

                lda 8,x
                sta 0,x

                lda 9,x
                sta 1,x

                lda 10,x
                sta 2,x

                lda 11,x
                sta 3,x

z_two_over:     rts
.scend


; ## TWO_R_FETCH ( -- n n ) "Copy top two entries from Return Stack"
; ## "2r@"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/TwoRFetch
        ;
        ; This is R> R> 2DUP >R >R SWAP but we can do it a lot faster in
        ; assembler. We use trickery to access the elements on the Return
        ; Stack instead of pulling the return address first and storing
        ; it somewhere else like for 2R> and 2>R. In this version, we leave
        ; it as Never Native; at some point, we should compare versions to
        ; see if an Always Native version would be better
        ; """
.scope
xt_two_r_fetch:
        ; make room on the Data Stack
                dex
                dex
                dex
                dex

                ; Get four bytes off of Return Stack. This assumes that
                ; we took a subroutine jump here so the first two entries
                ; are the return address
                txa
                tsx
                phx             ; 65c02 has no TXY, so do it the hard way
                ply
                tax

                ; The Return Stack addreses $0101 and $0102 are occupied by
                ; the return address for this word. This is a whole lot
                ; easier on the 65816

                lda $0103,y     ; LSB of top entry
                sta 0,x
                lda $0104,y     ; MSB of top entry
                sta 1,x
                lda $0105,y     ; LSB of bottom entry
                sta 2,x
                lda $0106,y     ; MSB of top entry
                sta 3,x

z_two_r_fetch:  rts
.scend


; ## TWO_R_FROM ( -- n1 n2 ) (R: n1 n2 -- ) "Pull two cells from Return Stack"
; ## "2r>"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/TwoRfrom
        ; Pull top two entries from Return Stack.
        ;
        ; Is the same as
        ; R> R> SWAP. As with R>, the problem with the is word is that
        ; the top value on the ReturnStack for a STC Forth is the
        ; return address, which we need to get out of the way first.
        ; Native compile needs to be handled as a special case.
        ; """
.scope
xt_two_r_from:
                ; save the return address
                pla                     ; LSB
                sta tmp1
                pla                     ; MSB
                sta tmp1+1

                ; --- CUT HERE FOR NATIVE CODING ---

        ; make room on stack
                dex
                dex
                dex
                dex

                ; In theory, we should test for underflow on the Return
                ; Stack. However, given the traffic there with an STC
                ; Forth, that's probably not really useful

                ; now we can access the data
                pla                     ; LSB
                sta 0,x
                pla                     ; MSB
                sta 1,x

                pla                     ; LSB
                sta 2,x
                pla                     ; MSB
                sta 3,x

                ; --- CUT HERE FOR NATIVE CODING ---

                ; restore return address
                lda tmp1+1              ; MSB
                pha
                lda tmp1                ; LSB
                pha


z_two_r_from:   rts
.scend

; ## TWO_SLASH ( n -- n ) "Divide TOS by two"
; ## "2/"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoDiv"""
xt_two_slash:
                jsr underflow_1

                ; We can't just LSR the LSB and ROR the MSB because that
                ; would do bad things to the sign
                lda 1,x
                asl                     ; extend the sign
                ror 1,x
                ror 0,x

z_two_slash:    rts


; ## TWO_STAR ( n -- n ) "Multiply TOS by two"
; ## "2*"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoTimes
        ;
        ; Also used for CELLS
        ; """
xt_two_star:
xt_cells:
                jsr underflow_1

                asl 0,x
                rol 1,x
z_cells:
z_two_star:     rts


; ## TWO_STORE ( n1 n2 addr -- ) "Store two numbers at given address"
; ## "2!"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoStore
        ; Stores so n2 goes to addr and n1 to the next consecutive cell.
        ; Is equivalent to  SWAP OVER ! CELL+ !
        ; """

xt_two_store:
                jsr underflow_3

                lda 0,x
                ldy 1,x
                inx
                inx
                sta tmp1
                sty tmp1+1

                lda 0,x         ; copy MSB
                sta (tmp1)
                lda 1,x         ; copy next
                ldy #1
                sta (tmp1),y
                lda 2,x         ; copy next
                iny
                sta (tmp1),y
                lda 3,x         ; copy MSB
                iny
                sta (tmp1),y

                inx             ; 2DROP
                inx
                inx
                inx

z_two_store:    rts


; ## TWO_SWAP ( n1 n2 n3 n4 -- n3 n4 n1 n1 ) "Exchange two double words"
; ## "2swap"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TwoSWAP"""
.scope
xt_two_swap:
                jsr underflow_4
two_swap_nouf:
                ; 0 <-> 4
                lda 0,x
                ldy 4,x
                sta 4,x
                sty 0,x

                ; 1 <-> 5
                lda 1,x
                ldy 5,x
                sta 5,x
                sty 1,x

                ; 2 <-> 6
                lda 2,x
                ldy 6,x
                sta 6,x
                sty 2,x

                ; 3 <-> 7
                lda 3,x
                ldy 7,x
                sta 7,x
                sty 3,x

z_two_swap:     rts
.scend


; ## TWO_TO_R ( n1 n2 -- )(R: -- n1 n2 "Push top two entries to Return Stack"
; ## "2>r"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/TwotoR
        ; Push top two entries to Return Stack.
        ;
        ; The same as SWAP >R >R
        ; except that if we jumped here, the return address will be in the
        ; way. May not be natively compiled unless we're clever and use
        ; special routines.
        ; """
.scope
xt_two_to_r:
                ; save the return address
                pla             ; LSB
                sta tmp1
                pla             ; MSB
                sta tmp1+1

                ; --- CUT HERE FOR NATIVE CODING ---
                jsr underflow_2

                ; now we can move the data
                lda 3,x         ; MSB
                pha
                lda 2,x         ; LSB
                pha

                ; now we can move the data
                lda 1,x         ; MSB
                pha
                lda 0,x         ; LSB
                pha

                inx
                inx
                inx
                inx

                ; --- CUT HERE FOR NATIVE CODING ---

                ; restore return address
                lda tmp1+1      ; MSB
                pha
                lda tmp1        ; LSB
                pha

z_two_to_r:     rts
.scend


; ## TWO_CONSTANT (C: d "name" -- ) ( -- d) "Create a constant for a double word"
; ## "2constant"  auto  ANS double
        ; """https://forth-standard.org/standard/double/TwoCONSTANT
        ;
        ; Based on the Forth code
        ; : 2CONSTANT ( D -- )  CREATE SWAP , , DOES> DUP @ SWAP CELL+ @ ;
        ; """
.scope
xt_two_constant:
                jsr underflow_2

                lda #<two_constant_runtime
                ldy #>two_constant_runtime
                jsr cmpl_createYA

                jsr xt_comma
                jmp xt_comma
z_two_constant:
.scend

two_constant_runtime:
                pla          ; pop rts addr
                ply
                sta tmp1
                sty tmp1+1

                ldy #4       ; copy 4 bytes to param stack
*               lda (tmp1),y
                dex
                sta 0,x
                dey
                bne -

                rts


; ## TWO_LITERAL (C: d -- ) ( -- d) "Compile a literal double word"
; ## "2literal"  auto  ANS double
        ; """https://forth-standard.org/standard/double/TwoLITERAL"""
        ; Based on the Forth code
        ; : 2LITERAL ( D -- ) SWAP POSTPONE LITERAL POSTPONE LITERAL ; IMMEDIATE
        ; """
.scope
xt_two_literal:
                jsr underflow_2 ; double number

                jsr swap_nouf
                jsr xt_literal
                jmp xt_literal
z_two_literal:
.scend


; ## TWO_VARIABLE ( "name" -- ) "Create a variable for a double word"
; ## "2variable"  auto  ANS double
        ; """https://forth-standard.org/standard/double/TwoVARIABLE
        ; The variable is initialized to zero.
        ;
        ; This can be realized in Forth as either
        ; CREATE 2 CELLS ALLOT  or just  CREATE 0 , 0 ,
        ; Note that in this case, the variable is not initialized to
        ; zero"""
.scope
xt_two_variable:
                jsr xt_variable     ;1st part is just like VARIABLE
                jmp cmpl_word0      ; compile 2nd word of 0
z_two_variable:
.scend


; ## TYPE ( addr u -- ) "Print string"
; ## "type"  auto  ANS core
        ; """https://forth-standard.org/standard/core/TYPE
        ; Works through EMIT to allow OUTPUT revectoring.
        ; """
.scope
xt_type:
                jsr underflow_2
                bra _test

_loop:
                lda (2,x)           ; Send the current character
                jsr emit_a

                jsr decinc          ; Move the address along
                                    ; Reduce the count (on the data stack)

                ; done if length is zero
_test:          lda 0,x
                ora 1,x
                bne _loop

                inx
                inx
                inx
                inx

z_type:         rts
.scend


decinc:         inc 2,x
                bne +
                inc 3,x
*
                lda 0,x
                bne +
                dec 1,x
*               dec 0,x

                rts


; ## U_DOT ( u -- ) "Print TOS as unsigned number"
; ## "u."  tested  ANS core
        ; """https://forth-standard.org/standard/core/Ud
        ;
        ; This is : U. 0 <# #S #> TYPE SPACE ; in Forth
        ; We use the internal assembler function print_u followed
        ; by a single space
        ; """
.scope
xt_u_dot:
                jsr underflow_1

                jsr print_u
                jsr xt_space

z_u_dot:        rts
.scend


; ## U_DOT_R ( u u -- ) "Print NOS as unsigned number right-justified with TOS with"
; ## "u.r"  tested  ANS core ext
        ; """https://forth-standard.org/standard/core/UDotR"""

.scope
xt_u_dot_r:
                jsr underflow_2

                lda 0,x
                inx
                inx
u_dot_r_A:      ; length in A
                pha
                jsr xt_zero
                jsr xt_less_number_sign
                jsr number_sign_s_nouf
                jsr xt_number_sign_greater
                pla
                jsr PsuZA
                jsr over_nouf
                jsr minus_nouf
                jsr xt_spaces
                jsr xt_type

z_u_dot_r:      rts
.scend


; ## U_GREATER_THAN ( n m -- f ) "Return true if NOS > TOS (unsigned)"
; ## "u>"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/Umore"""
.scope
xt_u_greater_than:
                jsr underflow_2

                lda 0,x
                cmp 2,x
                lda 1,x
                sbc 3,x
                inx
                inx

                lda #0
                adc #$ff
                sta 0,x         ; store flag
                sta 1,x

z_u_greater_than:    rts
.scend

; ## U_LESS_THAN ( n m -- f ) "Return true if NOS < TOS (unsigned)"
; ## "u<"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Uless"""
.scope
xt_u_less_than:
                jsr underflow_2
u_less_than_nouf:
                lda 2,x
                cmp 0,x
                lda 3,x
                sbc 1,x
                inx
                inx

                lda #0
                adc #$ff
                sta 0,x         ; store flag
                sta 1,x

z_u_less_than:    rts
.scend


; ## UD_DOT ( d -- ) "Print double as unsigned"
; ## "ud."  auto  Tali double
        ;
        ; """Based on the Forth code  : UD. <# #S #> TYPE SPACE ;
        ; """
.scope
xt_ud_dot:
                jsr underflow_2 ; double number

                jsr xt_less_number_sign
                jsr number_sign_s_nouf
                jmp dot_comm4
z_ud_dot:
.scend


; ## UD_DOT_R ( d u -- ) "Print unsigned double right-justified u wide"
; ## "ud.r"  auto  Tali double
        ;
        ; """Based on the Forth code : UD.R  >R <# #S #> R> OVER - SPACES TYPE ;
        ; """
.scope
xt_ud_dot_r:
                jsr underflow_3

                lda 0,x
                inx
                inx
                pha
                jsr xt_less_number_sign
                jsr number_sign_s_nouf
                jmp dot_r_comm2
z_ud_dot_r:
.scend


; ## UM_SLASH_MOD ( ud u -- ur u ) "32/16 -> 16 division"
; ## "um/mod"  auto  ANS core
        ; """https://forth-standard.org/standard/core/UMDivMOD
        ; Divide double cell number by single cell number, returning the
        ; quotient as TOS and any remainder as NOS. All numbers are unsigned.
        ; This is the basic division operation all others use. Based on FIG
        ; Forth code, modified by Garth Wilson, see
        ; http://6502.org/source/integers/ummodfix/ummodfix.htm
        ; This uses tmp1, tmp1+1, and tmptos
        ; """
.scope
xt_um_slash_mod:
                jsr underflow_3
um_slash_mod_nouf:
                ; catch division by zero
                lda 0,x
                ora 1,x
                bne _not_zero

                lda #err_divzero
                jmp error

_not_zero:
                ; We loop 17 times
                lda #17
                sta tmptos

_loop:
                ; rotate low cell of dividend one bit left (LSB)
                rol 4,x
                rol 5,x

                ; loop control
                dec tmptos
                beq _done

                ; rotate high cell of dividend one bit left (MSB)
                rol 2,x
                rol 3,x

                stz tmp1        ; store the bit we got from hi cell (MSB)
                rol tmp1

                ; subtract dividend hi cell minus divisor
                sec
                lda 2,x
                sbc 0,x
                sta tmp1+1
                lda 3,x
                sbc 1,x

                tay
                lda tmp1
                sbc #0
                bcc _loop

                ; make result new dividend high cell
                lda tmp1+1
                sta 2,x
                sty 3,x         ; used as temp storage

                bra _loop
_done:
                inx
                inx

                jsr swap_nouf

z_um_slash_mod: rts
.scend


; ## UM_STAR ( u u -- ud ) "Multiply 16 x 16 -> 32"
; ## "um*"  auto  ANS core
        ; """https://forth-standard.org/standard/core/UMTimes
        ; Multiply two unsigned 16 bit numbers, producing a 32 bit result.
        ; Old Forth versions such as FIG Forth call this U*
        ;
        ; This is based on modified FIG Forth code by Dr. Jefyll, see
        ; http://forum.6502.org/viewtopic.php?f=9&t=689 for a detailed
        ; discussion.
        ;
        ; We don't use the system scratch pad (SYSPAD) for temp
        ; storage because >NUMBER uses it as well, but instead tmp1 to
        ; tmp3 (tmp1 is N in the original code, tmp1+1 is N+1, etc).
        ; Old Forth versions such as FIG Forth call this U*
        ;
        ; Consider switching to a table-supported version based on
        ; http://codebase64.org/doku.php?id=base:seriously_fast_multiplication
        ; http://codebase64.org/doku.php?id=magazines:chacking16#d_graphics_for_the_masseslib3d>
        ; http://forum.6502.org/viewtopic.php?p=205#p205
        ; http://forum.6502.org/viewtopic.php?f=9&t=689
        ; """
.scope
xt_um_star:
                jsr underflow_2
um_star_nouf:
                ; to eliminate clc inside the loop, the value at
                ; tmp1 is reduced by 1 in advance
                clc
                lda 0,x         ; copy TOS to tmp2
                sbc #0
                sta tmp2

                lda 1,x
                sbc #0
                bcc _zero       ; is TOS zero?
                sta tmp2+1

                lda #0
                sta tmp1
                stx tmp3        ; tested for exit from outer loop
                dex
                dex
_outer_loop:
                ldy #8          ; counter inner loop
                lsr 4,x         ; think "2,x" then later "3,x"
_inner_loop:
                bcc _no_add
                sta tmp1+1      ; save time, don't CLC
                lda tmp1
                adc tmp2
                sta tmp1
                lda tmp1+1
                adc tmp2+1
_no_add:
                ror
                ror tmp1
                ror 4,x         ; think "2,x" then later "3,x"

                dey
                bne _inner_loop ; go back for one more shift?

                inx
                cpx tmp3
                bne _outer_loop ; go back for eight more shifts?

                ; all done, store high word of result
                sta 1,x
                lda tmp1
                sta 0,x
                rts

_zero:
                stz 2,x
                stz 3,x
_done:
z_um_star:      rts
.scend


; ## UNLOOP ( -- )(R: n1 n2 n3 ---) "Drop loop control from Return stack"
; ## "unloop"  auto  ANS core
        ; """https://forth-standard.org/standard/core/UNLOOP
        ; Must be native compiled.
        ; """
.scope
xt_unloop:
                pla         ; drop DO/?DO index from return stack
                ply
                pla         ; drop DO/?DO limit (FUFA) from return stack
                ply

z_unloop:       rts
.scend


; ## UNTIL (C: dest -- ) ( -- ) "Loop flow control"
; ## "until"  auto  ANS core
        ; """http://forth-standard.org/standard/core/UNTIL"""
.scope
xt_until:
                jsr cmpl_0branch    ; Compile a 0BRANCH

                ; The address to loop back to is on the stack.
                ; Just compile it as the destination for the
                ; 0branch.
                jmp xt_comma
z_until:
.scend


; ## UNUSED ( -- u ) "Return size of space available to Dictionary"
; ## "unused"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/UNUSED
        ; UNUSED does not include the ACCEPT history buffers. Total RAM
        ; should be HERE + UNUSED + <history buffer size>, the last of which
        ; defaults to $400
        ; """
xt_unused:
                dex
                dex

                lda #<cp_end
                sec
                sbc cp
                sta 0,x

                lda #>cp_end
                sbc cp+1
                sta 1,x

z_unused:       rts


; ## UPDATE ( -- ) "Mark current block as dirty"
; ## "update"  auto  ANS block
        ; """https://forth-standard.org/standard/block/UPDATE"""
xt_update:
                ; Turn on the dirty bit.
                ldy #buffstatus_offset
                lda (up),y
                ora #2          ; Turn on dirty flag (bit 2)
                sta (up),y

z_update:       rts


; ## USERADDR ( -- addr ) "Push address of base address of user variables"
; ## "useraddr"  tested  Tali Forth
xt_useraddr:
                lda #up
                jmp PsuZA
z_useraddr:


; ## VALUE ( n "name" -- ) "Define a value"
; ## "value"  auto  ANS core
        ; """https://forth-standard.org/standard/core/VALUE
        ;
        ; This is a dummy header for the WORDLIST. The actual code is
        ; identical to that of CONSTANT
        ; """


; ## VARIABLE ( "name" -- ) "Define a variable"
; ## "variable"  auto  ANS core
        ; """https://forth-standard.org/standard/core/VARIABLE
        ; There are various Forth definitions for this word, such as
        ; CREATE 1 CELLS ALLOT  or  CREATE 0 ,  We use a variant of the
        ; second one so the variable is initialized to zero
        ;
        ; For speed we use a native literal instead of a jsr CFA to dovar.
        ; """
xt_variable:
                lda #0                  ; Allow native!  Not regular HC
                jsr cmpl_wordheaderA

                lda cp                  ; calc addr of start of data
                clc
                adc #11
                tay
                lda cp+1
                adc #0
                pha

                lda #$a9                ; opcode for lda #
                jsr cmpl_word
                lda #$a0                ; opcode for ldy #
                ply
                jsr cmpl_word
                jsr cmpl_PsuYA
                jsr adjust_z            ; length only includes the literal
                                        ; so we can native compile it.
                lda #$60                ; opcode for rts
                jsr cmpl_a

cmpl_word0:     lda #0      ;allocate & initialize variable space
                tay
                jsr cmpl_word

z_variable:     rts


; ## WHILE ( C: dest -- orig dest ) ( x -- ) "Loop flow control"
; ## "while"  auto  ANS core
        ; """http://forth-standard.org/standard/core/WHILE"""
.scope
xt_while:
                jsr cmpl_0branch    ; Compile a 0branch

                ; Put the address (here) where the destination
                ; address needs to go so it can be put there later.
                jsr xt_here

                ; Fill in the destination address with 0 for now.
                jsr cmpl_word0

                ; Swap the two addresses on the stack.
                jmp xt_swap
z_while:
.scend


; ## WITHIN ( n1 n2 n3 -- ) "See if within a range"
; ## "within"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/WITHIN
        ;
        ; This an assembler version of the ANS Forth implementation
        ; at https://forth-standard.org/standard/core/WITHIN which is
        ; OVER - >R - R> U<  note there is an alternative high-level version
        ; ROT TUCK > -ROT > INVERT AND
        ; """"
xt_within:
                jsr underflow_3
                           ; ( n1 n2 n3 )
                sec
                lda 4,x
                sbc 2,x
                sta 4,x
                lda 5,x
                sbc 3,x
                sta 5,x
                            ; ( n1-n2 n2 n3 )
                sec
                lda 0,x
                sbc 2,x
                sta 2,x
                lda 1,x
                sbc 3,x
                sta 3,x
                            ; ( n1-n2 n3-n2 n3 )
                inx
                inx
                            ; ( n1-n2 n3-n2 )
                jmp u_less_than_nouf
z_within:


; ## WORD ( char "name " -- caddr ) "Parse input stream"
; ## "word"  auto  ANS core
        ; """https://forth-standard.org/standard/core/WORD
        ; Obsolete parsing word included for backwards compatibility only.
        ; Do not use this, use PARSE or PARSE-NAME. Skips leading delimiters
        ; and copies word to storage area for a maximum size of 255 bytes.
        ; Returns the result as a counted string (requires COUNT to convert
        ; to modern format), and inserts a space after the string. See "Forth
        ; Programmer's Handbook" 3rd edition p. 159 and
        ; http://www.forth200x.org/documents/html/rationale.html#rat:core:PARSE
        ; for discussions of why you shouldn't be using WORD anymore.
        ;
        ; Forth
        ; would be   PARSE DUP BUFFER1 C! OUTPUT 1+ SWAP MOVE BUFFER1
        ; We only allow input of 255 chars. Seriously, use PARSE-NAME.
        ; """
.scope
xt_word:
                jsr underflow_1

                ; Skip over leading delimiters - this is like PARSE-NAME,
                ; but unlike PARSE
                ldy toin                ; >IN
                dey
_loop:          iny
                cpy ciblen              ; quit if end of input
                beq _found_char
                lda (cib),y
                cmp 0,x                 ; ASCII of delimiter
                beq _loop
_found_char:    sty toin                ; Save index of where word starts

                ; The real work is done by parse
                jsr xt_parse            ; Returns ( addr u )

                ; Convert the modern ( addr u ) string format to obsolete
                ; ( caddr ) format. We just do this in the Dictionary
                lda 0,x
                sta (cp)                ; Save length of string
                pha                     ; Keep copy of length for later

                jsr xt_dup              ; ( addr u u )
                lda cp
                clc
                adc #1
                sta 2,x                 ; LSB of CP
                lda cp+1
                adc #0
                sta 3,x                 ; ( addr cp+1 u )

                jsr xt_move

                ; Return caddr
                jsr xt_here

                ; Adjust CP
                pla                     ; length of string
                clc
                adc cp
                sta cp
                lda cp+1
                adc #0                  ; we only need the carry
                sta cp+1
z_word:         rts
.scend

; ## WORDLIST ( -- wid ) "Create new wordlist (from pool of max_wordlists)"
; ## "wordlist" auto ANS search
        ; """https://forth-standard.org/standard/search/WORDLIST
        ; See the tutorial o Wordlists and the Search Order for
        ; more information.
        ; """
.scope
xt_wordlist:
                ; Get the current number of wordlists
                ldy #num_wordlists_offset
                lda (up),y      ; This is a byte variable, so only
                                ; the LSB needs to be checked.
                ; See if we are already at the max.
                cmp #max_wordlists
                bne _ok

                ; Print an error message if all wordlists used.
                lda #err_wordlist
                jmp error

_ok:            inc             ; Increment the wordlist#
                sta (up),y      ; Save it into byte variable #wordlists
                jmp PsuZA       ; and put it on the stack.

z_wordlist:
.scend


; ## WORDS ( -- ) "Print known words from Dictionary"
; ## "words"  tested  ANS tools
        ; """https://forth-standard.org/standard/tools/WORDS
        ; This is pretty much only used at the command line so we can
        ; be slow and try to save space. DROP must always be the first word in a
        ; clean system (without Forth words), BYE the last. There is no reason
        ; why we couldn't define this as a high level word except that it is
        ; really useful for testing
        ; """
.scope
xt_words:
                ; we follow Gforth by starting on the next line
                jsr xt_cr

                ; We pretty-format the output by inserting a line break
                ; before the end of the line. We can get away with pushing
                ; the counter to the stack because this is usually an
                ; interactive word and speed is not that important
                lda #0
                pha

                ; Set up for traversing the wordlist search order.
                dex                     ; Make room on the stack for
                dex                     ; a dictionary pointer.
                stz tmp3                ; Start at the beginning of
                                        ; the search order.
_wordlist_loop:
                ldy #num_order_offset   ; Check against byte variable #ORDER.
                lda tmp3
                cmp (up),y              ; See if we are done.
                beq _words_done         ; ran out of wordlists to search?

                ; start with last word in Dictionary
                ; Get the current wordlist id
                clc                     ; Index into byte array SEARCH-ORDER.
                adc #search_order_offset
                tay
                lda (up),y              ; Get the index into array WORDLISTS

                ; Get the DP for that wordlist.
                asl                     ; Turn offset into cells offset.
                clc
                adc #wordlists_offset
                tay
                lda (up),y              ; Save the DP for this wordlist
                sta 0,x                 ; on the stack. ( nt )
                iny
                lda (up),y
                sta 1,x

_loop:
                jsr xt_dup              ; ( nt nt )
                jsr xt_name_to_string   ; ( nt addr u )

                ; Insert line break if we're about to go past the end of the
                ; line
                pla
                clc
                adc 0,x
                inc                     ; don't forget the space between words
                cmp #MAX_LINE_LENGTH
                bcc +

                jsr xt_cr
                lda #0
*
                pha
                jsr xt_type             ; ( nt )

                jsr xt_space

                ; get next word, which begins two down
                jsr xt_two_plus_nouf    ; 1+ 1+
                jsr xt_fetch            ; @ ( nt+1 )

                ; if next address is zero, we're done
                lda 1,x
                bne _loop

                ; Move on to the next wordlist in the search order.
                inc tmp3
                bra _wordlist_loop

_words_done:
                pla                     ; drop counter

                inx                     ; drop word nt
                inx

z_words:        rts
.scend


; ## WORDSIZE ( nt -- u ) "Get size of word in bytes"
; ## "wordsize"  auto  Tali Forth
        ; """Given an word's name token (nt), return the size of the
        ; word's payload size in bytes (CFA plus PFA) in bytes. Does not
        ; count the final RTS.
        ; """
.scope
xt_wordsize:
                jsr underflow_1

                ; We get the start address of the word from its header entry
                ; for the start of the actual code (execution token, xt)
                ; which is four bytes down, and the pointer to the end of the
                ; code (z_word, six bytes down)
                lda 0,x
                sta tmp1
                lda 1,x
                sta tmp1+1

                ldy #nt_z
                lda (tmp1),y    ; LSB of z
                ldy #nt_xt
                sec
                sbc (tmp1),y    ; LSB of xt
                sta 0,x

                ldy #nt_z+1
                lda (tmp1),y    ; MSB of z
                ldy #nt_xt+1
                sbc (tmp1),y    ; MSB of xt
                sta 1,x

z_wordsize:     rts
.scend


; ## XOR ( n n -- n ) "Logically XOR TOS and NOS"
; ## "xor"  auto  ANS core
        ; """https://forth-standard.org/standard/core/XOR"""
.scope
xt_xor:
                jsr underflow_2

                lda 0,x
                eor 2,x
                sta 2,x

                lda 1,x
                eor 3,x
                sta 3,x

                inx
                inx

z_xor:          rts
.scend


; ## ZERO ( -- 0 ) "Push 0 to Data Stack"
; ## "0"  auto  Tali Forth
        ; """The disassembler assumes that this routine does not use Y. Note
        ; that CASE and FORTH-WORDLIST use the same routine, as the WD for Forth
        ; is 0."""
xt_case:
xt_forth_wordlist:
xt_zero:
                dex             ; push
                dex
                stz 0,x
                stz 1,x

z_case:
z_forth_wordlist:
z_zero:
                rts


; ## ZERO. ( -- 0 0 ) "Push 0. to Data Stack"
xt_zero_dot:    jsr xt_zero
                bra xt_zero


; ## ZERO_EQUAL ( n -- f ) "Check if TOS is zero"
; ## "0="  auto  ANS core
        ; """https://forth-standard.org/standard/core/ZeroEqual"""
; TODO Rewrite to the form of 0<>
.scope
xt_zero_equal:
                jsr underflow_1

                ldy #0       ; assume FALSE

                lda 0,x
                ora 1,x
                bne _store

                dey          ; a zero, so we need a TRUE flag

_store:         sty 0,x
                sty 1,x

z_zero_equal:   rts
.scend


; ## ZERO_GREATER ( n -- f ) "Return a TRUE flag if TOS is positive"
; ## "0>"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/Zeromore"""
.scope
xt_zero_greater:
                jsr underflow_1

                ldy #0          ; Default is FALSE (TOS is negative)

                lda 1,x         ; MSB
                bmi _done       ; TOS is negative, keep FLASE
                ora 0,x
                beq _done       ; TOS is zero, keep FALSE

                dey             ; TOS is postive, make true

_done:          sty 0,x
                sty 1,x

z_zero_greater: rts
.scend


; ## ZERO_LESS ( n -- f ) "Return a TRUE flag if TOS negative"
; ## "0<"  auto  ANS core
        ; """https://forth-standard.org/standard/core/Zeroless"""
.scope
xt_zero_less:
                jsr underflow_1

                ldy #0          ; Default is FALSE (TOS positive)

                lda 1,x         ; MSB
                bpl _done       ; TOS is positive, so keep FALSE

                dey             ; TOS is negative, make TRUE
_done:          sty 0,x
                sty 1,x

z_zero_less:    rts
.scend


; ## ZERO_UNEQUAL ( m -- f ) "Return TRUE flag if not zero"
; ## "0<>"  auto  ANS core ext
        ; """https://forth-standard.org/standard/core/Zerone"""
.scope
xt_zero_unequal:
                jsr underflow_1

                lda 0,x
                ora 1,x
                beq _store

                lda #$ff
_store:         sta 0,x
                sta 1,x

z_zero_unequal: rts
.scend

;
; ==========================================================
; EDITOR words

; This routine is used by both enter-screen and erase-screen
; to get a buffer for the given screen number and set SCR to
; the given screen number.  This word is not in the dictionary.
.scope
xt_editor_screen_helper:
                jsr xt_dup
                jsr xt_scr
                jsr xt_store
                jmp xt_buffer
.scend


; ## EDITOR_ENTER_SCREEN ( scr# -- ) "Enter all lines for given screen"
; ## "enter-screen"  auto  Tali Editor
.scope
xt_editor_enter_screen:
                ; Set the variable SCR and get a buffer for the
                ; given screen number.
                jsr xt_editor_screen_helper
                ; Drop the buffer address.
                jsr xt_drop
                ; Overwrite the lines one at a time.
                stz editor1
_prompt_loop:
                ; Put the current line number on the stack.
                dex
                dex
                lda editor1
                sta 0,x
                stz 1,x
                ; Use the O word to prompt for overwrite.
                jsr xt_editor_o
                ; Move on to the next line.
                inc editor1
                lda #16
                cmp editor1
                bne _prompt_loop
z_editor_enter_screen:
                rts
.scend


; ## EDITOR_ERASE_SCREEN ( scr# -- ) "Erase all lines for given screen"
; ## "erase-screen"  tested  Tali Editor
.scope
xt_editor_erase_screen:
                ; Set the variable SCR and get a buffer for the
                ; given screen number.
                jsr xt_editor_screen_helper
                ; Put 1024 (chars/screen) on stack.
                dex
                dex
                stz 0,x
                lda #4          ; 4 in MSB makes 1024.
                sta 1,x
                ; Erase the entire block (fill with spaces).
                jsr xt_blank
                ; Mark buffer as updated.
                jsr xt_update
z_editor_erase_screen:
                rts
.scend


; ## EDITOR_EL ( line# -- ) "Erase the given line number"
; ## "el"  tested  Tali Editor
.scope
xt_editor_el:
                ; Turn the line number into buffer offset.
                ; This also loads the block into the buffer if it's
                ; not there for some reason.
                jsr xt_editor_line
                lda #64         ; Put 64 (# of chars/line) on the stack.
                jsr PsuZA
                ; Fill with spaces.
                jsr xt_blank
                ; Mark buffer as updated.
                jsr xt_update
z_editor_el:    rts
.scend


; ## EDITOR_L ( -- ) "List the current screen"
; ## "l"  tested  Tali Editor
.scope
xt_editor_l:
                ; Load the current screen
                dex             ; Put SCR on the stack.
                dex
                ldy #scr_offset
                lda (up),y
                sta 0,x
                iny
                lda (up),y
                sta 1,x
                jsr xt_block    ; Get the current screen.

                jsr xt_cr

                ; Print the screen number.
                ; We're using sliteral, so we need to set up the
                ; appropriate data structure (see sliteral)
                jsr sliteral_runtime2
                jmp +
                .byte "Screen #"
*
                jsr xt_type

                ; Put the screen number and printed size for u.r on the stack.
                jsr xt_scr
                jsr xt_fetch
                lda #4          ; four spaces
                jsr u_dot_r_A

                ; The address of the buffer is currently on the stack.
                ; Print 64 chars at a time.  TYPE uses tmp1, so we'll
                ; keep track of the line number in tmp3.
                stz tmp3
_line_loop:
                jsr xt_cr
                ; Print the line number (2-space fixed width)
                lda tmp3
                jsr PsuZA
                lda #2
                jsr u_dot_r_A
                jsr xt_space

                ; Print one line using the address on the stack.
                jsr xt_dup
                lda #64
                jsr PsuZA
                jsr xt_type

                ; Add 64 to the address on the stack to move to the next line.
                clc
                lda #64
                adc 0,x
                sta 0,x
                bcc +
                inc 1,x
*
                ; Increment the line number (held in tmp3)
                inc tmp3

                ; See if we are done.
                lda tmp3
                cmp #16
                bne _line_loop

                jsr xt_cr
                ; Drop the address on the stack.
                inx
                inx

z_editor_l:     rts
.scend


; ## EDITOR_LINE ( line# -- c-addr ) "Turn a line number into address in current screen"
; ## "line"  tested  Tali Editor
.scope
xt_editor_line:
                jsr underflow_1

                ; Multiply the TOS by 64 (chars/line) to compute offset.
                lda #6          ; *64 is same as left shift 6 times.
                jsr lshift_A
                ; Load the current screen into a buffer
                ; and get the buffer address
                jsr xt_scr
                jsr xt_fetch
                jsr xt_block

                ; Add the offset to the buffer base address.
                jsr plus_nouf

z_editor_line:  rts
.scend


; ## EDITOR_O ( line# -- ) "Overwrite the given line"
; ## "o"  tested  Tali Editor
.scope
xt_editor_o:
                ; Erase the line
                jsr xt_dup
                jsr xt_editor_el
                ; Print prompt
                jsr xt_cr
                jsr xt_dup
                lda #2
                jsr u_dot_r_A
                jsr xt_space
                lda #42         ; ASCII for *
                jsr emit_a
                jsr xt_space
                ; Accept new input (directly into the buffer)
                jsr xt_editor_line
                lda #64         ; chars/line
                jsr PsuZA
                jsr xt_accept
                ; Drop result from accept.
                inx
                inx
                ; Mark buffer as updated.
                jsr xt_update

z_editor_o:     rts
.scend

; END
