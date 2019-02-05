; Disassembler for Tali Forth 2 
; Scot W. Stevenson <scot.stevenson@gmail.com>
; First version: 28. Apr 2018
; This version: 18. Dec 2018 forked

; This is the default disassembler for Tali Forth 2. Use by passing
; the address and length of the block of memory to be disassembled:
;
;       disasm ( addr length -- ) 

; The underflow checking is handled by the word's stub in
; native_words.asm, see there for more information.

; The code is disassembled in Simpler Assembler Notation (SAN), because that
; is, uh, simpler. See the documentation for more information.  Because
; disassemblers are used interactively with slow humans, we don't care that
; much about speed and put the emphasis at being small.

.scope
disassembler: ; using assembler tables
                jsr xt_over     ; convert length to limit
                jsr xt_plus
_instruction_loop:
               
                lda 2,x         ; All done?
                cmp 0,x
                lda 3,x
                sbc 1,x
                bcc +

                ; Clean up and leave
                jmp xt_two_drop         ; JSR/RTS

*
                jsr xt_cr       ; ( addr u ) 

                ; Print address at start of the line.
                ; Note we use whatever number base the user has
                jsr xt_over     ; ( addr u addr ) 
                jsr xt_u_dot    ; ( addr u ) 
                jsr xt_space

                ; find the assembler word with matching opcode
                ldy #2*2+1+wordlists_offset ; for tmp1=each word in assembler wordlist
                lda (up),y
                pha
                dey
                lda (up),y
                bra _w_1

_w_next:
                ldy #2+1 ; nt = nt->next
                lda (tmp1),y
                pha
                dey
                lda (tmp1),y
_w_1:           ply
                sta tmp1
                sty tmp1+1
                beq _w_notfound
        
                ldy #4          ; tmp2 = nt->xt
                lda (tmp1),y
                sta tmp2
                iny
                lda (tmp1),y
                sta tmp2+1
        
                ldy #3          ; opcode match?
                lda (tmp2),y
                cmp (2,x)
                bne _w_next

                lda (tmp2)      ; word starts with jsr abs?
                cmp #$20
                bne _w_next
        
                jsr _GetByte    ; consume the opcode

                lda tmp1        ; save nt for printing mnemonic
                ldy tmp1+1
                phy
                pha

               ; Since this is Simpler Assembler Notation (SAN) in a Forth
                ; system, we want to print any operand before we print the
                ; mnemonic ('1000 sta' instead of 'sta 1000'). This allows us
                ; to copy and paste directly from the disassembler to the
                ; assembler. 

                ldy #1          ; get jsr abs lo byte
                lda (tmp2),y
                cmp #<asm_ubyte
                beq _1_operand
                cmp #<asm_1byte
                beq _1_operand
                cmp #<asm_2byte
                beq _2_operand

                ; Default to no operand.
                ; Since we want to have a nicely formatted output, we need to
                ; indent the mnemonic by five spaces.
                lda #5
                jsr PsuZA               ; ( addr u 5 )
                jsr xt_spaces           ; ( addr u )
                bra _print_mnemonic

_2_operand:     ; 2 operand bytes
                jsr _GetByte            ; get LSB
                pha
                jsr _GetByte            ; get MSB
                tay
                pla
                bra _print_operand
                
_1_operand:     ; 1 operand byte
                jsr _GetByte
                ldy #0

_print_operand:
                ; We arrive here with the operand in YA.
                jsr PsuYA
                jsr PsuYA
                                
                ; We want the output to be nicely formatted in columns, so
                ; we use U.R. The maximal width of the number in decimal on an
                ; 16-bit addressed machine is five characters
                lda #5
                jsr u_dot_r_A          ; U.R ( addr+n u-n )

                jsr xt_sym

                ; fall through to _print_mnemonic

_print_mnemonic:
                ; Time to print the mnemonic.
                jsr xt_space
                pla                     ; get saved nt
                ply
                jsr PsuYA
                jsr xt_name_to_string
                jsr xt_type
   
                jmp _instruction_loop

_w_notfound:    jsr _GetByte
                jsr PsuZA
                jsr xt_u_dot
                jsr xt_space
                lda #'c
                jsr emit_a
                lda #$2c ; ',
                jsr emit_a
                jmp _instruction_loop

_GetByte: ; get next byte from instruction stream
                lda (2,x)       ; get the byte
                
                inc 2,x         ; increment adr
                bne +
                inc 3,x
*                
                rts
disassembler_end:
.scend


; .scope
; disassembler2: ; standalone
; _loop:
                ; jsr xt_cr       ; ( addr u ) 

                ; Print address at start of the line.
                ; Note we use whatever number base the user has
                ; jsr xt_over     ; ( addr u addr ) 
                ; jsr xt_u_dot    ; ( addr u ) 
                ; jsr xt_space

                ; jsr _GetByte    ; get opcode that addr points to
                ; pha             ; save a copy for later
                ; tay             ; use Y as the index
                ; lda _ATbl,y     ; get addressing mode index
                ; tay             ; get operand byte count
                ; lda _A0,y
                ; stz tmp1
                ; stz tmp1+1
                ; lsr
                ; lsr
                ; lsr
                ; lsr

                ; Since this is Simpler Assembler Notation (SAN) in a Forth
                ; system, we want to print any operand before we print the
                ; mnemonic ('1000 sta' instead of 'sta 1000'). This allows us
                ; to copy and paste directly from the disassembler to the
                ; assembler. 

                ; beq _no_operand         ; no operand bytes?
                ; dea
                ; beq _1_operand
                
                ; 2 operand bytes
                ; jsr _GetByte            ; get LSB
                ; pha
                ; jsr _GetByte            ; get MSB
                ; tay
                ; pla
                ; bra _print_operand
                
; _1_operand:     ; 1 operand byte
                ; jsr _GetByte
                ; ldy #0

; _print_operand:
                ; We arrive here with the operand in YA.
                ; jsr PsuYA
                ; jsr PsuYA
                ; We want the output to be nicely formatted in columns, so
                ; we use U.R. The maximal width of the number in decimal on an
                ; 16-bit addressed machine is five characters
                ; lda #5
                ; jsr u_dot_r_A          ; U.R ( addr+n u-n )

                ; jsr xt_sym
                
                ; bra _print_mnemonic

; _no_operand:
                ; We arrive here with the opcode table address on the stack,
                ; the lengths byte in Y and ( addr u ). Since we want to have
                ; a nicely formatted output, we need to indent the mnemonic by
                ; five spaces.
                ; lda #5
                ; jsr PsuZA               ; ( addr u 5 )
                ; jsr xt_spaces           ; ( addr u )

                ; fall through to _print_mnemonic
                
; _print_mnemonic:
                ; We arrive here with ???
                ; Time to print the mnemonic.
                ; jsr xt_space

                ; ply                 ; copy saved opcode
                ; phy
                ; lda _MTbl,y         ; get start offset in M0
                ; tay
                ; lda _M0+0,y         ; print 1st char
                ; and #$7f
                ; jsr emit_a
                ; lda _M0+1,y
                ; jsr emit_a
                ; lda _M0+2,y
                ; jsr emit_a

                ; add bit # to end of mnemonic
                ; lda _M0,y           ; get saved M value
                ; bpl +
                ; pla                 ; copy saved opcode
                ; pha
                ; lsr
                ; lsr
                ; lsr
                ; lsr
                ; and #7
                ; ora #'0
                ; jsr emit_a
; *
                ; print addressing mode
                ; ply                 ; get saved opcode
                ; lda _ATbl,y
                ; tay
                ; lda _A0,y
                ; and #$0f
                ; beq _am2
; _am1:           pha
                ; iny
                ; lda _A0,y
                ; jsr emit_a
                ; pla
                ; dec
                ; bne _am1
; _am2:
                
                
                ; lda 1,x                 ; All done?
                ; bmi _done
                ; ora 0,x
                ; beq _done
                ; jmp _loop
; _done:
                ; Clean up and leave
                ; jmp xt_two_drop         ; JSR/RTS


; _GetByte: ; get next byte from instruction stream
                ; lda 0,x         ; decrement length
                ; bne +
                ; dec 1,x
; *               dec 0,x

                ; lda (2,x)       ; get the byte
                
                ; inc 2,x         ; increment adr
                ; bne +
                ; inc 3,x
; *
                
                ; rts

; =========================================================

; _ATbl: ; address_mode indexed by opcode
        ; .byte _A1-_A0   ; 00 brk    enforce the signature byte
        ; .byte _AZXI-_A0 ; 01 ora.zxi
        ; .byte 0         ; 02
        ; .byte 0         ; 03
        ; .byte _AZ-_A0   ; 04 tsb.z
        ; .byte _AZ-_A0   ; 05 ora.z
        ; .byte _AZ-_A0   ; 06 asl.z
        ; .byte _AZ-_A0   ; 07 rmb0.z
        ; .byte 0         ; 08 php
        ; .byte _AN-_A0   ; 09 ora.#
        ; .byte _AA-_A0   ; 0A asl.a
        ; .byte 0         ; 0B
        ; .byte _A2-_A0   ; 0C tsb
        ; .byte _A2-_A0   ; 0D ora
        ; .byte _A2-_A0   ; 0E asl
        ; .byte _A2-_A0   ; 0F bbr0
        ; .byte _A1-_A0   ; 10 bpl
        ; .byte _AZIY-_A0 ; 11 ora.ziy
        ; .byte _AZI-_A0  ; 12 ora.zi
        ; .byte 0         ; 13
        ; .byte _AZ-_A0   ; 14 trb
        ; .byte _AZX-_A0  ; 15 ora.zx
        ; .byte _AZX-_A0  ; 16 asl.zx
        ; .byte _AZ-_A0   ; 17 rmb1.z
        ; .byte 0         ; 18 clc
        ; .byte _AY-_A0   ; 19 ora.y
        ; .byte _AA-_A0   ; 1A inc.a
        ; .byte 0         ; 1B
        ; .byte _A2-_A0   ; 1C trb
        ; .byte _AX-_A0   ; 1D ora.x
        ; .byte _AX-_A0   ; 1E asl.x
        ; .byte _A2-_A0   ; 1F bbr1
        ; .byte _A2-_A0   ; 20 jsr
        ; .byte _AZXI-_A0 ; 21 and.zxi
        ; .byte 0         ; 22
        ; .byte 0         ; 23
        ; .byte _AZ-_A0   ; 24 bit.z
        ; .byte _AZ-_A0   ; 25 and.z
        ; .byte _AZ-_A0   ; 26 rol.z
        ; .byte _AZ-_A0   ; 27 rmb2.z
        ; .byte 0         ; 28 plp
        ; .byte _AN-_A0   ; 29 and.#
        ; .byte _AA-_A0   ; 2A rol.a
        ; .byte 0         ; 2B
        ; .byte _A2-_A0   ; 2C bit
        ; .byte _A2Dot-_A0    ; 2D and.
        ; .byte _A2-_A0   ; 2E rol
        ; .byte _A2-_A0   ; 2F bbr2
        ; .byte _A1-_A0   ; 30 bmi
        ; .byte _AZIY-_A0 ; 31 and.ziy
        ; .byte _AZI-_A0  ; 32 and.zi
        ; .byte 0         ; 33
        ; .byte _AZXI-_A0 ; 34 bit.zxi
        ; .byte _AZX-_A0  ; 35 and.zx
        ; .byte _AZX-_A0  ; 36 rol.zx
        ; .byte _AZ-_A0   ; 37 rmb3.z
        ; .byte 0         ; 38 sec
        ; .byte _AY-_A0   ; 39 and.y
        ; .byte _AA-_A0   ; 3A dec.a
        ; .byte 0         ; 3B
        ; .byte _AX-_A0   ; 3C bit.x
        ; .byte _AX-_A0   ; 3D and.x
        ; .byte _AX-_A0   ; 3E rol.x
        ; .byte _A2-_A0   ; 3F bbr3
        ; .byte 0         ; 40 rti
        ; .byte _AZXI-_A0 ; 41 eor.zxi
        ; .byte 0         ; 42
        ; .byte 0         ; 43
        ; .byte 0         ; 44
        ; .byte _AZ-_A0   ; 45 eor.z
        ; .byte _AZ-_A0   ; 46 lsr.z
        ; .byte _AZ-_A0   ; 47 rmb4.z
        ; .byte 0         ; 48 pha
        ; .byte _AN-_A0   ; 49 eor.#
        ; .byte _AA-_A0   ; 4a lsr.a
        ; .byte 0         ; 4B
        ; .byte _A2-_A0   ; 4C jmp
        ; .byte _A2-_A0   ; 4D eor
        ; .byte _A2-_A0   ; 4E lsr
        ; .byte _A2-_A0   ; 4F bbr4
        ; .byte _A1-_A0   ; 50 bvc
        ; .byte _AZIY-_A0 ; 51 eor.ziy
        ; .byte _AZI-_A0  ; 52 eor.zi
        ; .byte 0         ; 53
        ; .byte 0         ; 54
        ; .byte _AZX-_A0  ; 55 eor.zx
        ; .byte _AZX-_A0  ; 56 lsr.zx
        ; .byte _AZ-_A0   ; 57 rmb5.z
        ; .byte 0         ; 58 cli
        ; .byte _AY-_A0   ; 59 eor.y
        ; .byte 0         ; 5A phy
        ; .byte 0         ; 5B
        ; .byte 0         ; 5C
        ; .byte _AX-_A0   ; 5D eor.x
        ; .byte _AX-_A0   ; 5E lsr.x
        ; .byte _A2-_A0   ; 5F bbr5
        ; .byte 0         ; 60 rts
        ; .byte _AZXI-_A0 ; 61 adc.zxi
        ; .byte 0         ; 62
        ; .byte 0         ; 63
        ; .byte _AZ-_A0   ; 64 stz.z
        ; .byte _AZ-_A0   ; 65 adc.z
        ; .byte _AZ-_A0   ; 66 ror.z
        ; .byte _AZ-_A0   ; 67 rmb6.z
        ; .byte 0         ; 68 pla
        ; .byte _AN-_A0   ; 69 adc.#
        ; .byte _AA-_A0   ; 6A ror.a
        ; .byte 0         ; 6B
        ; .byte _AI-_A0   ; 6C jmp.i
        ; .byte _A2-_A0   ; 6D adc
        ; .byte _A2-_A0   ; 6E ror
        ; .byte _A2-_A0   ; 6F bbr6
        ; .byte _A1-_A0   ; 70 bvs
        ; .byte _AZIY-_A0 ; 71 adc.ziy
        ; .byte _AZI-_A0  ; 72 adc.zi
        ; .byte 0         ; 73
        ; .byte _AZX-_A0  ; 74 stz.zx
        ; .byte _AZX-_A0  ; 75 adc.zx
        ; .byte _AZX-_A0  ; 76 ror.zx
        ; .byte _AZ-_A0   ; 77 rmb7.z
        ; .byte 0         ; 78 sei
        ; .byte _AY-_A0   ; 79 adc.y
        ; .byte 0         ; 7A ply
        ; .byte 0         ; 7B
        ; .byte _AXI-_A0  ; 7C jmp.xi
        ; .byte _AX-_A0   ; 7D adc.x
        ; .byte _AX-_A0   ; 7E ror.x
        ; .byte _A2-_A0   ; 7F bbr7
        ; .byte _A1-_A0   ; 80 bra
        ; .byte _AZXI-_A0 ; 81 sta.zxi
        ; .byte 0         ; 82
        ; .byte 0         ; 83
        ; .byte _AZ-_A0   ; 84 sty.z
        ; .byte _AZ-_A0   ; 85 sta.z
        ; .byte _AZ-_A0   ; 86 stx.z
        ; .byte _AZ-_A0   ; 87 smb0.z
        ; .byte 0         ; 88 dey
        ; .byte _AN-_A0   ; 89 bit.#
        ; .byte 0         ; 8A txa
        ; .byte 0         ; 8B
        ; .byte _A2-_A0   ; 8C sty
        ; .byte _A2-_A0   ; 8D sta
        ; .byte _A2-_A0   ; 8E stx
        ; .byte _A2-_A0   ; 8F bbs0
        ; .byte _A1-_A0   ; 90 bcc
        ; .byte _AZIY-_A0 ; 91 sta.ziy
        ; .byte _AZI-_A0  ; 92 sta.zi
        ; .byte 0         ; 93
        ; .byte _AZX-_A0  ; 94 sty.zx
        ; .byte _AZX-_A0  ; 95 sta.zx
        ; .byte _AZY-_A0  ; 96 stx.zy
        ; .byte _AZ-_A0   ; 97 smb1.z
        ; .byte 0         ; 98 tya
        ; .byte _AY-_A0   ; 99 sta.y
        ; .byte 0         ; 9A txs
        ; .byte 0         ; 9B
        ; .byte _A2-_A0   ; 9C stz
        ; .byte _AX-_A0   ; 9D sta.x
        ; .byte _AX-_A0   ; 9E stz.x
        ; .byte _A2-_A0   ; 9F bbs1
        ; .byte _AN-_A0   ; A0 ldy.#
        ; .byte _AZXI-_A0 ; A1 lda.zxi
        ; .byte _AN-_A0   ; A2 ldx.#
        ; .byte 0         ; A3
        ; .byte _AZ-_A0   ; A4 ldy.z
        ; .byte _AZ-_A0   ; A5 lda.z
        ; .byte _AZ-_A0   ; A6 ldx.z
        ; .byte _AZ-_A0   ; A7 smb2.z
        ; .byte 0         ; A8 tay
        ; .byte _AN-_A0   ; A9 lda.#
        ; .byte 0         ; AA tax
        ; .byte 0         ; AB
        ; .byte _A2-_A0   ; AC ldy
        ; .byte _A2-_A0   ; AD lda
        ; .byte _A2-_A0   ; AE ldx
        ; .byte _A2-_A0   ; AF bbs2
        ; .byte _A1-_A0   ; B0 bcs
        ; .byte _AZIY-_A0 ; B1 lda.ziy
        ; .byte _AZI-_A0  ; B2 lda.zi
        ; .byte 0         ; B3
        ; .byte _AZX-_A0  ; B4 ldy.zx
        ; .byte _AZX-_A0  ; B5 lda.zx
        ; .byte _AZY-_A0  ; B6 ldx.zy
        ; .byte _AZ-_A0   ; B7 smb3.z
        ; .byte 0         ; B8 clv
        ; .byte _AY-_A0   ; B9 lda.y
        ; .byte 0         ; BA tsx
        ; .byte 0         ; BB
        ; .byte _AX-_A0   ; BC ldy.x
        ; .byte _AX-_A0   ; BD lda.x
        ; .byte _AY-_A0   ; BE ldx.y
        ; .byte _A2-_A0   ; BF bbs4
        ; .byte _AN-_A0   ; C0 cpy.#
        ; .byte _AZXI-_A0 ; C1 cmp.zxi
        ; .byte 0         ; C2
        ; .byte 0         ; C3
        ; .byte _AZ-_A0   ; C4 cpy.z
        ; .byte _AZ-_A0   ; C5 cmp.z
        ; .byte _AZ-_A0   ; C6 dec.z
        ; .byte _AZ-_A0   ; C7 smb4.z
        ; .byte 0         ; C8 iny
        ; .byte _AN-_A0   ; C9 cmp.#
        ; .byte 0         ; CA dex
        ; .byte 0         ; CB
        ; .byte _A2-_A0   ; CC cpy
        ; .byte _A2-_A0   ; CD cmp
        ; .byte _A2-_A0   ; CE dec
        ; .byte _A2-_A0   ; CF bbs4
        ; .byte _A1-_A0   ; D0 bne
        ; .byte _AZIY-_A0 ; D1 cmp.ziy
        ; .byte _AZI-_A0  ; D2 cmp.zi
        ; .byte 0         ; D3
        ; .byte 0         ; D4
        ; .byte _AZX-_A0  ; D5 cmp.zx
        ; .byte _AZX-_A0  ; D6 dec.zx
        ; .byte _AZ-_A0   ; D7 smb5.z
        ; .byte 0         ; D8 cld
        ; .byte _AY-_A0   ; D9 cmp.y
        ; .byte 0         ; DA phx
        ; .byte 0         ; DB
        ; .byte 0         ; DC
        ; .byte _AX-_A0   ; DD cmp.x
        ; .byte _AX-_A0   ; DE dec.x
        ; .byte _A2-_A0   ; DF bbs5
        ; .byte _AN-_A0   ; E0 cpx.#
        ; .byte _AZXI-_A0 ; E1 sbc.zxi
        ; .byte 0         ; E2
        ; .byte 0         ; E3
        ; .byte _AZ-_A0   ; E4 cpx.z
        ; .byte _AZ-_A0   ; E5 sbc.z
        ; .byte _AZ-_A0   ; E6 inc.z
        ; .byte _AZ-_A0   ; E7 smb6.z
        ; .byte 0         ; E8 inx
        ; .byte _AN-_A0   ; E9 sbc.#
        ; .byte 0         ; EA nop
        ; .byte 0         ; EB
        ; .byte _A2-_A0   ; EC cpx
        ; .byte _A2-_A0   ; ED sbc
        ; .byte _A2-_A0   ; EE inc
        ; .byte _A2-_A0   ; EF bbs6
        ; .byte _A1-_A0   ; F0 beq
        ; .byte _AZIY-_A0 ; F1 sbc.ziy
        ; .byte _AZI-_A0  ; F2 sbc.zi
        ; .byte 0         ; F3
        ; .byte 0         ; F4
        ; .byte _AZX-_A0  ; F5 sbc.zx
        ; .byte _AZX-_A0  ; F6 inc.zx
        ; .byte _AZ-_A0   ; F7 smb7.z
        ; .byte 0         ; F8 sed
        ; .byte _AY-_A0   ; F9 sbc.y
        ; .byte 0         ; FA plx
        ; .byte 0         ; FB
        ; .byte 0         ; FC
        ; .byte _AX-_A0   ; FD sbc.x
        ; .byte _AX-_A0   ; FE inc.x
        ; .byte _A2-_A0   ; FF bbs7

; _A0: ; address mode info
        ; 1st byte:
            ; hi nibble is # of operand bytes
            ; lo nibble is # of characters in string
        ; .byte $00
; _A1:    .byte $10
; _A2:    .byte $20
; _A2Dot: .byte $21,"."
; _AA:    .byte $02,".a"
; _AI:    .byte $22,".i"
; _AN:    .byte $12,".#"
; _AX:    .byte $22,".x"
; _AXI:   .byte $23,".xi"
; _AY:    .byte $22,".y"
; _AZ:    .byte $12,".z"
; _AZI:   .byte $13,".zi"
; _AZIY:  .byte $14,".ziy"
; _AZX:   .byte $13,".zx"
; _AZY:   .byte $13,".zy"
; _AZXI:  .byte $14,".zxi"


; _MTbl: ; mnemonics indexed by opcode
        ; .byte _M_brk-_M0    ;00
        ; .byte _M_ora-_M0
        ; .byte 0
        ; .byte 0
        ; .byte _M_tsb-_M0    ;04
        ; .byte _M_ora-_M0
        ; .byte _M_asl-_M0
        ; .byte _M_rmb-_M0
        ; .byte _M_php-_M0    ;08
        ; .byte _M_ora-_M0
        ; .byte _M_asl-_M0
        ; .byte 0
        ; .byte _M_tsb-_M0    ;0c
        ; .byte _M_ora-_M0
        ; .byte _M_asl-_M0
        ; .byte _M_bbr-_M0
        ; .byte _M_bpl-_M0    ;10
        ; .byte _M_ora-_M0
        ; .byte _M_ora-_M0
        ; .byte 0
        ; .byte _M_trb-_M0    ;14
        ; .byte _M_ora-_M0
        ; .byte _M_asl-_M0
        ; .byte _M_rmb-_M0
        ; .byte _M_clc-_M0    ;18
        ; .byte _M_ora-_M0
        ; .byte _M_inc-_M0
        ; .byte 0
        ; .byte _M_trb-_M0    ;1C
        ; .byte _M_ora-_M0
        ; .byte _M_asl-_M0
        ; .byte _M_bbr-_M0
        ; .byte _M_jsr-_M0    ;20
        ; .byte _M_and-_M0
        ; .byte 0
        ; .byte 0
        ; .byte _M_bit-_M0    ;24
        ; .byte _M_and-_M0
        ; .byte _M_rol-_M0
        ; .byte _M_rmb-_M0
        ; .byte _M_plp-_M0    ;28
        ; .byte _M_and-_M0
        ; .byte _M_rol-_M0
        ; .byte 0
        ; .byte _M_bit-_M0    ;2c
        ; .byte _M_and-_M0
        ; .byte _M_rol-_M0
        ; .byte _M_bbr-_M0
        ; .byte _M_bmi-_M0    ;30
        ; .byte _M_and-_M0
        ; .byte _M_and-_M0
        ; .byte 0
        ; .byte _M_bit-_M0    ;34
        ; .byte _M_and-_M0
        ; .byte _M_rol-_M0
        ; .byte _M_rmb-_M0
        ; .byte _M_sec-_M0    ;38
        ; .byte _M_and-_M0
        ; .byte _M_dec-_M0
        ; .byte 0
        ; .byte _M_bit-_M0    ;3c
        ; .byte _M_and-_M0
        ; .byte _M_rol-_M0
        ; .byte _M_bbr-_M0
        ; .byte _M_rti-_M0    ;40
        ; .byte _M_eor-_M0
        ; .byte 0
        ; .byte 0
        ; .byte 0             ;44
        ; .byte _M_eor-_M0
        ; .byte _M_lsr-_M0
        ; .byte _M_rmb-_M0
        ; .byte _M_pha-_M0    ;48
        ; .byte _M_eor-_M0
        ; .byte _M_lsr-_M0
        ; .byte 0
        ; .byte _M_jmp-_M0    ;4c
        ; .byte _M_eor-_M0
        ; .byte _M_lsr-_M0
        ; .byte _M_bbr-_M0
        ; .byte _M_bvc-_M0    ;50
        ; .byte _M_eor-_M0
        ; .byte _M_eor-_M0
        ; .byte 0
        ; .byte 0             ;54
        ; .byte _M_eor-_M0
        ; .byte _M_lsr-_M0
        ; .byte _M_rmb-_M0
        ; .byte _M_cli-_M0    ;58
        ; .byte _M_eor-_M0
        ; .byte _M_phy-_M0
        ; .byte 0
        ; .byte 0             ;5C
        ; .byte _M_eor-_M0
        ; .byte _M_lsr-_M0
        ; .byte _M_bbr-_M0
        ; .byte _M_rts-_M0    ;60
        ; .byte _M_adc-_M0
        ; .byte 0
        ; .byte 0
        ; .byte _M_stz-_M0    ;64
        ; .byte _M_adc-_M0
        ; .byte _M_ror-_M0
        ; .byte _M_rmb-_M0
        ; .byte _M_pla-_M0    ;68
        ; .byte _M_adc-_M0
        ; .byte _M_ror-_M0
        ; .byte 0
        ; .byte _M_jmp-_M0    ;6C
        ; .byte _M_adc-_M0
        ; .byte _M_ror-_M0
        ; .byte _M_bbr-_M0
        ; .byte _M_bvs-_M0    ;70
        ; .byte _M_adc-_M0
        ; .byte _M_adc-_M0
        ; .byte 0
        ; .byte _M_stz-_M0    ;74
        ; .byte _M_adc-_M0
        ; .byte _M_ror-_M0
        ; .byte _M_rmb-_M0
        ; .byte _M_sei-_M0    ;78
        ; .byte _M_adc-_M0
        ; .byte _M_ply-_M0
        ; .byte 0
        ; .byte _M_jmp-_M0    ;7C
        ; .byte _M_adc-_M0
        ; .byte _M_ror-_M0
        ; .byte _M_bbr-_M0
        ; .byte _M_bra-_M0    ;80
        ; .byte _M_sta-_M0
        ; .byte 0
        ; .byte 0
        ; .byte _M_sty-_M0    ;84
        ; .byte _M_sta-_M0
        ; .byte _M_stx-_M0
        ; .byte _M_smb-_M0
        ; .byte _M_dey-_M0    ;88
        ; .byte _M_bit-_M0
        ; .byte _M_txa-_M0
        ; .byte 0
        ; .byte _M_sty-_M0    ;8c
        ; .byte _M_sta-_M0
        ; .byte _M_stx-_M0
        ; .byte _M_bbs-_M0
        ; .byte _M_bcc-_M0    ;90
        ; .byte _M_sta-_M0
        ; .byte _M_sta-_M0
        ; .byte 0
        ; .byte _M_sty-_M0    ;94
        ; .byte _M_sta-_M0
        ; .byte _M_stx-_M0
        ; .byte _M_smb-_M0
        ; .byte _M_tya-_M0    ;98
        ; .byte _M_sta-_M0
        ; .byte _M_txs-_M0
        ; .byte 0
        ; .byte _M_stz-_M0    ;9C
        ; .byte _M_sta-_M0
        ; .byte _M_stz-_M0
        ; .byte _M_bbs-_M0
        ; .byte _M_ldy-_M0    ;A0
        ; .byte _M_lda-_M0
        ; .byte _M_ldx-_M0
        ; .byte 0
        ; .byte _M_ldy-_M0    ;A4
        ; .byte _M_lda-_M0
        ; .byte _M_ldx-_M0
        ; .byte _M_smb-_M0
        ; .byte _M_tay-_M0    ;A8
        ; .byte _M_lda-_M0
        ; .byte _M_tax-_M0
        ; .byte 0
        ; .byte _M_ldy-_M0    ;AC
        ; .byte _M_lda-_M0
        ; .byte _M_ldx-_M0
        ; .byte _M_bbs-_M0
        ; .byte _M_bcs-_M0    ;B0
        ; .byte _M_lda-_M0
        ; .byte _M_lda-_M0
        ; .byte 0
        ; .byte _M_ldy-_M0    ;B4
        ; .byte _M_lda-_M0
        ; .byte _M_ldx-_M0
        ; .byte _M_smb-_M0
        ; .byte _M_clv-_M0    ;B8
        ; .byte _M_lda-_M0
        ; .byte _M_tax-_M0
        ; .byte 0
        ; .byte _M_ldy-_M0    ;BC
        ; .byte _M_lda-_M0
        ; .byte _M_ldx-_M0
        ; .byte _M_bbs-_M0
        ; .byte _M_cpy-_M0    ;C0
        ; .byte _M_cmp-_M0
        ; .byte 0
        ; .byte 0
        ; .byte _M_cpy-_M0    ;C4
        ; .byte _M_cmp-_M0
        ; .byte _M_dec-_M0
        ; .byte _M_smb-_M0
        ; .byte _M_iny-_M0    ;C8
        ; .byte _M_cmp-_M0
        ; .byte _M_dex-_M0
        ; .byte 0
        ; .byte _M_cpy-_M0    ;CC
        ; .byte _M_cmp-_M0
        ; .byte _M_dec-_M0
        ; .byte _M_bbs-_M0
        ; .byte _M_bne-_M0    ;D0
        ; .byte _M_cmp-_M0
        ; .byte _M_cmp-_M0
        ; .byte 0
        ; .byte 0             ;D4
        ; .byte _M_cmp-_M0
        ; .byte _M_dec-_M0
        ; .byte _M_smb-_M0
        ; .byte _M_cld-_M0    ;D8
        ; .byte _M_cmp-_M0
        ; .byte _M_phx-_M0
        ; .byte 0
        ; .byte 0             ;DC
        ; .byte _M_cmp-_M0
        ; .byte _M_dec-_M0
        ; .byte _M_bbs-_M0
        ; .byte _M_cpx-_M0    ;E0
        ; .byte _M_sbc-_M0
        ; .byte 0
        ; .byte 0
        ; .byte _M_cpx-_M0    ;E4
        ; .byte _M_sbc-_M0
        ; .byte _M_inc-_M0
        ; .byte _M_smb-_M0
        ; .byte _M_inx-_M0    ;E8
        ; .byte _M_sbc-_M0
        ; .byte _M_nop-_M0
        ; .byte 0
        ; .byte _M_cpx-_M0    ;EC
        ; .byte _M_sbc-_M0
        ; .byte _M_inc-_M0
        ; .byte _M_bbs-_M0
        ; .byte _M_beq-_M0    ;F0
        ; .byte _M_sbc-_M0
        ; .byte _M_sbc-_M0
        ; .byte 0
        ; .byte 0             ;F4
        ; .byte _M_sbc-_M0
        ; .byte _M_inc-_M0
        ; .byte _M_smb-_M0
        ; .byte _M_sed-_M0    ;F8
        ; .byte _M_sbc-_M0
        ; .byte _M_plx-_M0
        ; .byte 0
        ; .byte 0             ;FC
        ; .byte _M_sbc-_M0
        ; .byte _M_inc-_M0
        ; .byte _M_bbs-_M0

; _M0: ; mnemonics
            ; .byte "?  "
; _M_adc:     .byte "adc"
; _M_and:     .byte "and"
; _M_asl:     .byte "asl"
; _M_bbr:     .byte $80+$62,"br"
; _M_bbs:     .byte $80+$62,"bs"
; _M_bcc:     .byte "bcc"
; _M_bcs:     .byte "bcs"
; _M_beq:     .byte "beq"
; _M_bit:     .byte "bit"
; _M_bmi:     .byte "bmi"
; _M_bne:     .byte "bne"
; _M_bpl:     .byte "bpl"
; _M_bra:     .byte "bra"
; _M_brk:     .byte "brk"
; _M_bvc:     .byte "bvc"
; _M_bvs:     .byte "bvs"
; _M_clc:     .byte "clc"
; _M_cld:     .byte "cld"
; _M_cli:     .byte "cli"
; _M_clv:     .byte "clv"
; _M_cmp:     .byte "cmp"
; _M_cpx:     .byte "cpx"
; _M_cpy:     .byte "cpy"
; _M_dec:     .byte "dec"
; _M_dex:     .byte "dex"
; _M_dey:     .byte "dey"
; _M_eor:     .byte "eor"
; _M_inc:     .byte "inc"
; _M_inx:     .byte "inx"
; _M_iny:     .byte "iny"
; _M_jmp:     .byte "jmp"
; _M_jsr:     .byte "jsr"
; _M_lda:     .byte "lda"
; _M_ldx:     .byte "ldx"
; _M_ldy:     .byte "ldy"
; _M_lsr:     .byte "lsr"
; _M_nop:     .byte "nop"
; _M_ora:     .byte "ora"
; _M_pha:     .byte "pha"
; _M_php:     .byte "php"
; _M_phx:     .byte "phx"
; _M_phy:     .byte "phy"
; _M_pla:     .byte "pla"
; _M_plp:     .byte "plp"
; _M_plx:     .byte "plx"
; _M_ply:     .byte "ply"
; _M_rmb:     .byte $80+$72,"mb"
; _M_rol:     .byte "rol"
; _M_ror:     .byte "ror"
; _M_rti:     .byte "rti"
; _M_rts:     .byte "rts"
; _M_sec:     .byte "sec"
; _M_sed:     .byte "sed"
; _M_sei:     .byte "sei"
; _M_smb:     .byte $80+$73,"mb"
; _M_sbc:     .byte "sbc"
; _M_sta:     .byte "sta"
; _M_stx:     .byte "stx"
; _M_sty:     .byte "sty"
; _M_stz:     .byte "stz"
; _M_tax:     .byte "tax"
; _M_tay:     .byte "tay"
; _M_trb:     .byte "trb"
; _M_tsb:     .byte "tsb"
; _M_tsx:     .byte "tsx"
; _M_txa:     .byte "txa"
; _M_txs:     .byte "txs"
; _M_tya:     .byte "tya"
 
; ; used to calculate size of assembled disassembler code
; disassembler_end:
; .scend
