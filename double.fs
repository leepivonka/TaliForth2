\ double number extentions for Tali

: DLShift ( d1 u1 -- d )  \ shift d1 left u1 bits
  \ like https://forth-standard.org/standard/core/LSHIFT
  PopA	\ pop u1
  [ tay  IfNe
    DStack 1+ lda.dx
    Begin,
      lsr.a  DStack 0 + ror.zx  DStack 3 + ror.zx  DStack 2 + ror.zx  
      dey  UntilEq,
    DStack 1+ sta.dx
   Then,
  ] ;

;MDNStar: ; ( d1 n -- d2 )  multiply 32x16=48
;				; ( d1.lo d1.hi n )
;		jsr Swap	; ( d1.lo n d1.hi )
;		jsr To_R	; ( d1.lo n ) ( R: d1.hi )
;		lda 1,x
;		pha
;		lda 0,x
;		pha		; ( d1.lo n ) ( R: d1.hi n )
;		jsr MStar	; ( dprod.0 dprod.1 ) ( R d1.hi n )
;		jsr From_R	; ( dprod.0 dprod.1 n ) ( R: d1.hi )
;		jsr From_R	; ( dprod.0 dprod.1 n d1.hi )
;		jsr Rot		; ( dprod.0 n d1.hi dprod.1 )
;		jsr R_To	; ( dprod.0 n d1.hi ) ( R: dprod.1 )
;		jsr MStar	; ( dprod.0 dprod.1b dprod.2 ) ( R: dprod.1 )
;		jsr R_From	; ( dprod.0 dprod.1b dprod.2 dprod.1 )
;		jmp M+		; ( dprod.0 dprod.1 dprod.2 )

;MDNSlash: ; ( nd +n -- d2 )  divide 48/16=32
;				; ( nd.0 nd.1 nd.2 n )
;		lda 1,x
;		pha
;		lda 0,x
;		pha		; ( nd.0 nd.1 nd.2 n ) ( R: n )
;		jsr MSlash	; ( nd.0 d2.0 d2.1 ) ( R: n )
;		jsr Rot		; ( d2.0 d2.1 nd.0 ) ( R: n )
;		jsr Swap	; ( d2.0 nd.0 d2.1 ) 
;
;
;		jsr Abort	; incomplete???

	FEnd


 FHdr "M*/",NN ; ( d1 n1 +n2 -- d2 )  Return (d1*n1)/n2
  ; https://forth-standard.org/standard/double/MTimesDiv
MStarSlash:	jsr underflow_4 ; really _8

		lda 5,x			; save result sign
		eor 3,x
		php

		lda 5,x			; abs(d1)
		bpl +
		inx
		inx
		inx
		inx
		jsr DNegate
		dex
		dex
		dex
		dex
+
		lda 3,x			; abs(n1)
		bpl +
		inx
		inx
		jsr Negate
		dex
		dex
+

		lda #0			; tmp432 = d1 * n1
		sta tmp3+0
		sta tmp3+1
		sta tmp4+0
		sta tmp4+1
		lda 2,x
		jsr _MByte
		lda 3,x
		jsr _MByte

		jsr _DByte		; result = tmp432 / n2
		sta 5,x
		jsr _DByte
		sta 4,x
		jsr _DByte
		sta 7,x
		jsr _DByte
		sta 6,x

		inx			; Drop n2
		inx
		inx			; Drop n1
		inx

		plp			; apply result sign
		bmi +
		rts

+		jmp DNegate


_MByte: ; multiply byte
		sta tmp1+0		; save byte
		ldy #8
_MB2:		lsr tmp4+1		;   tmp234>>=1
		ror tmp4+0
		ror tmp3+1
		ror tmp3+0
		ror tmp2+1
		ror tmp2+0
		lsr tmp1+0		;   if bit set
		bcc _MB8
		clc
		lda tmp3+0
		adc 6,x
		sta tmp3+0
		lda tmp3+1
		adc 7,x
		sta tmp3+1
		lda tmp4+0
		adc 4,x
		sta tmp4+0
		lda tmp4+1
		adc 5,x
		sta tmp4+1
_MB8:		dey
		bne _MB2
		rts

_DByte: ; divide byte
		ldy #8		; for 8 bits
_DB2:		lda tmp4+0	;   if tmp4 >= n2
		cmp 0,x
		lda tmp4+1
		sbc 1,x
		bcc _DB4

		lda tmp4+0	;     tmp4 -= n2
		sbc 0,x
		sta tmp4+0
		lda tmp4+1
		sbc 1,x
		sta tmp4+1

_DB4:		rol tmp2+0	;   tmp432 <<=1 , save result bit
		rol tmp2+1
		rol tmp3+0
		rol tmp3+1
		rol tmp4+0
		rol tmp4+1

		dey		;  next bit
		bne _DB2

		lda tmp2+0	; return result bits
		rts

;		jsr To_R	; ( d1 n1 ) ( R: n2 )
;		jsr MDNStar	; ( nd ) ( R: n2 )
;		jsr R_From
;		jmp MDNSlash
;	FEnd



: QNegate ( q1 -- q2 ) [
  sec
  0 lda.#  6 sbc.zx  6 sta.zx
  0 lda.#  7 sbc.zx  7 sta.zx
  0 lda.#  4 sbc.zx  4 sta.zx
  0 lda.#  5 sbc.zx  5 sta.zx
  0 lda.#  2 sbc.zx  2 sta.zx
  0 lda.#  3 sbc.zx  3 sta.zx
  0 lda.#  0 sbc.zx  0 sta.zx
  0 lda.#  1 sbc.zx  1 sta.zx
  ] ;  See QNegate

: QAbs ( q1 -- q2 ) [
  1 lda.zx  ' QNegate bmi  ] ;  See QAbs

: UDM* ( ud1 ud2 -- uq )
  2Over 2Over [		\ ( ud1 ud2 ud1 ud2 )
  0 lda.#   8 sta.zx   9 sta.zx  10 sta.zx  11 sta.zx
           12 sta.zx  13 sta.zx  14 sta.zx  15 sta.zx
???


{         0.     2dup   |-> udm* $00000000. $00000000. }
{ $ffffffff.     2dup   |-> udm* $00000001. $FFFFFFFE. }
{ $87654321.     $1000. |-> udm* $54321000. $00000876. }
{     $1000. $87654321. |-> udm* $54321000. $00000876. }

: DM* ( d1 d2 -- q )
  [ 1 lda.zx 9 eor.zx php ]	\ save result sign
  DAbs 2Swap DAbs		\ make unsigned
  UDM*
  [ plp ' QNegate bmi		\ apply result sign
  ] ;  See DM*

: DUM/Mod ( uq ud1 -- udrem udquot ) [
  10 asl.zx  11 rol.zx  8 rol.zx  9 rol.zx
  32 ldy.#  Here
    6 rol.zx  7 rol.zx  4 rol.zx  5 rol.zx  _3 bcs
    6 lda.zx  2 cmp.zx  7 lda.zx  3 sbc.zx
      4 lda.zx  0 sbc.zx  5 lda.zx  1 sbc.zx
    bcc _5
_3  6 lda.zx  2 sbc.zx  6 sta.zx
    7 lda.zx  3 sbc.zx  7 sta.zx
    4 lda.zx  0 sbc.zx  4 sta.zx
    5 lda.zx  1 sbc.zx  5 sta.zx
    sec
_5  10 rol.zx  11 rol.zx  8 rol.zx  9 rol.zx
   dey  bne
  ' 2Drop jmp  ] ;  See DUM/Mod

{ $00000000. $00000000. $00000001. |-> dum/mod $00000000. $00000000. }
{ $00000001. $fffffffe. $ffffffff. |-> dum/mod $ffffffff. $ffffffff. }
{ $87654321. $00000009.       $10. |-> dum/mod $1.        $98765432. }

{ 87654321.       10. |-> DU/Mod 1. 8765432. }

{ 87654321. 0. 10. |-> DFM/Mod 1. 8765432. }

{ 87654321. 0. 10. |-> DSM/Rem 1. 8765432. }

{ $12345678.       $10. |-> D/Mod  8. $01234567. }
{ 123456789.        10. |-> D/Mod  9. 12345678. }

{ $12345678.       $10. |-> D/ $1234567. }

{ $12345678.       $10. \ ??? DMod ( d_dividend d_divisor -- d_rem )

{ 12345678. 3. 5. |-> D*/Mod 7407406. 4. }

{ 777777777. 111111. 424242. |-> D*/ 203703703. }
