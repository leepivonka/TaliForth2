\ FORTH assembler extras

Decimal
assembler-wordlist >order

: SeeL SeeLatest ;

: 2+ ( n1 -- n2 )
  1+ 1+ ;  SeeL

: Abort<>0 ( n -- )
  Abort" not zero" ;  SeeL
: IsClr  Depth Abort<>0  FDepth Abort<>0  ;  SeeL

: PushTmp1 ( -- n )
  [ Tmp1 lda.z  Tmp1 1+ ldy.z  ' PushYA jmp ] ; -1 Allot  Never-Native  SeeL
: PushTmp1.diy ( Y=offset -- n )
   [  Tmp1 lda.ziy  pha  iny  Tmp1 lda.ziy  tay  pla  ' PushYA jmp ] ; -1 Allot  SeeL
: PopTmp2 ( n -- )
  [ DStack  lda.zx  Tmp2 sta.z  DStack 1+ lda.zx  Tmp2 1+ sta.z  inx  inx ] ;  SeeL
: PushTmp3 ( -- n )
  [ Tmp3 lda.z  Tmp3 1+ ldy.z  ' PushYA jmp ] ; -1 Allot  SeeL

: JmpYA, ( YA=adr -- )  \ Compile JMP abs
  [ ' PushYA jsr,  ' Jmp, jmp ] ; -1 Allot  SeeL
: JsrYA, ( YA=adr -- )  \ Compile JSR abs
  [ ' PushYA jsr,  ' Jsr, jmp ] ; -1 Allot  SeeL
: JsrRel, ( A=offset_from_here -- )  \ Compile JSR abs to offset from here
  [ ' PushZA jsr, ] Here + Jsr, ;  SeeL

: DoVar 
  [ ' Create 13 + jmp, ] ;  -1 allot  SeeL

\ ---------- branch to absolute addr ----------------
: beqa ( adr -- )  Here - 2 - beq ;
: bnea ( adr -- )  Here - 2 - bne ;
: bcca ( adr -- )  Here - 2 - bcc ;
: bcsa ( adr -- )  Here - 2 - bcs ;
: bpla ( adr -- )  Here - 2 - bpl ;
: bmia ( adr -- )  Here - 2 - bmi ;
: bvca ( adr -- )  Here - 2 - bvs ;
: bvsa ( adr -- )  Here - 2 - bvs ;

\ -------- assembler If Then -----------------------------

: Then, ( adr id -- )
  [ Here ] Literal <> Abort" Then, not paired"
  Here Over - Dup $80 U>= Abort" then out of range"
    Swap 1- c!  \ patch the branch
  ;  SeeL
: IfC, ( A=opcode -- adr id )  \ common code
  [ $fe ldy.# ] PushYA ,  \ branch instruction
  Here  ['] Then,
  ;  SeeL
: Else, ( adr id -- adr id )
  $58 C,  \ compile clv  so we can use a relative branch
  [ $50 lda.# ] IfC,
  2Swap Then, ;  SeeL
: IfMi, ( -- adr id )
  [ $10 lda.# ] IfC,   \ bpl *
  ;  SeeL
: IfPl, ( -- adr id )
  [ $30 lda.# ] IfC,  \ bmi *
  ;  SeeL
: IfVs, ( -- adr id )
  [ $50 lda.# ] IfC,  \ bvc *
  ;  SeeL
: IfVc, ( -- adr id )
  [ $70 lda.# ] IfC,  \ bvs *
  ;  SeeL
: IfCs, ( -- adr id )
  [ $90 lda.# ] IfC,  \ bcc *
  ;  SeeL
: IfCc, ( -- adr id )
  [ $b0 lda.# ] IfC,  \ bcs *
  ;  SeeL
: IfEq, ( -- adr id )
  [ $d0 lda.# ] IfC,  \ bne *
  ;  SeeL
: IfNe, ( -- adr id )
  [ $f0 lda.# ] IfC,  \ beq *
  ;  SeeL

: ITTest1 ( -- )
  [ nop  IfCc,  $1234 bit  Then, nop ] ; SeeLatest
IsClr
: ITTest2 ( -- )
  [ nop  IfCc,  $1234 bit  Else,  $2345 bit  Then, sec ] ;  SeeLatest
IsClr

\ ---------- assembler Begin Until -----------------

: Begin, ( -- adr id )
  Here   \ save adr
  [ Here 3 - ] Literal  \ save id
  ;  SeeL

: Again, ( adr id -- )
  ['] Begin, <> Abort" not paired"
  jmp  \ compile jmp back
  ;  SeeL

: UntilC, ( adr id A=opcode -- )  \ common code
  [ pha ]
  ['] Begin, <> Abort" begin, not paired"
  [ pla  $fe ldy.# ] PushYA ,  \ bne *
  Here -  Dup $ff80 U< Abort" UntilEq, out of range"
  Here 1- C!  \ patch bne
  ;  SeeL
: UntilMi, ( adr id -- )  [ $10 lda.# ] UntilC, ;  SeeL
: UntilPl, ( adr id -- )  [ $30 lda.# ] UntilC, ;  SeeL
: UntilVs, ( adr id -- )  [ $50 lda.# ] UntilC, ;  SeeL
: UntilVc, ( adr id -- )  [ $70 lda.# ] UntilC, ;  SeeL
: UntilCs, ( adr id -- )  [ $90 lda.# ] UntilC, ;  SeeL
: UntilCc, ( adr id -- )  [ $b0 lda.# ] UntilC, ;  SeeL
: UntilEq, ( adr id -- )  [ $d0 lda.# ] UntilC, ;  SeeL
: UntilNe, ( adr id -- )  [ $f0 lda.# ] UntilC, ;  SeeL

: BUTest1 ( -- )
  [ nop
  Begin,  iny  iny  iny  iny  iny  UntilEq,
  ] ;  SeeLatest

: WhileC, ( A=opcode -- adr id )
  [ $fe ldy.#  ' PushYA jsr ] ,  \ compile bcc *
  Here  \ remember adr of branch to patch later
  [ Here 11 - ] Literal  \ id
  ;  SeeL
: WhileMi, ( -- adr id )  [ $10 lda.# ] WhileC, ;  SeeL
: WhilePl, ( -- adr id )  [ $30 lda.# ] WhileC, ;  SeeL
: WhileVs, ( -- adr id )  [ $50 lda.# ] WhileC, ;  SeeL
: WhileVc, ( -- adr id )  [ $70 lda.# ] WhileC, ;  SeeL
: WhileCs, ( -- adr id )  [ $90 lda.# ] WhileC, ;  SeeL
: WhileCc, ( -- adr id )  [ $b0 lda.# ] WhileC, ;  SeeL
: WhileEq, ( -- adr id )  [ $d0 lda.# ] WhileC, ;  SeeL
: WhileNe, ( -- adr id )  [ $f0 lda.# ] WhileC, ;  SeeL

: Repeat, ( adr_begin id_begin adr_while id_while -- )
  [ DStack 6 + lda.zx  DStack 7 + ldy.zx ] JmpYA,  \ compile jmp back
  ['] WhileC, <> Abort" WhileC, not paired"  \ check id_while
  Here Over -  Dup $80 U>= Abort" WhileC, out of range"  Swap 1- C!  \ patch WhileC,
  ['] Begin, <> Abort" Begin, not paired"  \ check id_begin
  Drop  \ adr_begin
  ;  SeeL

: BWRTest1 ( -- )
  [ nop
  Begin,  iny  WhileEq,  clc  Repeat,
  sec ] ;  SeeL

\ ---- Hi memory allocation ------------------------------
Variable HMPtr  \ hi memory next free space
: HMInit ( -- )
  CpEnd HMPtr ! ;  SeeL
: HMAlloc ( A=-size -- YA=ptr )
  [ clc  HMPtr adc  HMPtr sta   \ HMPtr+= A
    HMPtr 1+ ldy  IfCc, dey Then,  HMPtr 1+ sty
  ] ;  SeeL

\ ----- local symbols -----------------------------------

			\	.struct LSDefE ; symbol definition
0 Constant LSD.Next	\ Next	.word		; next LSDefE in chain
2 Constant LSD.Ref	\ Ref	.word		; anchor of LSRefE chain
4 Constant LSD.Value	\ Value	.word
6 Constant LSD.Name	\ Name	.ascic
			\	.endstruct

			\	.struct LSRefE ; symbol reference
0 Constant LSR.Next	\ Next	.word		; next LSRefE in chain
2 Constant LSR.Ptr	\ Ptr	.word
4 Constant LSR.Type	\ Type	.byte
5 Constant LSR.Size	\	.endstruct

Variable LSDAnchor  \ LSDefE list anchor

: LSInit ( -- ) \ initialize
  HMInit  \ init hi memory allocation
  [ 0 lda.# LSDAnchor sta  LSDAnchor 1+ sta ]  \ init symbol list
  ;  SeeL

: LSPrint ( -- )  \ print local symbol definitions
  LSDAnchor @ Begin		\ for each symbol
    Dup 0<> While
    CR
    Dup LSD.Name + Count Type	\   type name
    Space
    Dup LSD.Value + @ .Hex	\   type value
    Dup LSD.Ref + @ Begin	\   for each reference
      Dup 0<> While
      CR Space Space
      Dup LSR.Ptr + @ .Hex	\     type ptr
      Space
      Dup LSR.Type + C@ Emit	\     type type
      LSR.Next + @ Repeat Drop	\    next reference
   LSD.Next + @ Repeat Drop	\  next symbol
  ;  SeeL

: LSFinR ( Tmp1=^LSD Tmp2=^LSR -- )  \ resolve relative byte reference
  [ LSD.Value ldy.#  ] PushTmp1.diy [  \ get value
  ] PushTmp3 - 1- [  \ calc relative offset
  sec  DStack lda.zx  $80 adc.#  DStack 1+ lda.zx  0 adc.#  IfNe, ] \ check range
    ." Out of range at " .Hex Abort [ Then,
  ] PopA [  0 ldy.#  Tmp3 sta.ziy  \ patch the offset
  ] ;  SeeL

: LSFinish ( -- )  \ apply all patches
  [ LSDAnchor ldy  LSDAnchor 1+ lda  IfNe,  Begin, Tmp1 sty.z  Tmp1 1+ sta.z	\ for each symbol (Tmp1)
    LSD.Ref 1+ ldy.#  Tmp1 lda.ziy  pha  dey  Tmp1 lda.ziy  tay  pla  IfNe,	\ for each reference (Tmp2)
        Begin,  Tmp2 sty.z  Tmp2 1+ sta.z
      LSR.Ptr ldy.#  Tmp2 lda.ziy  Tmp3 sta.z  iny  Tmp2 lda.ziy  Tmp3 1+ sta.z  \ Tmp3= Ptr
      LSR.Type ldy.#  Tmp2 lda.ziy  'W' cmp.# IfEq,  \ resolve absolute word reference
        LSD.Value ldy.#  Tmp1 lda.ziy  pha  iny  Tmp1 lda.ziy
          1 ldy.#  Tmp3 sta.ziy  pla  dey  Tmp3 sta.ziy
         Then,
      LSR.Type ldy.#  Tmp2 lda.ziy  'R' cmp.# IfEq,  \ resolve relative byte reference
        ] LSFinR [  Then,
      LSR.Next 1+ ldy.#  Tmp2 lda.ziy  pha  dey  Tmp2 lda.ziy  tay  pla  UntilEq,  Then,  \ next reference
    LSD.Next 1+ ldy.#  Tmp1 lda.ziy  pha  dey  Tmp1 lda.ziy  tay  pla  UntilEq,  Then, \  next symbol
  ] ;  SeeL

: LSSym ( "name" -- Tmp1=^LSDefE )  \ Find (or create) LVDefE
  Parse-Name			\ ( -- addr len )
    PopA [ Tmp2 sta.z  ] PopYA [ Tmp3 sta.z  Tmp3 1+ sty.z
    sec  LSD.Name 1+ sbc.#  IfCc, dey Then, Tmp4 sta.z  Tmp4 1+ sty.z \ for name compare

  LSDAnchor ldy  LsDAnchor 1+ lda  IfNe,
    Begin,  Tmp1 1+ sta.z  Tmp1 sty.z  	\ for each LSD
      LSD.Name ldy.#  Tmp1 lda.ziy  Tmp2 cmp.z  IfEq,
        clc  LSD.Name 1+ adc.#  tay  Begin,
          dey  LSD.Name cpy.#  IfEq,  \ found?
            rts  Then,
          Tmp1 lda.ziy  Tmp4 cmp.ziy  UntilNe,
        Then,
      LSD.Next 1+ ldy.#  Tmp1 lda.ziy  pha  dey  Tmp1 lda.ziy  tay  pla  UntilEq,  \ next LSD
    Then,

  \ not found, create
  LSD.Name 1+ Negate $ff And lda.#  sec  Tmp2 sbc.z  ] HMAlloc [ Tmp1 sta.z  Tmp1 1+ sty.z  \ alloc new LSD
  LSDAnchor lda  LSD.Next ldy.#  Tmp1 sta.ziy  LSDAnchor 1+ lda  iny  Tmp1 sta.ziy
  Tmp1 lda.z  LSDAnchor sta  Tmp1 1+ lda.z  LSDAnchor 1+ sta
  0 lda.#
  LSD.Ref ldy.#  Tmp1 sta.ziy  iny  Tmp1 sta.ziy
  LSD.Value ldy.#  Tmp1 sta.ziy  iny  Tmp1 sta.ziy
  LSD.Name ldy.#  Tmp2 lda.z  Tmp1 sta.ziy
    Begin,  iny  Tmp4 lda.ziy  Tmp1 sta.ziy  Tmp2 dec.z  UntilEq,
  ] ;  SeeL

: LEqu ( value "name" -- )  \ label equate
  LSSym PushTmp1
  LSD.Value + !
  ;  SeeL

: LDef ( "name" -- )  \ define a label
  Here LEqu ;  SeeL

: LValue ( "name" -- value )  \ Get current value
  LSSym	PushTmp1		\ look up symbol
  LSD.Value + @
  ;  SeeL

: LRef ( "name" -- Tmp1=^LSD Tmp2=^LSR )  \ Allocate a reference
  LSSym		\ Tmp1=^LSDefE
  [ -1 LSR.Size - $ff And lda.# ] HMAlloc [ Tmp2 sta.z  Tmp2 1+ sty.z  \ Tmp2=^LSRefE
  LSD.Ref ldy.#  Tmp1 lda.ziy  pha  iny  Tmp1 lda.ziy
    LSR.Next 1+ ldy.#  Tmp2 sta.ziy  pla  dey  Tmp2 sta.ziy  \ Tmp2->Next=Tmp1->Ref
  Tmp2 lda.z  LSD.Ref ldy.#  Tmp1 sta.ziy  Tmp2 1+ lda.z  iny  Tmp1 sta.ziy  \ Tmp1->Ref=Tmp2
  ] ;  SeeL 

: LRefR ( "name" -- here )  \ Make a 1 byte relative instruction reference
  LRef				\ make a reference
  [ 'R' lda.#  LSR.Type ldy.#  Tmp2 sta.ziy  \ Tmp2->Type='R'
  ] Here [
  clc  DStack lda.zx  1 adc.#  LSR.Ptr ldy.#  Tmp2 sta.ziy  DStack 1+ lda.zx  0 adc.#  iny  Tmp2 sta.ziy  \ Tmp2->Ptr=Here+1
  ] ;  SeeL

: LRefW ( "name" -- here )  \ Make an absolute word reference
  LRef		\ make a reference
  Here
  [ 'W' lda.#  LSR.Type ldy.#  Tmp2 sta.ziy  \ Tmp2->Type='W'
  DStack lda.zx  LSR.Ptr ldy.#  Tmp2 sta.ziy  DStack 1+ lda.zx  iny  Tmp2 sta.ziy  inx  inx  \ Tmp2->Ptr=patch location
  ] Here ;  SeeL

: LRefA ( "name" -- here )  \ Make an absolute instruction reference
  LRef
  Here 1+  [ ' LRefW 6 + jmp ] ;  SeeL

: LSTest1 ( -- )
  [ LSInit
  nop
 LDef @1
  sec
  LRefA @1 jmp
  LRefA @9 jmp
  LRefR @1 bnea
  LRefR @9 bnea
  clc
 LDef @9
  LSFinish
  LSPrint
  ] ;  SeeL

\ ----------------------------------------------------------------------
