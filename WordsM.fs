\ Multi-precision integer operations.
\ All cells of single size specified by MCellBytes.
\ On separate MStack.
\ Inspired by Dawn Forth: http://forum.6502.org/viewtopic.php?f=9&t=8496

\ include FAsm.fs  \ get assembler tools

: SeeL  SeeLatest  ;  \ generate word disassembly listings
\ Tmp1, Tmp2, Tmp3, Tmp4, Tmp5 are scratch words in zero page.

Variable MCellBytes  SeeL  32 MCellBytes !  \ # of bytes in an M cell
Create MStack  SeeL  MCellBytes @ 8 * Allot
Here  Constant MSEmpty  SeeL  \ empty MSPtr value
Variable MSPtr  MSEmpty MSPtr !  SeeL \ MStack pointer

: MEmpty-Stack ( M: .. -- ) \ empty the MStack
  [ MSEmpty ldya.#  MSPtr sta  MSPtr 1+ sty ] ;  SeeL
: MDepth ( -- n )  \ # of entrys on MStack
  \ https://forth-standard.org/standard/core/DEPTH
  MSEmpty MSPtr @ - MCellBytes @ / ;  SeeL
: MTmp123 ( -- )  \ Tmp1= ^TOS, Tmp2= ^NOS, Tmp3= ^3OS
  [ MSPtr lda  MSPtr 1+ ldy              Tmp1 sta.z  Tmp1 1+ sty.z  \ Tmp1= ^ top on MStack
  clc  MCellBytes adc  IfCs, iny  Then,  Tmp2 sta.z  Tmp2 1+ sty.z  \ Tmp2= ^ 2nd on MStack
  clc  MCellBytes adc  IfCs, iny  Then,  Tmp3 sta.z  Tmp3 1+ sty.z  \ Tmp3= ^ 3rd on MStack
  ] ;  SeeL

: MSAlloc ( M: -- m )  \ alloc a new M stack cell, Tmp1=^TOS
  [ MSPtr lda  MSPtr 1+ ldy  sec  MCellBytes sbc  IfCc,  dey  Then,
  MSPtr sta  MSPtr 1+ sty  Tmp1 sta.z  Tmp1 1+ sty.z  \ Tmp1=^m
  MStack $ff And cmp.#  tya  MStack 8 RShift sbc.#  IfCc, ]  \ overflow?
    MEmpty-Stack  ." MStack overflow "   Abort [  Then,
  ] ;  SeeL

: M0=P ( M: m -- m P.z )  \ Is top MStack cell zero?
  MTmp123 [
  LSInit  \ init local labels
  MCellBytes ldy  dey  Begin,
    Tmp1 lda.ziy  LRefR @9 bnea
    dey  UntilMi,  0 lda.#
 LDef @9
  LSFinish  \ finish local labels
  ] ;  SeeL

: M0<P ( M: m -- m P.m )  \ Is top MStack cell negative?
  MTmp123 [  MCellBytes ldy  dey  Tmp1 lda.ziy ] ;  SeeL

: MCmp ( M: m1 m2 -- m1 m2 ) ( -- P.ncz )  \ compare, set CPU flags NCZ
  MTmp123 [  \ Tmp1= ^m2, Tmp2= ^m1
  LSInit  \ init local labels
  MCellBytes ldy  dey
  Tmp2 lda.ziy  sec  Tmp1 sbc.ziy  LRefR @18 beqa  \ compare hi word
    LRefR @NFlip bvsa  rts

 LDef @11  Tmp2 lda.ziy  Tmp1 cmp.ziy  LRefR @Ne bnea
 LDef @18  dey  LRefR @11 bpla
  0 lda.#  rts  \ return =
 LDef @Ne  php  ror.a  plp
 LDef @NFlip $80 eor.#  1 ora.#  \ return flags
  LSFinish  \ finish local labels
  ] ;  SeeL

: MSwap ( M: m1 m2 -- m2 m1 )  \ swap MStack NOS & TOS
  \ https://forth-standard.org/standard/core/SWAP
  MTmp123 [  \ Tmp1=^m2, Tmp2=^m1
  MCellBytes ldy  dey  Begin,
    Tmp1 lda.ziy  pha  Tmp2 lda.ziy  Tmp1 sta.ziy  pla  Tmp2 sta.ziy  dey  UntilMi,
  ] ;  SeeL

: MDup ( M: m -- m m )  \ push MStack TOS
  \ https://forth-standard.org/standard/core/DUP
  MSAlloc MTmp123 [
  MCellBytes ldy  dey  Begin,
    Tmp2 lda.ziy  Tmp1 sta.ziy  dey  UntilMi,
  ] ;  SeeL

: MOver ( M: m1 m2 -- m1 m2 m1 )  \ push MStack NOS
  \ https://forth-standard.org/standard/core/OVER
  MSAlloc MTmp123 [
  MCellBytes ldy  dey  Begin,
    Tmp3 lda.ziy  Tmp1 sta.ziy  dey  UntilMi,
  ] ;  SeeL

: MDrop ( M: m1 m2 -- m1 )  \ drop m2, Tmp1=^m2, Tmp2=^m1
  \ https://forth-standard.org/standard/core/DROP
  [ MSPtr lda  MSPtr 1+ ldy  Tmp1 sta.z  Tmp1 1+ sty.z  \ Tmp1= ^dropped mcell
  clc  MCellBytes adc  IfCs, iny  Then,  MSPtr sta  MSPtr 1+ sty
  Tmp2 sta.z  Tmp2 1+ sty.z  \ Tmp2= ^new TOS
  MSEmpty 1+ Dup $ff And cmp.#  tya  8 RShift sbc.#  IfCs,
    ] MEmpty-Stack  ." MStack underflow "  Abort [ Then,
  ] ;  SeeL

: MNip ( M: m1 m2 -- m2 )  \ remove MStack NOS
  \ https://forth-standard.org/standard/core/NIP
  MDrop [  \ Drop m2, Tmp1= ^m2, Tmp2= ^m1
  MCellBytes ldy  dey  Begin, Tmp1 lda.ziy  Tmp2 sta.ziy  dey UntilMi,
  ] ;  SeeL

: MMax ( M: m1 m2 -- m3 )  \ return signed max of m1 & m2
  \ https://forth-standard.org/standard/core/MAX
  MCmp [ ' MDrop bpla  ' MNip bmia ] ;  SeeL

: MMin ( M: m1 m2 -- m3 )  \ return signed min of m1 & m2
  \ https://forth-standard.org/standard/core/MIN
  MCmp [ ' MDrop bmia  ' MNip bpla ] ;  SeeL

: MFalse2 ( M: m1 m2 -- ) ( -- false )  \ drop m1 & m2, return false
  MDrop MDrop [ ' False jmp ] ;  SeeL
: MTrue2 ( M: m1 m2 -- ) ( -- true )  \ drop m1 & m2, return true
  MDrop MDrop [ ' True  jmp ] ;  SeeL
: M= ( M: m1 m2 -- ) ( -- f )  \ return m1==m2
  \ https://forth-standard.org/standard/core/Equal
  MCmp [ ' MTrue2 beqa  ' MFalse2 bnea ] ;  SeeL
: M<> ( M: m1 m2 -- ) ( -- f )  \ return m1!=m2
  \ https://forth-standard.org/standard/core/ne
  MCmp [ ' MTrue2 bnea  ' MFalse2 beqa ] ;  SeeL
: M< ( M: m1 m2 -- ) ( -- f )  \ return signed m1<m2
  \ https://forth-standard.org/standard/core/less
  MCmp [ ' MTrue2 bmia  ' MFalse2 bpla ] ;  SeeL
: M<= ( M: m1 m2 -- ) ( -- f )  \ return signed m1<=m2
  MCmp [ ' MTrue2 bmia  ' MFalse2 bnea  ' MTrue2 beqa ] ;  SeeL
: MU< ( M: m1 m2 -- ) ( -- f )  \ return unsigned m1<m2
  \ https://forth-standard.org/standard/core/Uless
  MCmp [ ' MTrue2 bcca  ' MFalse2 bcsa ] ;  SeeL
: MU<= ( M: m1 m2 -- ) ( -- f )  \ return unsigned m1<=m2
  MCmp [ ' MTrue2 bcca  ' MFalse2 bnea  ' MTrue2 beqa ] ;  SeeL

: M0= ( M: m -- ) ( -- f )  \ return m==0
  M0=P [ IfEq, $ff lda.#  Else, 0 lda.#  Then, tay  ] PushYA MDrop ;  SeeL
: M0< ( M: m -- ) ( -- f )  \ return signed m<0
  M0<P [ $80 and.#  IfNe, $ff lda.#  Then, tay ] PushYA MDrop ;  SeeL

: MPad ( A Y -- ) ( M: m1 -- m2 ) \ fill hi bytes of m with A from Y
  [ LSInit
  $80 and.#  IfNe, $ff lda.#  Then,  \ sign extend from A
 LRefA @18 jmp

 LDef @11  Tmp1 sta.ziy
 LDef @18  iny   MCellBytes cpy  LRefR @11 bcca
  LSFinish ] ;  SeeL 
: S>M ( n -- ) ( M: -- m )  \ convert signed n to m
  MSAlloc [ DStack lda.zx  0 ldy.#  Tmp1 sta.ziy  DStack 1+ lda.zx  iny  Tmp1 sta.ziy  inx  inx
  ' MPad bnea  ] ;  SeeL
: U>M ( u -- ) ( M: -- um )  \ convert unsigned u to um
  MSAlloc [ DStack lda.zx  0 ldy.#  Tmp1 sta.ziy  DStack 1+ lda.zx  iny  Tmp1 sta.ziy  inx  inx
  lsr.a  ' MPad bpla  ] ;  SeeL

: MU>D ( M: um1 -- um1 0 )  \ convert unsigned um1 to double unsigned m
  0 U>M ;  SeeL
: MS>D ( M: m1 -- m1 mhi )  \ convert signed m1 to double m
  M0<P [ ' MU>D bpla ] -1 S>M ; SeeL

: M>S ( M: m -- ) ( -- n )  \ convert m to n
  MTmp123 [ 1 ldy.#  Tmp1 lda.ziy  Tmp2 sta.z  dey  Tmp1 lda.ziy  Tmp2 ldy.z
  ] PushYA [ ' MDrop jmp ] ; SeeL

: MRolYA ( YA P.C -- P.C )  \ Rotate the bits of (YA) left through the carry bit.
  [ LSInit
  LRefA @AdrL sta  LRefA @AdrH sty  \ patch adr in rol instruction (self modifying code!)
  txa  pha  \ save data stack index
  MCellBytes ldy  0 ldx.#  Begin,
    Here 1+ LEqu @AdrL  Here 2+ LEqu @AdrH
    $ff00 rol.x   inx   dey  UntilEq,
  pla  tax  \ restore data stack index
  LSFinish ] ;  SeeL
: MRol ( M: m1 P.C -- m2 P.C )  \ Rotate the bits of m1 left through the carry bit.
  [ MSPtr lda  MSPtr 1+ ldy  ' MRolYA jmp  ] ;  SeeL
: M2* ( M: m1 -- m2 )  \ return m1*2
  \ https://forth-standard.org/standard/core/TwoTimes
  [ clc  ' MRol bcca ] ;  SeeL

: MRor ( M: m1 P.C -- m2 P.C )  \ Rotate the bits of m1 right through the carry bit.
  [ php ] MTmp123 [ plp  \ Tmp1= ^m1
  MCellBytes ldy  dey  Begin,
    Tmp1 lda.ziy  ror.a  Tmp1 sta.ziy  dey UntilMi,
  ] ;  SeeL
: MU2/ ( M: m1 -- m2 )  \ return unsigned m1/2
  [ clc  ' MRor bcca ] ;  SeeL
: M2/ ( M: m1 -- m2 )  \ return signed m1/2
  \ https://forth-standard.org/standard/core/TwoDiv
  M0<P [ asl.a  ' MRor jmp ] ;  SeeL
 
: M1+ ( M: m1 -- m2 )  \ Increment m1
  \ https://forth-standard.org/standard/core/OnePlus
  MTmp123 [  \ Tmp1= ^m1
  Tmp2 stx.z  \ save data stack index
  LSInit  \ init local labels
  MCellBytes ldx  0 ldy.#  sec  Begin,
    Tmp1 lda.ziy  0 adc.#  Tmp1 sta.ziy  LRefR @19 bcca  iny  dex  UntilEq,
 LDef @19
  Tmp2 ldx.z  \ restore data stack index
  LSFinish  \ finish local labels
  ] ;  SeeL 

: M1- ( M: m1 -- m2 )  \ decrement m1
  \ https://forth-standard.org/standard/core/OneMinus
  MTmp123  \ Tmp1= ^m1
  [ Tmp2 stx.z  \ save data stack index
  LSInit  \ init local labels
  MCellBytes ldx  0 ldy.#  clc  Begin,
    Tmp1 lda.ziy  0 sbc.#  Tmp1 sta.ziy  LRefR @20 bcsa  iny  dex  UntilMi,
 LDef @20
  Tmp2 ldx.z  \ restore data stack index
  LSFinish  \ finish local labels
  ] ;  SeeL

: MOr ( M: m1 m2 -- m3 )  \ Bitwise logical or
  \ https://forth-standard.org/standard/core/OR
  MDrop [  \ drop m2, Tmp1=^m2, Tmp2=^m1
  MCellBytes ldy  dey  Begin,
    Tmp2 lda.ziy  Tmp1 ora.ziy  Tmp2 sta.ziy  dey  UntilMi,
  ] ;  SeeL
: MAnd ( M: m1 m2 -- m3 )  \ Bitwise logical and
  \ https://forth-standard.org/standard/core/AND
  MDrop [  \ drop m2, Tmp1=^m2, Tmp2=^m1
  MCellBytes ldy  dey  Begin,
    Tmp2 lda.ziy  Tmp1 and.ziy  Tmp2 sta.ziy  dey  UntilMi,
  ] ;  SeeL
: MXor ( M: m1 m2 -- m3 )  \ Bitwise logical xor
  \ https://forth-standard.org/standard/core/XOR
  MDrop [  \ drop m2, Tmp1=^m2, Tmp2=^m1
  MCellBytes ldy  dey  Begin, 
    Tmp2 lda.ziy  Tmp1 eor.ziy  Tmp2 sta.ziy  dey  UntilMi,
  ] ;  SeeL
: MInvert ( M: m1 -- m2 )  \ bitwise invert
  MTmp123 [  \ Tmp1= ^m1
  MCellBytes ldy  dey  Begin, Tmp1 lda.ziy  $ff eor.#  Tmp1 sta.ziy  dey  UntilMi,
  ] ;  SeeL

: M+1 ( M: m1 m2 -- m3 )  \ Add m1 to m2 giving m3, name collision with M+
  \ Strange name so we don't redefine M+ from standard FORTH.
  \ https://forth-standard.org/standard/core/Plus
  MDrop  \ drop m2, Tmp1=^m2, Tmp2=^m1
  [ Tmp3 stx.z  \ save data stack index
  MCellBytes ldx  0 ldy.#  clc  Begin,
    Tmp2 lda.ziy  Tmp1 adc.ziy  Tmp2 sta.ziy  iny  dex  UntilEq,
  Tmp3 ldx.z  \ restore data stack index
  ] ;  SeeL

: M-1 ( M: m1 m2 -- m3 )  \ Subtract m2 from m1 giving m3, name collision with M-
  \ Strange name so we don't redefine M+ from standard FORTH.
  \ https://forth-standard.org/standard/core/Minus
  MDrop  \ drop m2, Tmp1=^m2, Tmp2=^m1
  [ Tmp3 stx.z  \ save data stack index
  MCellBytes ldx  0 ldy.#  sec  Begin,
    Tmp2 lda.ziy  Tmp1 sbc.ziy  Tmp2 sta.ziy  iny  dex  UntilEq,
  Tmp3 ldx.z  \ restore data stack index
  ] ;  SeeL

: MNegate ( M: m1 -- m2 )  \ return the two's complement of m1
  \ https://forth-standard.org/mmstandard/core/NEGATE
  MTmp123  \ Tmp1=^m1
  [ Tmp3 stx.z  \ save data stack index
  MCellBytes ldx  0 ldy.#  sec  Begin,
    0 lda.#  Tmp1 sbc.ziy  Tmp1 sta.ziy  iny  dex  UntilEq,
  Tmp3 ldx.z  \ restore data stack index
  ] ;  SeeL

: MAbs ( M: m1 -- m2 )  \ return absolute value of m1
  \ https://forth-standard.org/standard/core/ABS
  M0<P [ ' MNegate bmia ] ;  SeeL

: MDNegate ( M: mlo1 mhi1 -- mlo2 mhi2 )  \ return two's complement of double M.
  MTmp123  \ Tmp1=^mhi1, Tmp2=^mlo1
  [ Tmp3 stx.z  \ save data stack index
  MCellBytes ldx  0 ldy.#  sec  Begin,  \ do mlo
    0 lda.#  Tmp2 sbc.ziy  Tmp2 sta.ziy  iny  dex  UntilEq,
  MCellBytes ldx  0 ldy.#       Begin,  \ do mhi
    0 lda.#  Tmp1 sbc.ziy  Tmp1 sta.ziy  iny  dex  UntilEq,
  Tmp3 ldx.z  \ restore data stack index
  ] ;  SeeL

: Hex>M ( uhi ... ulo -- ) ( M: -- m ) \ convert words to M
  MSAlloc [  \ Tmp1= ^new m
  0 ldy.#  Begin,
    DStack lda.zx  inx  Tmp1 sta.ziy  iny  MCellBytes cpy UntilCs,
  ] ;  SeeL
: M.Hex ( M: m -- )  \ type m in hex
  [ MCellBytes ldy  dey  Begin, tya  pha
    ] MTmp123 [  \ Tmp1=^m
    pla  pha  tay  Tmp1 lda.ziy ] PushZA C.Hex [
    pla  tay  dey  UntilMi,
  ] MDrop ;  SeeL
    
: MU*ByteA ( A -- ) ( M: m1 -- m2 ) \ return unsigned m1*A
  [ Tmp5 sta.z ]  \ save multiplier byte
  MSAlloc  \ alloc work mcell
  MTmp123 [  \ Tmp1=^work, Tmp2=^m1
  MCellBytes ldy  dey  Begin,
    Tmp2 lda.ziy  Tmp1 sta.ziy  \ move m1 to work
    0 lda.#  Tmp2 sta.ziy  \ zero m2
    dey  UntilMi,
  Begin, Tmp5 lsr.z  \ for each multiplier bit
    IfCs,
      clc  0 ldy.#  Begin,  \ add work to m2
        Tmp2 lda.ziy  Tmp1 adc.ziy  Tmp2 sta.ziy  iny  tya  MCellBytes eor  UntilEq,
     Then,
    Tmp5 lda.z WhileNe,  \ any multiplier left?
    clc  Tmp1 lda  Tmp1 1+ ldy ] MRolYA [  \ double work
    Repeat,
  ' MDrop jmp ] ;  SeeL
: MU*Word ( A -- ) ( M: m1 -- m2 )  PopA MU*ByteA ;  SeeL  \ for WordsMTest.fs

: MUM* ( M: um1 um2 -- umlo umhi ) \ Multiply unsigned giving double
  \ https://forth-standard.org/standard/core/UMTimes
  MSAlloc  \ alloc work mcell
  MTmp123 [  \ Tmp1=^work, Tmp2=^umhi&um2, Tmp3=^umlo&um1
  Tmp5 stx.z  \ save data stack index so we can use X
 
  MCellBytes ldy  dey  Begin,  \ for each byte in mcell
    Tmp2 lda.ziy  Tmp1 sta.ziy  \ copy um2 to work
    0 lda.#  Tmp2 sta.ziy  \ zero umhi
    dey  UntilMi,

  0 ldy.#  Begin,  Tmp4 sty.z  \ for each um2/umlo byte
    Tmp3 lda.ziy  lsr.a  Tmp5 1+ sta.z  \ get byte
    8 lda.#  Tmp4 1+ sta.z  Begin,  \ for each bit in byte
      IfCs,  \ if um2 bit set
        MCellBytes ldx  clc  0 ldy.#  Begin,  \ add work to umhi
          Tmp1 lda.ziy  Tmp2 adc.ziy  Tmp2 sta.ziy  iny  dex UntilEq,
       Then,
      MCellBytes ldy  dey  Begin,  Tmp2 lda.ziy  ror.a  Tmp2 sta.ziy  dey UntilMi,  \ rot umhi right
      Tmp5 1+ ror.z
     Tmp4 1+ dec.z UntilEq,  \ next bit
    Tmp4 ldy.z  Tmp5 1+ lda.z  Tmp3 sta.ziy  \ store byte
   iny  MCellBytes cpy  UntilCs,  \ next byte

  Tmp5 ldx.z  \ restore data stack index
  ' MDrop jmp
  ] ;  SeeL

: MU* ( M: um1 um2 -- um3 )  \ unsigned muliply
  MUM* MDrop ;  SeeL
: M*1 ( M: m1 m2 -- m3 )  \ signed multiply, name collision with M*
  \ https://forth-standard.org/standard/core/Times
  MTmp123 [ MCellBytes lda  asl.a  tay  dey  Tmp1 lda.ziy  php  IfMi,
    ] MSwap MNegate [ Then, ]
  MU*
  [ plp  IfMi, ] MNegate [ Then, ]  ;  SeeL
: MM* ( M: m1 m2 -- mlo mhi )  \ signed multiply giving double
  \ https://forth-standard.org/standard/core/MTimes
  MTmp123 [
  MCellBytes ldy  dey  Tmp1 lda.ziy Tmp2 eor.ziy  php ]
  MAbs MSwap MAbs MUM*
  [ plp  IfMi, ] MDNegate [ Then, ]
  ;  SeeL

: MU/ModByteA ( M: umdividend -- umquotient ) ( udivisor -- A=uremainder )
  PopA [ Tmp5 sta.z  \ save divisor byte
  ] M2* [  \ get 1st dividend bit
  0 lda.#  \ init top byte of dividend
  MCellBytes ldy  Tmp4 sty.z  Begin,  \ for each dividend byte
    8 ldy.#  Tmp5 1+ sty.z  Begin,  \ for each bit in byte
      rol.a
      Tmp5 cmp.z  IfCs, Tmp5 sbc.z Then,
      pha  ] MRol [ pla
     Tmp5 1+ dec.z  UntilEq,  \ next bit
   Tmp4 dec.z  UntilEq,  \ next byte
  ] ;  SeeL
: MU/ModWordA MU/ModByteA ;  SeeL  \ for WordsMTest.fs
: PushA PushZA ;  SeeL  \ for WordsMTest.fs

: MUM/Mod ( M: umlo umhi umdivisor -- umremainder umquotient )  \ unsigned division
  \ https://forth-standard.org/standard/core/UMDivMOD
  MTmp123  \ Tmp1=^umdivisor, Tmp2=^umhi&umremainder, Tmp3=^umlo&umquotient
  [ LSInit  \ init local labels
  Tmp4 1+ stx.z  \ save data stack index
  MCellBytes ldy  Begin, dey  Tmp5 1+ sty.z  \ for each umlo&umquotient byte
    Tmp3 lda.ziy  asl.a  Tmp4 sta.z  \ get byte
    8 lda.#  Tmp5 sta.z  Begin,  \ for each bit in byte
      Tmp2 lda  Tmp2 1+ ldy ] MRolYA [  \ rol umhi
      LRefR @sub bcsa
      MCellBytes ldy  Begin, dey  Tmp2 lda.ziy  Tmp1 cmp.ziy  LRefR @1 bnea  tya UntilEq, \ test divisor
     LDef @1  LRefR @2 bcca
     LDef @sub  \ subtract divisor
      MCellBytes ldx  0 ldy.#  Begin, Tmp2 lda.ziy  Tmp1 sbc.ziy  Tmp2 sta.ziy  iny  dex UntilEq,
      sec
     LDef @2
      Tmp4 rol.z   \ shift dividend lo & quotient
     Tmp5 dec.z  UntilEq,  \ next bit
    Tmp5 1+ ldy.z  Tmp4 lda.z  Tmp3 sta.ziy  \ put byte
   tya  UntilEq,  \ next byte
  Tmp4 1+ ldx.z  \ restore data stack index
  ] MDrop  \ divisor
  [ ' MSwap jmp,
  LSFinish  \ finish local labels
  ] ;  SeeL

: MU/Mod ( M: um-dividend um-divisor -- um-remainder um-quotient ) \ Divide unsigned
  MSAlloc MTmp123 [
  MCellBytes ldy  Begin, dey
    Tmp2 lda.ziy  Tmp1 sta.ziy
    0 lda.#  Tmp2 sta.ziy
    tya  UntilEq,
  ' MUM/Mod jmp ] ;  SeeL

: MUMod ( M: mu-dividend mu-divisor -- mu-remainder )  \ Divide unsigned
  MU/Mod MDrop ;  SeeL

: MU/ ( M: mu-dividend mu-divisor -- mu-quotient ) \ Divide unsigned
  MU/Mod MNip ;  SeeL

\ : MFM/Mod ( M: mlo mhi mdivisor -- mremainder mquotient ) \ Floored signed division
\  \ http://forth-standard.org/standard/core/FMDivMOD
\  [ MSPtr lda, clc, MCellBytes adc, MSPtr sta, Tmp1 sta,  \ move over divisor
\  MCellBytes ldy, dey, dey, Tmp1 (),y lda, pha, IfMi, ] MDNegate [ Then,
\  MSPtr lda, sec, MCellBytes sbc, MSPtr sta, Tmp1 sta,  \ move back
\  MCellBytes ldy, dey, dey, Tmp1 (),y lda, pha, IfMi, ] MNegate [ Then,
\  ] MUM/Mod [ \ unsigned divide
\  3 d,s lda, IfMi, \ correct remainder
\    MSPtr lda, clc, MCellBytes adc, MSPtr sta,  \ move over quotient
\    ] MNegate [
\    MSPtr lda, sec, MCellBytes sbc, MSPtr sta,  \ move back
\    Then,
\
\  pla, 1 d,s eor, IfMi, \ correct quotient
\	sec
\	lda 2,x
\	beq @7
\	lda 1,s
\	sec
\	sbc 2,x
\	sta 2,x
\	clc
\ @7:
\	lda #0
\	sbc 0,x
\	sta 0,x
\   Then,
\  pla,  \ RDrop divisor sign
\  ] ;  SeeL


: MSM/Rem ( M: mlo mhi mdivisor -- mremainder mquotient ) \ Symmetric signed division
  \ http://forth-standard.org/standard/core/SMDivREM
  MDrop [  \ move over divisor, Tmp2= ^mhi
  MCellBytes ldy  dey  Tmp2 lda.ziy  pha  IfMi, ] MDNegate [ Then,
  ] MSAlloc [  \ move back, Tmp1= ^mdivisor
  MCellBytes ldy  dey  pla  pha  Tmp1 eor.ziy  pha   Tmp1 lda.ziy IfMi, ] MNegate [ Then,
  ] MUM/Mod [ \ unsigned divide
  pla IfMi, ] MNegate [ Then,  \ correct quotient sign
  pla IfMi, ] MDrop MNegate MSAlloc [ Then,  \ correct remainder sign
  ] ;  SeeL

: M@ ( adr -- ) ( M: -- m )  \ fetch m from adr
  \ https://forth-standard.org/standard/core/Fetch
  PopYA [ Tmp2 sta.z  Tmp2 1+ sty.z  \ Tmp2= adr
  ] MSAlloc [  \ Tmp1= ^m
  MCellBytes ldy Begin,
    dey  Tmp2 lda.ziy  Tmp1 sta.ziy  tya UntilEq,
  ] ;  SeeL

: M! ( adr -- ) ( M: m -- )  \ store m at adr
  \ https://forth-standard.org/standard/core/Store
  PopYA [ Tmp3 sta.z  Tmp3 1+ sty.z ]  \ Tmp3= ^dest
  MDrop  [  \ Tmp1=^m
  MCellBytes ldy  Begin,
    dey  Tmp1 lda.ziy  Tmp3 sta.ziy  tya  UntilEq,
  ] ;  SeeL

: M, ( M: m -- )  \ compile m
  Here M!  MCellBytes @ Allot ;  SeeL

: MVariable ( "name" -- )  \ Compile an M variable word
  \ https://forth-standard.org/standard/core/VARIABLE
  Create  0 U>M M, ;  SeeL

: MConstant ( M: m -- ) ( "name" -- )  \ compile an M constant word
  WordHeader,  \ compile word header
  Here 7 + LDYA.#  \ compile LDY #; LDA #
  [ ' M@ 3 + LDYA.# ] JmpYA,  \ compile JMP M@
  M,  \ compile data
  ;  SeeL

: M.Fmt1 ( M: m -- )  \ 1st part of format m into hold buffer
  0. <#
  [ Begin,  Base lda  ' MU/ModByteA 3 + jsr
    10 cmp.#  IfCs, 6 adc.#  Then, '0' adc.#  ' Hold 3 + jsr ]
    M0=P [ UntilEq, ]
  ;  SeeL

: M.Pad ( n -- )  \ add padding spaces to Hold
  HoldSize  \ calc current length
  -  \ calc padding needed
  Begin
    Dup 0> While  1-  Bl Hold  Repeat  Drop
  ;  SeeL
  
: MU.R ( M: um -- ) ( n -- )  \ type unsigned m, at least n chars
  \ https://forth-standard.org/standard/core/UDotR
  >R M.Fmt1  \ init Pad, convert um
  R> M.Pad
  #> MDrop
  Type Space ;  SeeL

: MU. ( M: um -- )  \ type unsigned m, minimum length
  \ https://forth-standard.org/standard/core/Ud
  0 MU.R Space ;  SeeL

: M.R ( M. m -- ) ( n -- )  \ type signed m, at least n chars
  \ https://forth-standard.org/standard/core/DotR
  >R
  M0<P [ php  IfMi, ] MNegate [ Then, ]
  M.Fmt1
  [ plp IfMi, ] '-' Hold [ Then, ]
  R> M.Pad
  #> MDrop
  Type ;  SeeL

: M. ( M. m -- )  \ type signed m, minimum length
  \ https://forth-standard.org/standard/core/d
  0 M.R Space ;  SeeL


: M.S \ Print the MStack
  \ https://forth-standard.org/standard/tools/DotS
  [ '<' lda.# ] EmitA  MDepth 0 U.R [ '>' lda.# ] EmitA  Space
  MSEmpty [ Begin, ]
    MCellBytes @ -  [ DStack lda.zx  MSPtr cmp  DStack 1+ lda.zx  MSPtr 1+ sbc  WhileCs, ]
    Dup M@ M.
    [ Repeat, ] Drop
  CR ;  SeeL

.S  \ look for stray compile stuff

: >M ( M: -- m ) ( c-addr u -- true | false) \ Convert a string to an m
  \ https://forth-standard.org/standard/float/toFLOAT
  0 U>M [
  LSInit
  LRefA @GetChar jsr  LRefR @Err2 bcsa  \ sign present?
  '-' cmp.#  php  IfNe,
    '+' cmp.#  LRefR @Char2 bnea
   Then,
  Begin,
    LRefA @GetChar jsr  LRefR @Done bcsa  \ while chars available
   LDef @Char2
    pha  Base lda ] MU*ByteA [ pla
    'a' cmp.# IfCs, $df and.# Then,  \ convert letters to uppercase
    sec  '0' sbc.#  LRefR @Err bcca
    10 cmp.#  IfCs, 6 sbc.#  Then,
    Base cmp  LRefR @Err bcsa
    MSPtr ldy  Tmp1 sty.z  MSPtr 1+ ldy  Tmp1 1+ sty.z
      0 ldy.#  clc  Tmp1 adc.ziy  Tmp1 sta.ziy
   LDef @Add2 LRefR @Add9 bcca  iny  tya  MCellBytes eor  LRefR @Add9 beqa
         Tmp1 lda.ziy  0 adc.#  Tmp1 sta.ziy   LRefR @Add2 bcsa
   LDef @Add9
   Again,
 LDef @Done plp  IfEq, ] MNegate [ Then,  \ apply negative sign?
  $ff lda.#  \ return success
 LDef @ret  inx  inx    DStack sta.zx  DStack 1+ sta.zx  rts

 LDef @Err  plp
 LDef @Err2  0 lda.#  \ return failure
   LRefR @ret beqa

 LDef @GetChar
  sec  DStack lda.zx  IfNe,  DStack dec.zx  \ char present?
    DStack 2+ lda.zxi  DStack 2+ inc.zx IfEq, DStack 3 + inc.zx  Then,  \ get char
    clc
   Then,
  LSFinish  ] ;  SeeL

: MLiteral ( M: m -- )  \ compile inline m literal
  [ Here 10 + ldya.# ] JsrYA,  \ compile JSR
  [ ' M, jmp  \ compile m; return

 \ LDef @Run  \ runtime
  pla  Tmp4 sta.z  pla  Tmp4 1+ sta.z
  tay  Tmp4 lda.z  clc  1 adc.#  IfCs, iny Then,  ' M@ 3 + jsr  \ get value
  Tmp4 lda.z  clc  MCellBytes adc  tay  Tmp4 1+ lda.z  0 adc.#  pha  tya  pha  \ fix RTS addr
  ] ;  Immediate  SeeL

: M' ( "string" -- ) ( M: -- m )  \ convert inline string to m
  Parse-Name		\ get word string
  >M			\ convert
  0= If  Abort  Then	\ error?
  [ State lda.z  ' MLiteral bnea ] \ compiling?
  ;  Immediate  SeeL
