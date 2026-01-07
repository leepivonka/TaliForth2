\ Tests for  Multi-precision integer operations.

: Timer ( "name" -- )  \ measure execution time of "name"
  '  \ get xt of "name"
  CC@ 2>R  \ remember start CPU cycles
  Execute  \ execute the word under test
  CC@ 2R> D- Space UD. ." cycles " ;  SeeL
: Clr  \ Empty data stack & MStack
  Empty-Stack MEmpty-Stack ;  SeeL
: IsClr ( .. -- )  \ verify data stack & MStack are empty
  Depth 0<> If ." IsClr S: " .S  Abort Then
  MSPtr @ MSEmpty <> If  ." IsClr M: " M.S ABort Then
  ;  SeeL
: =S ( n1 n2 -- )  \ compare n1 & n2, complain if not =
  2Dup <> If  ." =S: " . . Abort Then 2Drop ;  SeeL
: =M ( M: m1 m2 -- )  \ compare m1 & m2, complain if not =
  MOver MOver M<> If ." =M: " CR M. CR M. Abort Then MDrop MDrop ;  SeeL

\ 32 MCellBytes !  \ set # of bytes in an M cell; must be even
Clr
IsClr
Depth 0 =S  MDepth 0 =S

$1234 $5678 $2345 $6789 $3456 $789a $4567 $89ab $5678 $9abc $6789 $abcd $789a $bcde $89ab $cdef Hex>M
Depth 0 =S  MDepth 1 =S
MConstant MA  IsClr                  MA          M.Hex
$fedc $ba98 $edcb $a987 $dcba $9876 $cba9 $8765 $ba98 $7654 $a987 $6543 $9876 $5432 $8765 $4321 Hex>M
MConstant MB  IsClr                  MB          M.Hex
$9531 $fdb9 $eca8 $6420 $2092 $4004 $83a9 $a9bf $99a9 $8ddc $9294 $8243 $8239 $8282 $9827 $3283 Hex>M
MConstant MN                         MN          M.Hex
$7278 $6337 $8237 $8230 $73cc $cc23 $8283 $cdfe $88c8 $8dab $0a88 $e839 $defd $efde $0273 $9232 Hex>M
MConstant MP  IsClr                  MP          M.Hex
0 U>M MConstant M0  IsClr            M0          M.hex
3 U>M MConstant M+3  IsClr           M+3         M.Hex
: M-4 [ -4 S>M ] MLiteral ;  IsClr   M-4         M.Hex
-1 S>M        MConstant M1s          M1s         M.Hex
: MMax-UInt M1s ;  IsClr             MMax-UInt   M.Hex
M1s MU2/      MConstant MMax-Int     MMax-Int    M.Hex
MMax-Int M1+  MConstant MMin-Int     MMin-Int    M.Hex
: MMid-UInt MMax-Int ;  IsClr        MMid-UInt   M.Hex
MMid-UInt M1+ MConstant MMid-UInt+1  MMid-UInt+1 M.Hex
: MMSB  MMin-Int ;  IsClr            MMSB        M.Hex

MA MA Timer =M
MB MB Timer =M
MN MN Timer =M
MP MP Timer =M
IsClr

MA MB Timer MSwap MA =M MB =M
IsClr

MN Timer MDup  MN =M MN =M
IsClr

MA MB Timer MOver  MA =M MB =M MA =M
IsClr

MA Timer MDrop
IsClr

MA MB Timer MNip  MB =M
IsClr

MP     MP     Timer MMax  MP     =M  IsClr
MP     MN     Timer MMax  MP     =M  IsClr
MN     MP     Timer MMax  MP     =M  IsClr
MN     MN     Timer MMax  MN     =M  IsClr
+3 S>M +4 S>M Timer MMax  +4 S>M =M  IsClr
-3 S>M +4 S>M Timer MMax  +4 S>M =M  IsClr
+3 S>M -4 S>M Timer MMax  +3 S>M =M  IsClr
-3 S>M -4 S>M Timer MMax  -3 S>M =M  IsClr

MP     MP     Timer MMin  MP     =M  IsClr
MP     MN     Timer MMin  MN     =M  IsClr
MN     MP     Timer MMin  MN     =M  IsClr
MN     MN     Timer MMin  MN     =M  IsClr
+3 S>M +4 S>M Timer MMin  +3 S>M =M  IsClr
-3 S>M +4 S>M Timer MMin  -3 S>M =M  IsClr
+3 S>M -4 S>M Timer MMin  -4 S>M =M  IsClr
-3 S>M -4 S>M Timer MMin  -4 S>M =M  IsClr

MP    Timer M0=  False =S  IsClr
MN    Timer M0=  False =S  IsClr
0 S>M Timer M0=  True  =S  IsClr
1 S>M Timer M0=  False =S  IsClr

 MP    Timer M0<  False =S  IsClr
 MN    Timer M0<  True  =S  IsClr
 0 S>M Timer M0<  False =S  IsClr
 1 S>M Timer M0<  False =S  IsClr
-1 S>M Timer M0<  True  =S  IsClr

MN    MP    Timer M=  False =S  IsClr
MN    MN    Timer M=  True  =S  IsClr
MP    MN    Timer M=  False =S  IsClr
1 S>M 1 S>M Timer M=  True  =S  IsClr

MN    MP    Timer M<>  True  =S  IsClr
MN    MN    Timer M<>  False =S  IsClr
MP    MN    Timer M<>  True  =S  IsClr
1 S>M 1 S>M Timer M<>  False =S  IsClr

MN    MP    Timer M<  True  =S  IsClr
MN    MN    Timer M<  False =S  IsClr
MP    MN    Timer M<  False =S  IsClr
1 S>M 1 S>M Timer M<  False =S  IsClr

MN    MP    Timer M<=  True  =S  IsClr
MN    MN    Timer M<=  True  =S  IsClr
MP    MN    Timer M<=  False =S  IsClr
1 S>M 1 S>M Timer M<=  True  =S  IsClr

MN    MP    Timer MU<  False =S  IsClr
MN    MN    Timer MU<  False =S  IsClr
MP    MN    Timer MU<  True  =S  IsClr
1 S>M 1 S>M Timer MU<  False =S  IsClr

MN    MP    Timer MU<=  False =S  IsClr
MN    MN    Timer MU<=  True  =S  IsClr
MP    MN    Timer MU<=  True  =S  IsClr
1 S>M 1 S>M Timer MU<=  True  =S  IsClr

MN Timer MU>D  M0  =M MN =M IsClr
MN Timer MS>D  M1s =M MN =M IsClr
MP Timer MS>D  M0  =M MP =M IsClr

M-4 Timer M>S  -4 =S  IsClr

Timer MVariable MVar  IsClr
MN MVar Timer M! IsClr
MVar Timer M@  MN =M IsClr

\ MRol ( M: m1 P.C -- m2 P.C )
MN Timer M2*  MN MN M+1 =M IsClr

\ MRor ( M: m1 P.C -- m2 P.C )  \ Rotate the content of a cell in memory right through the CARRY bit.
\ MN: MU2/ ( M: m1 -- m2 )
\     M2/ ( M: m1 -- m2 )
 
-1 S>M Timer M1+ 0 S>M =M IsClr
 0 S>M Timer M1+ 1 S>M =M IsClr

 1 S>M Timer M1-  0 S>M =M IsClr
 0 S>M Timer M1- -1 S>M =M IsClr

5 S>M 3 S>M Timer MOr   7 S>M =M IsClr
5 S>M 3 S>M Timer MAnd  1 S>M =M IsClr
5 S>M 3 S>M Timer MXor  6 S>M =M IsClr

5 S>M -3 S>M Timer M+1 2 S>M =M IsClr

5 S>M -3 S>M Timer M-1 8 S>M =M IsClr

 5 S>M Timer MNegate     -5 S>M =M IsClr
 0 S>M Timer MNegate      0 S>M =M IsClr
 1 S>M Timer MNegate     -1 S>M =M IsClr
-1 S>M Timer MNegate      1 S>M =M IsClr
 2 S>M Timer MNegate     -2 S>M =M IsClr
-2 S>M Timer MNegate      2 S>M =M IsClr
    MA Timer MNegate  MA M+1 M0 =M IsClr

   5 S>M Timer MAbs       5 S>M =M IsClr
  -5 S>M Timer MAbs       5 S>M =M IsClr
   0 S>M Timer MAbs       0 S>M =M IsClr
   1 S>M Timer MAbs       1 S>M =M IsClr
  -1 S>M Timer MAbs       1 S>M =M IsClr
MMIN-INT Timer MAbs MMID-UINT+1 =M IsClr

5 S>M 0 S>M Timer MDNegate -1 S>M =M -5 S>M =M IsClr

\ Hex>M ( uhi ... -- ) ( M: -- m ) \ convert words to M
\ M.Hex ( M: m -- )  \ type M in hex

33 S>M 4 Timer MU*Word  132 S>M =M  IsClr

  0 S>M          0 S>M Timer MUM*  0 S>M =M       0 S>M =M IsClr
  0 S>M          1 S>M Timer MUM*  0 S>M =M       0 S>M =M IsClr
  1 S>M          0 S>M Timer MUM*  0 S>M =M       0 S>M =M IsClr
  1 S>M          2 S>M Timer MUM*  0 S>M =M       2 S>M =M IsClr
  2 S>M          1 S>M Timer MUM*  0 S>M =M       2 S>M =M IsClr
  3 S>M          3 S>M Timer MUM*  0 S>M =M       9 S>M =M IsClr
MMID-UINT+1 MU2/ 2 S>M Timer MUM*  0 S>M =M MMID-UINT+1 =M IsClr
MMID-UINT+1      2 S>M Timer MUM*  1 S>M =M       0 S>M =M IsClr
MMID-UINT+1      4 S>M Timer MUM*  2 S>M =M       0 S>M =M IsClr
    M1S          2 S>M Timer MUM*  1 S>M =M     M1S M2* =M IsClr
MMAX-UINT    MMAX-UINT Timer MUM* -2 S>M =M       1 S>M =M IsClr


123 U>M      456 U>M   Timer MU*  123 456 * U>M =M IsClr
MMAX-UINT    MMAX-UINT Timer MU*          1 S>M =M IsClr

 3 S>M                   -4 S>M Timer M*1 -12 S>M     =M IsClr
 0 S>M                    0 S>M Timer M*1  0 S>M      =M IsClr
 0 S>M                    1 S>M Timer M*1  0 S>M      =M IsClr
 1 S>M                    0 S>M Timer M*1  0 S>M      =M IsClr
 1 S>M                    2 S>M Timer M*1  2 S>M      =M IsClr
 2 S>M                    1 S>M Timer M*1  2 S>M      =M IsClr
 3 S>M                    3 S>M Timer M*1  9 S>M      =M IsClr
-3 S>M                    3 S>M Timer M*1 -9 S>M      =M IsClr
 3 S>M                   -3 S>M Timer M*1 -9 S>M      =M IsClr
-3 S>M                   -3 S>M Timer M*1  9 S>M      =M IsClr
MMID-UINT+1 MU2/          2 S>M Timer M*1 MMID-UINT+1 =M IsClr
MMID-UINT+1 MU2/ MU2/     4 S>M Timer M*1 MMID-UINT+1 =M IsClr
MMID-UINT+1 M2/           2 S>M Timer M*1 MMID-UINT+1 =M IsClr

 0 S>M    0 S>M   Timer MM*                M0 =M           M0 =M IsClr
 0 S>M    1 S>M   Timer MM*                M0 =M           M0 =M IsClr
 1 S>M    0 S>M   Timer MM*                M0 =M           M0 =M IsClr
 1 S>M    2 S>M   Timer MM*                M0 =M        2 S>M =M IsClr
 2 S>M    1 S>M   Timer MM*                M0 =M        2 S>M =M IsClr
 3 S>M    3 S>M   Timer MM*                M0 =M        9 S>M =M IsClr
-3 S>M    3 S>M   Timer MM*               M1s =M       -9 S>M =M IsClr
 3 S>M   -3 S>M   Timer MM*               M1s =M       -9 S>M =M IsClr
-3 S>M   -3 S>M   Timer MM*                M0 =M        9 S>M =M IsClr
 0 S>M   MMIN-INT Timer MM*                M0 =M           M0 =M IsClr
 1 S>M   MMIN-INT Timer MM*               M1s =M     MMIN-INT =M IsClr
 2 S>M   MMIN-INT Timer MM*          M1s      =M          M0  =M IsClr
 0 S>M   MMAX-INT Timer MM*               M0  =M           M0 =M IsClr
 1 S>M   MMAX-INT Timer MM*               M0  =M     MMAX-INT =M IsClr
 2 S>M   MMAX-INT Timer MM*                M0 =M MMAX-INT M2* =M IsClr
MMIN-INT MMIN-INT Timer MM*         MMSB MU2/ =M           M0 =M IsClr
MMAX-INT MMIN-INT Timer MM*  MMSB M2/         =M         MMSB =M IsClr
MMAX-INT MMAX-INT Timer MM*  MMSB M2/ MInvert =M        1 S>M =M IsClr

: MU/ModWord ( M: um1 -- um2 ) ( udivisor -- uremainder )
  MU/ModWordA PushA ;  SeeL
133 S>M 4 Timer MU/ModWord  33 S>M =M  1 =S  IsClr

   0 U>M          0 U>M      1 U>M Timer MUM/Mod        M0 =M    M0 =M IsClr
   1 U>M          0 U>M      1 U>M Timer MUM/Mod     1 U>M =M 0 U>M =M IsClr
   1 U>M          0 U>M      2 U>M Timer MUM/Mod        M0 =M 1 U>M =M IsClr
   3 U>M          0 U>M      1 U>M Timer MUM/Mod     3 U>M =M 0 U>M =M IsClr
   3 U>M          0 U>M      2 U>M Timer MUM/Mod     1 U>M =M 1 U>M =M IsClr
MMAX-UINT     2 U>M MUM*     2 U>M Timer MUM/Mod MMAX-UINT =M    M0 =M IsClr
MMAX-UINT     2 U>M MUM* MMAX-UINT Timer MUM/Mod     2 U>M =M 0 U>M =M IsClr
MMAX-UINT MMAX-UINT MUM* MMAX-UINT Timer MUM/Mod MMAX-UINT =M    M0 =M IsClr
 133 U>M MS>D                4 U>M Timer MUM/Mod    33 U>M =M 1 U>M =M IsClr

   0 U>M           1 U>M Timer MU/Mod             M0 =M    M0 =M IsClr
   1 U>M           1 U>M Timer MU/Mod          1 U>M =M 0 U>M =M IsClr
   1 U>M           2 U>M Timer MU/Mod             M0 =M 1 U>M =M IsClr
   3 U>M           2 U>M Timer MU/Mod          1 U>M =M 1 U>M =M IsClr
MMAX-UINT          2 U>M Timer MU/Mod MMAX-UINT MU2/ =M 1 U>M =M IsClr
MMAX-UINT      MMAX-UINT Timer MU/Mod          1 U>M =M 0 U>M =M IsClr
MMAX-UINT MMAX-UINT MU2/ Timer MU/Mod          2 U>M =M 1 U>M =M IsClr
 135 U>M           4 U>M Timer MU/Mod         33 U>M =M 3 U>M =M IsClr

\ MUMod ( mu-dividend mu-divisor -- mu-remainder )  \ Divide unsigned
\ MU/ ( mu-dividend mu-divisor -- mu-quotient ) \ Divide unsigned

\ : MFM/Mod ( mlo mhi mdivisor -- mremainder mquotient ) \ Floored signed division

     0 S>M MS>D          1 S>M Timer MSM/Rem        M0 =M     M0 =M IsClr
     1 S>M MS>D          1 S>M Timer MSM/Rem     1 S>M =M  0 S>M =M IsClr
     2 S>M MS>D          1 S>M Timer MSM/Rem     2 S>M =M  0 S>M =M IsClr
    -1 S>M MS>D          1 S>M Timer MSM/Rem    -1 S>M =M  0 S>M =M IsClr
    -2 S>M MS>D          1 S>M Timer MSM/Rem    -2 S>M =M  0 S>M =M IsClr
     0 S>M MS>D         -1 S>M Timer MSM/Rem     0 S>M =M  0 S>M =M IsClr
     1 S>M MS>D         -1 S>M Timer MSM/Rem    -1 S>M =M  0 S>M =M IsClr
     2 S>M MS>D         -1 S>M Timer MSM/Rem    -2 S>M =M  0 S>M =M IsClr
    -1 S>M MS>D         -1 S>M Timer MSM/Rem     1 S>M =M  0 S>M =M IsClr
    -2 S>M MS>D         -1 S>M Timer MSM/Rem     2 S>M =M  0 S>M =M IsClr
     2 S>M MS>D          2 S>M Timer MSM/Rem     1 S>M =M  0 S>M =M IsClr
    -1 S>M MS>D         -1 S>M Timer MSM/Rem     1 S>M =M  0 S>M =M IsClr
    -2 S>M MS>D         -2 S>M Timer MSM/Rem     1 S>M =M  0 S>M =M IsClr
     7 S>M MS>D          3 S>M Timer MSM/Rem     2 S>M =M  1 S>M =M IsClr
     7 S>M MS>D         -3 S>M Timer MSM/Rem    -2 S>M =M  1 S>M =M IsClr
    -7 S>M MS>D          3 S>M Timer MSM/Rem    -2 S>M =M -1 S>M =M IsClr
    -7 S>M MS>D         -3 S>M Timer MSM/Rem     2 S>M =M -1 S>M =M IsClr
  MMAX-INT MS>D          1 S>M Timer MSM/Rem  MMAX-INT =M     M0 =M IsClr
  MMIN-INT MS>D          1 S>M Timer MSM/Rem  MMIN-INT =M     M0 =M IsClr
  MMAX-INT MS>D       MMAX-INT Timer MSM/Rem     1 S>M =M  0 S>M =M IsClr
  MMIN-INT MS>D       MMIN-INT Timer MSM/Rem     1 S>M =M  0 S>M =M IsClr
     M1S 1 S>M           4 S>M Timer MSM/Rem  MMAX-INT =M  3 S>M =M IsClr
2 S>M MMIN-INT    MM*    2 S>M Timer MSM/Rem  MMIN-INT =M  0 S>M =M IsClr
2 S>M MMIN-INT    MM* MMIN-INT Timer MSM/Rem     2 S>M =M  0 S>M =M IsClr
2 S>M MMAX-INT    MM*    2 S>M Timer MSM/Rem  MMAX-INT =M  0 S>M =M IsClr
2 S>M MMAX-INT    MM* MMAX-INT Timer MSM/Rem     2 S>M =M  0 S>M =M IsClr
MMIN-INT MMIN-INT MM* MMIN-INT Timer MSM/Rem  MMIN-INT =M  0 S>M =M IsClr
MMIN-INT MMAX-INT MM* MMIN-INT Timer MSM/Rem  MMAX-INT =M  0 S>M =M IsClr
MMIN-INT MMAX-INT MM* MMAX-INT Timer MSM/Rem  MMIN-INT =M  0 S>M =M IsClr
MMAX-INT MMAX-INT MM* MMAX-INT Timer MSM/Rem  MMAX-INT =M  0 S>M =M IsClr


 12345 U>M 8 Timer MU.R IsClr
 12345 U>M   Timer MU.  IsClr
-12345 S>M 8 Timer M.R  IsClr
-12345 S>M   Timer M.   IsClr
MP           Timer M.   IsClr

123 S>M 456 S>M
Timer M.S
Timer MDepth 2 =S
MDrop MDrop IsClr

Timer M'  0 M0  =M IsClr
Timer M' +3 M+3 =M IsClr
Timer M' -4 M-4 =M IsClr

-1234 Timer S>M  M' -1234 =M IsClr
 1234 Timer S>M  M'  1234 =M IsClr
 1234 Timer U>M  M'  1234 =M IsClr

: MPowers ( -- )  \ print powers of 2
  0  1 U>M  Begin
    CR Dup U. 9 Emit  MDup MU.  1+ M2*  MDup M0= Until Drop MDrop ; SeeL
MPowers

