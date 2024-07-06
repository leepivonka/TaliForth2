\ ------------------------------------------------------------------------
testing double words: 2constant 2variable d+ d- d. d.r d>s dabs dnegate

marker double_tests

decimal

T{ : k1 [ $12345678. 2Literal ] ; -> }T
T{ k1 -> $12345678. }T
T{ $23456789. 2Constant k2 -> }T
T{ k2 -> $23456789. }T
T{ $98765432. 2Value k3 -> }T
T{ k3 -> $98765432. }T

T{ k1 2Drop -> }T

T{ k2 2Dup -> k2 k2 }T

T{ k1 k2 2Nip -> k2 }T

T{ k1 k2 2Over -> k1 k2 k1 }T



T{ 2variable 2v1 -> }T
T{ 0. 2v1 2! -> }T
T{ 2v1 2@ -> 0. }T
T{ -1 -2 2v1 2! -> }T
T{ 2v1 2@ -> -1 -2 }T
T{ : cd2 2variable ; -> }T
T{ cd2 2v2 -> }T
T{ : cd3 2v2 2! ; -> }T
T{ -2 -1 cd3 -> }T
T{ 2v2 2@ -> -2 -1 }T
T{ 2variable 2v3 immediate 5 6 2v3 2! -> }T
T{ 2v3 2@ -> 5 6 }T

T{ Here $98765432. 2, -> Here 4 - }T
T{ Here 4 - @ -> $9876 }T
T{ Here 2 - @ -> $5432 }T

T{ $87654321. 2v1 D! -> }T
T{ 2v1 D@ -> $87654321. }T
T{ 2v1  @ -> $4321 }T
T{ 2v1 2 + @ -> $8765 }T

\ see core_a.fs
\ T{ $dcba9876.  2>R -> }T
\ T{ 2R@ -> $dcba9876. }T
\ T{ R> -> $dcba }T
\ T{ R> -> $9876 }T
\ T{ $45566778. 2>R -> }T
\ T{ 2R> -> $45566778. }T


\ Repeats in case we call this test alone
0 constant 0s
0 invert constant 1s
0 invert 1 rshift  constant max-int
0 invert 1 rshift invert  constant min-int

T{  0.  5. d+ ->  5. }T                         \ small integers
T{ -5.  0. d+ -> -5. }T
T{  1.  2. d+ ->  3. }T
T{  1. -2. d+ -> -1. }T
T{ -1.  2. d+ ->  1. }T
T{ -1. -2. d+ -> -3. }T
T{ -1.  1. d+ ->  0. }T
T{  0  0  0  5 d+ ->  0  5 }T                  \ mid range integers
T{ -1  5  0  0 d+ -> -1  5 }T
T{  0  0  0 -5 d+ ->  0 -5 }T
T{  0 -5 -1  0 d+ -> -1 -5 }T
T{  0  1  0  2 d+ ->  0  3 }T
T{ -1  1  0 -2 d+ -> -1 -1 }T
T{  0 -1  0  2 d+ ->  0  1 }T
T{  0 -1 -1 -2 d+ -> -1 -3 }T
T{ -1 -1  0  1 d+ -> -1  0 }T

T{ min-int 0 2dup d+ -> 0 1 }T
T{ min-int s>d min-int 0 d+ -> 0 0 }T

T{ 1 2 2constant 2c1 -> }T
T{ 2c1 -> 1 2 }T
T{ : cd1 2c1 ; -> }T
T{ cd1 -> 1 2 }T
T{ : cd2 2constant ; -> }T
T{ -1 -2 cd2 2c2 -> }T
T{ 2c2 -> -1 -2 }T
T{ 4 5 2constant 2c3 immediate 2c3 -> 4 5 }T
T{ : cd6 2c3 2literal ; cd6 -> 4 5 }T

max-int 2/ constant hi-int \ 001...1
min-int 2/ constant lo-int \ 110...1

1s max-int  2constant max-2int \ 01...1
0 min-int   2constant min-2int \ 10...0
max-2int 2/ 2constant hi-2int  \ 001...1
min-2int 2/ 2constant lo-2int  \ 110...0

T{ : cd1 [ max-2int ] 2literal ; -> }T
T{ cd1 -> max-2int }T
T{ 2variable 2v4 immediate 5 6 2v4 2! -> }T
T{ : cd7 2v4 [ 2@ ] 2literal ; cd7 -> 5 6 }T
T{ : cd8 [ 6 7 ] 2v4 [ 2! ] ; 2v4 2@ -> 6 7 }T

T{  hi-2int       1. d+ -> 0 hi-int 1+ }T     \ large double integers
T{  hi-2int     2dup d+ -> 1s 1- max-int }T
T{ max-2int min-2int d+ -> -1. }T
T{ max-2int  lo-2int d+ -> hi-2int }T
T{  lo-2int     2dup d+ -> min-2int }T
T{  hi-2int min-2int d+ 1. d+ -> lo-2int }T

T{  0.  5. d- -> -5. }T              \ small integers
T{  5.  0. d- ->  5. }T
T{  0. -5. d- ->  5. }T
T{  1.  2. d- -> -1. }T
T{  1. -2. d- ->  3. }T
T{ -1.  2. d- -> -3. }T
T{ -1. -2. d- ->  1. }T
T{ -1. -1. d- ->  0. }T
T{  0  0  0  5 d- ->  0 -5 }T        \ mid-range integers
T{ -1  5  0  0 d- -> -1  5 }T
T{  0  0 -1 -5 d- ->  1  4 }T
T{  0 -5  0  0 d- ->  0 -5 }T
T{ -1  1  0  2 d- -> -1 -1 }T
T{  0  1 -1 -2 d- ->  1  2 }T
T{  0 -1  0  2 d- ->  0 -3 }T
T{  0 -1  0 -2 d- ->  0  1 }T
T{  0  0  0  1 d- ->  0 -1 }T
T{ min-int 0 2dup d- -> 0. }T
T{ min-int s>d max-int 0 d- -> 1 1s }T

T{ max-2int max-2int d- -> 0. }T    \ large integers
T{ min-2int min-2int d- -> 0. }T
T{ max-2int  hi-2int d- -> lo-2int dnegate }T
T{  hi-2int  lo-2int d- -> max-2int }T
T{  lo-2int  hi-2int d- -> min-2int 1. d+ }T
T{ min-2int min-2int d- -> 0. }T
T{ min-2int  lo-2int d- -> lo-2int }T


max-2int 71 73 m*/ 2constant dbl1
min-2int 73 79 m*/ 2constant dbl2
: d>ascii ( d -- caddr u )
   dup >r <# dabs #s r> sign #>    ( -- caddr1 u )
   here swap 2dup 2>r chars dup allot move 2r>
;

dbl1 d>ascii 2constant "dbl1"
dbl2 d>ascii 2constant "dbl2"

: doubleoutput
   cr ." you should see lines duplicated:" cr
   5 spaces "dbl1" type cr
   5 spaces dbl1 d. cr
   8 spaces "dbl1" dup >r type cr
   5 spaces dbl1 r> 3 + d.r cr
   5 spaces "dbl2" type cr
   5 spaces dbl2 d. cr
   10 spaces "dbl2" dup >r type cr
   5 spaces dbl2 r> 5 + d.r cr
;
doubleoutput
\ T{ doubleoutput -> }T

T{                0. D0< -> <FALSE> }T
T{                1. D0< -> <FALSE> }T
T{  MIN-INT        0 D0< -> <FALSE> }T
T{        0  MAX-INT D0< -> <FALSE> }T
T{          MAX-2INT D0< -> <FALSE> }T
T{               -1. D0< -> <TRUE>  }T
T{          MIN-2INT D0< -> <TRUE>  }T

T{                0. D0>= -> True  }T
T{                1. D0>= -> True  }T
T{  MIN-INT        0 D0>= -> True  }T
T{        0  MAX-INT D0>= -> True  }T
T{          MAX-2INT D0>= -> True  }T
T{               -1. D0>= -> False }T
T{          MIN-2INT D0>= -> False }T

T{               1. D0= -> <FALSE> }T
T{ MIN-INT       0  D0= -> <FALSE> }T
T{         MAX-2INT D0= -> <FALSE> }T
T{     -1   MAX-INT D0= -> <FALSE> }T
T{               0. D0= -> <TRUE>  }T
T{              -1. D0= -> <FALSE> }T
T{      0   MIN-INT D0= -> <FALSE> }T

T{               1. D0<> -> True  }T
T{ MIN-INT       0  D0<> -> True  }T
T{         MAX-2INT D0<> -> True  }T
T{     -1   MAX-INT D0<> -> True  }T
T{               0. D0<> -> False }T
T{              -1. D0<> -> True  }T
T{      0   MIN-INT D0<> -> True  }T

T{                0. D0<= -> True    }T
T{                1. D0<= -> <FALSE> }T
T{  MIN-INT        0 D0<= -> <FALSE> }T
T{        0  MAX-INT D0<= -> <FALSE> }T
T{          MAX-2INT D0<= -> <FALSE> }T
T{               -1. D0<= -> <TRUE>  }T
T{          MIN-2INT D0<= -> <TRUE>  }T

T{                0. D0> -> False }T
T{                1. D0> -> True  }T
T{  MIN-INT        0 D0> -> True  }T
T{        0  MAX-INT D0> -> True  }T
T{          MAX-2INT D0> -> True  }T
T{               -1. D0> -> False }T
T{          MIN-2INT D0> -> False }T

T{                0. D0>= -> True  }T
T{                1. D0>= -> True  }T
T{  MIN-INT        0 D0>= -> True  }T
T{        0  MAX-INT D0>= -> True  }T
T{          MAX-2INT D0>= -> True  }T
T{               -1. D0>= -> False }T
T{          MIN-2INT D0>= -> False }T


T{              0. D2* -> 0. D2* }T
T{ MIN-INT       0 D2* -> 0 1 }T
T{         HI-2INT D2* -> MAX-2INT 1. D- }T
T{         LO-2INT D2* -> MIN-2INT }T

T{       0. D2/ -> 0.        }T
T{       1. D2/ -> 0.        }T
T{      0 1 D2/ -> MIN-INT 0 }T
T{ MAX-2INT D2/ -> HI-2INT   }T
T{      -1. D2/ -> -1.       }T
T{ MIN-2INT D2/ -> LO-2INT   }T

T{         0. UD2/ -> 0.         }T
T{         1. UD2/ -> 0.         }T
T{        0 1 UD2/ -> MIN-INT 0  }T
T{   MAX-2INT UD2/ -> HI-2INT    }T
T{        -1. UD2/ -> $7fffffff. }T
T{ $80000000. UD2/ -> $40000000. }T

T{       0.       1. D< -> <TRUE>  }T
T{       0.       0. D< -> <FALSE> }T
T{       1.       0. D< -> <FALSE> }T
T{      -1.       1. D< -> <TRUE>  }T
T{      -1.       0. D< -> <TRUE>  }T
T{      -2.      -1. D< -> <TRUE>  }T
T{      -1.      -2. D< -> <FALSE> }T
T{      -1. MAX-2INT D< -> <TRUE>  }T
T{ MIN-2INT MAX-2INT D< -> <TRUE>  }T
T{ MAX-2INT      -1. D< -> <FALSE> }T
T{ MAX-2INT MIN-2INT D< -> <FALSE> }T
T{ MAX-2INT 2DUP -1. D+ D< -> <FALSE> }T
T{ MIN-2INT 2DUP  1. D+ D< -> <TRUE>  }T

T{       0.       1. D<= -> <TRUE>  }T
T{       0.       0. D<= -> <True>  }T
T{       1.       0. D<= -> <FALSE> }T
T{      -1.       1. D<= -> <TRUE>  }T
T{      -1.       0. D<= -> <TRUE>  }T
T{      -2.      -1. D<= -> <TRUE>  }T
T{      -1.      -2. D<= -> <FALSE> }T
T{      -1. MAX-2INT D<= -> <TRUE>  }T
T{ MIN-2INT MAX-2INT D<= -> <TRUE>  }T
T{ MAX-2INT      -1. D<= -> <FALSE> }T
T{ MAX-2INT MIN-2INT D<= -> <FALSE> }T
T{ MAX-2INT 2DUP -1. D+ D<= -> <FALSE> }T
T{ MIN-2INT 2DUP  1. D+ D<= -> <TRUE>  }T

T{      -1.      -1. D= -> <TRUE>  }T
T{      -1.       0. D= -> <FALSE> }T
T{      -1.       1. D= -> <FALSE> }T
T{       0.      -1. D= -> <FALSE> }T
T{       0.       0. D= -> <TRUE>  }T
T{       0.       1. D= -> <FALSE> }T
T{       1.      -1. D= -> <FALSE> }T
T{       1.       0. D= -> <FALSE> }T
T{       1.       1. D= -> <TRUE>  }T
T{   0   -1    0  -1 D= -> <TRUE>  }T
T{   0   -1    0   0 D= -> <FALSE> }T
T{   0   -1    0   1 D= -> <FALSE> }T
T{   0    0    0  -1 D= -> <FALSE> }T
T{   0    0    0   0 D= -> <TRUE>  }T
T{   0    0    0   1 D= -> <FALSE> }T
T{   0    1    0  -1 D= -> <FALSE> }T
T{   0    1    0   0 D= -> <FALSE> }T
T{   0    1    0   1 D= -> <TRUE>  }T
T{ MAX-2INT MIN-2INT D= -> <FALSE> }T
T{ MAX-2INT       0. D= -> <FALSE> }T
T{ MAX-2INT MAX-2INT D= -> <TRUE>  }T
T{ MAX-2INT HI-2INT  D= -> <FALSE> }T
T{ MAX-2INT MIN-2INT D= -> <FALSE> }T
T{ MIN-2INT MIN-2INT D= -> <TRUE>  }T
T{ MIN-2INT LO-2INT  D= -> <FALSE> }T
T{ MIN-2INT MAX-2INT D= -> <FALSE> }T

T{      -1.      -1. D<> -> False   }T
T{      -1.       0. D<> -> True  }T
T{      -1.       1. D<> -> True  }T
T{       0.      -1. D<> -> True  }T
T{       0.       0. D<> -> False   }T
T{       0.       1. D<> -> True  }T
T{       1.      -1. D<> -> True  }T
T{       1.       0. D<> -> True  }T
T{       1.       1. D<> -> False   }T
T{   0   -1    0  -1 D<> -> False   }T
T{   0   -1    0   0 D<> -> True  }T
T{   0   -1    0   1 D<> -> True  }T
T{   0    0    0  -1 D<> -> True  }T
T{   0    0    0   0 D<> -> False   }T
T{   0    0    0   1 D<> -> True  }T
T{   0    1    0  -1 D<> -> True  }T
T{   0    1    0   0 D<> -> True  }T
T{   0    1    0   1 D<> -> False   }T
T{ MAX-2INT MIN-2INT D<> -> True  }T
T{ MAX-2INT       0. D<> -> True  }T
T{ MAX-2INT MAX-2INT D<> -> False   }T
T{ MAX-2INT HI-2INT  D<> -> True  }T
T{ MAX-2INT MIN-2INT D<> -> True  }T
T{ MIN-2INT MIN-2INT D<> -> False   }T
T{ MIN-2INT LO-2INT  D<> -> True  }T
T{ MIN-2INT MAX-2INT D<> -> True  }T

T{       0.       1.    D> -> False }T
T{       0.       0.    D> -> False }T
T{       1.       0.    D> -> True  }T
T{      -1.       1.    D> -> False }T
T{      -1.       0.    D> -> False }T
T{      -2.      -1.    D> -> False }T
T{      -1.      -2.    D> -> True  }T
T{      -1. MAX-2INT    D> -> False }T
T{ MIN-2INT MAX-2INT    D> -> False }T
T{ MAX-2INT      -1.    D> -> True  }T
T{ MAX-2INT MIN-2INT    D> -> True  }T
T{ MAX-2INT 2DUP -1. D+ D> -> True  }T
T{ MIN-2INT 2DUP  1. D+ D> -> False }T

T{       0.       1.    D>= -> False }T
T{       0.       0.    D>= -> True  }T
T{       1.       0.    D>= -> True  }T
T{      -1.       1.    D>= -> False }T
T{      -1.       0.    D>= -> False }T
T{      -2.      -1.    D>= -> False }T
T{      -1.      -2.    D>= -> True  }T
T{      -1. MAX-2INT    D>= -> False }T
T{ MIN-2INT MAX-2INT    D>= -> False }T
T{ MAX-2INT      -1.    D>= -> True  }T
T{ MAX-2INT MIN-2INT    D>= -> True  }T
T{ MAX-2INT 2DUP -1. D+ D>= -> True  }T
T{ MIN-2INT 2DUP  1. D+ D>= -> False }T



T{    1234  0 d>s ->  1234   }T
T{   -1234 -1 d>s -> -1234   }T
T{ max-int  0 d>s -> max-int }T
T{ min-int -1 d>s -> min-int }T

T{       1.       2. DMAX ->  2.      }T
T{       1.       0. DMAX ->  1.      }T
T{       1.      -1. DMAX ->  1.      }T
T{       1.       1. DMAX ->  1.      }T
T{       0.       1. DMAX ->  1.      }T
T{       0.      -1. DMAX ->  0.      }T
T{      -1.       1. DMAX ->  1.      }T
T{      -1.      -2. DMAX -> -1.      }T
T{ MAX-2INT  HI-2INT DMAX -> MAX-2INT }T
T{ MAX-2INT MIN-2INT DMAX -> MAX-2INT }T
T{ MIN-2INT MAX-2INT DMAX -> MAX-2INT }T
T{ MIN-2INT  LO-2INT DMAX -> LO-2INT  }T

T{ MAX-2INT       1. DMAX -> MAX-2INT }T
T{ MAX-2INT      -1. DMAX -> MAX-2INT }T
T{ MIN-2INT       1. DMAX ->  1.      }T
T{ MIN-2INT      -1. DMAX -> -1.      }T

T{       1.       2. DMIN ->  1.      }T
T{       1.       0. DMIN ->  0.      }T
T{       1.      -1. DMIN -> -1.      }T
T{       1.       1. DMIN ->  1.      }T
T{       0.       1. DMIN ->  0.      }T
T{       0.      -1. DMIN -> -1.      }T
T{      -1.       1. DMIN -> -1.      }T
T{      -1.      -2. DMIN -> -2.      }T
T{ MAX-2INT  HI-2INT DMIN -> HI-2INT  }T
T{ MAX-2INT MIN-2INT DMIN -> MIN-2INT }T
T{ MIN-2INT MAX-2INT DMIN -> MIN-2INT }T
T{ MIN-2INT  LO-2INT DMIN -> MIN-2INT }T

T{ MAX-2INT       1. DMIN ->  1.      }T
T{ MAX-2INT      -1. DMIN -> -1.      }T
T{ MIN-2INT       1. DMIN -> MIN-2INT }T
T{ MIN-2INT      -1. DMIN -> MIN-2INT }T

T{ 0. dnegate -> 0. }T
T{ 1. dnegate -> -1. }T
T{ -1. dnegate -> 1. }T
T{ max-2int dnegate -> min-2int swap 1+ swap }T
T{ min-2int swap 1+ swap dnegate -> max-2int }T
T{ $456789ab. DNegate -> $ba987655. }T
T{ $e3344556. DNegate -> $1ccbbaaa. }T

T{       1. dabs -> 1.       }T
T{      -1. dabs -> 1.       }T
T{ max-2int dabs -> max-2int }T
T{ min-2int 1. d+ dabs -> max-2int }T
T{         0. DAbs ->         0. }T
T{ $12345678. DAbs -> $12345678. }T
T{ $87654321. DAbs -> $789abcdf. }T


T{  5.  7  11 m*/ -> 3. }T
T{ -5. -7  11 m*/ -> 3. }T
T{ -5.  7 -11 m*/ -> 3. }T

T{ HI-2INT   1 M+ -> HI-2INT   1. D+ }T
T{ MAX-2INT -1 M+ -> MAX-2INT -1. D+ }T
T{ MIN-2INT  1 M+ -> MIN-2INT  1. D+ }T
T{ LO-2INT  -1 M+ -> LO-2INT  -1. D+ }T

T{       1.       2. 3. 2ROT ->       2. 3.       1. }T
T{ MAX-2INT MIN-2INT 1. 2ROT -> MIN-2INT 1. MAX-2INT }T

T{ 1 2 2VALUE t2val -> }T
T{ t2val -> 1 2 }T
T{ 3 4 TO t2val -> }T
T{ t2val -> 3 4 }T
: sett2val t2val 2SWAP TO t2val ;
T{ 5 6 sett2val t2val -> 3 4 5 6 }T

T{       1.       1. DU< -> FALSE }T
T{       1.      -1. DU< -> TRUE  }T
T{      -1.       1. DU< -> FALSE }T
T{      -1.      -2. DU< -> FALSE }T
T{ MAX-2INT  HI-2INT DU< -> FALSE }T
T{  HI-2INT MAX-2INT DU< -> TRUE  }T
T{ MAX-2INT MIN-2INT DU< -> TRUE  }T
T{ MIN-2INT MAX-2INT DU< -> FALSE }T
T{ MIN-2INT  LO-2INT DU< -> TRUE  }T

T{       1.       1. DU<= -> True  }T
T{       1.      -1. DU<= -> TRUE  }T
T{      -1.       1. DU<= -> FALSE }T
T{      -1.      -2. DU<= -> FALSE }T
T{ MAX-2INT  HI-2INT DU<= -> FALSE }T
T{  HI-2INT MAX-2INT DU<= -> TRUE  }T
T{ MAX-2INT MIN-2INT DU<= -> TRUE  }T
T{ MIN-2INT MAX-2INT DU<= -> FALSE }T
T{ MIN-2INT  LO-2INT DU<= -> TRUE  }T

T{       1.       1. DU> -> False }T
T{       1.      -1. DU> -> False }T
T{      -1.       1. DU> -> True  }T
T{      -1.      -2. DU> -> True  }T
T{ MAX-2INT  HI-2INT DU> -> True  }T
T{  HI-2INT MAX-2INT DU> -> False }T
T{ MAX-2INT MIN-2INT DU> -> False }T
T{ MIN-2INT MAX-2INT DU> -> True  }T
T{ MIN-2INT  LO-2INT DU> -> False }T

T{       1.       1. DU>= -> True  }T
T{       1.      -1. DU>= -> False }T
T{      -1.       1. DU>= -> True  }T
T{      -1.      -2. DU>= -> True  }T
T{ MAX-2INT  HI-2INT DU>= -> True  }T
T{  HI-2INT MAX-2INT DU>= -> False }T
T{ MAX-2INT MIN-2INT DU>= -> False }T
T{ MIN-2INT MAX-2INT DU>= -> True  }T
T{ MIN-2INT  LO-2INT DU>= -> False }T

T{         0. D1+ ->         1. }T
T{       $ff. D1+ ->      $100. }T
T{     $ffff. D1+ ->    $10000. }T
T{   $ffffff. D1+ ->  $1000000. }T
T{ $93ffffff. D1+ -> $94000000. }T

T{ $7292737a.  0 DRShift -> $7292737a. }T
T{ $7292737a. 12 DRShift ->    $72927. }T



T{ -123456789.  D. -> }T
T{  123456. 8  D.R -> }T
T{ 3456789012. UD. -> }T
T{  123456. 8 UD.R -> }T


T{ $5678 $9876 UM* -> $337f1b50. }T
T{ $ffff $ffff UM* -> $fffe0001. }T
T{ $fffe5678. $ffff UM/Mod -> $5677 $ffff }T
T{ $337f2c77. $5678 UM/Mod -> $1127 $9876 }T

$8000 Constant MSB

T{       0       0 M* ->       0 S>D }T
T{       0       1 M* ->       0 S>D }T
T{       1       0 M* ->       0 S>D }T
T{       1       2 M* ->       2 S>D }T
T{       2       1 M* ->       2 S>D }T
T{       3       3 M* ->       9 S>D }T
T{      -3       3 M* ->      -9 S>D }T
T{       3      -3 M* ->      -9 S>D }T
T{      -3      -3 M* ->       9 S>D }T
T{       0 MIN-INT M* ->       0 S>D }T
T{       1 MIN-INT M* -> MIN-INT S>D }T
T{       2 MIN-INT M* ->       0 1S  }T
T{       0 MAX-INT M* ->       0 S>D }T
T{       1 MAX-INT M* -> MAX-INT S>D }T
T{       2 MAX-INT M* -> MAX-INT     1 LSHIFT 0 }T
T{ MIN-INT MIN-INT M* ->       0 MSB 1 RSHIFT   }T
T{ MAX-INT MIN-INT M* ->     MSB MSB 2/         }T
T{ MAX-INT MAX-INT M* ->       1 MSB 2/ INVERT  }T

: ?floored [ -3 2 / -2 = ] LITERAL IF 1. D- THEN ;

T{       5.       7             11 M*/ ->  3. }T
T{       5.      -7             11 M*/ -> -3. ?floored }T
T{      -5.       7             11 M*/ -> -3. ?floored }T
T{      -5.      -7             11 M*/ ->  3. }T
T{ MAX-2INT       8             16 M*/ -> HI-2INT }T
T{ MAX-2INT      -8             16 M*/ -> HI-2INT DNEGATE ?floored }T
T{ MIN-2INT       8             16 M*/ -> LO-2INT }T
T{ MIN-2INT      -8             16 M*/ -> LO-2INT DNEGATE }T

T{ MAX-2INT MAX-INT        MAX-INT M*/ -> MAX-2INT }T
T{ MAX-2INT MAX-INT 2/     MAX-INT M*/ -> MAX-INT 1- HI-2INT NIP }T
T{ MIN-2INT LO-2INT NIP DUP NEGATE M*/ -> MIN-2INT }T
T{ MIN-2INT LO-2INT NIP 1- MAX-INT M*/ -> MIN-INT 3 + HI-2INT NIP 2 + }T
T{ MAX-2INT LO-2INT NIP DUP NEGATE M*/ -> MAX-2INT DNEGATE }T
T{ MIN-2INT MAX-INT            DUP M*/ -> MIN-2INT }T


T{ HI-2INT   1 M+ -> HI-2INT   1. D+ }T
T{ MAX-2INT -1 M+ -> MAX-2INT -1. D+ }T
T{ MIN-2INT  1 M+ -> MIN-2INT  1. D+ }T
T{ LO-2INT  -1 M+ -> LO-2INT  -1. D+ }T

T{ HI-2INT      1 UM+ -> HI-2INT      1. D+ }T
T{ MAX-2INT $ffff UM+ -> MAX-2INT $ffff. D+ }T
T{ MIN-2INT     1 UM+ -> MIN-2INT     1. D+ }T
T{ LO-2INT  $ffff UM+ -> LO-2INT  $ffff. D+ }T


\ FHdr "DA@",NN ; ( -- d ) ( F: d -- d )  copy double aux data entry on FP stack to data stack
\ FHdr "2>A",NN ; ( d -- ) ( F: -- d )  Move double data stack entry to 1 FP stack entry
\ FHdr "2A>",0 ; ( -- d ) ( F: d -- )  move double aux data entry on FP to data stack
\ FHdr "2Dup>A",0 ; ( d -- d ) ( F: -- d )  Copy double data stack entry to 1 FP stack entry

\ Free memory used for these tests
double_tests
