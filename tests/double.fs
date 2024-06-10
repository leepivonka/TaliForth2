\ ------------------------------------------------------------------------
testing double words: 2constant 2variable d+ d- d. d.r d>s dabs dnegate

marker double_tests

decimal

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


\ max-2int 71 73 m*/ 2constant dbl1 
\ min-2int 73 79 m*/ 2constant dbl2
\ : d>ascii ( d -- caddr u ) 
   \ dup >r <# dabs #s r> sign #>    ( -- caddr1 u ) 
   \ here swap 2dup 2>r chars dup allot move 2r> 
\ ;

\ dbl1 d>ascii 2constant "dbl1" 
\ dbl2 d>ascii 2constant "dbl2"

\ : doubleoutput 
   \ cr ." you should see lines duplicated:" cr 
   \ 5 spaces "dbl1" type cr 
   \ 5 spaces dbl1 d. cr 
   \ 8 spaces "dbl1" dup >r type cr 
   \ 5 spaces dbl1 r> 3 + d.r cr 
   \ 5 spaces "dbl2" type cr 
   \ 5 spaces dbl2 d. cr 
   \ 10 spaces "dbl2" dup >r type cr 
   \ 5 spaces dbl2 r> 5 + d.r cr 
\ ;

\ T{ doubleoutput -> }T

T{        123456789. D0< ->  false  }T
T{       -123456789. D0< ->  true   }T
T{                0. D0< -> <FALSE> }T
T{                1. D0< -> <FALSE> }T
T{  MIN-INT        0 D0< -> <FALSE> }T
T{        0  MAX-INT D0< -> <FALSE> }T
T{          MAX-2INT D0< -> <FALSE> }T
T{               -1. D0< -> <TRUE>  }T
T{          MIN-2INT D0< -> <TRUE>  }T

T{       123456789. D0= ->  false  }T
T{      -123456789. D0= ->  false  }T
T{               1. D0= -> <FALSE> }T
T{ MIN-INT        0 D0= -> <FALSE> }T
T{         MAX-2INT D0= -> <FALSE> }T
T{      -1  MAX-INT D0= -> <FALSE> }T
T{               0. D0= -> <TRUE>  }T
T{              -1. D0= -> <FALSE> }T
T{       0  MIN-INT D0= -> <FALSE> }T

T{      123456789. D2* -> 246913578. }T
T{              0. D2* ->         0. }T
T{ MIN-INT       0 D2* -> 0 1 }T
T{         HI-2INT D2* -> MAX-2INT 1. D- }T
T{         LO-2INT D2* -> MIN-2INT }T

T{  246913578. D2/ -> 123456789. }T
T{          0. D2/ -> 0.         }T
T{          1. D2/ -> 0.         }T
T{         0 1 D2/ -> MIN-INT 0  }T
T{    MAX-2INT D2/ -> HI-2INT    }T
T{         -1. D2/ -> -1.        }T
T{    MIN-2INT D2/ -> LO-2INT    }T

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


T{    1234  0 d>s ->  1234   }T 
T{   -1234 -1 d>s -> -1234   }T 
T{ max-int  0 d>s -> max-int }T 
T{ min-int -1 d>s -> min-int }T

T{       1. dabs -> 1.       }T 
T{      -1. dabs -> 1.       }T 
T{ max-2int dabs -> max-2int }T 
T{ min-2int 1. d+ dabs -> max-2int }T

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

( TODO m*/ not implemented ) 
( TODO M*/ not implemented yet )

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

T{       1.       1. DU< -> <FALSE> }T
T{       1.      -1. DU< -> <TRUE>  }T
T{      -1.       1. DU< -> <FALSE> }T
T{      -1.      -2. DU< -> <FALSE> }T
T{ MAX-2INT  HI-2INT DU< -> <FALSE> }T
T{  HI-2INT MAX-2INT DU< -> <TRUE>  }T
T{ MAX-2INT MIN-2INT DU< -> <TRUE>  }T
T{ MIN-2INT MAX-2INT DU< -> <FALSE> }T
T{ MIN-2INT  LO-2INT DU< -> <TRUE>  }T


\ Free memory used for these tests
double_tests
