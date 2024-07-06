
2Variable t_overhead
2variable total-cycles

true strip-underflow !
20 nc-limit !

: cycles ( xt -- ud )
    cc@ 2>R execute cc@ 2R> D- t_overhead 2@ D- ;

: show-cycles ( xt -- )
    cycles
    2dup total-cycles 2@ d+ total-cycles 2!
    ud. ." cycles." cr
;

0. total-cycles 2!
0. t_overhead 2! ' align cycles t_overhead 2!

\ benchmarks from https://theultimatebenchmark.org/#sec-13
: ddbench 1 32767 0 do dup drop loop drop ; See ddbench

32000 constant bigint
variable result
: intcalcs
  1 dup result dup >r !
  begin
    dup bigint <
  while
    dup negate r@ +! 1+
    dup r@ +! 1+
    r@ @ over * r@ ! 1+
    r@ @ over / r@ ! 1+
  repeat
  r> drop drop
  ;  See intcalcs

: fib2 ( n1 -- n2 )
   0 1 rot 0 do
      over + swap loop
   drop ;  See fib2

: fib2-bench
    400 0 do i fib2 drop loop
  ;  See fib2-bench

: bottom ;              : 1st bottom bottom ;   : 2nd 1st 1st ;
: 3rd 2nd 2nd ;         : 4th 3rd 3rd ;         : 5th 4th 4th ;
: 6th 5th 5th ;         : 7th 6th 6th ;         : 8th 7th 7th ;
: 9th 8th 8th ;         : 10th 9th 9th ;        : 11th 10th 10th ;
: 12th 11th 11th ;      : 13th 12th 12th ;      : 14th 13th 13th ;
: 15th 14th 14th ;      : 16th 15th 15th ;      : 17th 16th 16th ;
: 18th 17th 17th ;      : 19th 18th 18th ;      : 20th 19th 19th ;
See 19th
See 20th

 8192 constant sieve-size
 variable sieve-flags
 0 sieve-flags !
 sieve-size allot

 : sieve-bench
   sieve-flags sieve-size 1 fill  ( set array )
   0 ( 0 count ) sieve-size 0
   do sieve-flags i + c@
     if i dup + 3 + dup i +
        begin dup sieve-size <
        while 0   over sieve-flags +  c!  over +  repeat
        drop drop 1+
     then
 loop
 drop
 ;  see sieve-bench


: gcd ( a b -- gcd )
   OVER IF
     BEGIN
       DUP WHILE
          2DUP U> IF SWAP THEN OVER -
     REPEAT DROP ELSE
     DUP IF NIP ELSE 2DROP 1 THEN
   THEN ;  see gcd

: gcd1-bench 80 0 DO
      80 0 DO j i gcd drop loop
      loop ;  see gcd1-bench

: num>str ( n -- addr bytecount ) 0 <# #S #> ;  see num>str

: lasteqfirst? ( addr offsetlast -- flag )
  OVER + C@ SWAP C@ = ;  see lasteqfirst?

: ispalindrome? ( addr offsetlast -- flag )
  DUP 1 <              IF 2DROP 1 EXIT THEN
  2DUP lasteqfirst? 0= IF 2DROP 0 EXIT THEN
  2 - SWAP 1+ SWAP RECURSE ;  see ispalindrome?

: pal-bench ( -- ) 10 BEGIN
    DUP   num>str  \ ( n addr len )
    2DUP 1-        \ ( n addr len addr len-1 )
    ispalindrome?  \ ( n addr len flag )
    drop 2drop     \ for output: if type space else 2drop then
    1+ DUP 4096 =
  UNTIL DROP ;  see pal-bench


( Benchmark mit der Collatz-Funktion  V1.1 RG 2017-10-05   )
: cn+1 		( cn -- cm )
  2 /mod swap
  if dup 10922 < 	( kein ueberlauf ? )
    if 3 * 2 +
    else drop 0 then
  then
  ;  see cn+1
: coll. 	( cn -- )
  begin dup 1 > while
    cn+1 dup .
  repeat
  drop		( always 1 )
  ;  see coll.
: ccnt 		( cn -- cnt)
  0 swap 	( cnt cn )
  begin dup 1 > while
  cn+1 dup
	if swap 1 + swap ( zaehlen )
	else drop 0
	then
  repeat
  drop
  ;  see ccnt
: cmax 		( k -- max )
  0 swap	( max k )
  begin dup 0 > while
    dup ccnt 	( max k cnt )
    rot  	( k cnt max )
    max		( k max )
    swap	( max k )
    1 -		( max k-1 )
  repeat
  drop
  ;  see cmax
: coll-bench 384 cmax drop ;  see coll-bench

' ddbench show-cycles
' intcalcs show-cycles
' fib2-bench show-cycles
' 20th show-cycles
' sieve-bench show-cycles
' gcd1-bench show-cycles
' pal-bench show-cycles
' coll-bench show-cycles
 total-cycles 2@ ud. .( cycles )
