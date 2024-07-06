\ Based on tester.fr by John Hayes S1I
\   (C) 1995 JOHNS HOPKINS UNIVERSITY / APPLIED PHYSICS LABORATORY
\   MAY BE DISTRIBUTED FREELY AS LONG AS THIS COPYRIGHT NOTICE REMAINS.

\ SET THE FOLLOWING FLAG TO TRUE FOR MORE VERBOSE OUTPUT; THIS MAY
\ ALLOW YOU TO TELL WHICH TEST CAUSED YOUR SYSTEM TO HANG.
variable verbose  false verbose !

Decimal

: SeeL ;

FVariable FAccuracy  SeeL

variable actual-depth  SeeL   \ STACK RECORD
create actual-results  SeeL  16 cells allot
variable actual-FDepth  SeeL  \ STACK RECORD
create actual-FResults  SeeL  10 Floats Allot

\ Added by SamCo 2018-05 to show actual results of previous test.
: show-results \ ( -- ) Print the previous test's actual results.
   ." { "  actual-depth @ 0 ?Do
      actual-depth @ i - 1- \ Print them in reverse order to match test.             
      cells actual-results + @ .
    Loop
   ." } ( F: " actual-FDepth @ 0 ?Do
      actual-FDepth @ i - 1- \ Print them in reverse order to match test.             
      Floats actual-FResults + F@ F.
    Loop
  ;  SeeL

: error  \ ( C-ADDR U -- ) DISPLAY AN ERROR MESSAGE FOLLOWED BY
  \ THE LINE THAT HAD THE ERROR.
   type ( source type ) \ CR   \ DISPLAY LINE CORRESPONDING TO ERROR
   empty-stack    \ THROW AWAY EVERY THING ELSE
   show-results \ Added by SamCo to show what actually happened.
   Abort
   ;  SeeL


: {  \ ( -- ) start test
   Empty-Stack Empty-FStack ;  SeeL

: ->  \ ( ... -- ) RECORD DEPTH AND CONTENT OF STACK.
  depth dup actual-depth !  \ RECORD DEPTH
  0 ?Do  i cells actual-results + !  Loop \ SAVE THEM
  FDepth Dup actual-FDepth !  \ RECORD DEPTH
  0 ?Do  I Floats actual-FResults + F!  Loop \ SAVE THEM
  ;  SeeL

\ cc@ returns the emulator cycle counter as a double integer
: no-op ;
: || \ ( -- ) \ execute & time following word
  ' cc@ 2>r execute cc@ 2r> d-  ['] no-op cc@ 2>r execute cc@ 2r> d-  d-
      base @ >r decimal ." cyc=" d. r> base !  \ count cycles
  ;  SeeL

: |->  \ ( ... -- )
  || -> ;  SeeL

: } ( ... -- ) \ COMPARE STACK (EXPECTED) CONTENTS WITH SAVED (ACTUAL) CONTENTS.
  Depth actual-Depth @ = If  \ IF DEPTHS MATCH
    Depth 0 ?Do    \ FOR EACH STACK ITEM
      i cells actual-Results + @ \ COMPARE ACTUAL WITH EXPECTED
         <> If s" INCORRECT RESULT: " error leave then
     Loop
   else     \ DEPTH MISMATCH
    s" WRONG NUMBER OF RESULTS: " error
   then

  FDepth actual-FDepth @ = if  \ IF DEPTHS MATCH
    FDepth 0 ?Do    \ FOR EACH STACK ITEM
      i Floats actual-FResults + F@ \ COMPARE ACTUAL WITH EXPECTED
         FAccuracy F@ F~ 0= If s" INCORRECT FResult: " error leave then
     Loop
   else     \ DEPTH MISMATCH
    s" WRONG NUMBER OF FResults: " error
   then 
  ;  SeeL

: testing \ ( -- ) TALKING COMMENT.
  source verbose @ if
    dup >r type cr r> >in !
   else >in ! drop
   then ;  SeeL
