; Tali Forth 2 Remix for the 6502

; Based on Tali Forth 2 by
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Sam Colwell

; Variable size word header version 20240628

; These assignments are "weak" and will only assign if the label
; does not have anything assigned to it.  The user can override these
; defaults by assigning values in their platform file before
; including this file.

; Assemble all words unless overridden in the platform file.
TALI_OPTIONAL_WORDS := ["fp","fpe", "fpieee","fptrancendentals", "fphyperbolic", "ed", "editor", "ramdrive", "block", "environment?", "assembler", "wordlist" ]

; "fp" is floating-point (~3.8k)
; "fpe" is floating-point 16bit-precision multiply & divide (~0.2k)
; "fpieee" is IEEE floating-point fetch & store (~0.4K)
; "fptrancendentals" is floating-point trancendental functions (~1K)
; "fphyperbolic" is floating-point hyperbolic functions
; "ed" is a string editor. (~1.5K)
; "editor" is a block editor. (~0.25K)
; "ramdrive" is for testing block words without a block device. (~0.2K)
; "block" is the optional BLOCK words. (~1.4K)
; "environment?" is the ENVIRONMENT? word.  While this is a core word
;     for ANS-2012, it uses a lot of strings and therefore takes up a lot
;     of memory. (~0.1K)
; "assembler" is an assembler & disassembler. (~2.5K)
; "wordlist" is for the optional SEARCH-ORDER words (eg. wordlists)
;     Note: Without "wordlist", you will not be able to use any words from
;     the EDITOR wordlists (they should probably be disabled
;     by also removing "editor" and "assembler"), and all new words will
;     be compiled into the FORTH wordlist. (~0.9K)


; Default line ending is line feed.
TALI_OPTION_CR_EOL :?= [ "lf" ]


; Standard throw codes: https://forth-standard.org/standard/exception
; Table 9.1: THROW code assignments
err_Abort		= -1
err_AbortQuote		= -2
err_Stack_Overflow	= -3
err_Stack_Underflow	= -4
err_Return_Stack_Overflow = -5
err_Return_Stack_Underflow = -6
err_DoLoop_TooDeep	= -7	; do-loops nested too deeply during execution
err_Dictionary_Overflow	= -8
err_InvalidMemoryAddr	= -9	; invalid memory address
err_DivideBy0		= -10	; division by zero
err_OutOfRange		= -11	; result out of range
err_ArgTypeMismatch	= -12	; argument type mismatch
err_UndefinedWord	= -13
err_CompileOnly		= -14	; interpreting a compile-only word
err_Forget		= -15	; invalid FORGET
err_EmptyName		= -16	; attempt to use zero-length string as a name
err_PicStringOverflow	= -17	; pictured numeric output string overflow
err_ParsedStringOverflow = -18	; parsed string overflow
err_NameTooLong		= -19	; definition name too long
err_WriteToRO		= -20	; write to a read-only location
err_Unsupported		= -21	; unsupported operation  (e.g., AT-XY on a too-dumb terminal)
err_ControlMismatch	= -22	; control structure mismatch
err_AddrAlignment	= -23	; address alignment exception
err_InvalidNumericArg	= -24	; invalid numeric argument
err_ReturnStackImbalance = -25	; return stack imbalance
err_LoopParmUnavailable = -26	; loop parameters unavailable
err_InvalidRecursion	= -27	; invalid recursion
err_UserInterrupt	= -28	; user interrupt
err_CompilerNesting	= -29	; compiler nesting
err_Obsolete		= -30	; obsolescent feature
err_NoBody		= -31	; >BODY used on non-CREATEd definition
err_InvalidName		= -32	; invalid name argument (e.g., TO name)
err_BlockRead		= -33	; block read exception
err_BlockWrite		= -34	; block write exception
err_InvalidBlock	= -35	; invalid block number
err_InvalidFilePosition = -36	; invalid file position
err_FileIO		= -37	; file I/O exception
err_FileMissing		= -38	; non-existent file
err_EndOfFile		= -39	; unexpected end of file
err_InvalidBase		= -40	; invalid BASE for floating point conversion
err_LossOfPrecision	= -41	; loss of precision
err_FPDivideBy0		= -42	; floating-point divide by zero
err_FPOutOfRange	= -43	; floating-point result out of range
err_FPStackOverflow	= -44	; floating-point stack overflow
err_FPStackUnderflow	= -45	; floating-point stack underflow
err_FPInvalidArg	= -46	; floating-point invalid argument
err_CompileWordsDeleted = -47	; compilation word list deleted
err_PostponeInvalid	= -48	; invalid POSTPONE
err_SearchOrderOverflow = -49	; search-order overflow
err_SearchOrderUnderflow = -50	; search-order underflow
err_CompileWordlistChanged = -51 ; compilation word list changed
err_ControlStackOverflow = -52	; control-flow stack overflow
err_ExceptionStackOverflow = -53 ; exception stack overflow
err_FPUnderflow		= -54	; floating-point underflow
err_FPFault		= -55	; floating-point unidentified fault
err_Quit		= -56	; QUIT
err_ConsoleIO		= -57	; exception in sending or receiving a character
err_BracketIf		= -58	; [IF], [ELSE], or [THEN] exception
err_Allocate		= -59	; ALLOCATE
err_Free		= -60	; FREE
err_Resize		= -61	; RESIZE
err_CloseFile		= -62	; CLOSE-FILE
err_CreateFile		= -63	; CREATE-FILE
err_DeleteFile		= -64	; DELETE-FILE
err_FilePosition	= -65	; FILE-POSITION
err_FileSize		= -66	; FILE-SIZE
err_FileStatus		= -67	; FILE-STATUS
err_FlushFile		= -68	; FLUSH-FILE
err_OpenFile		= -69	; OPEN-FILE
err_ReadFile		= -70	; READ-FILE
err_ReadLine		= -71	; READ-LINE
err_RenameFile		= -72	; RENAME-FILE
err_RepositionFile	= -73	; REPOSITION-FILE
err_ResizeFile		= -74	; RESIZE-FILE
err_WriteFile		= -75	; WRITE-FILE
err_WriteLine		= -76	; WRITE-LINE
err_MalformedXChar	= -77	; Malformed xchar
err_Substitute		= -78	; SUBSTITUTE
err_Replaces		= -79	; REPLACES

; Tali extra throw codes
err_Refill		= -149	; Refill failed
err_Defer		= -150	; Defer not set
err_AlreadyInterpreting	= -151	; entering interpret when already interpreting
err_AlreadyCompiling	= -152	; entering compile when already compiling
err_TooManyWordlists   	= -154



; These are the general
; definitions; platform-specific definitions such as the
; memory map are kept in the platform folder.


 .section zp
zp0: ; ZERO PAGE VARIABLES
; These are kept at the top of Zero Page, with the most important variables at
; the begin because the Data Stack grows towards this area from DStack0: If there is
; an overflow, the lower, less important variables will be clobbered first,
; giving the system a chance to recover. In other words, they are part of the
; floodplain.

; The four variables insrc, cib, ciblen, and toin must stay together in this
; sequence for the words INPUT>R and R>INPUT to work correctly.

;up:	.word ?		; User Pointer (Address of user variables)
cp:	.word ?		; Compiler Pointer (see HERE)
InSrc:	.word ?		; input Source for SOURCE-ID
Cib:	.word ?		; address of current input buffer
CibLen:	.word ?		; length of current input buffer
ToIn:	.word ?		; pointer to CIB (>IN in Forth)
state:	.word ?		; STATE: -1 compile, 0 interpret
status: .word ?		; internal status flags
			; (used by : :NONAME ; ACCEPT)
			;   Bit 7 = Redefined word message postpone
			;         When set before calling CREATE, it will
			;         not print the "redefined xxxx" message if
			;         the word exists. Instead, this bit will
			;         be reused and after CREATE has run, it will
			;         be set if the word was redefined and 0 if
			;         not. This bit should be 0 when not in use.
			; Bit 6 = 1 for normal ":" definitions
			;         WorkWord contains nt of word being compiled
			;       = 0 for :NONAME definitions
			;         WorkWord contains xt of word being compiled
			; Bit 5 = 1 for NUMBER returning a double word
			;       = 0 for NUMBER returning a single word
			; Bit 3 = 1 makes CTRL-n recall current history
			;       = 0 CTRL-n recalls previous history
			; Bit 2 = Current history buffer msb
			; Bit 1 = Current history buffer (0-7, wraps)
			; Bit 0 = Current history buffer lsb
			; status+1 is used by ACCEPT to hold history lengths.
WorkWord: .word ?	; nt or xt of word being compiled (see status)
dp:	.word ?		; Dictionary Pointer temp
DoIndex: .word ?	; current Do index
tmp1:	.word ?		; temporary storage
tmp2:	.word ?		; temporary storage
tmp3:	.word ?		; temporary storage
tmp4:	.word ?		; temporary storage (tmpdsp)
tmp5:	.word ?		; temporary storage (tmptos)
editor1: .word ?	; temporary for editors
editor2: .word ?	; temporary for editors
editor3: .word ?	; temporary for editors
DStack:	.fill DDim*2	; data stack
DStack0 = *-DStack	;   initial Data Stack index value
 .endsection zp

 .section code
Cold_zp_table: ; initial values for the variables in zero page.
	; This must match the layout of the 1st part of zero page.
;	.word user0		; up
	.word cp0		; cp
	.word 0			; insrc (SOURCE-ID is 0 for keyboard)
	.word buffer0		; cib
	.word 0			; ciblen
	.word 0			; toin
	.word 0			; state (0 = interpret)
	.word 0			; status
  ; No further ZP variables are initialized.
Cold_zp_table_length = *-Cold_zp_table
 .endsection code

 .section bss
User0: ; User Variables: --------------------------------------

PrecisionV: .byte ?	; # of decimal places for FP print
base:	.word ?		; number radix, default decimal

nc_limit: .word ?	; limit for Native Compile size
uf_strip: .word ?	; flag to strip underflow detection code

output:	.word ?		; vector for EMIT
input:	.word ?		; vector for KEY
HaveKey: .word ?	; vector for KEY?

; Block variables
BlkV:	.word ?		; BLK
ScrV:	.word ?		; SCR

; Wordlists
CurrentV: .byte ?	; CURRENT (byte) (Compilation wordlist)
Num_WordlistsV: .byte ?	; #WORDLISTS (byte)
max_wordlists = 12	; Maximum number of wordlists supported
			;   4 Tali built-ins + 8 user wordlists
WordlistsV: .fill 2*max_wordlists	; WORDLISTS (cells)
			;          (FORTH, EDITOR, ASSEMBLER, ROOT, +8 more)
Num_OrderV: .byte ?	; #ORDER (byte)
			;          (Number of wordlists in search order)
Search_OrderV: .fill 9	; SEARCH-ORDER (bytes)
			;   Allowing for 9 to keep offsets even.
MarkEnd:

ToHold:	.byte ?		; pad buffer index for formatted output

RndState: .dword ?	; random # state
 .endsection bss

; Wordlist index #s
wid_Forth = 0
wid_Editor = 1
wid_Assembler = 2
wid_Root = 3

 .section code
Cold_user_table: ; inital values for the user variables.
  ; This must match the layout of the 1st part of user variables.
	.byte 8			; PrecisionV
	.word 10		; base
	.word 8			; nc-limit
	.word 0			; uf_strip (off by default)
	.word kernel_putc	; output
	.word kernel_getc	; input
	.word kernel_havekey	; havekey
	.word 0				; BLK
	.word 0				; SCR
	.byte 0				; CURRENT = FORTH-WORDLIST
	.byte 4				; #WORDLISTS (FORTH EDITOR ASSEMBLER ROOT)
	.word forth_dictionary_start	; FORTH-WORDLIST
	.word editor_dictionary_start	; EDITOR-WORDLIST
	.word assembler_dictionary_start ; ASSEMBLER-WORDLIST
	.word root_dictionary_start	; ROOT-WORDLIST
	.word 0,0,0,0,0,0,0,0		; User wordlists
	.byte 2				; #ORDER
	.byte wid_Forth,wid_Assembler,0,0,0,0,0,0,0	; search-order
  ; No further user variables are initialized.
Cold_user_table_length = *-Cold_user_table
 .endsection code

; ASCII CHARACTERS
AscCC   = $03	; break (CTRL-c)
AscBELL = $07	; bell sound
AscBS   = $08	; backspace
AscHT	= $09	; tab
AscLF   = $0a	; line feed
AScVT	= $0b	; vertical tab
AscFF	= $0c	; form feed
AscCR   = $0d	; carriage return
AscCN   = $0e	; CTRL-n (used to recall next input history)
AscCP   = $10	; CTRL-p (used to recall previous input history)
AscESC  = $1b	; escape
AscSP   = $20	; space
AscDQuote = $22	; double quote
AscBackslash = $5c ; backslash
AscDEL  = $7f	; delete (CTRL-h)



; Insert point for Tali Forth after kernel hardware setup
;forth: ; we have a word called this


; Dictionary code for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; Updated by Sam Colwell
; First version: 05. Dec 2016 (Liara Forth)
; This version: 24 Apr 2024

; Dictionary headers are usually together with the code, which allows
; the xt pointer to be sometimes omitted.
; code length to be 1 byte.
; We roughly follow the Gforth terminology:
;   The Execution Token (xt) is the address of the first byte of a word's code.
;   The Name Token (nt) is a pointer to the word's header structure.

;   prev NT --> +---------------+
;               :               :
;               :               :
;
;                     ...
;
 .virtual 0 ; FORTH word header structure (what nt points to)
				; +---------------+
		.fill $80	; |  unused space |   to align the last char of name
				; |		  |   space before used name chars is never touched, can overlap previous code.
				; |  Name string  |   8-bit mixed case ascii, right justified
				; |		  |   position chosen so incrementing beyond it flagged as negative
				; |		  |   last char of name
wh_NameLastChar	= *-1		; |		  |   Note string is not zero-terminated
				; +---------------+   Arranged so next index is negative ($80)
wh_HashNameLen:			    .byte ?	; |
wh_HNL_HashMask =		    %11100000	; |      lo 3 bits of last char of name
wh_HNL_NameLengthMask =		    %00011111	; |      length of name
				; +---------------+
wh_Flags:			    .byte ?	; |  flag bits
FP	=			    %00000001	; |	Far previous NT (two byte pointer rather than one byte offset)
DB	=			    %00000010	; |	Disjoint body (two byte pointer rather than adjoining body code)
CO	=			    %00000100	; |	Compile Only
IM	=			    %00001000	; |     Immediate Word
						; |     Never/always native flags using this table:
				;		  |	     NN  AN
				;		  |	    +---+---+
				;		  |	    | 0 | 0 |  -- : Normal word called by JSR (non-native) or inlined (native)
NN	=			    %00010000	; |	    | 1 | 0 |  NN : Word can only be called by JSR (never native)
AN	=			    %00100000  	; |	    | 0 | 1 |  AN : Word can only be inlined (always native)
ST	=			    %00110000	; |	    | 1 | 1 |  ST : Normal word with return stack juggling that
				;		  |	    +---+---+       must be removed when inlining (R>, R@, >R etc)
				;		  |
				;		  |	The total header length is between 4 and 7 bytes plus the the word's name.     
				;		  |	Flag bits 0 and 1, FP and DB, can be used to calculate the variable part
wh_HeaderExtendMask =		     FP+DB	; |	of the header length directly by adding 4 to `flags & %00000011`.
				;		  |	For example a header with both FP and DB equal to zero has length 4,
				;		  |	whereas a header with FP=0 and DB=1 has length 6.
UF	=			    %01000000   ; |     strippable underflow
				; +---------------+
wh_CodeLength:			   .byte ?	; |  Code length  |   Code length for native compile, max 255
				; |		  |   Longer code flagged as NN with size=255
				; +---------------+
wh_LinkNt:			; | 1 or 2 bytes  |   Previous NT  |   FP=0: prev NT = NT start - one byte offset
				; |		  |   FP=1: prev NT = two byte pointer (0=end of list)
				; +---------------+
				; | 0 or 2 bytes  |   Start of code |   DB=0: XT start = NT end
				; |		  |   DB=1: XT start = two byte pointer
				; +---------------+
				;          ...
; XT start			; +---------------+
				; |  Word body	  |   When BD=0 the code body follows the header
				; : (65c02 code)  :
				; :		  :   Words that want CFA & DFA (eg CREATEd words) have a JSR to the handler code,
				; :		  :   followed by the DFA area.  The JSR return address (+1) is the pointer to the DFA.
; XT end			; +---------------+

 .endvirtual ; end of word header struct

WordFlags .var 0 ; initialize for macro
XtPtr1  .var 0  ; initialize for macro

WordHeader .macro name,flags=NN,XtPtr=0,XtLen=3 ; compile a FORTH Word Header
;	WordHeader_Extend	; user-supplied extension
L1:	.text \name	;  name of word as a string, ending at wh_NameLastChar
	.cerror (*-L1)>wh_HNL_NameLengthMask ; name too long
Nt0 = *-(wh_NameLastChar+1)	; remember our nt
	.cerror wh_HNL_HashMask!=$e0 ; hard coded
	.byte ((\name[-1]&7)<<5)+(*-L1)	;wh_HashNameLen

WordFlags ::= \flags	; modifyable copy, remember for later
LinkDisplacement = Nt0-WordListLink
	.if (LinkDisplacement & $ff00) != 0
	  WordFlags ::= WordFlags | FP 
	 .endif
	.if \XtPtr!=0 ; supplied pointer
	  WordFlags ::= WordFlags | DB
	 .endif
	.byte WordFlags	;wh_Flags
	.byte \xtlen	;wh_CodeLength
	.if (WordFlags & FP)!=0 ;wh_LinkNt	link to previous word in dictionary chain (0=end)
	  .word WordListLink
	 .else
	  .byte LinkDisplacement
	 .endif
	.if (WordFlags & DB)!=0
	  .word \XtPtr
XtPtr1 ::= 0
	 .else
XtPtr1 ::= *
	 .endif
WordListLink ::= Nt0 ; remember the nt of this word for later
	.endmacro

WordEnd .macro  ; Mark end of a word's appended code
	.cerror (WordFlags & DB )!=0 ; does not work on words with disjoint body
CL	.var *-XtPtr1
	.if CL>$ff
	  .cerror (WordFlags & NN)==0 ; "code size >255 & not marked NN"
CL	 .var $ff
	 .endif
en = *	; remember here
  * = WordListLink+wh_CodeLength
	.byte CL	;wh_CodeLength
  * = en  ; back to here
	.endmacro


DStackCheck .macro depth,ErrorDestination ; compile a data stack underflow check
	cpx #-2*\depth+DStack0+1
	bcs \ErrorDestination
	.endmacro


; Many existing words can be natively compiled (compiled inline) or
; as a subroutine jump target; the system decides which variant to use based on
; a threshold the user can set. By default, all user-created words are flagged
; never-native. The user can override this by using the always-native word
; just after defining their new word.  The NN flag forbids native compiling,
; the AN flag forces it.

; The words are sorted roughly with the more common ones closer to the head of
; the dictionary (further down in code) so they are found earlier.
; This only affects dictionary search speed, not execution speed.

; Each word a two special
; status line that begins with "  ; ## ", which allows auto-generation of the
; WORDLIST.md file and other entries in the docs folder. Status entries are:
;
;	TBA --> fragment --> coded --> tested --> auto
;
; "Auto" means that the word is automatically tested by the test suite (good),
;   "tested" means that it was tested by hand in some way (okay), 
;   "coded" mean it hasn't been tested at all (bad).
; See the test suite for more details.


 .section code

; FORTH-WORDLIST
WordListLink .var 0	 ; start of FORTH wordlist

 WordHeader "Cold",NN ; ( -- ) "Reset the Forth system"
  ; Reset the Forth system. Does not restart the kernel,
  ; use the 6502 reset for that.
Cold:
		cld

		ldx #rsp0		; Initialize 6502 stack (Return Stack)
		txs

		ldx #Cold_zp_table_length-1 ; Initialize important zero page variables from ROM
-		lda Cold_zp_table,x	;   We can use X here
		sta zp0,x		;   because Tali hasn't set up the param stack yet.
		dex
		bpl -

		ldy #Cold_user_table_length-1	; Initialize the user variables.
-		lda Cold_user_table,y
		sta User0,y
		dey
		bpl -

		jsr Empty_Stack		; Clear Data Stack. This is repeated in ABORT, but this way we
					; can load high-level words with EVALUATE

  .if "block" in TALI_OPTIONAL_WORDS ;-------------------------------------------
		jsr BlockInit
  .endif

		jsr CR

  .if forth_words_len>0
		; Run the text in forth_words.asm via EVALUATE.

		lda #<forth_words_start		; push start address
		ldy #>forth_words_start
		jsr PushYA

		lda #<forth_words_len		; push length
		ldy #>forth_words_len
		jsr PushYA

		jsr Evaluate
  .endif

  .if user_words_len>0
		; Run the text in user words via EVALUATE.

		lda #<user_words_start		; push start address
		ldy #>user_words_start
		jsr PushYA

		lda #<user_words_len		; push length
		ldy #>user_words_len
		jsr PushYA

		jsr Evaluate

   .endif

		; Initialize all of the history buffers by putting a zero in
		; each length byte.
		lda #0
		sta hist_buff
		sta hist_buff+$80
		sta hist_buff+$100
		sta hist_buff+$180
		sta hist_buff+$200
		sta hist_buff+$280
		sta hist_buff+$300
		sta hist_buff+$380

		jmp Abort_Core2	;Quit
	WordEnd


forth_words_start: .binary "forth_words.asc" ; High-level Forth words, see forth_code/README.md
forth_words_len = *-forth_words_start

user_words_start: .binary "user_words.asc" ; User-defined Forth words, see forth_code/README.md
user_words_len = *-user_words_start


 WordHeader "NoOp",0 ; ( -- ) sample word (different name than the assembler one)
		nop
  WordEnd
		rts


 WordHeader "CC@",NN,platform_CCAT,3 ; ( -- ud ) fetch cpu cycle counter


 WordHeader "Bye",NN,platform_bye,3 ; ( -- )  Exit FORTH
  ; ## "bye"  tested  ANS tools ext
  ; https://forth-standard.org/standard/tools/BYE


 WordHeader "TypeSymbol",NN ; ( addr -- )  print addr as symbol
TypeSymbol:	jsr Two
		jsr Spaces
		lda #'('
		jsr Emit_A
		jsr Space
		jsr Dup
		lda #'$'
		jsr Emit_A
		jsr Dot_Hex
		jsr Space
		jsr DictSearchXt	; ( addr_end addr operand offset nt )
		jsr Name_To_String	; Change nt into the name
		jsr Type		; print it.
		jsr Space
		lda DStack+0,x		; if non-zero offset
		ora DStack+1,x
		beq +
		lda #'+'		;   print offset
		jsr Emit_A
		jsr Dup
		jsr U_Dot
+		inx
		inx	
		lda #')'
		jsr Emit_A
		jmp Space
	WordEnd

; WordHeader "DictSearchXt",NN ; ( xt -- offset nt ) search wordlists for word with best match
DictSearchXt:
		dex
		dex
		jsr Over
		lda #$ff		; init best offset
		sta DStack+5,x		; ( offset nt xt )

		lda #$100-2		; for each wordlist
		pha
_ListNext:	pla
		clc
		adc #2			; to next wordlist
		cmp #max_wordlists*2
		bcs _ListDone
		pha
		tay
		lda WordlistsV+0,y	; Get the DP for that wordlist.
		sta tmp1+0
		lda WordlistsV+1,y
		sta tmp1+1
		bne _WordTest
		beq _ListNext		;   empty list

_ListDone:	inx			; Drop pattern xt
		inx			; ( offset nt )
		rts

_WordNext:	jsr LinkNext		; tmp1= next nt in list
		beq _ListNext		; end of Dictionary?

_WordTest:	jsr NameToIntTmp	; tmp2 = xt of this word

		sec			; tmp3 = xt offset for this word
		lda DStack+0,x
		sbc tmp2+0
		sta tmp3+0
		lda DStack+1,x
		sbc tmp2+1
		sta tmp3+1

		jsr _test

		ldy #0			; is this word a constant?
		lda (tmp2),y
		cmp #$a0
		beq _PushYA
		cmp #$a9
		beq _PushZA
		bne _WordNext

_PushZA:	iny			; get lda # operand
		lda DStack+0,x
		sbc (tmp2),y
		sta tmp3+0
		lda DStack+1,x
		sbc #0
		sta tmp3+1
		iny			; check jmp opcode
		lda (tmp2),y
		cmp #$4c
		bne _WordNext
		iny			; check jmp operand lo
		lda (tmp2),y
		cmp #<PushZA
		bne _WordNext
		iny			; check jmp operand hi
		lda (tmp2),y
		cmp #>PushZA
		beq _KTest
		bne _WordNext

_PushYA:	ldy #2			; check LDA # opcode
		lda (tmp2),y
		cmp #$a9
		bne _WordNext
		iny			; get lda # operand
		lda DStack+0,x
		sbc (tmp2),y
		sta tmp3+0
		ldy #1			; get LDY # operand
		lda DStack+1,x
		sbc (tmp2),y
		sta tmp3+1
		ldy #4			; check JMP opcode
		lda (tmp2),y
		cmp #$4c
		bne _WordNext
		iny			; check JMP operand lo
		lda (tmp2),y
		cmp #<PushYA
		bne _WordNext3
		iny			; check JMP operand hi
		lda (tmp2),y
		cmp #>PushYA
		bne _WordNext3

_KTest:		jsr _test

_WordNext3:	jmp _WordNext


_test: ; is tmp3 better?

		lda tmp3+0		; this word have smaller offset?
		cmp DStack+4,x
		lda tmp3+1
		sbc DStack+5,x
		bcs _trts

		lda tmp3+0		;   save this better offset
		ldy tmp3+1
		sta DStack+4,x
		sty DStack+5,x

		lda tmp1+0		;   save this better nt
		ldy tmp1+1
		sta DStack+2,x
		sty DStack+3,x

_trts:		rts

;	WordEnd


LinkNext: ; step to next nt in wordlist
	; in: tmp1= nt
	; out: tmp1= next nt
	; 	P.Z=end of list
		ldy #wh_Flags
		lda (tmp1),y
		and #FP
		beq _LinkShort

_LinkLong:	ldy #wh_LinkNt+1	; tmp1 = tmp1->LinkNt (ptr)
		lda (tmp1),y
		pha
		dey
		lda (tmp1),y
		sta tmp1+0
		pla
		sta tmp1+1
		rts

_LinkShort:	ldy #wh_LinkNt		; tmp1 -= tmp1->LinkNt byte offset
		sec
		lda tmp1+0
		sbc (tmp1),y
		sta tmp1+0
		lda tmp1+1
		sbc #0
		sta tmp1+1
		rts


 WordHeader "DStack",NN ; ( -- addr )  ptr to DStack
		lda #DStack
		jmp PushZA
	WordEnd

 WordHeader "RStack",NN ; ( -- addr )  ptr to RStack
		ldy #>RStack
		lda #<RStack
		jmp PushYA
	WordEnd


; Random #s ------------------------------------------------------

 WordHeader "RndState",NN ; ( -- addr )  Double variable: random # state
		ldy #>RndState
		lda #<RndState
		jmp PushYA
	WordEnd

 WordHeader "Rand",0 ; ( -- YA )  generate next random #
  ; 32bit Galois LFSR  https://en.wikipedia.org/wiki/Linear-feedback_shift_register#Galois_LFSRs
Rand:
		lsr RndState+1
		ror RndState+0
		ror RndState+3
		ror RndState+2
		bcc _19			;  IfCs,
		lda RndState+1
		eor #$d0
		sta RndState+1
		lda RndState+2
		eor #$01
		sta RndState+2
_19:					;   Then,
		lda RndState+2
		ldy RndState+3
	WordEnd
		rts

 WordHeader "RandM",NN ; ( umod - u )  random integer between 0 & umod-1
RandM:		jsr Dup		; ( umod umod )
		jsr Rand
		lsr a		; make positive
		sta DStack+3,x
		sty DStack+2,x	; ( rand umod )
		jmp Mod
	WordEnd

 .endsection code

.if "fp" in TALI_OPTIONAL_WORDS ;-----------------------------------------------------
;======================================================================
; 12. The optional Floating-Point word set

; Not fully IEEE compliant.
; Has SF@ & SF! & DF@ & DF! to interoperate with IEEE formats
; Trying to be small, fast, simple
; NANs not supported
; Denormals not supported
; Limited exception handling
; Rounding isn't IEEE, but we include some extra mantissa bits

; FP stack entries & internal format storage have:
;   4 bytes of 2s complement binary mantissa
;   1 byte  of 2s complement binary exponent

 .section zp
FIndex:	.byte ?		; floating-point stack index. empty=FDim, full=0

; FP stack has FDim entries, defined in platform.
; FP stack is a set of byte arrays, each with FDim # of entries indexed by FIndex:
FSExp:   .fill FDim	; FP stack exponent        array
FSMant0: .fill FDim	; FP stack mantissa MSByte array
FSMant1: .fill FDim	; FP stack mantissa 2nd    array
FSMant2: .fill FDim	; FP stack mantissa 3rd    array
FSMant3: .fill FDim	; FP stack mantissa 4th    array
  ; Will FP stack fit in ZP?  It could be placed in bss if needed.
  ; How to move this decision out to platform???

 .endsection zp

 .section code

;--- labels for writing words that play with FP internals------------------------

 WordHeader "FIndex",NN ; ( -- addr )  Byte Variable: FP stack index
		lda #FIndex
		jmp PushZA
	WordEnd

 WordHeader "FDim",NN ; ( -- n )  Constant: # of FP stack entries
		lda #FDim
		jmp PushZA
	WordEnd

 WordHeader "FSExp",NN ; ( -- addr )  Byte variable: FP exponent byte array
		lda #FSExp
		jmp PushZA
	WordEnd

 WordHeader "FSMant0",NN ; ( -- addr )  Byte variable: FP mantissa MSByte array
		lda #FSMant0
		jmp PushZA
	WordEnd

 WordHeader "FSMant1",NN ; ( -- addr )  Byte variable: FP mantissa 2nd byte array
		lda #FSMant1
		jmp PushZA
	WordEnd

 WordHeader "FSMant2",NN ; ( -- addr )  Byte variable: FP mantissa 3rd byte array
		lda #FSMant2
		jmp PushZA
	WordEnd

 WordHeader "FSMant3",NN ; ( -- addr )  Byte variable: FP mantissa 4th byte array
		lda #FSMant3
		jmp PushZA
	WordEnd


; WordHeader "Float",NN ; ( -- u )  size of a float  gforth
;		lda #5
;		jmp PushZA
;	WordEnd

 WordHeader "Float+",NN ; ( r-addr1 -- r-addr2 )  Add the size in address units of a floating-point number.
  ; https://forth-standard.org/standard/float/FLOATPlus
FloatPlus:	lda #5
		jmp Plus_A
	WordEnd

 WordHeader "Floats",NN ; ( n1 -- n2 )  return size of n1 floats
  ; https://forth-standard.org/standard/float/FLOATS
Floats:		lda #5
		jsr PushZA
		jmp Star
	WordEnd


 WordHeader "FAlign",0 ; ( -- )  reserve enough space to align the compiler pointer
  ; If the data-space pointer is not float aligned, reserve enough data space to make it so. 
  ; https://forth-standard.org/standard/float/FALIGN
FAlign: 		; 6502 doesn't need any alignment, do nothing.
	WordEnd
		rts

 WordHeader "FAligned",0 ; ( addr -- r-addr )  FP align the address
  ; https://forth-standard.org/standard/float/FALIGNED
  ; r-addr is the first float-aligned address greater than or equal to addr. 
FAligned:		; 6502 doesn't need any alignment, do nothing
	WordEnd
		rts


 WordHeader "Hex>F",NN ; ( d_mant n_exp -- ) ( F: -- r )  create r from parts
HexToF:		jsr FAllocX		; alloc FP stack entry, X= fp stack index
		ldx tmp1+0		; restore data stack index
		ldy FIndex		; Y= FP stack index
		jsr PopA		; pop n_exp
		sta FSExp,y
		lda DStack+1,x
		sta FSMant0,y
		lda DStack+0,x
		sta FSMant1,y
		lda DStack+3,x
		sta FSMant2,y
		lda DStack+2,x
		sta FSMant3,y
		jmp Two_Drop
	WordEnd

 WordHeader "F>Hex",NN ; ( F: r -- ) ( -- d_mantissa n_exponent )  get parts of r
FToHex:		ldy FIndex	; Y= FP stack index
		dex
		dex
		dex
		dex
		dex
		dex
		lda FSMant0,y	; copy mantissa
		sta DStack+3,x
		lda FSMant1,y
		sta DStack+2,x
		lda FSMant2,y
		sta DStack+5,x
		lda FSMant3,y
		sta DStack+4,x
		lda FSExp,y	; copy exponent
		sta DStack+0,x
		and #$80	;   sign extend
		beq +
		lda #$ff
+		sta DStack+1,x
		inc FIndex	; FDrop
		rts
	WordEnd

 WordHeader "F.Hex",NN ; ( F: r -- )	display r in hex
FDotHex:	jsr FToHex	; get parts of r
		jsr Not_Rot
		jsr Dot_Hex	; do mantissa
		jsr Dot_Hex
		lda #':'
		jsr Emit_A
		jmp C_Dot_Hex	; do exponent
	WordEnd


; WordHeader "FCmp",NN ; ( F: r1 r2 -- r1 r2 ) ( -- n )  wrapper for FCmpA
;		jsr FCmpA		; compare #s
;		tay			; return >0, 0, <0
;		jmp PushYA
;	WordEnd

 WordHeader "FCmpA",NN ; ( F: r1 r2 -- r1 r2 CPU_regs ) \ floating point compare
  ; assumes normalized values
  ; returns: flags N & Z, A=0 for NOS=TOS, A<0 for NOS<TOS, A>0 for NOS>TOS
FCmpA:		stx tmp1		; save data stack index
		ldx FIndex		; X= FP stack index

		lda FSMant0+0,x		; r2 mantissa = 0 ?
		beq _r2Zero
		ldy FSMant0+1,x		; r1 mantissa = 0 ?
		beq _r1Zero
		eor FSMant0+1,x		; compare mantissa sign
		bmi _MantissaSignDifferent

		sec			; compare exponent
		lda FSExp+1,x
		sbc FSExp+0,x
		bne _ExponentDifferent

		tya			; compare mantissa MSB
		sbc FSMant0+0,x		;   always same sign so can't overflow
		bne _13
		lda FSMant1+1,x		; compare mantissa 1
		sbc FSMant1+0,x
		bne _12
		lda FSMant2+1,x		; compare mantissa 2
		sbc FSMant2+0,x
		bne _12
		lda FSMant3+1,x		; compare mantissa LSB
		sbc FSMant3+0,x
		beq _13
_12:		ror a
_14:		eor #$80
		ora #1
_13:		ldx tmp1		; restore data stack index
		tay			; set CPU flags
		rts

_r1Zero:	lda FSMant0+0,x		; return r2
		bne _14
		beq _13

_r2Zero:	lda FSMant0+1,x		; return 0-r1
		jmp _13

_ExponentDifferent:
		bvc +
		eor #$80
+
		eor FSMant0+0,x
		ldx tmp1		; restore data stack index
		ora #1			; set CPU flags
		rts

_MantissaSignDifferent:
		tya
		ldx tmp1		; restore data stack index
		ora #1
		rts
	WordEnd


 WordHeader "FAllocX",NN ; ( F: -- r )  alloc a FP stack entry, returns X=fp
FAllocX:	stx tmp1+0	; save data stack index
		ldx FIndex	; X= floating point stack index
		dex		; alloc FP stack entry
		cpx #FDim	; overflow or underflow?
		bcs _err
		stx FIndex
		rts		; return X= FP stack index

_err:		php		; save sign bit
		ldx tmp1+0	; restore data stack index
		plp		; restore sign bit
		jmp Throw_FPStack
	WordEnd


 WordHeader "FMax",NN ; ( F: r1 r2 -- r3 )  r3 is the greater of r1 and r2. 
  ; https://forth-standard.org/standard/float/FMAX
FMax:		jsr FCmpA
		bpl FDrop
		bmi FNip
	WordEnd

 WordHeader "FMin",NN ; ( F: r1 r2 -- r3 )  r3 is the lesser of r1 and r2.
  ; https://forth-standard.org/standard/float/FMIN
FMin:		jsr FCmpA
		bmi FDrop
		bpl FNip
	WordEnd


 WordHeader "FDepth",NN ; ( -- n )  # of entries on the FP stack
  ; https://forth-standard.org/standard/float/FDEPTH
FDepth:		lda #FDim
		sec
		sbc FIndex
		jmp PushZA
	WordEnd

 WordHeader "FDrop",0 ; ( F: r -- )  Drop 1 FP stack entry
  ; https://forth-standard.org/standard/float/FDROP
FDrop:		inc FIndex
	WordEnd
		rts

 WordHeader "F2Drop",0 ; ( F: r1 r2 -- )  Drop 2 FP stack entries
F2Drop:		inc FIndex
		inc FIndex
	WordEnd
		rts

 WordHeader "FNip",0 ; ( F: r1 r2 -- r2 )   Drop NOS on FP stack
  ; like https://forth-standard.org/standard/core/NIP
FNip:		stx tmp1		; save data stack index
		ldx FIndex		; X= FP stack index
		lda FSExp+0,x		; copy exponent
		sta FSExp+1,x
		lda FSMant0+0,x		; copy mantissa
		sta FSMant0+1,x
		lda FSMant1+0,x
		sta FSMant1+1,x
		lda FSMant2+0,x
		sta FSMant2+1,x
		lda FSMant3+0,x
		sta FSMant3+1,x
		inc FIndex		; FDrop
		ldx tmp1		; restore data stack index
	WordEnd
		rts

 WordHeader "FDup",0 ; ( F: r -- r r )  Push a copy of FP TOS
  ; https://forth-standard.org/standard/float/FDUP
FDup:		jsr FAllocX		; alloc FP stack entry, X=fp stack index
		lda FSExp+1,x		; copy exponent
		sta FSExp+0,x
		lda FSMant0+1,x		; copy mantissa
		sta FSMant0+0,x
		lda FSMant1+1,x
		sta FSMant1+0,x
		lda FSMant2+1,x
		sta FSMant2+0,x
		lda FSMant3+1,x
		sta FSMant3+0,x
		ldx tmp1+0		; restore data stack index
	WordEnd
		rts

; WordHeader "FDup0<>",NN ; ( F: r -- r ) ( -- flag )
;FDup0Eq:	ldy FIndex
;		lda FSMant0,y		; =0 ?
;		bne FDup
;	WordEnd
;		rts

; WordHeader "FDup0<",NN ; ( F: r -- r ) ( -- flag )
;FDup0Lt:	ldy FIndex
;		lda FSMant0,y		; <0 ?
;		bmi FDup
;	WordEnd
;		rts

 WordHeader "FOver",0 ; ( F: r1 r2 -- r1 r2 r1 )  Push a copy of FP NOS
  ; https://forth-standard.org/standard/float/FOVER
FOver:		jsr FAllocX		; alloc FP stack entry, X=fp stack index
		lda FSExp+2,x		; copy exponent
		sta FSExp+0,x
		lda FSMant0+2,x		; copy mantissa
		sta FSMant0+0,x
		lda FSMant1+2,x
		sta FSMant1+0,x
		lda FSMant2+2,x
		sta FSMant2+0,x
		lda FSMant3+2,x
		sta FSMant3+0,x
		ldx tmp1+0		; restore data stack index
	WordEnd
		rts

 WordHeader "FPick",0 ; ( r(u) ... r(1) r(0) u -- r(u) ... r(1) r(0) r(u) )
  ; Push ith data stack number.  1 FPICK is equivalent to FOVER.
  ; like https://forth-standard.org/standard/core/PICK
FPick:		jsr PopA		; pop u (desired entry #)
FPickA:		clc			; Y= fp stack index of [u]
		adc FIndex
FPick3:		tay

		jsr FAllocX		; alloc FP stack entry, X=fp stack index
		lda FSExp,y		; copy exponent
		sta FSExp,x
		lda FSMant0,y		; copy mantissa
		sta FSMant0,x
		lda FSMant1,y
		sta FSMant1,x
		lda FSMant2,y
		sta FSMant2,x
		lda FSMant3,y
		sta FSMant3,x
		ldx tmp1+0		; restore data stack index
	WordEnd
		rts

 WordHeader "F2Dup",NN ; ( F: r1 r2 -- r1 r2 r1 r2 )  push copy of top 2 on FP stack
F2Dup:		jsr FOver
		jmp FOver
	WordEnd

; WordHeader "F2Dup<",NN ; ( F: r1 r2 -- r1 r2 ) ( -- )
;F2DupLt:	jsr FCmpA
;		bmi F2Dup
;	WordEnd
;		rts

 WordHeader "FSwap",0 ; ( F: r1 r2 -- r2 r1 )  Swap FP TOS & NOS
  ; https://forth-standard.org/standard/float/FSWAP
FSwap:		stx tmp1+0		; save data stack index
		ldx FIndex		; X=FP stack index

		lda FSExp+0,x		; do FSExp
		ldy FSExp+1,x
		sta FSExp+1,x
		sty FSExp+0,x

		lda FSMant0+0,x		; do FSMant0
		ldy FSMant0+1,x
		sta FSMant0+1,x
		sty FSMant0+0,x

		lda FSMant1+0,x		; do FSMant1
		ldy FSMant1+1,x
		sta FSMant1+1,x
		sty FSMant1+0,x

		lda FSMant2+0,x		; do FSMant2
		ldy FSMant2+1,x
		sta FSMant2+1,x
		sty FSMant2+0,x

		lda FSMant3+0,x		; do FSMant3
		ldy FSMant3+1,x
		sta FSMant3+1,x
		sty FSMant3+0,x

		ldx tmp1+0		; restore data stack index
	WordEnd
		rts

; WordHeader "FNSwap",NN ; ( F: rN+1  rN  rN-1... r0 --  rN+1  r0  rN-1 ... r1 rN ) ( u -- )
  ; Exchange the fp value at the N-th location on the fp stack with the value at the top of the
  ; fp stack.  1 FNSwap is equivalent to FSwap
;FNSwap:	jsr PopA		; pop u
;FNSwapA:	???

 WordHeader "FTuck",NN ; ( F: r1 r2 -- r2 r1 r2 )
  ; like https://forth-standard.org/standard/core/TUCK
FTuck:		jsr FSwap
		jmp FOver
	WordEnd

 WordHeader "FRot",0 ; ( F: r1 r2 r3 -- r2 r3 r1 ) \ Rotate the top three FP stack entries.
  ; https://forth-standard.org/standard/float/FROT
FRot:		stx tmp1		; save data stack index

	.cerror FSMant0-FSExp!=FDim	; make sure our assumptions hold
	.cerror FSMant1-FSMant0!=FDim
	.cerror FSMant2-FSMant1!=FDim
	.cerror FSMant3-FSMant2!=FDim

		lda FIndex		; for FSMant3, FSMant2, FSMant1, FSMant0, FSExp
		clc
		adc #4*FDim
		bne _3

_2:		txa			;    next byte
	;	sec
		sbc #FDim
_3:		tax

		lda FSExp+2,x		;   do a byte
		ldy FSExp+1,x
		sty FSExp+2,x
		ldy FSExp+0,x
		sty FSExp+1,x
		sta FSExp+0,x

		cpx FIndex		; done?
		bne _2

;--- or ---
;		stx tmp1		; save data stack index
;		ldx FIndex		; load FP stack index
;
;		lda FSExp+2,x		; do FSExp
;		ldy FSExp+1,x
;		sty FSExp+2,x
;		ldy FSExp+0,x
;		sty FSExp+1,x
;		sta FSExp+0,x
;
;		lda FSMant0+2,x		; do FSMant0
;		ldy FSMant0+1,x
;		sty FSMant0+2,x
;		ldy FSMant0+0,x
;		sty FSMant0+1,x
;		sta FSMant0+0,x
;
;		lda FSMant1+2,x		; do FSMant1
;		ldy FSMant1+1,x
;		sty FSMant1+2,x
;		ldy FSMant1+0,x
;		sty FSMant1+1,x
;		sta FSMant1+0,x
;
;		lda FSMant2+2,x		; do FSMant2
;		ldy FSMant2+1,x
;		sty FSMant2+2,x
;		ldy FSMant2+0,x
;		sty FSMant2+1,x
;		sta FSMant2+0,x
;
;		lda FSMant3+2,x		; do FSMant3
;		ldy FSMant3+1,x
;		sty FSMant3+2,x
;		ldy FSMant3+0,x
;		sty FSMant3+1,x
;		sta FSMant3+0,x
;---
		ldx tmp1		; restore param stack index
	WordEnd
		rts

 WordHeader "F-Rot",NN ; ( F: r1 r2 r3 -- r3 r1 r2 )
FMRot:		jsr FRot
		jmp FRot
	WordEnd


 .if "fpaux" in TALI_OPTIONAL_WORDS ;------------------------------------------------------
; the FP stack can also be used as an auxilliary data stack.
; Note that this data on the FP stack is not a valid FP number.
; http://forum.6502.org/viewtopic.php?f=9&t=493&hilit=aux

 WordHeader ">A",0 ; ( n -- ) ( F: -- n )  Move data stack entry to 1 FP stack entry
ToA:		jsr DupToA	; Copy
		inx		; Drop
		inx
	WordEnd
		rts

 WordHeader "Dup>A",0 ; ( n -- n ) ( F: -- n )  Copy data stack entry to 1 FP stack entry
DupToA:		dec FIndex	; alloc fp stack entry
		ldy FIndex	; Y= FP stack index
		lda DStack+0,x	; copy data
		sta FSMant1,y
		lda DStack+1,x
		sta FSMant0,y
	WordEnd
		rts

 WordHeader "2>A",NN ; ( d -- ) ( F: -- d )  Move double data stack entry to 1 FP stack entry
DToA:		jsr DDupToA
		jmp Two_Drop	; 2Drop
	WordEnd

 WordHeader "2Dup>A",0 ; ( d -- d ) ( F: -- d )  Copy double data stack entry to 1 FP stack entry
DDupToA:	dec FIndex	; alloc fp stack entry
		ldy FIndex	; Y= FP stack index
		lda DStack+0,x	; copy data
		sta FSMant1,y
		lda DStack+1,x
		sta FSMant0,y
		lda DStack+2,x
		sta FSMant3,y
		lda DStack+3,x
		sta FSMant2,y
	WordEnd
		rts

 WordHeader "A>",0 ; ( -- n ) ( F: n -- )  move aux data entry on FP to data stack
AFrom:		ldy FIndex	; Y= FP stack index
		inc FIndex	; FDrop (doesn't affect Y)
AFrom3:		dex		; alloc data stack space
		dex
		lda FSMant1,y	; copy data
		sta DStack+0,x
		lda FSMant0,y
		sta DStack+1,x
	WordEnd
		rts

 WordHeader "A@",NN ; ( -- n ) ( F: n -- n )  copy aux data entry on FP stack to data stack
AFetch:		ldy FIndex	; Y= FP stack index
		jmp AFrom3
	WordEnd

 WordHeader "2A>",0 ; ( -- d ) ( F: d -- )  move double aux data entry on FP to data stack
DAFrom:		ldy FIndex	; Y= FP stack index
		inc FIndex	; FDrop (doesn't affect Y)
DAFrom3:	dex		; alloc data stack space
		dex
		dex
		dex
		lda FSMant1,y	; copy data
		sta DStack+0,x
		lda FSMant0,y
		sta DStack+1,x
		lda FSMant3,y
		sta DStack+2,x
		lda FSMant2,y
		sta DStack+3,x
	WordEnd
		rts

 WordHeader "DA@",NN ; ( -- d ) ( F: d -- d )  copy double aux data entry on FP stack to data stack
DAFetch:	ldy FIndex	; Y= FP stack index
		jmp DAFrom3
	WordEnd

 .endif ; fpaux

 .if 0 ; these don't seem very useful

 WordHeader "F>R",ST ; ( F: r -- ) ( r: -- r )
FToR:		pla		; save RTS addr
		sta tmp1+0
		pla
		sta tmp1+1

		ldy FIndex
		lda FSExp,y
		pha
		lda FSMant0,y
		pha
		lda FSMant1,y
		pha
		lda FSMant2,y
		pha
		lda FSMant3,y
		pha
		inc FIndex	; FDrop

		lda tmp1+1	; restore RTS addr
		pha
		lda tmp1+0
		pha
	WordEnd
		rts

 WordHeader "FR>",ST ; ( r: r -- ) ( F: -- r )
FRTo:		pla		; save rts addr
		sta tmp2+0
		pla
		sta tmp2+1

		jsr FAllocX	; alloc FP stack entry & set X
		pla
		sta FSMant3,x
		pla
		sta FSMant2,x
		pla
		sta FSMant1,x
		pla
		sta FSMant0,x
		pla
		sta FSExp,x
		ldx tmp1	; restore data stack index

		lda tmp2+1	; restore RTS addr
		pha
		lda tmp2+0
		pha
	WordEnd
		rts

 .endif ; 0


 WordHeader "F@",0 ; ( addr -- ) ( F: -- r )  Fetch a FP number, internal format
  ; https://forth-standard.org/standard/float/FFetch
  ; Also see: SF@ DF@ for IEEE formats
FAt:		jsr PopYA		; pop addr
FAt_YA:		sta tmp2+0		; save addr
		sty tmp2+1

		ldy #0			; starting offset from tmp2
FAt_Tmp2Y:	jsr FAllocX		; alloc FP stack entry, X= fp stack index
		lda (tmp2),y		; copy mantissa
		sta FSMant3,x
		iny
		lda (tmp2),y
		sta FSMant2,x
		iny
		lda (tmp2),y
		sta FSMant1,x
		iny
		lda (tmp2),y
		sta FSMant0,x
		iny			; copy exponent
		lda (tmp2),y
		sta FSExp,x
		ldx tmp1		; restore data stack index
	WordEnd
		rts

 WordHeader "F!",0 ; ( addr -- ) ( F: r -- )  Store a FP number, internal format
  ; https://forth-standard.org/standard/float/FStore
  ; Also see: SF! DF! for IEEE formats
FStore:		jsr PopYA		; pop addr
FStore_YA:	sta tmp1+0		; save addr
		sty tmp1+1

		ldy #0
		stx tmp2		; save data stack index
		ldx FIndex		; X= FP stack index
		lda FSMant3,x		; copy mantissa
		sta (tmp1),y
		lda FSMant2,x
		iny
		sta (tmp1),y
		lda FSMant1,x
		iny
		sta (tmp1),y
		lda FSMant0,x
		iny
		sta (tmp1),y
		lda FSExp,x		; copy exponent
		iny
		sta (tmp1),y
		inc FIndex		; FDrop
		ldx tmp2		; restore data stack index
	WordEnd
		rts


 WordHeader "F,",NN ; ( F: f -- )  compile a float
FComma:		lda cp+0		; store f at Here
		ldy cp+1
		jsr FStore_YA
		lda #5			; Float
		jsr PushZA
		jmp Allot
	WordEnd


 WordHeader "F0!",NN ; ( addr -- )  Store a zero in a FP internal format variable
FZStore:	jsr F0
		jmp FStore
	WordEnd


 WordHeader "F0=",NN ; ( F: r -- ) ( -- flag )  flag is true if and only if r is equal to zero. 
  ; https://forth-standard.org/standard/float/FZeroEqual
FZEq:		ldy FIndex
		lda FSMant0,y
		bne FFalse1

FTrue1: ; ( F: r -- ) ( -- true )
		inc FIndex	; FDrop
		jmp True	; return true
	WordEnd

 WordHeader "F0<>",NN ; ( F: r -- ) ( -- flag )  return r <> 0
FZNe:		ldy FIndex
		lda FSMant0,y
		bne FTrue1
		beq FFalse1
	WordEnd

 WordHeader "F0<",NN ; ( F: r -- ) ( -- flag )  return r < 0
  ; https://forth-standard.org/standard/float/FZeroless
FZLt:		ldy FIndex
		lda FSMant0,y
		bmi FTrue1

FFalse1: ; ( F: r -- ) ( -- false )
		inc FIndex	; FDrop
		jmp False	; return false
	WordEnd

 WordHeader "F0>=",NN ; ( F: r -- ) ( -- flag )  return r >= 0
FZGe:		ldy FIndex
		lda FSMant0,y
		bpl FTrue1
		bmi FFalse1
	WordEnd

 WordHeader "F0>",NN ; ( F: r -- ) ( -- flag )  return r > 0
FZGt:		ldy FIndex
		lda FSMant0,y
		bmi FFalse1
		bne FTrue1
		beq FFalse1
	WordEnd

 WordHeader "F0<=",NN ; ( F: r -- ) ( -- flag )  return r <= 0
FZLe:		ldy FIndex
		lda FSMant0,y
		bmi FTrue1
		bne FFalse1
		beq FTrue1
	WordEnd


 WordHeader "F<",NN ; ( F: r1 r2 -- ) ( -- flag )  return r1 < r2
  ; https://forth-standard.org/standard/float/Fless
FLt:		jsr FCmpA
		bmi FTrue2

FFalse2: ; ( F: r1 r2 -- ) ( -- false )
		inc FIndex	; FDrop
		inc FIndex	; FDrop
		jmp False	; return False
	WordEnd

 WordHeader "F>=",NN ; ( F: r1 r2 -- ) ( -- flag )  return r1 >= r2
FGe:		jsr FCmpA
		bpl FTrue2
		bmi FFalse2
	WordEnd

 WordHeader "F>",NN ; ( F: r1 r2 -- ) ( -- flag )  return r1 > r2
FGt:		jsr FCmpA
		bmi FFalse2
		bne FTrue2
		beq FFalse2
	WordEnd

 WordHeader "F<=",NN ; ( F: r1 r2 -- ) ( -- flag )  return r1 <= r2
FLe:		jsr FCmpA
		bmi FTrue2
		bne FFalse2

FTrue2: ; ( F: r1 r2 -- ) ( -- true )
		inc FIndex	; FDrop
		inc FIndex	; FDrop
		jmp True	; return True
	WordEnd

 WordHeader "F<>",NN ; ( F: r1 r2 -- ) ( -- flag )  return r1 <> r2
FNe:		jsr FCmpA
		bne FTrue2
		beq FFalse2
	WordEnd

 WordHeader "F=",NN ; ( F: r1 r2 -- ) ( -- flag )  return r1 = r2
FEq:		jsr FCmpA
		bne FFalse2
		beq FTrue2
	WordEnd


 WordHeader "F~Abs",NN ; ( F: r1 r2 r3 -- flag )  approximate equality with absolute error |r1-r2|<r3
FTAbs:		jsr FMRot
		jsr FMinus
		jsr FAbs
		jmp FGt
	WordEnd

 WordHeader "F~Rel",NN ; ( F: r1 r2 r3 -- flag )  approximate equality with relative error |r1-r2|<r3*|r1+r2|
FTRel:		jsr FOver
		lda #3
		jsr FPickA
		jsr FPlus
		jsr FAbs
		jsr FStar		; r1 r2 r3*|r1+r2|
		jsr FMRot		; r3*|r1+r2| r1 r2
		jsr FMinus
		jsr FAbs
		jmp FGt
	WordEnd

 WordHeader "F~",NN ; ( F: r1 r2 r3 -- flag )  Approximate equality
  ; https://forth-standard.org/standard/float/Ftilde
FTilde:		ldy FIndex
		lda FSMant0,y
		bmi _10			; r3<0
		bne FTAbs		; r3>0
		inc FIndex		; r3=0
		bne FEq

_10:		jsr FNegate
		jmp FTRel
	WordEnd


 WordHeader "FVariable",IM+NN ; ( "name" -- )  Create FP variable (internal format)
  ; https://forth-standard.org/standard/float/FVARIABLE
  ; FVARIABLE <name>   compile time: ( -- )
  ; <name>            run time:  ( -- adr)
  ; A defining word executed in the form:
  ;	FVARIABLE <name>     
  ; to create a dictionary entry for <name> and allot a floating point storage
  ; in the parameter field.  The application must initialize the stored
  ; value.  When <name> is later executed, it will place the storage
  ; address on the stack. 
FVariable:	jsr Create		; compile word header & push PFA adr
		jsr adjust_z		; fix word code length
		jsr F0			; alloc & init data
		jmp FComma
	WordEnd

  
 WordHeader "FLiteral",IM+NN ; ( F: r -- )  Compile FP literal
  ; https://forth-standard.org/standard/float/FLITERAL
FLiteral:	jsr FLitTest
		beq _Short
					; full precision literal
		lda #<FLitI		; compile JSR FLitI
		ldy #>FLitI
		jsr Jsr_Comma_YA
		jmp FComma		; inline operand, return

_Short:		jsr FLitShort
		jmp Jsr_Comma_YA	; compile jsr FLitYA, & return
	WordEnd

FLitTest: ; Will short work?
		ldy FIndex
		lda FSMant1,y		; will short work?
		ora FSMant2,y
		ora FSMant3,y
		rts

FLitShort: ; compile LDA #; LDY #
		lda #$a9		; compile LDA #mant0
		jsr C_Comma_A
		lda FSMant0,y
		jsr C_Comma_A
		lda #$a0		; compile LDY #exp
		jsr C_Comma_A
		lda FSExp,y
		jsr C_Comma_A
		inc FIndex		; FDrop
		lda #<FLitYA		; point at FLitYA
		ldy #>FLitYA
		rts

FLitI: ; ( -- ) ( F: -- r )  push inline fvalue
		pla			; tmp2= RTS addr
		sta tmp2+0
		clc			; bump RTS addr over inline float data
		adc #5
		tay
		pla
		sta tmp2+1
		adc #0
		pha
		tya
		pha

		ldy #1			; correct for RTS addr
		jmp FAt_Tmp2Y		; fetch inline data, & return

FloatLit .macro mantissa,exponent ; float constant in native format
		.dword \mantissa	; 32 bit signed binary mantissa.  $40000000 = +0.5
		.char \exponent		; 8 bit signed exponent.  $00 = 2**0
	.endmacro
FloatLitI .macro mantissa,exp ; float literal
		jsr FLitI
		FloatLit \mantissa,\exp
	.endmacro


 WordHeader "FConstant",NN ; ( F: r "name" -- )  Create FP constant word
  ; https://forth-standard.org/standard/float/FCONSTANT
FConstant:	jsr Header_Comma	; compile word header

		jsr FLitTest		; will short work?
		beq _Short
					; full precisision
		lda #<FConstantRun	; compile call
		ldy #>FConstantRun
		jsr Jsr_Comma_YA
		jsr adjust_z
		jmp FComma		; inline operand, & return

_Short:
		jsr FLitShort		; compile load value
		jsr Jmp_Comma_YA	; compile JMP
		jmp adjust_z
	WordEnd

FConstantRun: ; ( F: -- r )  runtime for long FConstant
		pla			; tmp2= pop RTS addr
		sta tmp2+0
		pla
		sta tmp2+1
		ldy #1			; correct for RTS addr
		jmp FAt_Tmp2Y		; fetch inline data, & return


FLitYA: ; ( YA -- ) ( F: -- r )  push short fvalue, Y=Exp A=Mant0
		jsr FAllocX		; alloc FP stack entry, X=fp stack index
		sty FSExp,x		; exp= Y
		sta FSMant0,x		; Mant= A,0,0,0
		lda #0
		sta FSMant1,x
		sta FSMant2,x
		sta FSMant3,x
		ldx tmp1+0		; restore data stack index
		rts

 WordHeader "0.e",NN ; ( F: -- r )  FConstant 0  aka "F0.0"
F0:		lda #0
		ldy #$80
		bne FLitYA
	WordEnd

 WordHeader "1000.e",NN ; ( F: -- r )  FConstant 1000
F1000:		lda #$7d
		ldy #10
		bne FLitYA
	WordEnd

 WordHeader "10.e",NN ; ( F: -- r )  FConstant 10  aka "F10.0" 
F10:		lda #$50
		ldy #4
		bne FLitYA
	WordEnd

 WordHeader "2.e",NN ; ( F: -- r)  FConstant 2
F2:		lda #$40
		ldy #2
		bne FLitYA
	WordEnd

 WordHeader "1.e",NN ; ( F: -- r )  FConstant 1  aka "F1.0"
F1:		lda #$40
		ldy #1
		bne FLitYA
	WordEnd

 WordHeader ".1e",NN ; ( F: -- r )  FConstant .1
F10th:		jsr FConstantRun
		FloatLit $66666667,-3
	WordEnd

 WordHeader "Pi",NN ; ( F: -- r )  FConstant PI
FPi:		jsr FConstantRun
		FloatLit $6487ed51,2
	WordEnd

 WordHeader "Pi/2",NN ; ( F: -- r )  FConstant Pi/2
FPiH:		jsr FConstantRun
		FloatLit $6487ed51,1
	WordEnd

 WordHeader "Pi/4",NN ; ( F: -- r )  FConstant Pi/4
FPiQ:		jsr FConstantRun
		FloatLit $6487ed51,0
	WordEnd

 WordHeader "2Pi",NN ; ( F: -- r )  FConstant 2*Pi
F2Pi:		jsr FConstantRun
		FloatLit $6487ed51,3
	WordEnd

 WordHeader "F.E",NN ; ( F: -- r )  FConstant e
FE:		jsr FConstantRun
		FloatLit $56fc2a2c,2
	WordEnd


 WordHeader "FValue",IM+NN ; ( F: r “(spaces)name” -- )  Create FP Value word
  ; https://forth-standard.org/standard/float/FVALUE
  ; Skip leading space delimiters. Parse name delimited by a space. Create a definition for
  ; name with the execution semantics defined below, with an initial value equal to r.
  ; name is referred to as a “F-Value”.
  ; name Execution: ( f: -- r )
  ;	Place r on the stack. The value of r is that given when name was created, until the phrase
  ;	x TO name is executed, causing a new value of r to be assigned to name.
  ; TO name Run-time: ( F: r -- )
  ;	Assign the value r to name.
FValue:		jsr Header_Comma	; compile word header
		lda #<FValue_runtime	; compile JSR FValue_runtime
		ldy #>FValue_runtime
		jsr Jsr_Comma_YA
		jsr adjust_z		; fix word length
		jmp FComma		; alloc & init value
	WordEnd

FValue_runtime: ; unique, so  TO & optimizer can recognize as FValue
		jmp FConstantRun


 WordHeader "FScale",NN ; ( n -- ) ( F: r1 -- r2 )  Add n to the exponent of an fp number.  aka FLShift
FScale:		jsr PopA		; pop n
FScaleA:	stx tmp1		; save data stack index
		ldx FIndex		; X= FP stack index
		ldy FSMant0,x		; mantissa zero?
		beq _8
		clc
		adc FSExp,x
		sta FSExp,x
		bvs _overflow		; overflow or underflow?
_8:		ldx tmp1		; restore data stack index
		rts

_overflow: ;	bpl _zero		; underflow could just return zero
		ldx tmp1		; restore data stack index
		jsr Throw_FpOutOfRange
	WordEnd

 WordHeader "F2*",NN ; ( F: r1 -- r2 )  Double a fp number
F2Star:		lda #1
		bne FScaleA
	WordEnd

 WordHeader "F2/",NN ; ( F: r1 -- r2 )  Halve a fp number
F2Slash:	lda #$ff
		bne FScaleA
	WordEnd


; WordHeader "FM1+",0 ; ( F: q1 -- q2 )  increment mantissa
;FMOnePlus:	stx tmp1		; save data stack index
;		ldx FIndex		; switch X to FP stack index
;		inc FSMant3,x
;		bne @3
;		inc FSMant2,x
;		bne @3
;		inc FSMant1,x
;		bne @3
;		inc FSMant0,x
;@3:
;		ldx tmp1		; restore data stack index
;	WordEnd
;		rts


; WordHeader "FM2/",0 ; ( F: q1 -- q2 carry )  shift mantissa right 1 bit
;  ; Returns carry.  Preserves Y.
;FM2Slash:	stx tmp1		; switch X to FP stack index
;		ldx FIndex
;		lda FSMant0,x
;		asl a
;		ror FSMant0,x
;		ror FSMant1,x
;		ror FSMant2,x
;		ror FSMant3,x
;		ldx tmp1		; restore data stack index
;	WordEnd
;		rts


 WordHeader "Floor",NN ; ( F: r1 -- r2 )  Round toward negative infinity 
  ; https://forth-standard.org/standard/float/FLOOR
Floor:		ldy FIndex
		lda FSMant0,y		; negative?
		bpl _a
		lda FSExp,y		; > -1 ?
		bpl _a

		lda #$80		; return -1
		sta FSMant0,y
		lda #0
		sta FSExp,y
		sta FSMant1,y
		sta FSMant2,y
		sta FSMant3,y
		rts
		
_a:		lda #31
		jsr FShiftA
		jmp FNormX
	WordEnd

; WordHeader "FShift",NN ; ( F: r1 n -- r2 )  align r1 to desired exponent
  ; n=designed exponent
  ; Returns exponent in Y, lo bit in carry
;FShift:	jsr PopA
FShiftA:	stx tmp1+0		; save data stack index
		ldx FIndex		; X= FP stack index
FShiftAX:	tay			; save desired alignment
		sec			; calc bit shift count
		sbc FSExp,x
		beq _leave
		bvs _overflow
		bmi _leave
		cmp #32
		bcs _zero

		sty FSExp,x
		tay
		lda FSMant0,x
_12:		cmp #$80		; mantissa >>=1
		ror a
		ror FSMant1,x
		ror FSMant2,x
		ror FSMant3,x
		dey
		bne _12
		sta FSMant0,x
_30:		ldy FSExp,x
		ldx FIndex		; restore fp stack index (FShiftAX could have had a funny one)
		rts

_overflow:	bpl _leave
_zero:		sty FSExp,x		; return zero
		lda #0
		sta FSMant0,x
		sta FSMant1,x
		sta FSMant2,x
		sta FSMant3,x
_leave:		clc
		bcc _30
;	WordEnd


 WordHeader "FTrunc",NN ; ( F: r1 -- r2 ) Round towards zero
  ; https://forth-standard.org/standard/float/FTRUNC
FTrunc:		ldy FIndex
		lda FSMant0,y
		bpl Floor
		jsr FNegate
		jsr Floor
		jmp FNegate


 WordHeader "FRound",NN ; ( F: r1 -- r2 )  Round to nearest
  ; https://forth-standard.org/standard/float/FROUND
FRound:		lda #31
		jsr FShiftA
		bcc _15
		ldx tmp1+0		; restore data stack index
		jmp F1Plus

_15:		jmp FNormX
	WordEnd


 WordHeader "FIntFrc",NN ; ( F: r1 -- r_frac r_int )  Separate out integer part
FIntFrc:	jsr FDup	; ( r1 r1 )
		jsr Floor	; ( r1 rint )
		jsr FTuck	; ( rint r1 rint )
		jsr FMinus	; ( rint rfrac )
		jmp FSwap	; ( rfrac rint )
	WordEnd



; WordHeader "FMAlign",NN ; ( F: r2 r1 -- r2 r1 )  wrapper for FMAlignX
;		jsr FMAlignX
;		ldx tmp1		; restore data stack index
;	WordEnd
;		rts

 WordHeader "FMAlignX",NN ; ( F: r2 r1 -- r2 r1 )  Denormalize to make exponents =
  ; X is data stack index as usual on input.
  ; returns X= FP stack index
FMAlignX:	stx tmp1+0	; save data stack index
		ldx FIndex	; load FP stack index
		cpx #FDim-1	; check FP stack for >=2 entries
		bcs Throw_FPStack_3

		; Note: sometimes we get denormalized mantissas

		lda FSExp+0,x	; compare exponents
		sec
		sbc FSExp+1,x
		bmi _1		; r1 smaller?
		bne _2		; r2 smaller?
		rts

_1: ; r1 looks smaller
		bvs _2b		; was this a big positive #?
_1b:		lda FSExp+1,x	; make r1 like r2
		jmp FShiftAX

_2: ; r2 looks smaller
		bvs _1		; was this a big negative #?
_2b:		lda FSExp+0,x	; make r2 like r1
		inx
		jmp FShiftAX
	WordEnd

Throw_FPStack_3: jsr Throw_FPStack

 WordHeader "FNorm",NN ; ( F: r1 -- r2 )  Normalize r.
FNorm:		stx tmp1+0		; save data stack index
FNormX:		ldx FIndex		; switch to FP stack
		ldy FSExp,x
		lda FSMant0,x		; mantissa negative?
		bmi _Neg
					; mantissa is positive
		bne _Pos2		; do byte shift
		jsr _ShiftB
		bne _Pos2
		jsr _ShiftB
		bne _Pos2
		jsr _ShiftB
		beq _zero		; no significant bits left?
_Pos2:		clc
		bmi _RShft
_Pos3:		dey			; do bit shift
		asl FSMant3,x
		rol FSMant2,x
		rol FSMant1,x
		rol a
		bpl _Pos3

_RShft:  ; right shift 1 bit
		iny
		ror a
		ror FSMant1,x
		ror FSMant2,x
		ror FSMant3,x
_28:	; finish
		sta FSMant0,x
		sty FSExp,x
		ldx tmp1+0	; restore data stack index
		rts

_Neg:	; mantissa is negative
		cmp #$ff
		bne _Neg2	; do byte shift
		jsr _ShiftB
		cmp #$ff
		bne _Neg2
		jsr _ShiftB
		cmp #$ff
		bne _Neg2
		jsr _ShiftB
;		cmp #$ff
;		beq _zero	; no significant bits left?
_Neg2:		cmp #0
;		sec
		bpl _RShft
_Neg3:		dey
		asl FSMant3,x
		rol FSMant2,x
		rol FSMant1,x
		rol a
		bmi _Neg3
		bpl _RShft

_Zerop:		pla
		pla		; pop rts addr from _ShiftB
_zero:		lda #0
		sta FSMant3,x
		sta FSMant2,x
		sta FSMant1,x
		ldy #$80
		bne _28

_ShiftB: ; shift left 1 byte
		tya		; exponent -= 8
		sec
		sbc #8
		tay
		bvs _zerop	;is this useful? do we need more of them?
		lda FSMant1,x
		pha
		lda FSMant2,x
		sta FSMant1,x
		lda FSMant3,x
		sta FSMant2,x
		lda #0
		sta FSMant3,x
		pla
		rts
	WordEnd


 WordHeader "F+",NN ; ( F: r1 r2 -- r3 )  Add r1 & r2, giving r3.
  ;  https://forth-standard.org/standard/float/FPlus
FPlus:		jsr FMAlignX	; align mantissas, X= fp stack index
		clc		; add mantissas
		lda FSMant3+1,x
		adc FSMant3+0,x
		sta FSMant3+1,x
		lda FSMant2+1,x
		adc FSMant2+0,x
		sta FSMant2+1,x
		lda FSMant1+1,x
		adc FSMant1+0,x
		sta FSMant1+1,x
		lda FSMant0+1,x
		adc FSMant0+0,x

FPlusFin: ; finish F+ & F-
		inx		; FDrop r2
		stx FIndex
FPlusFin3:	bvc _19		; if overflow
		ror a		;   shift mantissa right 1 bit
		ror FSMant1,x
		ror FSMant2,x
		ror FSMant3,x
		inc FSExp,x	;   adjust exponent
			; rounding???
			; overflow???
_19:
		sta FSMant0,x
		jmp FNormX	; normalize, return
	WordEnd

 WordHeader "F1+",NN ; ( F: r1 -- r2 )  Add 1
F1Plus:		jsr F1
		jmp FPlus
	WordEnd

 WordHeader "F-",NN ; ( F: r1 r2 -- r3 )  Subtract r2 from r1, giving r3.
  ; https://forth-standard.org/standard/float/FMinus
FMinus:		jsr FMAlignX	; align mantissas
		sec		; subtract mantissas
		lda FSMant3+1,x
		sbc FSMant3+0,x
		sta FSMant3+1,x
		lda FSMant2+1,x
		sbc FSMant2+0,x
		sta FSMant2+1,x
		lda FSMant1+1,x
		sbc FSMant1+0,x
		sta FSMant1+1,x
		lda FSMant0+1,x
		sbc FSMant0+0,x
		jmp FPlusFin
	WordEnd

 WordHeader "F1-",NN ; ( F: r1 -- r2 )  Subtract 1
F1Minus:	jsr F1
		jmp FMinus
	WordEnd


 WordHeader "FNegate",NN ; ( F: r1 -- r2 )  Negate r1.
  ; https://forth-standard.org/standard/float/FNEGATE
FNegate:	stx tmp1	; save data stack index
		ldx FIndex	; X= FP stack index

		sec		; mantissa = 0 - mantissa
		lda #0
		sbc FSMant3,x
		sta FSMant3,x
		lda #0
		sbc FSMant2,x
		sta FSMant2,x
		lda #0
		sbc FSMant1,x
		sta FSMant1,x
		lda #0
		sbc FSMant0,x
		jmp FPlusFin3	; finish up, return
	WordEnd

 WordHeader "FAbs",NN ; ( F: r1 -- r2 )  Absolute value of r.
  ; https://forth-standard.org/standard/float/FABS
FAbs:		ldy FIndex
		lda FSMant0,y		; mantissa negative?
		bmi FNegate
		rts
	WordEnd

Throw_FPStack_4: jmp Throw_FPStack

; WordHeader "FPos",NN ; ( F: r1 r2 -- r1 r2 CPU.flags )  make r1 & r2 positive & return result sign
FPos:		ldy FIndex		; load FP stack index
		cpy #FDim-1		; check FP stack for 2
		bcs Throw_FPStack_4

		lda FSMant0+0,y		; calc result sign
		eor FSMant0+1,y
		php

		lda FSMant0+1,y		; if NOS negative
		bpl +
		inc FIndex		;   point at NOS
		jsr FNegate		;   negate NOS
		dec FIndex		;   restore fp
+
		jsr FAbs		; abs TOS

		stx tmp1+0		; save data stack index
		ldx FIndex		; load FP stack index
		plp			; restore sign flag
		rts
  ;	WordEnd


 .if 0
 WordHeader "F*1",NN ; ( F: r1 r2 -- r3 )  Multiply r1 by r2, giving r3.
  ; https://forth-standard.org/standard/float/FTimes
FStar1:
		jsr FPos		; make r1 & r2 positive,
		php			;   save r3 sign

		lda FSExp+0,x		; add exponents
		sec
		adc FSExp+1,x
		sta FSExp+1,x
		bvc _49			; IfVs, 
		bcs _zero		;   underflow?
		ldx tmp1+0		;   restore data stack index
		plp			; RDrop sign flag
		jsr Throw_FpOutOfRange

_zero:		inx			; F2Drop
		inx
		stx FIndex
		plp			; rdrop saved result sign
		ldx tmp1+0		; restore data stack index
		jmp F0			; return zero

_49:

		lda FSMant0+1,x		; save r1 mantissa
		pha
		lda FSMant1+1,x
		pha
		lda FSMant2+1,x
		pha
		lda FSMant3+1,x

		ldy #0			; init r3 mantissa
		sty FSMant0+1,x
		sty FSMant1+1,x
		sty FSMant2+1,x
		sty FSMant3+1,x

		jsr _Byte		; do bytes of r1 mantissa
		pla
		jsr _Byte
		pla
		jsr _Byte
		pla
		jsr _Byte

		inc FIndex		; FDrop
		ldx tmp1+0		; restore data stack index

		plp			; fix result sign
		bpl +
		jmp FNegate
+
		jmp FNorm

_Byte: ; do a byte of r1 (in A)
		sta tmp1+1
		ldy #8			; for each bit in byte
_b1:		lsr tmp1+1		;   if bit set
		bcc _b3

		clc			;     r3 += r2
		lda FSMant3+1,x
		adc FSMant3+0,x
		sta FSMant3+1,x
		lda FSMant2+1,x
		adc FSMant2+0,x
		sta FSMant2+1,x
		lda FSMant1+1,x
		adc FSMant1+0,x
		sta FSMant1+1,x
		lda FSMant0+1,x
		adc FSMant0+0,x
		sta FSMant0+1,x

_b3:		lsr FSMant0+1,x		;   r3 >>= 1
		ror FSMant1+1,x
		ror FSMant2+1,x
		ror FSMant3+1,x

		dey			;  next bit
		bne _b1
		rts
	WordEnd

  .else

 WordHeader "F*",NN ; ( F: r1 r2 -- r3 )  Multiply r1 by r2, giving r3.
  ; https://forth-standard.org/standard/float/FTimes
FStar:
		jsr FPos		; make r1 & r2 positive,
		php			;   save r3 sign

		lda FSExp+0,x		; add exponents
		sec
		adc FSExp+1,x
		sta FSExp+1,x
		bvc _49			; IfVs, 
		bcs _zero		;   underflow?
		ldx tmp1+0		;   restore data stack index
		plp			; RDrop sign flag
		jsr Throw_FpOutOfRange

_zero:		inx			; F2Drop
		inx
		stx FIndex
		plp			; rdrop saved result sign
		ldx tmp1+0		; restore data stack index
		jmp F0			; return zero

_49:

		lda #0			; tmp32= 0
		sta tmp2+0
		sta tmp2+1
		sta tmp3+0
		sta tmp3+1

		lda FSMant3+1,x
		jsr _Byte		; do bytes of r1 mantissa
		lda FSMant2+1,x
		jsr _Byte
		lda FSMant1+1,x
		jsr _Byte
		lda FSMant0+1,x
		jsr _Byte

		lda tmp2+0		; r1.mant= tmp32
		sta FSMant3+1,x
		lda tmp2+1
		sta FSMant2+1,x
		lda tmp3+0
		sta FSMant1+1,x
		lda tmp3+1
		sta FSMant0+1,x

		inc FIndex		; FDrop
		ldx tmp1+0		; restore data stack index

		plp			; fix result sign
		bpl +
		jmp FNegate
+
		jmp FNorm

_Byte: ; do a byte of r1 (in A)
		eor #$ff
		sta tmp1+1
		ldy #8			; for each bit in byte
_b1:		lsr tmp1+1		;   if bit set
		bcs _b3

		lda tmp2+0		;     tmp32 += r2
		adc FSMant3+0,x
		sta tmp2+0
		lda tmp2+1
		adc FSMant2+0,x
		sta tmp2+1
		lda tmp3+0
		adc FSMant1+0,x
		sta tmp3+0
		lda tmp3+1
		adc FSMant0+0,x
		sta tmp3+1

_b3:		lsr tmp3+1		;   tmp32 >>= 1
		ror tmp3+0
		ror tmp2+1
		ror tmp2+0

		dey			;  next bit
		bne _b1
		rts
	WordEnd
  .endif


 WordHeader "FSqr",NN ; ( F: r1 -- r2 )  square
FSqr:		jsr FDup
		jmp FStar
	WordEnd


 WordHeader "F10*",NN ; ( F: r1 -- r2 )  multiply by 10
F10Star:	jsr FDup
		ldy FIndex
		lda FSExp+0,y
		clc
		adc #1
		sta FSExp+0,y
		clc
		adc #2
		sta FSExp+1,y
		jmp FPlus
	WordEnd


 WordHeader "F/",NN ; ( F: r1 r2 -- r3 )  Divide r1 by r2, giving r3.
  ; https://forth-standard.org/standard/float/FDiv
FSlash:		jsr FPos		; make r1 & r2 positive
		php			;   save result sign

		lda FSExp+1,x		; calc exponent
		clc
		adc #1
		sec
		sbc FSExp+0,x
		sta FSExp+1,x
		bvc _49			;  IfVs
		bcs _Zero		; underflow?
		plp			; drop saved result sign
		ldx tmp1+1		; restore data stack index
		jsr Throw_FpOutOfRange	; overflow

_Zero:		inx			; F2Drop
		inx
		stx FIndex
		plp			; drop saved result sign
		ldx tmp1+0		; restore data stack index
		jmp F0			; return 0

_49:

		lda FSMant3+1,x		; tmp32 = r1
		sta tmp2+0
		lda FSMant2+1,x
		sta tmp2+1
		lda FSMant1+1,x
		sta tmp3+0
		lda FSMant0+1,x
		sta tmp3+1

		jsr _Byte
		sta FSMant0+1,x
		jsr _Byte
		sta FSMant1+1,x
		jsr _Byte
		sta FSMant2+1,x
		jsr _Byte
		sta FSMant3+1,x

		lsr FSMant0+1,x
		ror FSMant1+1,x
		ror FSMant2+1,x
		ror FSMant3+1,x

		inc FIndex		; FDrop
		ldx tmp1+0		; restore X
		plp			; fix sign
		bpl +
		jsr FNegate
+		jmp FNorm

_Byte:
		ldy #8			; for 8 bits
_1:
		lda tmp3+1		;   if r1>=r2
		cmp FSMant0+0,x
		bne _4
		lda tmp3+0
		cmp FSMant1+0,x
		bne _4
		lda tmp2+1
		cmp FSMant2+0,x
		bne _4
		lda tmp2+0
		cmp FSMant3+0,x
_4:		bcc _2

		lda tmp2+0		;     tmp32 -= r2
		sbc FSMant3+0,x
		sta tmp2+0
		lda tmp2+1
		sbc FSMant2+0,x
		sta tmp2+1
		lda tmp3+0
		sbc FSMant1+0,x
		sta tmp3+0
		lda tmp3+1
		sbc FSMant0+0,x
		sta tmp3+1

_2:		rol tmp1+1		;   accum quotient bits

		asl tmp2+0
		rol tmp2+1
		rol tmp3+0
		rol tmp3+1		;   tmp32<<=1
		dey			;  next
		bne _1

		lda tmp1+1		; return bits
		rts
	WordEnd

 WordHeader "F1/",NN ; ( F: r1 -- r2 )  FP reciprocal
F1Slash:	jsr F1
		jsr FSwap
		jmp FSlash
	WordEnd


 .if "fpe" in TALI_OPTIONAL_WORDS ;----------------------------------------------------
;-------------------------------------------------------------
; 16bit mantissa floating point (1/2 precision) functions.
; These accept & return normal FP entries on the FP stack,
; but only use 16bits of the input mantissa values.
; They are faster, for when lower precision can be tolerated.

 WordHeader "E*",NN ; ( F: r1 r2 -- r3 )  Multiply r1 by r2, giving r3. (16bit precision)
EStar:
		jsr FPos		; make r1 & r2 positive
		php			;   remember result sign

		lda FSExp+0,x		; add exponents
		sec
		adc FSExp+1,x
		sta FSExp+1,x
		bvs _ExpOvfl

		lda FSMant0+1,x		; save e1
		pha
		lda FSMant1+1,x

		ldy #0
		sty FSMant0+1,x
		sty FSMant1+1,x		; zero result

		jsr _Byte		; do FSMant1+1 byte
		pla
		jsr _Byte		; do FSMant0+1 byte

		jmp EFix3		; finish

_ExpOvfl: ; exponent overflowed
		bcs _Zero		;underflow?
		ldx tmp1+0		; restore X
		jsr Throw_FpOutOfRange	;overflow

_Zero:		lda #0
		sta FSMant1+1,x
		sta FSMant0+1,x
		beq EFix3

_Byte: ; do a multiplier byte in A
		sta tmp1+1
		ldy #8			; for 8 bits
_b1:		lsr tmp1+1		;   if bit set
		bcc _b5
		clc			;     r3 += r2 
		lda FSMant1+0,x
		adc FSMant1+1,x
		sta FSMant1+1,x
		lda FSMant0+0,x
		adc FSMant0+1,x
		sta FSMant0+1,x
_b5:
		ror FSMant0+1,x		;   r3 <<= 1
		ror FSMant1+1,x
		dey			;  next bit
		bne _b1
		rts
	WordEnd

EFix3: ; commonm code
		lda #0			; zero unused mantissa
		sta FSMant2+1,x
		sta FSMant3+1,x

		inc FIndex		; FDrop
		ldx tmp1+0		; restore X

		plp			;fix result sign
		bpl +
		jmp FNegate

+		jmp FNorm


 WordHeader "ESqr",NN ; ( F: r1 -- r2 )  FP 16bit-precision Square
ESqr:		jsr FDup
		jmp EStar
	WordEnd


 WordHeader "E/",NN ; ( F: r1 r2 -- r3 )  Divide r1 by r2, giving r3, 16bit precision
ESlash:		jsr FPos	; make r1 & r2 positive
		php		; remember result sign

		lda FSExp+1,x		; calc r3 exponent
		sec
		sbc FSExp+0,x
		bvs _ExpOvfl
		clc
		adc #1
		sta FSExp+1,x

		jsr _Byte		; gen quotient hi byte
		pha
		jsr _Byte		; gen quotient lo byte
		sta FSMant1+1,x
		pla
		lsr a
		sta FSMant0+1,x
		ror FSMant1+1,x

		jmp EFix3

_ExpOvfl:
	;	bcs _8			; underflow?
		plp			; rdrop saved result sign
		ldx tmp1+0		; restore data stack index
		jsr Throw_FpOutOfRange	; overflow

_Byte: ; returns quotient byte in A
		ldy #8			; for 8 bits
_b1:		lda FSMant1+1,x		;   if r1 >= r2
		cmp FSMant1+0,x
		lda FSMant0+1,x
		sbc FSMant0+0,x
		bcc _b5

		sta FSMant0+1,x
		lda FSMant1+1,x		;     r1 -= r2
		sbc FSMant1+0,x
		sta FSMant1+1,x
		sec
_b5:
		rol tmp1+1		;   save quotient bit
		asl FSMant1+1,x		;   r1 *= 2
		rol FSMant0+1,x
		dey			;  next bit
		bne _b1
		lda tmp1+1		; return bits
		rts
	WordEnd

 WordHeader "E1/",NN ; ( F: r1 -- r2 )  FP 16bit-precision reciprocal
E1Slash:	jsr F1
		jsr FSwap
		jmp ESlash
	WordEnd

 .endif ; fpe


 WordHeader "S>F",NN ; ( n -- ) ( F: -- r )  convert n to r.  aka N>F
  ; https://forth-standard.org/standard/float/StoF
SToF:		jsr PopYA		; pop n
SToFYA:		jsr FAllocX		; alloc FP stack entry
		sty FSMant0,x		; copy n to mantissa
		sta FSMant1,x
		lda #0			; pad mantissa
		sta FSMant2,x
		sta FSMant3,x
		lda #15			; set exponent
		sta FSExp,x
		jmp FNormX		; normalize, return
	WordEnd


 WordHeader "D>F",NN ; ( d -- ) ( F: -- r )  convert d to r
  ; https://forth-standard.org/standard/float/DtoF
FDToF:		jsr FAllocX		; alloc FP stack entry
		ldx tmp1+0		; restore data stack index
		ldy FIndex		; Y= fp stack index
		lda DStack+2,x		; mantissa= d
		sta FSMant3,y
		lda DStack+3,x
		sta FSMant2,y
		lda DStack+0,x
		sta FSMant1,y
		lda DStack+1,x
		sta FSMant0,y
		lda #31			; set exponent
		sta FSExp,y
		jsr Two_Drop
		jmp FNorm		; normalize, return
	WordEnd


 WordHeader "F>S",NN ; ( F: r -- ) ( -- n )  Convert r to n.  aka F>N
  ; https://forth-standard.org/standard/float/FtoS
FToS:		ldy FIndex
		lda FSMant0,y		; save sign
		php
		bpl +
		jsr FNegate
+
		lda #15
		jsr FShiftA
		ldx tmp1+0		; restore data stack index
		cpy #15+1		; always positive, so unsigned compare works
		bcs _overflow

		ldy FIndex
		dex
		dex
		lda FSMant0,y
		sta DStack+1,x
		lda FSMant1,y
		sta DStack+0,x

		inc FIndex		; FDrop

		plp			; apply saved sign
		bmi +
		rts

+		jmp Negate

_overflow:	plp			; RDrop saved sign
		lda #$100+err_OutOfRange
		jsr ThrowA
	WordEnd


 WordHeader "F>D",NN ; ( F: r -- ) ( -- d )  convert r to d
  ; https://forth-standard.org/standard/float/FtoD
  ; d is the double-cell signed-integer equivalent of the integer portion of r.
  ; The fractional portion of r is discarded.
FToD:		ldy FIndex
		lda FSMant0,y		; save sign
		php
		bpl +
		jsr FNegate
+
		lda #31
		jsr FShiftA
		ldx tmp1+0		; restore data stack index
		cpy #31+1		; always positive, so unsigned compare works
		bcs _overflow

		ldy FIndex
		dex			; d= mantissa
		dex
		dex
		dex
		lda FSMant0,y
		sta DStack+1,x
		lda FSMant1,y
		sta DStack+0,x
		lda FSMant2,y
		sta DStack+3,x
		lda FSMant3,y
		sta DStack+2,x

		inc FIndex		; FDrop

		plp			; apply saved sign
		bmi +
		rts

+		jmp DNegate

_overflow:	plp			; RDrop sign
		lda #$100+err_OutOfRange
		jsr ThrowA
	WordEnd


 WordHeader "FRnd",NN ; ( F:  -- r )  get random # between 0 & 1
FRnd:		jsr Rand		; generate next RndState
		jsr FAllocX		; alloc FP stack entry
					; mantissa = RndState
		lsr a			;   make positive
		sta FSMant0,x
		sty FSMant1,x
		lda RndState+0
		sta FSMant2,x
		lda RndState+1
		sta FSMant3,x
		lda #0			; exponent=0
		sta FSExp,x
		jmp FNormX		; normalize, return
	WordEnd


 WordHeader "FSqrt",NN ; ( F: r1 -- r2 )  Square root.  Newton's method. Short but slowish
  ; https://forth-standard.org/standard/float/FSQRT
FSqrt:		ldy FIndex
		lda FSMant0,y		; zero?
		bne +
		rts			;   just return the zero
+
		jsr FDup		; get trial value
		ldy FIndex
		lda FSExp,y		; halve the exponent of trial value
		cmp #$80
		ror a
		sta FSExp,y

		lda #9			; for 9 iterations
_3:		pha
		jsr F2Dup		;   calc new trial value
		jsr FSlash
		jsr FPlus
		jsr F2Slash
		pla			;  next
		sec
		sbc #1
		bne _3
		jmp FNip		; return trial value
	WordEnd


; : FSqrt ( F: r1 -- r2 )  square root, Using pseudo-division
  ; https://forth-standard.org/standard/float/FSQRT
;	phx		;save integer param index
;	ldx FIndex	;load FP param index
;
;	lda FSMant6,x	; get r1 mantissa
;	ldy FSMant7,x
;	beq @Zero	; Is r1 zero?
;	bmi @Negative	; is r1 negative?
;	sta tmp+0	; X.lo=r1
;	sty tmp+2
;	stz tmp+4	; X.hi=0
;	stz tmp+6
;
;	inc FSExp,x	; fix exponent for algorithm
;
;	lda FSExp,x	; halve exponent
;	asl a
;	ror FSExp,x
;	bcc @39		; was odd?
;	inc FSExp,x
;	lsr tmp+2
;	ror tmp+0
;@39:
;
;	stz FSMant7,x	; sqrt=0
;	stz FSMant6,x
;
;	ldy #31		; for 31 bits of sqrt
;	sty tmp+8
;@41:
;
;	lda tmp+2	;   x>=sqrt?
;	cmp #$4000
;	lda tmp+4
;	sbc FSMant6,x
;	tay
;	lda tmp+6
;	sbc FSMant7,x
;	bcc @69
;	sta tmp+6	;     X-=sqrt
;	sty tmp+4
;	lda tmp+2
;	sbc #$4000
;	sta tmp+2
;	sec
; @69:
;
;	rol FSMant6,x	;   shift carry into sqrt
;	rol FSMant7,x
;	asl tmp+0	;   x<<=2
;	rol tmp+2
;	rol tmp+4
;	rol tmp+6
;	asl tmp+0
;	rol tmp+2
;	rol tmp+4
;	rol tmp+6
;	dec tmp+8	;  UntilEq
;	bne @41
;
;	jmp FNorm+1	; normalize & return
;
; @Zero: ; r1 is zero
;	plx		;   just return it
;	rts
;
; @Negative: ; r1 is negative
;	plx
;	jsr Throw_FpOutOfRange


 WordHeader "Precision",NN ; ( -- u )  Get number of significant digits currently used by F., FE., or FS.
  ; https://forth-standard.org/standard/float/PRECISION
Precision:	lda PrecisionV
		jmp PushZA
	WordEnd
		rts

 WordHeader "Set-Precision",NN ; ( u -- )  Set the number of significant digits currently used by F., FE., or FS. to u. 
  ; https://forth-standard.org/standard/float/SET-PRECISION
		jsr PopA
		sta PrecisionV
	WordEnd
		rts

; WordHeader "Represent",NN ; ( F: r -- ) ( c-addr u -- n flag1 flag2 )
  ; https://forth-standard.org/standard/float/REPRESENT
  ; At c-addr, place the character-string external representation of the significand of the floating-point number r.
  ; Return the decimal-Base exponent as n, the sign as flag1 and valid result as flag2. The character string
  ; shall consist of the u most significant digits of the significand represented as a decimal fraction with
  ; the implied decimal point to the left of the first digit, and the first digit zero only if all digits are
  ; zero. The significand is rounded to u digits following the round to nearest rule; n is adjusted, if necessary,
  ; to correspond to the rounded magnitude of the significand. If flag2 is true then r was in the
  ; implementation-defined range of floating-point numbers. If flag1 is true then r is negative. 
  ;
  ; An ambiguous condition exists if the value of Base is not decimal ten. 
  ;
  ; When flag2 is false, n and flag1 are implementation defined, as are the contents of c-addr. Under these
  ; circumstances, the string at c-addr shall consist of graphic characters. 
  ; ???



 WordHeader "(F.)",NN ; ( F: r -- ) ( -- adr )  Convert FP to string, fixed-point
  ; Display, with a trailing space, using fixed-point notation: 
  ;	[-] <digits>.<digits0>
PFDot:		jsr pfcst		;start collecting chars, make r positive
		jsr PFDotSub
		jmp pfcen


PFDotSub:
		jsr Precision	;alloc & init work area
		jsr One
_mantNzFound  = DStack+3	; nonzero mantissa digit processed
_NumSigDigits = DStack+2	; # of significant digits
_decimalPos   = DStack+0	; decimal point position

		jsr FLt10		; scale down to <10, counting exponent

_c:					;do
		lda _decimalPos,x	;  insert decimal point here?
		bne _c2
		lda #'.'
		jsr pfch
_c2:		dec _decimalPos,x

		jsr FDup		;  do a digit
		jsr FToS		; ( work n )
		lda DStack+0,x
		ora _mantNzFound+2,x	;  doing significant digits?
		sta _mantNzFound+2,x
		beq _c7
		dec _NumSigDigits+2,x
		bpl +
		inx			;    drop integer
		inx
		bne _d
+
_c7:		lda DStack+0,x		;  store the char
		ora #'0'
		jsr pfch
		jsr SToF		; ( work )
		jsr FMinus
		jsr F10Star
		ldy FIndex		;  until mantissa==0
		lda FSMant0,y
		bne _c

_d:		dec _decimalPos,x	;do trailing zeros
		bmi _d9
		lda #'0'
		jsr pfch
		jmp _d
_d9:

		inx			; drop work area
		inx
		inx
		inx
		inc FIndex		; FDrop
		rts
	WordEnd


FLt10:	; scale down to <10, counting exponent
_1:		ldy FIndex		; while r >= 10
		lda FSExp,y
		bmi _9			;   exponent negative?
		cmp #4
		bcc _9			;   < 4 ?
		bne _2			;   > 4 ?
		lda FSMant0,y
		cmp #$50
		bcc _9
_2:		inc DStack+0,x		;   exp ++
		jsr F10			;   r /= 10
		jsr FSlash
		jmp _1
_9:		rts


; ------------
; character storage in Pad

pfcst: ; ( -- ) ( F: r1 -- r2 )  Start
		lda #1
		sta ToHold

		ldy FIndex		; if r1 negative
		lda FSMant0,y
		bpl _19
		lda #'-'		;   append sign
		jsr pfch
		jmp FNegate		;   make positive, return

_19:		rts

pfch: ; ( A -- )  Append a char
		ldy ToHold
		sta (cp),y
		inc ToHold
		rts

pfcen: ; ( -- adr )  End, returns counted string
		lda ToHold
		sec
		sbc #1
		ldy #0
		sta (cp),y		; fill in length
		jmp Here		; push start addr

pfciA: ; ( A -- )  Append an integer
		tay
		bpl _15			; IfMi,
		eor #$ff
		clc
		adc #1
		pha
		lda #'-'
		bne _19
_15:		pha
		lda #'+'
_19:
		jsr pfch
		pla
		ldy #'0'-1
_20:		iny
		sec
		sbc #10
		bcs _20
		adc #10
		pha
		tya
		jsr pfch
		pla
		ora #'0'
		bne pfch


 WordHeader "F.",NN ; ( F: r -- )  Print r in fixed-point notation
  ; https://forth-standard.org/standard/float/Fd
FDot:		jsr PFDot
FDot2:		jsr Count
		jsr Type
		jmp Space
	WordEnd


 WordHeader "F.S",NN ; ( -- )  Non-destructive print of the float stack contents.
FDotS:		lda #FDim-1	; for each FP stack entry
		bne _8
_2:		pha

		jsr FPick3	;   print it
		jsr FDot

		pla		;  next
		sec
		sbc #1
_8:		cmp FIndex
		bcs _2
	WordEnd
		rts


 WordHeader "(FS.)",NN ; ( F: r -- adr )  Convert r to scientific notation string
PFSDot:		jsr Zero		; alloc work area
_exp    = DStack+0

		jsr pfcst		; start collecting chars, make r positive

		jsr FLt10		; scale down to <10

_c1:		ldy FIndex		; while r < 1
		lda FSMant0,y		;   mantissa == 0 ?
		beq _c9
		lda FSExp,y		;   exp < 1 ?
		beq _c2
		bpl _c9
_c2:		dec DStack+0,x		;  exp -= 1
		jsr F10Star		;  r *= 10
		jmp _c1
_c9:

PFSDotM:	jsr PFDotSub		; do mantissa

		lda #'E'
		jsr pfch		; do exponent
		lda DStack+0,x		;   _exp
		jsr pfciA

		inx			; Drop work area
		inx
		jmp pfcen		; finish string
	WordEnd


 WordHeader "FS.",NN ; ( F: r -- )  Print r in scientific notation.
  ; https://forth-standard.org/standard/float/FSd
FSDot:		jsr PFSDot
		jmp FDot2
	WordEnd


 WordHeader "(FE.)",NN ; ( F: r -- adr )  Convert r to engineering notation string
PFEDot:		jsr Zero		; alloc work area
_exp    = DStack+0

		jsr pfcst		; start collecting chars, make r positive

_30:		ldy FIndex		; while r < 1
		lda FSMant0,y
		beq _39
		lda FSExp,y
		beq _31
		bpl _39
_31:		dec _exp,x		;  exp -= 3
		dec _exp,x
		dec _exp,x
		jsr F1000		;  r *= 1000
		jsr FStar
		jmp _30
_39:

_a:		ldy FIndex		; while r >= 1000
		lda FSExp,y
		bmi _a9			; exponent negative?
		cmp #10
		bcc _a9			;   < 10 ?
		bne _a2			;   > 10 ?
		lda FSMant0,y
		cmp #$7d
		bcc _a9
_a2:		inc _exp,x		;   exp += 3
		inc _exp,x
		inc _exp,x
		jsr F1000		;   r /= 1000
		jsr FSlash
		jmp _a
_a9:

		jmp PFSDotM
	WordEnd

 WordHeader "FE.",NN ; ( F: r -- )  Print r in engineering format
  ; https://forth-standard.org/standard/float/FEd
  ; Display, with a trailing space, the top number on the floating-point stack using engineering notation, where
  ; the significand is greater than or equal to 1.0 and less than 1000.0 and the decimal exponent is a multiple
  ; of three. 
  ;
  ; An ambiguous condition exists if the value of Base is not (decimal) ten or if the character string
  ; representation exceeds the size of the pictured numeric output string buffer. 
FEDot:		jsr PFEDot
		jmp FDot2
	WordEnd


 WordHeader 'F"',IM+NN ; ( "string" -- ) ( F: -- r )  get inline string as float
FQuote:		jsr Parse_Name		; get string
		jsr ToFloat		; convert
		lda DStack+0,x		; error?
		beq _Err
		inx
		inx

		lda State		; compiling?
		bne _compile
		rts

_compile:	jmp FLiteral		; compile a FP literal

_Err:		lda #100+err_FPInvalidArg
		jsr ThrowA
	WordEnd


 WordHeader ">Float",NN ; ( F: -- r ) ( c-addr u -- true | false)  Convert string to FP
  ; https://forth-standard.org/standard/float/toFLOAT
  ; An attempt is made to convert the string specified by c-addr and u to internal floating-point representation.
  ; If the string represents a valid floating-point number in the syntax below, its value r and true are returned.
  ; If the string does not represent a valid floating-point number only false is returned. 
  ;
  ; A string of blanks should be treated as a special case representing zero.
  ;
  ; The syntax of a convertible string := <significand>[<exponent>] 
  ;
  ; <significand> := [<sign>]{<digits>[.<digits0>] | .<digits> }
  ; <exponent>    := <marker><digits0>
  ; <marker>      := {<e-form> | <sign-form>}
  ; <e-form>      := <e-char>[<sign-form>]
  ; <sign-form>   := { + | - }
  ; <e-char>      := { D | d | E | e }
  ;
ToFloat:
		jsr Zero	; alloc & init workspace
		jsr Zero
		jsr Zero
_addr		= DStack+8
_len		= DStack+6
_mantfound 	= DStack+5		;mantissa digits found
_exponent 	= DStack+4		;exponent
_decimalPos 	= DStack+3		;decimal point position
_DecPointFound1 = DStack+2
_MantissaNegative1 = DStack+1
_ExponentNegative1 = DStack+0

		jsr F0			; init result

_11:		jsr _GetChar		; get mantissa prefix
		bcs _trueb ;_finishb
		cmp #' '
		beq _11
		cmp #'+'
		beq _20
		cmp #'-'
		bne _21
		sta _MantissaNegative1,x ; remember mantissa is negative

_20:		jsr _GetChar		; do next mantissa digits
		bcs _finishb
_21:		cmp #'.'
		beq _27
		cmp #'E'
		beq _30
		cmp #'e'
		beq _30
		cmp #'D'
		beq _30
		cmp #'d'
		beq _30
		sec			;   a digit?
		sbc #'0'
		bcc _fail
		cmp #9+1
		bcs _fail
		pha			; append digit to mantissa
		jsr F10Star
		pla
		ldy #0
		jsr SToFYA
		jsr FPlus
		inc _mantfound,x	; remember we found a mantissa digit
		lda _DecPointFound1,x	; if mantissa decimal point found
		beq +
		dec _decimalPos,x	;   increment decimal point position
+
		jmp _20

_27:	; found a decimal point
		ldy _DecPointFound1,x	; already have one?
		bne _fail
		sta _DecPointFound1,x	; we have one now!
		beq _20

_fail: ; conversion failed
		jsr FDrop
		lda #0			;return false
		beq _return

_trueb:		jmp _true
_finishb:	jmp _finish

_30:	; 'E' or 'D' found, get exponent prefix
		jsr _GetChar
		bcs _finish
		cmp #'+'
		beq _40
		cmp #'-'
		bne _41
		sta _ExponentNegative1,x ; remember exponent is negative

_40:		jsr _GetChar		;get exponent digits
		bcs _finish
_41:		sec			;  digit?
		sbc #'0'
		bcc _fail
		cmp #9+1
		bcs _fail
		sta tmp1
		asl _exponent,x		;  exponent *= 10
		lda _exponent,x
		asl a
		asl a
		adc _exponent,x
		adc tmp1		;  + digit
		sta _exponent,x
		jmp _40

_finish: ; all chars processed
		lda _mantfound,x	;some mantissa digits found?
		beq _fail

		lda _ExponentNegative1,x ;apply exponent sign
		beq +
		lda #0
		sec
		sbc _exponent,x
		sta _exponent,x
+
		lda _exponent,x		;apply decimal position to exponent
		clc
		adc _decimalPos,x
		sta _exponent,x
		jmp _93

_93b:		jsr F10Star		;apply exponent to mantissa
		dec _exponent,x
_93:		beq _94
		bpl _93b

_94b:		jsr F10
		jsr FSlash
		inc _exponent,x
_94:		bmi _94b

_95:
		lda _MantissaNegative1,x ;apply mantissa sign
		beq +
		jsr FNegate
+
	;	jsr FNorm
_true:		lda #$ff		;return true

_return:
		sta _addr+0,x		; replace _addr with flag
		sta _addr+1,x
		inx			; Drop work
		inx
		inx			; 2Drop work
		inx
		inx
		inx
		inx			; Drop len
		inx
		rts


_GetChar:
		sec
		lda _len,x
		beq _gc_rts		; if end, return C=1
		dec _len,x
		lda (_addr,x)
		inc _addr+0,x
		bne +
		inc _addr+1,x
+
		clc			; get char, return C=0
_gc_rts:	rts



 WordHeader "FKey",NN ; ( F: -- r )  get float from console
  ; Like BASIC's INPUT for 1 float.
  ; Not standard, but useful for application programs.
FKey:	;	lda base		; save base
	;	pha
	;	jsr Decimal
		jsr Here		; ( addr )
		jsr Here		; ( addr addr )
		lda #40			; ( addr addr 40 )
		jsr PushZA
		jsr Accept		; ( addr len )
		jsr ToFloat		; ( true | false)
	;	pla			; restore base
	;	sta base
		inx			; err?
		inx
		lda DStack-2,x
		beq _err
		rts

_err:		jsr SLiteral_runtime
		  jmp +
		  .text " ? "
+		jsr Type
		jmp FKey


 .if "fpdo" in TALI_OPTIONAL_WORDS ;------------------------------------------------------
; Floating-point Do +Loop

 .section bss
DoFData: .fill 5*2*FDim ; index & limit, should be in RAM
 .endsection bss

 WordHeader "FI'",NN ; ( r: r1 r2 -- r1 ) ( F: -- r1 )  get float loop limit
FIQuote:	lda #1
		bne FIA
	WordEnd

 WordHeader "FJ",NN ; ( r: r1 r2 r3 -- r1 r2 r3 ) ( F: -- r1 )  get 2nd float loop index
FJ:		lda #2
		bne FIA
	WordEnd

 WordHeader "FI",NN ; ( r: r -- r ) ( F: -- r )  get float loop index
FI:		lda #0
FIA:		jsr DoFGetPtr
		jmp FAt_YA
	WordEnd


DoFGetPtr: ; get ptr to DoFData entry
	; in: A=entry index
	; out: YA=ptr
		clc
		adc DoStkIndex
		sta tmp1+0
		asl a
		asl a
		adc tmp1+0
		adc #<DoFData
		pha
		lda #0
		adc #>DoFData
		tay
		pla
		rts


 WordHeader "FDo",IM+NN ; ( F: rlimit rstart -- )  start float DO loop
FDo:	
		dec DoStkIndex		; alloc DO stack entry
		ldy DoStkIndex
		bmi _TooDeep

		jsr Do_Leave_Init

		lda #<_Runtime
		ldy #>_Runtime
		jsr Jsr_Comma_YA	; compile jsr _Runtime

		jsr Here		; remember loop body start addr

		lda #<FDo		; identifier
		jmp PushZA
					; ( saved_DoLeave loopback_Addr id )

_TooDeep:	lda #$100+err_DoLoop_TooDeep
		jsr ThrowA

_Runtime:
		lda #0		; store index
		jsr DoFGetPTr
		jsr FStore_YA
		lda #1		; store limit
		jsr DoFGetPtr
		jmp FStore_YA


 WordHeader "F+Loop",IM+NN ; ( F: r -- )  end FDO loop
FPlusLoop:
		lda #<_Runtime		; compile JSR _Runtime
		ldy #>_Runtime
		jsr Jsr_Comma_YA

		lda #<FDo		; check id
		jsr Loop_End_3

		lda #>FUnloop
		ldy #<FUnloop
		jmp Jsr_Comma_A		; compile jsr FUnloop
	WordEnd

_Runtime:
		jsr FI		; add step
		jsr FPlus
		jsr FIQuote	; compare
		jsr FCmpA
		bpl _RunDone
		inc FIndex	; FDrop limit
		lda #0		; store new index
		jsr DoFGetPtr
		jsr FStore_YA
		clv		; signal loop back
		rts

_RunDone:	inc FIndex	; FDrop limit
		inc FIndex	; FDrop index
		sev		; signal loop exit
		rts

 WordHeader "FUnloop",0 ( F: -- )  discard
FUnloop:	inc DoStkIndex
	WordEnd
		rts

 .endif ; fpdo


 .if "fpieee" in TALI_OPTIONAL_WORDS ;-------------------------------------------------

; --------------------------------------------------------------------------------
; 32-bit IEEE single-precision number format:
;   seeeeeee emmmmmmm mmmmmmm mmmmmmm
;   s=sign (at msb)
;   e=exponent (+128)
;   m=mantissa (leading 1 assumed) (at lsb)
; $3e200000 = 0.15625,  $c2ed4000 = -118.625

 WordHeader "SF@",NN ; ( sf-addr -- ) ( F: -- r )  Fetch IEEE single
  ; https://forth-standard.org/standard/float/SFFetch
  ; Fetch the 32-bit IEEE single-precision number stored at sf-addr to the floating-point stack as r
  ; in the internal; representation. If the IEEE single-precision significand has more precision than
  ; the internal representation, it will be rounded to the internal representation using the round to
  ; nearest rule. An ambiguous condition exists if the exponent of the IEEE single-precision
  ; representation is too large to be accommodated by the internal representation. 
SFAt:		jsr PopYA		; pop sf_addr
SFAt_YA:	sta tmp2+0		; save sf_addr
		sty tmp2+1

		jsr FAllocX		; alloc FP stack entry

		ldy #1			; copy exponent
		lda (tmp2),y
		asl a
		dey
		lda (tmp2),y
		rol a
		sec
		sbc #126
		sta FSExp,x

		ldy #1			; copy mantissa
		lda (tmp2),y
		ora #$80
		lsr a
		sta FSMant0,x
		iny
		lda (tmp2),y
		ror a
		sta FSMant1,x
		iny
		lda (tmp2),y
		ror a
		sta FSMant2,x
		lda #0
		ror a
		sta FSMant3,x

		ldx tmp1+0		; restore data stack index

		ldy #0			; get sign
		lda (tmp2),y
		bpl +
		jsr FNegate
+
	WordEnd
		rts

 WordHeader "SF!",NN ; ( F: r -- ) ( sf-addr -- )  Store FP as IEEE single
  ; https://forth-standard.org/standard/float/SFStore
  ; Store the floating-point number r as a 32-bit IEEE single-precision number at sf-addr.
  ; If the significand of the internal representation of r has more precision than the IEEE single-precision
  ; format, it will be rounded using the round to nearest rule. An ambiguous condition exists if the exponent
  ; of r is too large to be accommodated by the IEEE single-precision format. 
SFStore:	jsr PopYA		; pop sf_addr
SFStore_YA:	sta tmp2+0		; save
		sty tmp2+1

		ldy FIndex		; negative?
		lda FSMant0,y
		and #$80		;  save sign
		sta tmp3+1
		bpl +
		jsr FNegate
+

		stx tmp1+0		; save data stack index
		ldx FIndex		; switch to FP stack index

		lda FSExp,x		; +128
		clc
		adc #126
		sta FSExp,x

		asl FSMant3,x
		lda FSMant2,x
		rol a
		ldy #3			; +3 IEEE
		sta (tmp2),y
		lda FSMant1,x
		rol a
		dey			; +2 IEEE
		sta (tmp2),y
		lda FSMant0,x
		rol a
		rol a
		lsr FSExp,x
		ror a
		dey			; +1 IEEE
		sta (tmp2),y

		lda FSExp,x
		ora tmp3+1		;  insert sign
		dey			; +0 IEEE
		sta (tmp2),y

		inc FIndex		; FDrop

		ldx tmp1+0		; restore data stack index
	WordEnd
		rts
; zero???
; overflow???

; WordHeader "SFAlign",NN ; ( -- )  Align compile ptr for IEEE single
  ; https://forth-standard.org/standard/float/SFALIGN
  ; If the data-space pointer is not single-float aligned, reserve enough data space to make it so. 
  ; Even though 6502 doesn't care, should we align to make structures compatible to other machines?
;SFAlign:	lda cp+0
;		and #3
;		beq _9
;		jsr C_Comma_A
;		jmp SFAlign
;	WordEnd
;_9:		rts

; WordHeader "SFAligned",NN ; ( addr -- sf-addr )  Align addr for IEEE single
  ; sf-addr is the first single-float-aligned address greater than or equal to addr. 
  ; https://forth-standard.org/standard/float/SFALIGNED
  ; Even though 6502 doesn't care, should we align to make structures compatible to other machines?
;SFAligned:	lda #4-1
;		jsr Plus_A
;		lda DStack+0,x
;		and #$fc
;		sta STack+0,x
;	WordEnd
;_9:		rts


 WordHeader "SFloat+",NN ; ( sf-addr1 -- sf-addr2 )  Add size of IEEE single to addr
  ; Add the size in address units of a 32-bit IEEE single-precision number to sf-addr1, giving sf-addr2. 
  ; https://forth-standard.org/standard/float/SFLOATPlus
SFloatPlus:	lda #4
		jmp Plus_A
	WordEnd


 WordHeader "SFloats",NN ; ( n1 -- n2 )  
  ; n2 is the size in address units of n1 32-bit IEEE single-precision numbers.
  ; https://forth-standard.org/standard/float/SFLOATS
SFloats:	lda #2
		jmp LShift_A
	WordEnd

; --------------------------------------------------------------------------------
; 64-bit IEEE double-precision number format:
;   seeeeeee eeeemmmm mmmmmmm mmmmmmm mmmmmmmm mmmmmmmm mmmmmmmm mmmmmmmm
;   s=sign (at msb)
;   e=exponent (11 bits) (+1023)
;   m=mantissa (leading 1 assumed) (at msb)
; $3e200000 = 0.15625,  $c2ed4000 = -118.625

 WordHeader "DF!",NN ; ( F: r -- ) ( df-addr -- )  Store FP in IEEE double format
  ; Store the floating-point number r as a 64-bit IEEE double-precision number at df-addr.
  ; If the significand of the internal representation of r has more precision than the IEEE double-precision
  ; format, it will be rounded using the round to nearest rule. An ambiguous condition exists if the exponent
  ; of r is too large to be accommodated in IEEE double-precision format. 
  ; https://forth-standard.org/standard/float/DFStore
DFStore:	jsr PopYA		; pop df_addr
DFStore_YA:	sta tmp2+0		; save df_addr
		sty tmp2+1

		ldy FIndex		; negative?
		lda FSMant0,y
		and #$80		; remember sign
		sta tmp3+1
		bpl +			; make positive
		jsr FNegate
+
		stx tmp1+0		; save data stack index
		ldx FIndex

		lda #0
		ldy #7			; +7 IEEE
		sta (tmp2),y
		dey			; +6 IEEE
		sta (tmp2),y

		jsr _shift		; shift mantissa
		jsr _shift

		dey			; +5 IEEE
		sta (tmp2),y
		lda FSMant3,x
		dey			; +4 IEEE
		sta (tmp2),y
		lda FSMant2,x
		dey			; +3 IEEE
		sta (tmp2),y
		lda FSMant1,x
		dey			; +2 IEEE
		sta (tmp2),y

		lda FSMant0,x		; clear extra bits
		and #$f
		sta FSMant0,x

		lda FSExp,x
		sec
		sbc #2
		sta FSExp,x
		asl a
		asl a
		asl a
		asl a
		ora FSMant0,x
		dey			; +1 IEEE
		sta (tmp2),y

		lda FSExp,x
		php
		lsr a
		lsr a
		lsr a
		lsr a
		clc
		adc #$30
		plp
		bmi +
		clc
		adc #$10
+		ora tmp3+1		; add sign bit
		dey			; +0 IEEE
		sta (tmp2),y

		inc FIndex		; FDrop
		ldx tmp1+0		; restore data stack index
		rts		

_shift:		lsr FSMant0,x
		ror FSMant1,x
		ror FSMant2,x
		ror FSMant3,x
		ror a
	WordEnd
		rts


 WordHeader "DF@",NN ; ( df-addr -- ) ( F: -- r )  Fetch IEEE double
  ; Fetch the 64-bit IEEE double-precision number stored at df-addr to the floating-point stack as r in
  ; the internal representation. If the IEEE double-precision significand has more precision than the internal
  ; representation it will be rounded to the internal representation using the round to nearest rule. An
  ; ambiguous condition exists if the exponent of the IEEE double-precision representation is too large to be
  ; accommodated by the internal representation. 
  ; https://forth-standard.org/standard/float/DFFetch
DFAt:		jsr PopYA		; pop df_addr
DFAt_YA:	sta tmp2+0		; save df_addr
		sty tmp2+1
		jsr FAllocX		; alloc FP stack entry

		ldy #0			; +0 IEEE byte
		lda (tmp2),y
		sta FSExp,x
		iny			; +1 IEEE byte
		lda (tmp2),y
		sta FSMant0,x
		iny			; +2 IEEE byte
		lda (tmp2),y
		sta FSMant1,x
		iny			; +3 IEEE byte
		lda (tmp2),y
		sta FSMant2,x
		iny			; +4 IEEE byte
		lda (tmp2),y
		sta FSMant3,x

		iny			; +5 IEEE byte
		lda (tmp2),y
		jsr _shifta		; shift exponent & mantissa bits into place
		jsr _shifta

		lda FSMant0,x		; finish shifting exponent bits
		asl a
		rol FSExp,x
		asl a
		rol FSExp,x

		lda FSExp,x		; remove +1023
		clc
		adc #2
		sta FSExp,x

		lda FSMant0,x
		and #$7f
		ora #$40		;   insert implied 1
		sta FSMant0,x		;   insert sign=0

		ldx tmp1		; restore data stack index

		ldy #0			; get sign from +0 IEEE byte
		lda (tmp2),y
		bpl +
		jsr FNegate
+
		rts

_shifta:	asl a
		rol FSMant3,x
		rol FSMant2,x
		rol FSMant1,x
		rol FSMant0,x
		rol FSExp,x
		rts
	WordEnd


; WordHeader "DFAlign",NN ; ( -- )  Align compile ptr for IEEE double
  ; If the data-space pointer is not double-float aligned, reserve enough data space to make it so. 
  ; https://forth-standard.org/standard/float/DFALIGN
  ; Even though 6502 doesn't care, should we align to make structures compatible to other machines?
;DFAlign:	lda cp+0
;		and #7
;		beq _9
;		jsr C_Comma_A
;		jmp DFAlign
;	WordEnd
;_9:		rts

; WordHeader "DFAligned",0 ; ( addr -- df-addr )  Align addr for IEEE double
  ; df-addr is the first double-float-aligned address greater than or equal to addr. 
  ; https://forth-standard.org/standard/float/DFALIGNED
  ; Even though 6502 doesn't care, should we align to make structures compatible to other machines?
;DFAligned:	lda #8-1
;		jsr Plus_A
;		lda DStack+0,x
;		and #$f8
;		sta DStack+0,x
;	WordEnd
;_9:		rts


 WordHeader "DFloat+",NN ; ( df-addr1 -- df-addr2 )
  ; Add the size in address units of a 64-bit IEEE double-precision number to df-addr1, giving df-addr2. 
  ; https://forth-standard.org/standard/float/DFLOATPlus
DFloatPlus:	lda #8
		jmp Plus_A
	WordEnd

 WordHeader "DFloats",NN ; ( n1 -- n2 )
  ; n2 is the size in address units of n1 64-bit IEEE double-precision numbers. 
  ; https://forth-standard.org/standard/float/DFLOATS
DFloats:	lda #3
		jmp LShift_A
	WordEnd

 .endif ; "fpieee"


 .if "fptrancendentals" in TALI_OPTIONAL_WORDS ;------------------------------------------
; Float transendental functions

; -------------------------------------------------------------
; minimax
; http://www.geometrictools.com/Source/Functions.html
; http://www.research.scea.com/gdc2003/fast-math-functions.html


 WordHeader "FMPoly",NN ; ( YA=^tbl -- ) ( F: rx -- rx rpoly )  shared polynomial evaluation
FMPoly:		jsr PopYA
FMPolyYA:	jsr PushYA		; push coefficent addr
		jsr FAt_YA		; fetch 1st coefficent
		jmp _4

_2:		jsr FOver
		jsr FStar
		lda DStack+0,x		; fetch next coefficent
		ldy DStack+1,x
		jsr FAt_YA
		jsr FPlus
_4:		lda #5			; bump coefficent ptr
		jsr Plus_A
		lda (DStack+0,x)	; end of coefficent list?
		bne _2
		inx			; Drop coefficent ptr
		inx
	WordEnd
		rts


 WordHeader "FLog2M1M",NN ; ( r -- r )  Minimax polynomial approximations for log2(x)
  ; the logarithm base 2 of x, for x in [1,2].
  ; Range reduction is used for x outside [1,2] to obtain approximations to log2(x) for any x > 0.
FLog2M1M:	lda #<_c
		ldy #>_c
		jsr FMPolyYA
		jmp FStar
	WordEnd

_c: ; #define GTE_C_LOG2_DEG7_MAX_ERROR 3.2546531700261561e-7
		FloatLit $7C97CFCC,-6 ;C7 +1.5209108363023915e-02
		FloatLit $AF948764,-3 ;C6 -7.8534970641157997e-02
		FloatLit $62D4C866,-2 ;C5 +1.9302965529095673e-01
		FloatLit $ACC39CE2,-1 ;C4 -3.2514018752954144e-01
		FloatLit $792BC61A,-1 ;C3 +4.7332419162501083e-01
		FloatLit $A3C4E107,0  ;C2 -7.2055423726162360e-01
		FloatLit $5C54A591,1  ;C1 +1.4426664401536078
		.word 0

 WordHeader "FLog2",NN ; ( F: r1 -- r2 )  base 2 log
FLog2:		ldy FIndex
		lda FSMant0,y	; bad param?
		bmi _OutOfRange
		beq _OutOfRange
		lda FSExp,y
		pha		; remember orig exponent
		lda #1
		sta FSExp,y	; set to 1

		jsr F1Minus
		jsr FLog2M1M

		ldy #0		; add orig exponent to float
		pla
		sec
		sbc #1
		bpl +
		dey
+
		jsr SToFYA
		jmp FPlus

_OutOfRange:	jmp Throw_FpOutOfRange
	WordEnd

 WordHeader "FLn",NN ; ( F: r1 -- r2 )  (base e) natural log
  ; https://forth-standard.org/standard/float/FLN
FLn:		jsr FLog2
		FloatLitI $58b90bf8,0	;1/ln(2)
		jmp FStar
	WordEnd

 WordHeader "FLnP1",NN ; ( F: r1 -- r2 ) (base e) natural log
  ; https://forth-standard.org/standard/float/FLNPOne
FlnP1:		jsr F1Plus
		jmp FLn
	WordEnd

 WordHeader "FLog",NN ; ( F: r1 -- r2 )  (base 10) common log
  ;  https://forth-standard.org/standard/float/FLOG
  ; r2 is the Base-ten logarithm of r1. An ambiguous condition exists if r1 <=0.
FLog:		jsr FLog2
		FloatLitI $4d104d3c,-1	; 1/Log(2)
		jmp FStar
	WordEnd


 WordHeader "FExp2M1M",NN ; ( F: r1 -- r2 )  2^r1-1 for r1 in [0,1]
  ; Minimax polynomial approximations for exp2(x)-1
  ; Use range reduction for x outside [0,1] to obtain approximations to exp2(x) for any x >= 0.
FExp2M1M:	lda #<_c
		ldy #>_c
		jsr FMPolyYA
		jmp FStar
	WordEnd

_c: ; GTE_C_EXP2_DEG5_MAX_ERROR 1.3162098333463490e-7
		FloatLit $7C4FDCe7,-9 ;C5 1.8968500441332026e-03
		FloatLit $494CCADe,-6 ;C4 8.9477503096873079e-03
		FloatLit $726442fb,-4 ;C3 5.5855296413199085e-02
		FloatLit $7AF49050,-2 ;C2 2.4014712313022102e-01
		FloatLit $58B93Ca2,0  ;C1 6.9315298010274962e-01
		.byte 0

 WordHeader "FExp2",NN ; ( F: r1 -- r2 )  base 2 exponential
FExp2:		jsr FIntFrc	; ( f: r_rem r_int )
		jsr FToS
		jsr FExp2M1M
		jsr F1Plus
		jsr PopA
		ldy FIndex
		clc
		adc FSExp,y
		sta FSExp,y
	WordEnd
		rts

 WordHeader "FExp",NN ; ( F: r1 -- r2 )  base e (natural) exponential
  ; Raise e to the power r1, giving r2. 
  ; https://forth-standard.org/standard/float/FEXP
FExp:		FloatLitI $5c551d94,1	;1/ln(2)
		jsr FStar
		jmp FExp2
	WordEnd

 WordHeader "FExpM1",NN ; ( F: r1 -- r2 ) base e (natural) exponential
  ; No better accuracy in this implementation, but here for standard completeness.
  ; https://forth-standard.org/standard/float/FEXPMOne
FExpM1:		jsr FExp
		jmp F1Minus
	WordEnd

 WordHeader "FALog",NN ; ( F: r1 -- r2 )  base 10 (common) exponential
  ; Raise ten to the power r1, giving r2. 
  ; https://forth-standard.org/standard/float/FALOG
FALog:		FloatLitI $6a4d3c25,2	;1/log(2)
		jsr FStar
		jmp FExp2
	WordEnd


 WordHeader "F**",NN ; ( F: r1 r2 -- r3 )  Raise r1 to the power r2 
  ; https://forth-standard.org/standard/float/FTimesTimes
FPower:		jsr FSwap
		jsr FLog2
		jsr FStar
		jmp FExp2
	WordEnd


 WordHeader "Deg2Rad",NN ; ( F: r1 -- r2 )  Convert degrees to radians.
Deg2Rad:	FloatLitI $477d1A8A,-5	; PI/180
		jmp FStar
	WordEnd

 WordHeader "Rad2Deg",NN ; ( F: r1 -- r2 )  Convert radians to degrees.
Rad2Deg:	FloatLitI $72977068,6	; 180/PI
		jmp FStar
	WordEnd


 WordHeader "FAReduce",NN ; ( F: r1 -- r2 )  C=mirrored around pi/2
  ; Trig: reduce angle to -pi/2..pi/2
FAReduce:	ldy FIndex
		lda FSMant0,y		; zero?
		beq _ok
		lda FSExp,y		; get exponent
		bmi _ok			; small?
		cmp #1			; maybe need mirror?
		bmi _ok			;if exponent <=0
		beq _ok
		cmp #2			; maybe need rotation?
		bcs _exp2
		lda FSMant0,y
		cmp #$65		;in -pi/2..pi/2 ?
		bcc _ok
		cmp #$100-$65
		bcs _ok
_mir:					; mirror angle around +-pi/2
		jsr FPi
		ldy FIndex
		lda FSMant0+1,y
		bpl _3
		jsr FNegate
_3:		jsr FSwap
		jsr FMinus
		sec		; mirrored
		rts

_ok:		clc		; not mirrored
		rts

_exp2:		bne _rot	;if exp>2 then fix
		lda FSMant0,y
		cmp #$65	;if mant>pi or mant<-pi then fix
		bcc _mir
		cmp #$100-$65
		bcs _mir
_rot:		jsr F2Pi
		jsr FSlash
		jsr FIntFrc
		inc FIndex	; FDrop integer part
		ldy FIndex	;if >=.5
		lda FSExp,y
		tay
		bmi _rot4
		jsr F1Minus	;  subtract 1
_rot4:		jsr F2Pi
		jsr FStar
		jmp FAReduce	; rotate done, look again
	WordEnd


 .if 0
_1:
		stx tmp1		; switch to FP stack
		ldx FIndex
		lda FSMant0,x		; get hi mantissa
		ldy FSExp,x		; get exponent
		bmi _ok1		;if exponent <=0
		beq _ok1
		ldx tmp1
		cpy #2
		bcs _exp2
		cmp #$65	;in -pi/2..pi/2 ?
		bcc _ok2
		cmp #$100-$65
		bcs _ok2
_mir:		pha		; mirror angle around +-pi/2
		jsr FPi
		pla
		bpl _3
		jsr FNegate
_3:		jsr FSwap
		jsr FMinus
		sec
		rts

_ok1:		ldx tmp1
_ok2:		clc
		rts

_exp2:		bne _fix	;if exp>2 then fix
		cmp #$65	;if mant>pi or mant<-pi then fix
		bcc _mir
		cmp #$100-$65
		bcs _mir
_fix:		jsr F2Pi
		jsr FSlash
		jsr FIntFrc
		jsr FNip
		ldy FIndex	;if >=.5
		lda FSExp,y
		tay
		bmi _fix4
		jsr F1Minus		;  subtract 1
_fix4:		jsr F2Pi
		jsr FStar
		jmp _1
	WordEnd
 .endif

 WordHeader "FSin",NN ; ( F: r1 -- r2 )  sine
  ; https://forth-standard.org/standard/float/FSIN
  ; Range reduction for x outside [-pi/2,pi/2] is obtained using periodicity and trigonometric identities.
FSin:		jsr FAReduce
		jmp FSinM
	WordEnd

; WordHeader "FSinM",NN ; ( F: r1 -- r2 )   sin(x) for x in [-pi/2,pi/2].
  ; Minimax polynomial approximation
FSinM:		jsr FDup
		jsr FSqr	; x x^2
		lda #<_c
		ldy #>_c
		jsr FMPolyYA
		jsr FStar
		jsr F1Plus	; C0 +1.0
		jmp FStar
;	WordEnd

_c: ; GTE_C_SIN_DEG9_MAX_ERROR 5.2010746265374053e-9
		FloatLit $5721a7ba,-18	;C4 +2.5967200279475300e-06
		FloatLit $982a0b3a,-12	;C3 -1.9805100675274190e-04
		FloatLit $44438f4c,-6	;C2 +8.3329962509886002e-03
		FloatLit $aaaaae2b,-2	;C1 -1.6666656235308897e-01
		.byte 0

 WordHeader "FCsc",NN ; ( r1 -- r2 )  CoSecant
FCsc:		jsr FSin
		jmp F1Slash		; 1/SIN(r1)
	WordEnd

 WordHeader "FCos",NN ; ( F: r1 -- r2 )  cosine
  ; https://forth-standard.org/standard/float/FCOS
FCos:		jsr FPiH
		jsr FPlus
		jmp FSin
	WordEnd

 WordHeader "FSec",NN ; ( F: r1 -- r2 )  Secant
FSec:		jsr FCos
		jmp F1Slash		; 1/COS(r1)
	WordEnd

 WordHeader "FSinCos",NN ; ( F: r1 -- r2 r3 )
  ; r2 is the sine of the radian angle r1. r3 is the cosine of the radian angle r1. 
  ; https://forth-standard.org/standard/float/FSINCOS
FSinCos:	jsr FDup
		jsr FSin
		jsr FSwap
		jmp FCos
	WordEnd


 WordHeader "FTan",NN ; ( F: r1 -- r2 )  tangent
  ; https://forth-standard.org/standard/float/FTAN
  ; this reduces to abs(r)<=pi/4
FTan:		jsr FAReduce
		bcc _30
		jsr _30
		jmp FNegate

_30:
		jsr FDup
		jsr FAbs
		jsr FPiQ
		jsr FLe
		inx			; pop f
		inx
		lda DStack+0,x
		bne FTanM
		jsr FPiH
		ldy FIndex		; get sign
		lda FSMant0,y
		bpl +
		jsr FNegate
+
		jsr FSwap
		jsr FMinus
		jsr FTanM
		jmp F1Slash
	WordEnd

; WordHeader "FTanM",NN ; ( F: r1 -- r2 )  tan(x) for x in [-pi/4,pi/4].
  ; Minimax polynomial approximation
FTanM:		jsr FDup
		jsr FSqr		; x x^2
		lda #<_c
		ldy #>_c
		jsr FMPolyYA
		jsr FStar
		jsr F1Plus		; c0 1.0
		jmp FStar
;	WordEnd

_c: ; GTE_C_TAN_DEG11_MAX_ERROR 1.5426257940140409e-7
		FloatLit $584DAE1E,-5 ;C5 2.1558456793513869e-02
		FloatLit $57EB0A51,-6 ;C4 1.0732193237572574e-02
		FloatLit $7714D998,-4 ;C3 5.8145237645931047e-02
		FloatLit $43EA10F9,-2 ;C2 1.3264516053824593e-01
		FloatLit $5557E21B,-1 ;C1 3.3337224456224224e-01
		.byte 0


 WordHeader "FCot",NN ; ( r1 -- r2 )  CoTangent
FCot:		jsr FTan
		jmp F1Slash	; =1/TAN(r1)
	WordEnd


 WordHeader "FACos",NN ; ( F: r1 -- r2 )  Inverse cosine
  ; https://forth-standard.org/standard/float/FACOS
FACos:		ldy FIndex
		lda FSMant0,y
		php			; save sign
		jsr FAbs
  ; Minimax approximations for acos(x) of the form f(x) = sqrt(1-x)*p(x) for x in [0,1].
		lda #<_c
		ldy #>_c
		jsr FMPolyYA
		jsr FSwap
		jsr FNegate
		jsr F1Plus
		jsr FSqrt
		jsr FStar
		plp			; was r1 negative?
		bpl +
		jsr FPi
		jsr FSwap
		jsr FMinus
+
	WordEnd
		rts

_c: ; GTE_C_ACOS_DEG6_MAX_ERROR 1.8491291330427484e-7
		FloatLit $4B9F5E8B,-8 ;C6 +2.3078166879102469e-03
		FloatLit $A429CCAD,-6 ;C5 -1.1210537323478320e-02
		FloatLit $6EA3A198,-5 ;C4 +2.7011519960012720e-02
		FloatLit $9BE1105A,-4 ;C3 -4.8887131453156485e-02
		FloatLit $5AEA71C6,-3 ;C2 +8.8784960563641491e-02
		FloatLit $922156E0,-2 ;C1 -2.1458939285677325e-01
		FloatLit $6487ED41,1  ;C0 +1.5707963267948966
		.byte 0

 WordHeader "FASec",NN ; ( F: r1 -- r2 )  Inverse Secant
  ; =ATN(SQR(r1*r1-1))+(SGN(r1)-1)*PI/2
FASec:		jsr F1Slash
		jmp FACos
	WordEnd

 WordHeader "FASin",NN ; ( F: r1 -- r2 )  Inverse sine
  ; https://forth-standard.org/standard/float/FASIN
  ; An ambiguous condition exists if |r1| is greater than one. 
  ; ARCSIN(X)=ATN(X/SQR(X*X+1));
FASin:		jsr FACos
		jsr FNegate
		jsr FPiH
		jmp FPlus
	WordEnd

 WordHeader "FACsc",NN ; ( F: r1 -- r2 )  Inverse CoSecant
  ; =ATN(1/SQR(r1*r1-1))+(SGN(r1)-1)*PI/2;
FACsc:		jsr F1Slash
		jmp FASin
	WordEnd


 WordHeader "FATan",NN ; ( F: r1 -- r2 )  Inverse tangent
  ; https://forth-standard.org/standard/float/FATAN
  ; Range reduction for x outside [-1,1] is obtained by atan(x) = pi/2 - atan(1/x) for x > 0 and
  ; atan(x) = -pi/2 - atan(1/x) for x < 0.
FATan:
		ldy FIndex
		lda FSExp,y		; Abs(r1)<1 ?
		bmi FATanM
		beq FATanM

		lda FSMant0,y		; save sign
		php
		jsr F1Slash
		jsr FATanM
		jsr FNegate
		jsr FPiH
		plp
		bpl _18
		jsr FNegate
_18:		jmp FPlus

; WordHeader "FATanM",NN ; ( F: r1 -- r2 )  atan(x) for x in [-1,1].
  ; Minimax polynomial approximation
FATanM:		jsr FDup		; x x
		jsr FSqr		; x xsqr
		lda #<_c
		ldy #>_c
		jsr FMPolyYA
		jsr FStar		; x poly
		jsr F1Plus		; x poly
		jmp FStar
;	WordEnd

_c: ; GTE_C_ATAN_DEG13_MAX_ERROR 3.5859104691865484e-7
		FloatLit $762D0898,-7 ;C6 +7.2128853633444123e-03
		FloatLit $B832A155,-4 ;C5 -3.5059680836411644e-02
		FloatLit $53A2D7A8,-3 ;C4 +8.1675882859940430e-02
		FloatLit $BB8591BE,-2 ;C3 -1.3374657325451267e-01
		FloatLit $65AA6506,-2 ;C2 +1.9856563505717162e-01
		FloatLit $AAB02112,-1 ;C1 -3.3324998579202170e-01
		.byte 0

 WordHeader "FACot",NN ; ( F: r1 -- r2 )  Inverse CoTangent
  ;  =-ATN(r1)+PI/2
FACot:		jsr F1Slash
		jmp FATan

;		jsr FPiH
;		jsr Swap
;		jsr FATan
;		jmp FMinus
	WordEnd


 WordHeader "FATan2",NN ; ( F: ry rx -- r3 )  2 parameter inverse tangent
  ; r3 is the radian angle whose tangent is ry/rx.
  ; An ambiguous condition exists if ry and rx are zero. 
  ; https://forth-standard.org/standard/float/FATANTwo
FATan2:		ldy FIndex
		lda FSMant0+0,y	; get rx sign
		beq _rxzero
		asl a		;   C=rx sign
		lda FSMant0+1,y	;   N=ry sign
		php		; save signs
		jsr FSlash
		jsr FATan
		plp		; pop signs
		bcs _rxneg
_rts:		rts

_rxneg:		php
		jsr FPi
		plp
		bpl +
		jsr FNegate
+		jmp FPlus

_rxzero:	inc FIndex	; FDrop rx
		lda FSMant0,y
	;	beq Throw_Stack
		php
		inc FIndex	; FDrop ry
		jsr FPiH
		plp
		bpl _rts
		jmp FNegate
	WordEnd


 WordHeader "FSgn",NN ; ( F: r1 -- r2 )  signum, returns 1,0,-1
FSgn:		ldy FIndex
		lda FSMant0,y
		beq _zero
		inc FIndex
		lda FSMant0,y
		bmi FM1
		jmp F1

_zero:		rts
	WordEnd

; WordHeader "-1.e",NN ; ( F: -- r )  return -1
FM1:		lda #$80
		ldy #0
		jmp FLitYA
;	WordEnd


 .if "fphyperbolic" in TALI_OPTIONAL_WORDS ;--------------------------------------------

 WordHeader "FSinH",NN ; ( F: r1 -- r2 )  Hyperbolic sine
  ; =(EXP(r1)-EXP(-r1))/2
  ; https://forth-standard.org/standard/float/FSINH
FSinH:		jsr FDup
		jsr FExp
		jsr FSwap
		jsr FNegate
		jsr FExp
		jsr FMinus
		jmp F2Slash
	WordEnd

 WordHeader "FCscH",NN ; ( F: r1 -- r2 )  Hyperbolic cosecant
  ; =2/(EXP(r1)-EXP(-r1))
FCscH:		jsr FSinH
		jmp F1Slash
	WordEnd

 WordHeader "FCosH",NN ; ( F: r1 -- r2 )  Hyperbolic cosine
  ;  r2 is the hyperbolic cosine of r1. 
  ; =(EXP(X)+EXP(-X))/2
  ; https://forth-standard.org/standard/float/FCOSH
FCosH:		jsr FDup
		jsr FExp
		jsr FSwap
		jsr FNegate
		jsr FExp
		jsr FPlus
		jmp F2Slash
	WordEnd

 WordHeader "FSecH",NN ; ( F: r1 -- r2 )  Hyperbolic secant
  ; =2/(EXP(r1)+EXP(-r1))
FSecH:		jsr FCosH
		jmp F1Slash
	WordEnd

 WordHeader "FTanH",NN ; ( F: r1 -- r2 )  Hyperbolic tangent
  ; =-EXP(-r1)/(EXP(r1)+EXP(-r1))*2+1
  ; https://forth-standard.org/standard/float/FTANH
FTanH:		jsr FDup
		jsr FNegate	; X -X
		jsr FExp	; X Exp(-X)
		jsr FSwap	; Exp(-X) X
		jsr FExp	; Exp(-X) Exp(X)
		jsr FOver	; Exp(-X) Exp(X) Exp(-X)
		jsr FPlus	; Exp(-X) Exp(X)+Exp(-X)
		jsr FSlash	; Exp(-X)/(Exp(X)+Exp(-X))
		jsr F2Star	; Exp(-X)/(Exp(X)+Exp(-X))*2
		jsr FNegate
		jmp F1Plus
	WordEnd

 WordHeader "FCotH",NN ; ( F: r1 -- r2 )  Hyperbolic cotangent
  ; = EXP(-r1)/(EXP(r1)-EXP(-r1))*2+1
FCotH:		jsr FTanH
		jmp F1Slash
	WordEnd


 WordHeader "FASinH",NN ; ( F: r1 -- r2 )  Inverse hyperbolic sine
  ; r2 is the floating-point value whose hyperbolic sine is r1.
  ; An ambiguous condition exists if r1 is less than zero. 
  ; =LOG(X+SQR(X*X+1))
  ; https://forth-standard.org/standard/float/FASINH
FASinH:		jsr FDup	; X X
		jsr FSqr	; X X*X
		jsr F1Plus	; X X*X+1
		jsr FSqrt	; X sqrt(X*X+1)
		jsr FPlus	; 
		jmp FLn
	WordEnd

 WordHeader "FACscH",NN ; ( F: r1 -- r2 )  Inverse hyperbolic cosecant
  ; =LOG((SGN(r1)*SQR(r1*r1+1)+1)/r1)
FACscH:		jsr F1Slash
		jmp FASinH
	WordEnd

 WordHeader "FACosH",NN ; ( F: r1 -- r2 )  inverse hyperbolic cosine
  ; An ambiguous condition exists if r1 is less than one. 
  ; =LOG(X+SQR(X*X-1))
  ; https://forth-standard.org/standard/float/FACOSH
FACosH:		jsr FDup
		jsr FSqr
		jsr F1Minus
		jsr FSqrt
		jsr FPlus
		jmp FLn
	WordEnd

 WordHeader "FASecH",NN ; ( F: r1 -- r2 )  Inverse hyperbolic secant
  ; =LOG((SQR(X*X+1)+1)/X)
FASecH:		jsr F1Slash
		jmp FACosH
	WordEnd

 WordHeader "FATanH",NN ; ( F: r1 -- r2 )  Inverse hyperbolic tangent
  ;  r2 is the floating-point value whose hyperbolic tangent is r1.
  ; An ambiguous condition exists if r1 is outside the range of -1E0 to 1E0. 
  ; =LOG((1+X)/(1-X))/2
  ; https://forth-standard.org/standard/float/FATANH
FAtanH:		jsr FDup	; x x
		jsr F1Plus	; x x+1
		jsr FSwap	; 1+x x
		jsr F1
		jsr FSwap
		jsr FMinus	; 1+x 1-x
		jsr FSlash	; (1+x)/(1-x)
		jsr FLn
		jmp F2Slash
	WordEnd

 WordHeader "FACotH",NN ; ( F: r1 -- r2 )  Inverse hyperbolic cotangent
  ; =LOG((r1+1)/(r1-1))/2
FACotH: 	jsr F1Slash
		jmp FATanH
	WordEnd

 .endif ; fphyperbolic

 .endif ; fptransendentals

 .endsection code

 .endif ; fp


 .section code


.if "ed" in TALI_OPTIONAL_WORDS

 WordHeader "Ed",NN ; ( -- u ) "Line-based editor"
; ## "ed"  fragment  Tali Forth
	; """Start the line-based editor ed6502. See separate file
	; ed.asm or the manual for details.
	; """
Ed:		jmp ed6502
	WordEnd


; ed6502 - Ed-like line-based editor for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; First version: 13. Okt 2018
; This version: 28. Dec 2018

; Ed is a line-orientated editor for Tali Forth 2 based on the classic Unix
; editor of the same name. It is included because a) I like line editors and
; this is my project, so there, and b) as a very simple editor that will work
; even if there is no vt100 terminal support, just with ASCII if needs be. For
; further information on ed, see

;   https://en.wikipedia.org/wiki/Ed_(text_editor)
;   https://www.gnu.org/software/ed/ed.html
;   https://www.gnu.org/software/ed/manual/ed_manual.html
;   https://sanctum.geek.nz/arabesque/actually-using-ed/
;   http://www.psue.uni-hannover.de/wise2017_2018/material/ed.pdf

; We start editor from Forth with
;
;	ed ( -- addr u )
;
; The return values ( addr u ) are the address and length of the text written.
; If no text was written, u is zero and addr is undefined.

; In the working memory, the text is stored as a simple linked list of lines.
; Each node consists of three 16-bit entries:

;	- pointer to next entry (0 for end of list)
;	- pointer to beginning of string ( addr )
;	- length of string ( u )

; The editor only works in interaction with slow humans, so speed is not
; a primary concern. We try to keep the size down instead.

; Where to put variables is a bit of a problem. To convert the numbers, we need
; UM/MOD, which uses the scratchpad, and ACCEPT uses tmp1, tmp2, and tmp3 at
; some point, so we either have to pay very close attention, or we do something
; else. After some experimenting, it seems that the easiest way for this sort
; of hybrid Forth/assembler system is to keep the parameters for the commands
; on the Data Stack in the form of ( para1 para2 ):

;	TOS: parameter 2 (after the comma)
;	NOS: parameter 1 (before the comma)

; The third and fourth entries on the stack are the ( addr-t u-t ) entries the
; text will be/has been written to, or u as 0 if nothing was defined.

; We also need a pointer to the beginning of the text (first node of the list),
; the number of the current line, and a flag to mark if the text has been
; changed. We have six bytes of zero page reserved for any editor to use. Note
; that this means that we can't use two editors at the same time, which won't
; be a problem until we can multitask.

ed_head	 = editor1  ; pointer to first list element (addr) (2 bytes)
ed_cur	 = editor2  ; current line number (1 is first line) (2 bytes)
ed_flags = editor3  ; Flags used by ed, where
;	bit 7 parameters - 0: none, 1: have at least one parameter
;	bit 6 changed	 - 0: text not changed, 1: text was changed
;	bit 0 printing	 - 0: no line numbers (p), 1: with line numbers (n)

;  Byte editor3+1 is used to hold BASE and put it back at the end.


ed6502:
		; Save the current base and set to decimal.
		lda base
		sta editor3+1
		jsr Decimal

		; Start a new empty linked list at HERE. This is also
		; the current line
		lda #0
		sta ed_head
		sta ed_head+1

		; The current line is 0, because we start counting at
		; line 1 for the humans
		sta ed_cur
		sta ed_cur+1

		; At the beginning, we have no parameters (bit 7), no line
		; numbers (bit 0), and nothing was changed (bit 6)
		sta ed_flags

		; We put zeros as placeholders for the text we've written to
		; (the "target") on the stack. Because the stack picture is
		; going to get very confusing very fast, we'll mark them
		; specially with "-t" suffixes in the stack comments.
		jsr Zero
		jsr Zero		; ( addr-t u-t )

		jsr CR

ed_input_loop:
		; Set parameter flag to none (bit 7); default printing is
		; without line numbers (bit 0). We leave the changed flag (bit
		; 6) because we might be coming from a previous add
		lda #$ff-%10000001
		and ed_flags
		sta ed_flags

		; We really don't want to have to write a complete
		; parser for such a simple editor, so we walk through the
		; possibilities the hard way. Get input from the user. This
		; routine handles any errors from REFILL
		jsr ed_get_input

		; If we were not given an empty line, see what we were given
		lda ciblen+0
		bne _command_mode

		; We were given an empty line. Advance one line, print it, and
		; make it the new current line
		lda ed_cur+0
		ldy ed_cur+1
		jsr PushYA		; ( addr-t u-t u )

		; This counts as having a parameter
		lda #%10000000
		ora ed_flags
		sta ed_flags

		jsr One_Plus		; ( addr-t u-t u+1 )
		jsr ed_is_valid_line
		bcs +
		jmp ed_error_1drop	; New line number is not legal, abort
+
		; We have a legal line number.
		; We need two entries on
		; the parameter list (four if you count the target
		; address) to be able to work with the rest of the program.
		jsr Zero		; ( addr-t u-t u+1 0 )

		jmp _line_number_only_from_external

_command_mode:

		; We were given something other than an empty line. Set the
		; parameter variables to zero as the default. There is no line
		; zero, because we're coding for normal, sane humans, not weird
		; computer people. Some commands like "a" will take a "line 0",
		; however. We use the ed_flags bit 7 to signal if we are
		; without parameters.
		jsr Zero		; parameter 1 is NOS ( addr-t u-t 0 )
		jsr Zero		; parameter 2 is TOS ( addr-t u-t 0 0 )

		; We start off by taking care of any parameters. These can be
		; '%' for the complete text, '$' for the last line, a line
		; number, or a line number followed by a ',' and then either
		; the '$' for the last line or another number. (The original
		; Unix ed has more options, but we're ignoring them for the
		; moment.) In pseudocode, what we are doing in this stage looks
		; something like this:

		;	 case char = '.':
		;	       para1 = current line
		;
		;	 case char = '$':
		;	       para1 = last line
		;
		;	 case char = '%' or ',':
		;	       para1 = 1
		;	       para2 = last line
		;
		;	 case char = ';':
		;	       para1 = current line
		;	       para2 = last line
		;
		;	 case number:
		;	       para1 = number
		;	       get next char
		;
		;	       if char = ',':
		;		       get next char
		;
		;		       case char = '$':
		;			       para2 = last line
		;
		;		       case number:
		;			       para2 = number
		;
		;		       else error
		;
		;	       else get previous char
		;
		;	 else error
		;
		;	 get next char
		;	 process command char

		; We use the Y register as an offset to the beginning of the
		; character input buffer (cib) because we're never going to
		; have more than 255 characters of input with ed and we don't
		; want to have to duplicate the complete machinery required for
		; >IN. In other words, >IN has no meaning for ed. This means
		; that every jmp to _check_command must have Y in a defined
		; state, which is different from the rest of Tali Forth.

		; Parameter processing could probably be handled more
		; efficiently with a loop construct similar to the way the
		; commands are taken care of below. We'll revisit this once ed
		; is feature complete, because of the evils of premature
		; optimization.

		ldy #0			; get char
		lda (cib),y

;_prefix_dot:
		; --- . --- Designate current line for further operations
		cmp #'.'
		bne _prefix_dollar

		jsr ed_have_text

		lda ed_cur
		sta DStack+2,x
		lda ed_cur+1
		sta DStack+3,x		; ( addr-t u-t cur 0 )

		; We have a parameter
		lda #%10000000
		ora ed_flags
		sta ed_flags

		; If we were only given a '.', we print the current line and are
		; done
		lda ciblen
		cmp #1			; sets Z if A was 1
		bne +

		; We know that we have some text and the number of the last
		; line was provided by _last_line, so in theory we don't have
		; to check if this is a legal line number. However, we keep one
		; entry point, so the check is repeated further down. Call it
		; paranoia.
		jmp _line_number_only_from_external
+
		; We have processed the first parameter, and know that we have
		; more than just a dot here. We now need to see if the next
		; character is a comma or a command character. To do this, we
		; need to modify the stack to ( addr-t u-t para1 0 addr u )

		lda cib+0
		ldy cib+1
		jsr PushYA

		lda ciblen+0
		ldy ciblen+1
		jsr PushYA

		jsr One_Minus		; ( addr-t u-t para1 0 addr u-1 )
		jsr NOS_One_Plus	; ( addr-t u-t para1 0 addr+1 u-1 )

		jmp _check_for_para2

_prefix_dollar:
		; --- $ --- Designate last line for further operations
		cmp #'$'
		bne _prefix_percent

		jsr ed_have_text

		inx
		inx			; ( addr-t u-t 0 )

		jsr ed_last_line	; ( addr-t u-t 0 para1 )
		jsr Swap		; ( addr-t u-t para1 0 )

		; We have a parameter
		lda #%10000000
		ora ed_flags
		sta ed_flags

		; If we were only given a '$', we print the last line and are
		; done
		lda ciblen
		cmp #1			; sets Z if A was 1
		bne +

		; We know that we have some text and the number of the last
		; line was provided by _last_line, so in theory we don't have
		; to check if this is a legal line number. However, we keep one
		; entry point for the moment and repeat the check further down
		; out of paranoia
		jmp _line_number_only_from_external
+
		; We are one character into the input buffer cib, so we advance
		; Y as the index accordingly
		ldy #1

		jmp _check_command

_prefix_percent:
		; --- % and , --- Designate whole text for futher operations
		cmp #'%'
		beq _whole_text
		cmp #','
		bne _prefix_semicolon

_whole_text:
		; If there is no text yet, print an error
		jsr ed_have_text

		; We have at least one line of text. The first parameter
		; is therefore line one, the second the last line
		lda #1
		sta DStack+2,x		; LSB of NOS is para 1
		lda #0
		sta DStack+3,x		; ( addr-t u-t para1 0 )

_semicolon_entry:
		; Get the number (not the address) of the last line and
		; store it as the second parameter
		inx
		inx			; DROP ( addr-t u-t para1 )
		jsr ed_last_line	  ; ( addr-t u-t para1 para2 )

		; We have a parameter
		lda #%10000000
		ora ed_flags
		sta ed_flags

		; We are one character into the input buffer cib, so we advance
		; Y as the index accordingly
		ldy #1

		jmp _check_command

_prefix_semicolon:
		; --- ; --- Designate from current line to end of text
		cmp #';'
		bne _prefix_number

		jsr ed_have_text

		; The first parameter is the current line
		lda ed_cur
		sta DStack+2,x
		lda ed_cur+1
		sta DStack+3,x		; ( addr-t u-t cur 0 )

		; The second parameter is the last line. We've done this part
		; before for the '%' and ',' parameters, so we reuse that code
		jmp _semicolon_entry

_prefix_number:
		; --- <NUM> --- Check if we have been given a number

		; We use the built-in Forth routines for this, which involves
		; calling >NUMBER, which calls UM*, which uses tmp1, tmp2, and
		; tmp3. So we can't use any of those temporary variables. We
		; arrive here with ( addr-t u-t 0 0 ), which doesn't help us at
		; all because the string we are looking at is in ( cib ciblen )

		; Set up >NUMBER using CIB and CIBLEN as the location of the
		; string to check. First, though, add the "accumulator" of
		; >NUMBER as a double number, that is, to single-cell numbers
		jsr Zero
		jsr Zero		; ( addr-t u-t 0 0 0 0 )

		lda cib+0
		ldy cib+1
		jsr PushYA

		lda ciblen+0
		ldy ciblen+1
		jsr PushYA		; ( addr-t u-t 0 0 0 0 cib ciblen )

		jsr To_Number		; ( addr-t u-t 0 0 ud addr2 u2 )

		; If we converted all the characters in the string (u2 is
		; zero), then the user just gave us a line number to
		; jump to and nothing else. Otherwise, take another look
		lda DStack+0,x
		ora DStack+1,x
		bne _have_unconverted_chars

		; We must have a line number and nothing else. Make this
		; the current line number and print the line. Remember
		; that at this point, the line number still could be a zero
		jsr Two_Drop		; ( addr-t u-t 0 0 ud )

		jsr D_To_S		; ( addr-t u-t 0 0 u )
		jsr Not_Rot		; ( addr-t u-t u 0 0 )

		inx
		inx			; ( addr-t u-t u 0 ) drop through

_line_number_only_from_external:
		jsr Swap		; ( addr-t u-t 0 u )

		jsr ed_is_valid_line
		bcs +

		; This is not a valid line number, so we bail
		jmp ed_error_2drop
+
		; Legal line number, so make it the current number
		jsr Swap		; ( addr-t u-t u 0 )
		jsr ed_para1_to_cur

		; We have a parameter
		lda #%10000000
		ora ed_flags
		sta ed_flags

		jmp ed_cmd_p_from_external

_have_unconverted_chars:
		; We have some unconverted characters left. If none of the
		; characters were converted, we probably just got a
		; command character and need to skip the rest of the prefix
		; processing. In this case, the number of unconverted
		; characters is equal to the length of the string.
					; ( addr-t u-t 0 0 ud addr2 u2 )
		lda ciblen+0
		cmp DStack+0,x
		bne _no_command_yet
		lda ciblen+1
		cmp DStack+1,x
		bne _no_command_yet

		; The length of the input string is equal to the length of the
		; unprocessed string that >NUMBER returned. Put differently,
		; the first character isn't a number. We know that it isn't '$'
		; or '%' either, so we assume that it's a command character.

		; Clear up the stack and process that command character
		txa
		clc
		adc #8
		tax			; ( addr-t u-t 0 0 )

		; If we weren't given a number, this means we didn't explicitly
		; get a 0 either. So we don't have a parameter. This is the
		; default case, but out of paranoia we explicity clear the flag
		lda #$ff-%10000000
		and ed_flags
		sta ed_flags

		; We don't have any offset, so we go with Y as zero
		ldy #0

		jmp _check_command

_no_command_yet:
		; There actually seems to be a parameter number present.
		; Save the number we converted as the first parameter. We
		; arrive here with ( addr-t u-t 0 0 ud addr2 u2 ) from
		; >NUMBER. To avoid too long stack comments, we leave the
		; target addresses out in this next code segment.
					; ( ... 0 0 ud addr2 u2 )

		jsr To_R		; >R ( ... 0 0 ud addr2 ) (R: u2)
		jsr Not_Rot		; -ROT ( ... 0 0 addr2 ud ) (R: u2)
		jsr D_To_S		; D>S  ( ... 0 0 addr2 para1 ) (R: u2)

		lda DStack+0,x		; LSB
		sta DStack+6,x
		lda DStack+1,x		; MSB
		sta DStack+7,x		; ( ... para1 0 addr2 para1 ) (R: u2)

		inx
		inx			; ( addr-t u-t para1 0 addr2 ) (R: u2)
		jsr R_From		; R> ( addr-t u-t para1 0 addr2 u2 ) fall through

		; We have a parameter
		lda #%10000000
		ora ed_flags
		sta ed_flags

_check_for_para2:
		; That was the first parameter. If the next character is
		; a comma, then there is a second parameter (another number
		; or '$'). Otherwise we expect a command. This is the entry
		; point if the first character was a dot (eg '.,3p')
		lda (DStack+2,x)

		cmp #','
		beq _got_comma

		; It's not a comma, so it's going to be a command character.
		; We need to figure out how many digits our number has so
		; we can adjust Y as the offset. We don't have to do this with
		; 16 bit because no input string is going to be that long
		sec
		lda ciblen
		sbc DStack+0,x
		tay

		jsr Two_Drop		; Remove the leftover stuff from >NUMBER
					; ( addr-t u-t para1 0 )

		jmp _check_command

_got_comma:
		; It's a comma, so we have a second parameter. The next
		; character can either be '$' to signal the end of the text
		; or another number. First, though, move to that next char
		jsr NOS_One_Plus	; ( addr-t u-t para1 0 addr2+1 u2 )
		jsr One_Minus		; ( addr-t u-t para1 0 addr2+1 u2-1 )

		; See if this is an end-of-line '$'
		lda (DStack+2,x)
		cmp #'$'
		bne _para2_not_dollar

		; It's a dollar sign, which means para2 is the number of the
		; last line of the text. We need to adjust Y as the offset. We
		; assume that no command line will be longer than 255
		; characters in ed so we can get away with just looking at
		; the LSB
		sec
		lda ciblen
		sbc DStack+2,x
		tay

		; However, we need to move Y up by one because we were on the
		; '$' and not on the character after that
		iny
		tya
		pha

		; Dump all the stuff from >NUMBER off the stack. This saves
		; one byte compared to six INX instructions, and a byte saved
		; is a byte earned.
		txa
		clc
		adc #6
		tax			; ( addr-t u-t para1 )

		jsr ed_last_line	  ; ( addr-t u-t para1 para2 )

		pla
		tay
		jmp _check_command

_para2_not_dollar:
		; It's not a dollar sign, so it is either another number or an
		; error. We try for a number first. We arrive here with ( para1
		; 0 addr2+1 u2-1 ), which u2-1 pointing to the first mystery
		; character after the comma. Again, we skip the ( addr-t u-t )
		; at the beginning of the stack comment here.
		jsr To_R		; >R ( ... para1 0 addr2+1 ) (R: u2-1)
		jsr Zero		; 0 ( ... para1 0 addr2+1 0 ) (R: u2-1)
		jsr Zero		; 0 ( ... para1 0 addr2+1 0 0 ) (R: u2-1)
		jsr Rot		; ROT ( ... para1 0 0 0 addr2+1 ) (R: u2-1)
		jsr R_From		; R> ( ... para1 0 0 0 addr2+1 u2-1)

		; We'll need a copy of the length of the rest of the string to
		; see if we've actually done any work
		jsr Dup		; DUP ( ... para1 0 0 0 addr2+1 u2-1 u2-1)
		jsr To_R		; >R ( ... para1 0 0 0 addr2+1 u2-1 ) (R: u2-1)

		jsr To_Number	; >NUMBER ( ... para1 0 ud addr3 u3 ) (R: u2-1)

		; If the original string and the leftover string have the same
		; length, then nothing was converted and we have an error
		jsr Dup		; DUP ( ... para1 0 ud addr3 u3 u3 ) (R: u2-1)
		jsr R_From		; R> ( ... para1 0 ud addr3 u3 u3 u2-1 )
		jsr Equal		; = ( ... para1 0 ud addr3 u3 f )

		lda DStack+0,x
		ora DStack+1,x
		beq _second_number

		; The strings are the same length, so nothing was converted, so
		; we have an error. We have to get all that stuff off the
		; stack first
		txa
		clc
		adc #12
		tax			; back to ( addr-t u-t )

		jmp ed_error

_second_number:
		; We have a second number, so we add it to para2. We arrive here
		; with ( para1 0 ud addr3 u3 f )
		inx
		inx			; ( addr-t u-t para1 0 ud addr3 u3 )

		; Calculate the offset for Y
		sec
		lda ciblen
		sbc DStack+0,x
		pha

		; Clean up the stack
		jsr Two_drop		; 2DROP ( addr-t u-t para1 0 ud )
		jsr D_To_S		; D>S  ( addr-t u-t para1 0 para2 )
		jsr Nip			; NIP ( addr-t u-t para1 para2 )

		pla
		tay

		; fall through to _check_command

_check_command:
		; There should be a command next.  Check if we had any line
		; numbers specified, and if not, load in the current line number.
		bit ed_flags
		bmi _check_command_have_arg

		; No parameters - put the current line number in the first parameter.
		lda ed_cur+0
		sta DStack+2,x
		lda ed_cur+1
		sta DStack+3,x

_check_command_have_arg:
		; At this point, we assume that we have handled any parameters
		; which are now in their place on the stack, which must have
		; the format ( addr-t u-t para1 para2 ). Also, any offset to CIB
		; is going to be in Y. Bit 7 in ed_flags signals if we have
		; a parameter or not.

		; Command character checking works by comparing the char we
		; have at CIB+Y with a list of legal characters. The index in
		; the list is the index of the command's routine in a jump
		; table. The list itself is zero-terminated, which is okay
		; because we've taken care of any legal parameters.
		lda (cib),y		; get mystery char from input
		sta tmp1+0

		sta tmp1+1		; We need X for awhile, so it has to
					; take a break from being the Data Stack Pointer (DStack).
		ldx #$ff		; for each char
_cmd_loop:	inx			;   next char
		lda ed_cmd_list,x
		beq _illegal_command	;   end of list?
		cmp tmp1+0
		bne _cmd_loop		;   not found?
					; We have a command match.
		txa
		asl
		tax			; X * 2 for table

		; Note we're jumping with the DStack still on the stack, so each
		; command routine has to pull it into X the very first thing
		; with its very own PLX. There doesn't seem to be a sane way to
		; avoid this.
					; jmp to command handler
		lda ed_cmd_table+1,x
		pha
		lda ed_Cmd_table+0,x
		pha
		ldx tmp1+1		; restore X
		php
		rti			; jump to command routine

_illegal_command:
		; Whatever the user gave us, we don't recognize it
		jmp ed_error_2drop

ed_next_command:
		; Clean up the stack and return to the input loop. We
		; arrive here with ( addr-t u-t para1 para2 ).
		jsr Two_Drop		; ( addr-t u-t ) Fall through

_next_command_empty:
		; The beginning of the input loop takes care of resetting the
		; parameter flag
		jmp ed_input_loop

ed_all_done:
		; That's enough for ed today. We have to clear out the input
		; buffer or else the Forth main main loop will react to the
		; last input command
		lda #0
		sta ciblen
		sta ciblen+1

		; Clean up the stack
		jsr Two_drop			; ( addr-t u-t )

		; Restore the base
		lda editor3+1
		sta base

		rts


; === COMMAND ROUTINES ====

; We enter all command subroutines with ( addr-t u-t para1 para2 )
; At this point, we don't need the offset in Y anymore
; so we are free to use it as we please.

; There is potential to rewrite many of the command routines with an abstract
; construct in the form of (pseudocode):

;	f = cmd		; command such as d, p, n, as a function
;	map f range(para1, para2)

; That is, have one routine with a looping structure and pass the actual work
; as a function. However, this is 8-bit assembler and not, say, Haskell, so
; that abstraction will wait for a future round of refracturing when we have
; everything complete and working.

; -------------------------
ed_cmd_a:
	; a -- Add text after current/given line. If no line is given, we use
	; the current line. We accept the number '0' and then start adding at
	; the very beginning. The second parameter is always ignored. This
	; routine is used by i as well.

		; We don't care about para2, because a just adds stuff starting
		; the line we were given
		inx
		inx			;  DROP ( addr-t u-t para1 )

		; This is now handled at the top of check_command.
;		; If we weren't given a parameter, make the current line the
;		; parameter
;		bit ed_flags
;		bmi ed_cmd_a_have_para
;
;		lda ed_cur
;		sta DStack+0,x
;		lda ed_cur+1
;		sta DStack+1,x		;  ( addr-t u-t cur ) drop through

ed_entry_cmd_i:
		; This is where i enters with a parameter that is calculated to
		; be one before the current line, or given line, or so that we
		; accept 0. We are ( addr-t u-t num )

ed_cmd_a_have_para:
		jsr ed_num_to_addr	  ;  ( addr-t u-t addr1 )
		jsr CR

_next_string_loop:
		; This is where we land when we are continuing in with another
		; string after the first one. ( addr-t u-t addr1 )
		jsr ed_get_input

		; If there is only one character and that character is a
		; dot, we're done with adding text and switch back to command
		; mode
		ldy #0
		lda (cib),y
		cmp #'.'
		bne _add_line

		; So it's a dot, but that the only character in the line?
		; We want the length to be one character exactly
		ldy ciblen
		cpy #1
		bne _add_line
		ldy ciblen+1
		bne _add_line

		; Yes, it is a dot, so we're done adding lines.
		inx
		inx

		; The string is stored and the new node is full. Time to set the
		; changed flag
		lda #%01000000
		ora ed_flags
		sta ed_flags

		jsr CR
		jmp ed_input_loop

_add_line:
		; Break the linked list so we can insert another node
		jsr Dup			; ( addr-t u-t addr1 addr1 )
		jsr Here		; ( addr-t u-t addr1 addr1 here )
		jsr Swap		; ( addr-t u-t addr1 here addr1 )
		jsr Fetch		; ( addr-t u-t addr1 here addr2 )
		jsr Comma		; ( addr-t u-t addr1 here )

		; We're going to need that HERE for the next line if more
		; than one line is added. This is a good time to save it on
		; the stack
		jsr Tuck		; TUCK ( addr-t u-t here addr1 here )

		; We have now saved the link to the next node at HERE, which is
		; where the CP was pointing. CP has been advanced by one cell,
		; but we still have the original as HERE on the stack. That
		; address now has to go where addr2 was before.
		jsr Swap		; ( addr-t u-t here here addr1 )
		jsr Store		; ( addr-t u-t here )

		; Thus concludes the mucking about with node links. Now we have
		; to create a new header. The CP we access with HERE points to
		; the cell after the new node address, which is where we want
		; to put ( ) for the new string
		jsr Here		; HERE ( addr-t u-t here here2)

		; Reserve two cells (four bytes on the 6502) for the ( addr u )
		; of the new string
		lda #4
		jsr PushZA
		jsr Allot

		; HERE now points to after the new header. Since we're really
		; going to add something, we can increase the current line
		; number
		inc ed_cur
		bne +
		inc ed_cur+1
+
		; We have the new line sitting in ( cib ciblen ) and need to
		; a) move it somewhere safe and b) get ready for the next
		; line. We arrive here with ( addr-t u-t here here2 ), where here2
		; is where the new string needs to be. The MOVE command we're
		; going to use has the format ( addr1 addr2 u )

		jsr Here	; ( addr-t u-t here here2 here3 )

		lda cib+0
		ldy cib+1
		jsr PushYA	; ( addr-t u-t here here2 here3 cib )

		jsr Over	; ( addr-t u-t here here2 here3 cib here3 )

		lda ciblen+0
		ldy ciblen+1
		jsr PushYA	; ( addr-t u-t here here2 here3 cib here3 ciblen )

		jsr Move	; ( addr-t u-t here here2 here3 )

		; We need to adjust CP be the length of the string
		clc
		lda cp
		adc ciblen
		sta cp
		bcc +
		lda cp+1
		adc ciblen+1
		sta cp+1
+
		; The string is now moved safely out of the input buffer to the
		; main memory at ( here3 ciblin ). Now we have to fix that
		; fact in the header. We start with the address.
		jsr Over		; ( addr-t u-t here here2 here3 here2 )
		jsr Store		; ( addr-t u-t here here2 )

		jsr Cell_Plus_NoUf	; ( addr-t u-t here here2+2 )
		jsr Dup			; ( addr-t u-t here here2+2 here2+2 )

		lda ciblen
		sta DStack+2,x
		lda ciblen+1
		sta DStack+3,x		; ( addr-t u-t here ciblen here2+2 )

		jsr Store		; ( addr-t u-t here )

		jsr CR			; Add a line feed for visuals

		; Remeber that original HERE we've been dragging along all the
		; time? Now we find out why. We return to the loop to pick up
		; the next input
		jmp _next_string_loop

; -------------------------
ed_cmd_d:
	; d -- Delete one or more lines. This might have to be coded as
	; a subroutine because other commands such as 'c' might be easier to
	; implement that way. Note that a lot of this code is very similar to
	; the loop for 'p'. We arrive here with ( addr-t u-t para1 para2 )

		jsr ed_have_text
		jsr ed_no_line_zero

		; At least the first line is valid. Most common case is one
		; line, so we check to see if we even have a second parameter.
		lda DStack+0,x
		ora DStack+1,x
		bne +

		; The second parameter is a zero, so delete one line
		jsr Over		; ( addr-t u-t para1 0 para1 )
		jsr _cmd_d_common	; ( addr-t u-t para1 0 )
		jmp _cmd_d_done
+
		; We have been given a range. Make sure that the second
		; parameter is legal. We arrive here with ( addr-t u-t para1 para2 )
		jsr ed_is_valid_line	  ; result is in C flag
		bcs _cmd_d_loop

		; para2 is not valid. Complain and abort
		jmp ed_error_2drop

_cmd_d_loop:
		; Seems to be a legal range. Walk through and delete If para1
		; is larger than para2, we're done. Note that Unix ed throws an
		; error if we start out that way, we might do that in future as
		; well. This is not the same code as for 'p', because we have
		; to delete from the back
					; ( addr-t u-t para1 para2 )

		lda DStack+0,x		; para1 > para2 ?
		cmp DStack+2,x
		lda DStack+1,x
		sbc DStack+3,x
		bcc _cmd_d_done_with_flag

		; Para2 is still larger or the same size as para1, so we
		; continue

		jsr Dup		; DUP ( addr-t u-t para1 para2 para2 )
		jsr _cmd_d_common	; ( addr-t u-t para1 para2 )
		jsr One_minus	; 1- ( addr-t u-t para1 para2-1 )

		jmp _cmd_d_loop

_cmd_d_done_with_flag:
					; ( addr-t u-t para1 para2 )

		; The current line is set to the first line minus
		; one. Since we don't accept '0d', this at least
		; hast to be one
		jsr NOS_One_Minus

		lda DStack+2,x
		sta ed_cur
		lda DStack+3,x
		sta ed_cur+1		; drop through to _cmd_d_done

_cmd_d_done:
		; Text has changed, set flag
		lda #%01000000
		ora ed_flags
		sta ed_flags

		jsr CR

		jmp ed_next_command

_cmd_d_common:
	; Internal subroutine to delete a single line when given the line
	; number TOS. Consumes TOS. What we do is take the link to the next
	; node and put it in the previous node. The caller is responsible
	; for setting ed_changed. We arrive here with ( u )

		jsr Dup			; ( addr-t u-t u u )
		jsr ed_num_to_addr	; ( addr-t u-t u addr )
		jsr Fetch		; ( addr-t u-t u addr1 )
		jsr Swap		; ( addr-t u-t addr1 u )
		jsr One_minus		; ( addr-t u-t addr1 u-1 )
		jsr ed_num_to_addr	; ( addr-t u-t addr1 addr-1 )
		jmp Store		; ( addr-t u-t )

; -------------------------
ed_cmd_equ:
	; = --- Print the given line number or the current line number if no
	; value is given. This is useful if you want to know what the number of
	; the last line is ("$=")

		; If we don't have a text, we follow Unix ed's example and
		; print a zero. It would seem to make more sense to throw an
		; error, but who are we to argue with Unix.
		lda ed_head
		ora ed_head+1
		bne _cmd_equ_have_text

		; Fake it: load 0 as para2 and then print. The 0 goes in a new
		; line just like with Unix ed
		jsr Zero		; ( addr-t u-t para1 para2 0 )
		jmp _cmd_equ_done

_cmd_equ_have_text:
		; We have taken care of the case where we don't have a text. If
		; we have a line zero, it is explicit, and we don't do that
		jsr ed_no_line_zero

		; If we have no parameters, just print the current line number
		bit ed_flags
		bmi _cmd_equ_have_para

		lda ed_cur+0		; ( addr-t u-t para1 para2 ? )
		ldy ed_cur+1
		jsr PushYA

		jmp _cmd_equ_done	; ( addr-t u-t para1 para2 cur )

_cmd_equ_have_para:
		; We have at least one parameter, and we know it is not zero.
		; We follow the behavior of Unix ed here: If there is one
		; parameter, we print its line number. If there are two
		; separated by a comma (etc), we print the second line number
		; of the range
		lda DStack+0,x
		ora DStack+1,x
		bne _cmd_equ_two_paras

		; We've got one parameter
		jsr Over		; ( addr-t u-t para1 para2 para1)
		jmp _cmd_equ_done

_cmd_equ_two_paras:
		jsr Dup			; ( addr-t u-t para1 para2 para2) drop through

_cmd_equ_done:
		jsr CR			; number goes on new line
		jsr U_Dot		; ( addr-t u-t para1 para2 )
		jsr CR

		jmp ed_next_command


; -------------------------
ed_cmd_f:
	; f -- Print the address that a write command ("w") will go to or set
	; it. If no parameter was passed, we print the address we have on hand,
	; with a parameter, we set that to the new address. We accept a zero,
	; though that would be a weird place to write, but we do need a text

		bit ed_flags
		bmi _cmd_f_have_para

		jsr CR

		; No parameters, just a naked "f", so print the address buried
		; at the fourth position of the stack: We arrive here with
		; ( addr-t u-t 0 0 )
		jsr To_R		; ( addr-t u-t 0 ) ( R: 0 )
		jsr Rot			; ( u-t 0 addr-t ) ( R: 0 )
		jsr Dup			; ( u-t 0 addr-t addr-t ) ( R: 0 )
		jsr U_Dot		; ( u-t 0 addr-t ) ( R: 0 )
		jsr Not_Rot		; ( addr-t u-t 0 ) ( R: 0 )
		jsr R_From		; ( addr-t u-t 0 0 )

		jmp _cmd_f_done

_cmd_f_have_para:
		; We do no sanity tests at all. This is Forth, if the user
		; wants to blow up the Zero Page and the Stack, sure, go right
		; ahead, whatever.
		jsr Over
		jsr CR
		jsr U_Dot

		lda DStack+2,x
		sta DStack+6,x
		lda DStack+3,x
		sta DStack+7,x		; fall through to _cmd_f_done

_cmd_f_done:
		jsr CR

		jmp ed_next_command


; -------------------------
ed_cmd_i:
	; i --- Add text before current line. We allow '0i' and 'i' just like
	; the Unix ed. Note that this routine just prepares the line numbers so
	; we can reuse most of the code from a.

		; We don't care about para2, because i just adds stuff before
		; the line we were given.
		inx
		inx			;  DROP ( addr-t u-t para1 )

		; If we weren't given a parameter, make the current line the
		; parameter
		bit ed_flags
		bmi _cmd_i_have_para

		; No parameter, take current line
		lda ed_cur
		sta DStack+0,x
		lda ed_cur+1
		sta DStack+1,x		;  ( addr-t u-t cur ) drop through

_cmd_i_have_para:
		; If the parameter is zero, we skip the next part and behave
		; completely like the "a" command
		lda DStack+0,x
		ora DStack+1,x
		beq _cmd_i_done

		; We have some other line number, so we start one above it
		jsr One_minus		; ( addr-t u-t para1-1 )
		jsr Zero		; ( addr-t u-t para1-1 0 )
		jsr Max			; ( addr-t u-t para1-1 | 0 )
_cmd_i_done:
		jmp ed_entry_cmd_i


; -------------------------
ed_cmd_n:
	; n -- Print lines with a line number. We just set a flag here and
	; let p do all the heavy work.

		lda #%00000001
		ora ed_flags
		sta ed_flags

		jmp ed_cmd_p_entry_for_cmd_n


; -------------------------
ed_cmd_p:
	; p -- Print lines without line numbers. This routine is also used
	; by n, the difference is in a flag. Note that this routine is
	; able to handle line numbers greater than 255 even though it's
	; hard to believe somebody would actually use this editor for anything
	; that long. I'm really sure Leo Tolstoy would not have created "War
	; and Peace" on a 65c02.

ed_cmd_p_from_external:
		; This is coming from p, the variant without line numbers. We
		; set the ed_flags' bit 0 to zero to mark this
		lda #$ff-%00000001
		and ed_flags
		sta ed_flags

ed_cmd_p_entry_for_cmd_n:
		jsr ed_have_text
		jsr ed_no_line_zero

		jsr CR

		; We now know that there is some number in para1. The most
		; common case is that para2 is zero and we're being asked to
		; print a single line
		lda DStack+0,x
		ora DStack+1,x
		bne _cmd_p_loop

		; Update the current line to this line.
		lda DStack+2,x
		sta ed_cur
		lda DStack+3,x
		sta ed_cur+1

		; Print a single line and be done with it. We could use
		; DROP here and leave immediately but we want this routine
		; to have a single exit at the bottom.
		jsr Over		; ( addr-t u-t para1 para2 para1 )
		jsr _cmd_p_common	; ( addr-t u-t para1 para2 )

		jmp _cmd_p_all_done

_cmd_p_loop:
		; We are being asked to print more than one line, which
		; is a bit trickier. If para1 is larger than para2, we're
		; done. Note that Unix ed throws an error if we start out
		; that way, we might do that in future as well
					; ( addr-t u-t para1 para2 )
		lda DStack+0,x		; para1 > para2 ?
		cmp DStack+2,x
		lda DStack+1,x
		sbc DStack+3,x
		bcc _cmd_p_done

		; Para2 is still larger or the same size as para1, so we
		; continue
		jsr Over		; ( addr-t u-t para1 para2 para1 )
		jsr _cmd_p_common	; ( addr-t u-t para1 para2 )

		jsr NOS_One_Plus

		jmp _cmd_p_loop

_cmd_p_done:	; We arrive here with ( addr-t u-t para1 para2 )

		; Update the current line number with the last line printed.
		lda DStack+0,x
		sta ed_cur
		lda DStack+1,x
		sta ed_cur+1

_cmd_p_all_done:
		jmp ed_next_command


_cmd_p_common:
	; Internal subroutine to print a single line when given the line number
	; TOS. Consumes TOS. Used by both n and p. We arrive here with
	; ( addr-t u-t para1 ) as the line number

		; See if we're coming from p (no line numbers, ed_flag is zero)
		; or from n (line numbers and a TAB, ed_flag is $FF)
		lda ed_flags
		lsr			; bit 0 now in carry
		bcc _cmd_p_common_no_num

		; Bit 0 is set, this is coming from n. Print the line number
		; followed by a tab
		jsr Dup			; ( addr-t u-t para1 para1 )
		jsr U_Dot		; ( addr-t u-t para1 )

		lda #AscHT
		jsr Emit_A

_cmd_p_common_no_num:
		; One way or the other, print the the node's string
		jsr ed_num_to_addr	  ; ( addr-t u-t addr )
		jmp ed_print_addr


; -------------------------
ed_cmd_q:
	; q -- Quit if all work as been saved, complain otherwise

		bit ed_flags		; bit 6 is change flag
		bvc +
		jmp ed_error_2drop
+
		jmp ed_all_done		   ; can't fall thru because of PLX


; -------------------------
ed_cmd_qq:
	; Q -- Quit unconditionally, dumping any work that is unsaved
	; without any warning.

		jmp ed_all_done


; -------------------------
ed_cmd_w:
	; w --- Write text to system memory. In contrast to the Unix ed word,
	; we provide the address before the command, such as "8000w". If no
	; address is given -- just 'w' -- we write to whatever was fixed with
	; 'f'. Currently, we can only enter
	; the address in decimal.

		jsr ed_have_text

		bit ed_flags		; parameter given?
		bmi _cmd_w_have_para

		; We arrive here with ( addr-t u-t 0 0 )
		lda DStack+7,x		; $0000 thru $00ff ?
		bne +

		; We assume the user just entered "w" without a parameter.
		; This will clobber the system.
		; We'll generate an error to protect the users from themselves
		jmp ed_error_2drop
+
		; we assume user knows what they are doing. Get the address.
		lda DStack+6,x
		sta DStack+2,x
		lda DStack+7,x
		sta DStack+3,x		; ( addr-t u-t addr-t ? )

		jmp _cmd_w_para_ready

_cmd_w_have_para:
		; We were given a parameter, which we now make the new
		; default parameter. This is different from Unix w, where
		; the filename set by f is not changed by w
		lda DStack+2,x
		sta DStack+6,x
		lda DStack+3,x
		sta DStack+7,x		; drop through to _cmd_w_para_ready

_cmd_w_para_ready:
		; We don't care about the second parameter, the first one must
		; be an address. There is actually no way to test if this is an
		; address because any number could be a 16-bit address. Anyway,
		; we overwrite para2 with the address where the pointer to the
		; first entry in the list is kept.
		lda #<ed_head
		sta DStack+0,x
		lda #>ed_head
		sta DStack+1,x		; ( addr-t u-t addr-t addr-h )

		; We need to keep a copy of the original target address to
		; calculate how many chars (including carriage returns) we
		; saved at the end of this routine
		jsr Over		; ( addr-t u-t addr-t addr-h addr-t )
		jsr To_R		; ( addr-t u-t addr-t addr-h ) ( R: addr-t )

_cmd_w_loop:
		jsr Fetch		; ( addr-t u-t addr-t addr1 ) ( R: addr-t )

		; If we're at the end of the list, quit. For the next block of
		; text, we ignore the ( addr-t u-t ) at the beginning
		lda DStack+0,x
		ora DStack+1,x
		beq _cmd_w_eol

		jsr Two_dup		; ( addr-t addr-1 addr-t addr-1 ) ( R: addr-t addr-1 addr-t )
		jsr Two_to_r		; ( addr-t addr-1 ) (R: ... )

		; Get the address and length of the string from the header
		; of this node
		jsr Cell_Plus_NoUf	; ( addr-t addr1+2 ) (R: ... )
		jsr Dup			; ( addr-t addr1+2 addr1+2 ) ( R: ... )
		jsr Fetch		; ( addr-t addr1+2 addr-s ) ( R: ... )
		jsr Swap		; ( addr-t addr-s addr1+2 ) ( R: ... )
		jsr Cell_Plus_NoUf	; ( addr-t addr-s addr1+2 ) (R: ... )
		jsr Fetch		; ( addr-t addr-s u-s ) ( R: ... )
		jsr Not_Rot		; ( u-s addr-t addr-s ) ( R: ... )
		jsr Swap		; ( u-s addr-s addr-t ) ( R: ... )
		jsr Rot			; (addr-s addr-t u-s ) ( R: ... )

		; We need a copy of the string length u-s to adjust the pointer
		; to the store area later
		jsr Dup			; (addr-s addr-t u-s u-s ) ( R: ... )
		jsr To_R		; (addr-s addr-t u-s ) ( R: ... u-s )

		jsr Move		; ( )( R: addr-t addr-1 addr-t )

		; Calculate the position of the next string in the save area.
		; What we don't do is remember the length of the individual
		; strings; instead at the end we will subtract addresses to
		; get the length of the string
		jsr R_From		; ( u-s )  ( R: addr-t addr-h addr-t )
		jsr Two_r_from		; ( u-s addr-t addr-h ) ( R: addr-t )
		jsr Not_Rot		; ( addr-h u-s addr-t ) ( R: addr-t )
		jsr Plus		; ( addr-h addr-t1 ) ( R: addr-t )

		; But wait, our strings are terminated by Line Feeds in
		; memory, so we need to add one
		jsr Dup			; ( addr-h addr-t1 addr-t1 ) ( R: addr-t )

		lda #AscLF
		jsr PushZA		; ( addr-h addr-t1 addr-t1 c ) ( R: addr-t )

		jsr Swap		; ( addr-h addr-t1 c addr-t1 ) ( R: addr-t )
		jsr Store		; ( addr-h addr-t1 ) ( R: addr-t )
		jsr One_plus		; ( addr-h addr-t1+1 ) ( R: addr-t )

		; Now we can handle the next line
		jsr Swap		; ( addr-t1+1 addr-h ) ( R: addr-t )

		jmp _cmd_w_loop

_cmd_w_eol:
		; We're at the end of the text buffer and arrive here with
		; ( addr-tn addr-n ) ( R: addr-t ) What we do now is calculate
		; the number of characters saved and put that value in the 3OS
		; position
		jsr Swap		; ( addr-t u-t addr-n addr-tn ) ( R: addr-t )
		jsr R_From		; ( addr-t u-t addr-n addr-tn addr-t )
		jsr Minus		; ( addr-t u-t addr-n u )

		lda DStack+0,x
		sta DStack+4,x
		lda DStack+1,x
		sta DStack+5,x		; ( addr-t u addr-n u )

		; Unix ed puts the number of characters on a new line, so we
		; do as well
		jsr CR
		jsr Dup			; ( addr-t u addr-n u u )
		jsr U_Dot		; ( addr-t u addr-n u )
		jsr CR

		; Reset the changed flag
		lda #$ff-%01000000
		and ed_flags
		sta ed_flags

		jmp ed_next_command


; === ERROR HANDLING ===

ed_error_2drop:
		; Lots of times we'll have para1 and para2 on the stack when an
		; error occurs, so we drop stuff here
		inx			; Drop para2
		inx
ed_error_1drop:
		inx			; Drop para1
		inx
ed_error:
		; Error handling with ed is really primitive: We print a question
		; mark and go back to the loop. Any code calling this routine must
		; clean up the stack itself: We expect it to be empty. Note that
		; ed currently does not support reporting the type of error on
		; demand like Unix ed does
		jsr CR

		lda #'?'
		jsr Emit_A

		jsr CR

		jmp ed_input_loop


; === HELPER FUNCTIONS ===

ed_get_input:
	; Use REFILL to get input from the user, which is left in
	; ( cib ciblen ) as usual.
		jsr Refill		;  ( addr-t u-t f )

		; If something went wrong while getting the user input, print
		; a question mark and try again. No fancy error messages
		; for ed!
		lda DStack+0,x
	;	ora DStack+1,x
		bne +

		; Whatever went wrong, we can't handle it here anyway. We
		; clear the return stack, dump the error flag and call
		; a normal error
		pla
		pla

		jmp ed_error_1drop
+
		; Drop the flag
		inx
		inx

		rts

; -----------------------------
ed_have_text:
	; See if we have any lines at all. If not, abort with an error. We
	; could in theory set a flag every time we add a text, but this is
	; more robust, if somewhat longer
		lda ed_head
		ora ed_head+1
		bne +

		; We don't have any lines. Clean up the return stack and throw
		; an error
		pla
		pla
		jmp ed_error
+
		rts

; -----------------------------
ed_is_valid_line:
	; See if the line number in TOS is valid. If yes, returns the carry
	; flag set ("true"), otherwise cleared ("false"). Does not change
	; the value of TOS. Line numbers must be 0 < number <= last_line.
	; This routine calls _last_line.
		sec				; default is legal line number

		; First see if we have a zero
		lda DStack+0,x
		ora DStack+1,x
		beq _nope_zero	; ( n )

		; Not a zero. Now see if we're beyond the last line
		jsr Dup				; ( n n )
		jsr ed_last_line		; ( n n last )
		jsr Swap			; ( n last n )
		jsr Less_Than			; ( n f )

		lda DStack+0,x			; 0 flag is good
		ora DStack+1,x
		bne _too_small

		; We're good, clean up and leave
		inx
		inx			; DROP flag ( n )

		sec			; signal valid
		rts

_too_small:
		inx
		inx

_nope_zero:
		clc			; signal not valid
		rts


; -----------------------------
ed_last_line: ; ( -- u )
	; Calculate the number of the last line (not its address) and return
	; it TOS. Note this shares code with _num_to_addr. Assumes that user
	; has made sure there are any lines at all

		jsr Zero		; Set counter to zero

		lda #ed_head
		jsr PushZA		; ( count addr )
_loop:
		jsr Fetch		; ( count addr|0 )

		lda DStack+1,x		; at end-of-list?
		beq _done
					; Not done.
		jsr NOS_One_Plus	; Increase counter
		jmp _loop		; continue

_done:					; ( u )
		rts


; -----------------------------
ed_no_line_zero:
	; Make sure we weren't given an explicit zero as the line number with
	; commands that don't accept it (that is, pretty much everybody except
	; a). If para1 is a zero and we have parameters (bit 7 of ed_flag set),
	; throw an error

		; See if para1 is zero
		lda DStack+2,x
		ora DStack+3,x
		bne _done

		; It's zero. If bit 7 of ed_flag is set, this is an explicit
		; parameter
		bit ed_flags
		bpl _done

		jmp ed_error_2drop

_done:		; All is well, we can continue
		rts

; -----------------------------
ed_num_to_addr:
	; Given a line number as TOS, replace it by the address of the node.
	; If the line number is zero, we return the address of the header
	; node. If the line number is beyond the last line, we return a
	; zero, though we're assuming the user will check for a legal
	; line number before calling this routine. Assumes we have checked that
	; we have any text at all.

		; One way or another we're going to start with the
		; address of the pointer to the head of the list
		lda #ed_head
		jsr PushZA		; ( u addr-h )

		jmp _test

_loop:
		; Get the next line
		jsr Fetch		; @ ( u addr1 )

		lda DStack+1,x		; at end of list?
		beq _finished

		jsr NOS_One_Minus	; decrement the line count

_test:		lda DStack+2,x		; is the nth element we're looking for?
		ora DStack+3,x
		bne _loop

_finished:				; We arrive here with ( addr u )
		jmp Nip			; ( addr )

; -----------------------------
ed_para1_to_cur:
	; Switch the current line number to whatever the first parameter
	; is. We do this a lot so this routine saves a few bytes
		lda DStack+2,x
		sta ed_cur
		lda DStack+3,x
		sta ed_cur+1

		rts


; -----------------------------
ed_print_addr: ; ( addr -- )
	; Given the address of a node TOS, print the string it comes with.
	; Assumes we have made sure that this address exists. It would be
	; nice to put the CR at the beginning, but that doesn't work with
	; the n commands, so at the end it goes. Consumes TOS.
		jsr Cell_Plus		; ( addr+2 )

		jsr Dup			; ( addr+2 addr+2 )

		jsr Cell_Plus		; ( addr+2 addr+4 )

		jsr Fetch		; ( addr+2 u-s )
		jsr Swap		; ( u-s addr+2 )
		jsr Fetch		; ( u-s addr-s )

		jsr Swap		; ( addr-s u-s )
		jsr Type
		jmp CR


; === COMMAND TABLES ===

; The commands are all one character and kept in a 0-terminated string that is
; walked by a loop. Their index corresponds to the index of their routine's
; address in the jump table. To create a new command, add it's letter at the
; correct position in the command list and the routine's address in the command
; jump table. Oh, and write the routine as well. Capital letters such as 'Q' are
; coded in their routine's address as double letters ('_cmd_qq').

ed_cmd_list:	.text "afidpn=wqQ", 0

ed_cmd_table:
		.word ed_cmd_a, ed_cmd_f, ed_cmd_i, ed_cmd_d, ed_cmd_p, ed_cmd_n
		.word ed_cmd_equ, ed_cmd_w, ed_cmd_q, ed_cmd_qq


ed6502_end:	; Used to calculate size of editor code

.endif ; ed



 WordHeader "See",NN ; ( "name" -- ) "Print information about a Forth word"
; ## "see" tested  ANS tools
	; """https://forth-standard.org/standard/tools/SEE
	; SEE takes the name of a word and prints its name token (nt),
	; execution token (xt), size in bytes, flags used, and then dumps the
	; code and disassembles it.
	; """
See:
		jsr Tick_Nt		; ( nt )

		jsr CR

		lda base		; Save the current number base
		pha
;		jsr Hex			; use hexadecimal instead.

		lda #<str_see_nt	; print nt
		ldy #>str_see_nt
		jsr Print_ASCIIZ_YA_no_lf
		jsr Dup			; ( nt nt )
		jsr Dot_Hex
		jsr Space		; ( nt )

		jsr Dup			; ( nt nt )
		jsr Name_To_Int		; ( nt xt )

		lda #<str_see_xt	; print xt
		ldy #>str_see_xt
		jsr Print_ASCIIZ_YA_no_lf
		jsr Dup			; ( nt xt xt )
		jsr Dot_Hex
		jsr CR			; ( nt xt )

		; We print letters for flags and then later follow it with 1 or
		; 0 to mark if which flag is set
		lda #<str_see_flags
		ldy #>str_see_flags
		jsr Print_ASCIIZ_YA_no_lf

		jsr Over		; ( nt xt nt )
		lda #Wh_Flags
		jsr Plus_A		; ( nt xt ^flags )
		lda (DStack+0,x)	; ( nt xt ^flags )
		sta DStack+0,x		; ( nt xt flags )

		; This is crude, but for the moment it is good enough
		ldy #2*7		; for bit 7..0
_flag_loop:	jsr Space
		lda _FlagLabels+0,y	;   print label
		jsr Emit_A
		lda _FlagLabels+1,y
		jsr Emit_A
		lda #'='
		jsr Emit_A
		asl DStack+0,x		;   c = flag, rotate the rest
		lda #0
		adc #'0'
		jsr Emit_A
		dey
		dey
		bpl _flag_loop
		inx
		inx			; ( nt xt )

		jsr CR

		lda #<str_see_size	; print size
		ldy #>str_see_size
		jsr Print_ASCIIZ_YA_no_lf
		jsr Swap		; ( xt nt )
		jsr WordSize		; ( xt u )
		jsr Decimal
		lda DStack+0,x		; if = 255
		cmp #$ff
		bne _size5
		jsr sliteral_runtime	;   type "large"
		  jmp _size2
		  .text "large"
_size2:		jsr Type
		jmp _size9		;  else
_size5:		jsr Dup			;    type size
		jsr U_Dot
_size9:					; ( xt u )
		jsr CR

		pla			; restore base
		sta base
					; ( xt u )
 .if "assembler" in TALI_OPTIONAL_WORDS
		jsr Two_dup
 .endif
		jsr Dump		; dump
		jsr CR
 .if "assembler" in TALI_OPTIONAL_WORDS
		jsr DisAsm		; disassemble
 .endif
	WordEnd
		rts

_FlagLabels:	.text "FPDBCOIMNNANUF__"


.if "wordlist" in TALI_OPTIONAL_WORDS ;------------------------------------------------

 WordHeader "Forth-WordList",NN ; ( -- u ) "WID for the Forth Wordlist"
  ; ## "forth-wordlist"  auto  ANS search
  ; https://forth-standard.org/standard/search/FORTH-WORDLIST
Forth_WordList:	jmp Zero
		.cerror wid_Forth!=0
	WordEnd

 WordHeader "Editor-WordList",NN ; ( -- u ) "WID for the Editor wordlist"
  ; ## "editor-wordlist"	tested	Tali Editor
  ; Commonly used like `editor-wordlist >order` to add the editor
  ; words to the search order so they can be used.  This will need
  ; to be done before any of the words marked "Tali Editor" can be
  ; used.  See the tutorial on Wordlists and the Search Order for
  ; more information.
Editor_WordList:
		jmp One
		.cerror wid_Editor!=1
	WordEnd

 WordHeader "Assembler-WordList",NN ; ( -- u ) "WID for the Assembler wordlist"
  ; ## "assembler-wordlist"  tested  Tali Assembler
  ; Commonly used like `assembler-wordlist >order` to add the
  ; assembler words to the search order so they can be used.
  ; See the tutorial on Wordlists and the Search Order for
  ; more information.
Assembler_WordList:
		jmp Two
		.cerror wid_Assembler!=2
	WordEnd

 WordHeader "Root-Wordlist",NN ; ( -- u ) "WID for the Root (minimal) wordlist"
  ; ## "root-wordlist"  tested  Tali Editor
Root_WordList:	lda #wid_Root
		jmp PushZA
	WordEnd


 WordHeader "Only",NN ; ( -- )  Set search order to minimum wordlist
  ; ## "only"  auto  ANS search ext
  ; https://forth-standard.org/standard/search/ONLY
Only:		jsr True	; Push -1
		jmp Set_Order	; set the minimum search order.
	WordEnd


 WordHeader "Also",NN ; ( -- )  Make room in the search order for another wordlist
  ; ## "also"  auto  ANS search ext
  ; http://forth-standard.org/standard/search/ALSO
Also:		jsr Get_Order
		jsr Over
		jsr Swap
		jsr One_plus
		jmp Set_Order
	WordEnd


 WordHeader "Previous",NN ; ( -- )  Remove the first wordlist in the search order
  ; ## "previous"	 auto  ANS search ext
  ; http://forth-standard.org/standard/search/PREVIOUS
Previous:	jsr Get_Order
		jsr Nip
		jsr One_minus
		jmp Set_Order
	WordEnd


 WordHeader ">Order",NN ; ( wid -- )  Add wordlist at beginning of search order
  ; ## ">order"  tested  Gforth search
  ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Word-Lists.html
To_Order:
		jsr To_R		; Put the wid on the return stack for now.

		jsr Get_Order		; Get the current search order.

		jsr R_From		; Get back the wid and add it to the list.
		jsr Swap
		jsr One_plus

		jmp Set_Order		; Set the search order with the new list.
	WordEnd


 WordHeader "Order",NN ; ( -- )  Print current word order list and current WID
  ; ## "order"  auto  ANS core
  ; https://forth-standard.org/standard/search/ORDER
  ; Note the search order is displayed from first search to last
  ; searched and is therefore exactly the reverse of the order in which
  ; Forth stacks are displayed.
  ;
  ; A Forth implementation of this word is:
  ;
  ;	: .wid ( wid -- )
  ;	dup 0=	if ." Forth "  drop    else
  ;	dup 1 = if ." Editor " drop    else
  ;	dup 2 = if ." Assembler " drop else
  ;	dup 3 = if ." Root " drop      else
  ;		   . ( just print the number )
  ;	then then then then ;
  ;
  ; : ORDER ( -- )
  ;	cr get-order 0 ?do .wid loop
  ;	space space get-current .wid ;
  ;
  ; This is an interactive program, so speed
  ; is not as important as size.
Order:
		jsr CR

		ldy #0			; for each search_orderV[]
		beq _test
_loop:		tya
		pha

		lda Search_orderV,y
		jsr print_wid_stringA	; internal helper function

		pla
		tay
		iny			;  next
_test:		cpy Num_orderV
		bcc _loop

		; This follows the convention of Gforth
		jsr CR
		lda CurrentV		; print the current wordlist.
	;	jmp print_wid_stringA


print_wid_stringA: ; ( A -- )  Given a WID in A, print the corresponding string.
  ; If there is no such word list defined, just print the number.
  ; Assumes we will not have more than 256 WIDs.
  ; In theory, we could speed this up by having the WID be the same as the
  ; number of the strings. However, ORDER is used rather infrequently and
  ; this would make changes to the strings.asm file very dangerous, so we
  ; follow the slightly more complicated route with a translation table.

		cmp #wid_Root+1		; do we have a string?
		bcc _string

		jsr PushZA		; print the WID number
		jmp U_Dot		; JSR/RTS

_string:	asl			; Get the word index based on WID 0 to 3
		tay
		dex			; TOS= word nt
		dex
		lda _wid_Table+0,y
		sta DStack+0,x
		lda _wid_Table+1,y
		sta DStack+1,x		; ( nt )
		jsr Name_To_String	; ( addr u )
		lda #$100-9		;   cut off "_wordlist"
		jsr Minus_A
		jsr Type		; Print
		jmp Space
	WordEnd

_wid_Table: ; nt of wordlist names, indexed by the WID if < 4 .
	.word forth_wordlist-wh_LinkNt-1	; WID 0:
	  .cerror wid_Forth!=0
	.word editor_wordlist-wh_LinkNt-1	; WID 1:
	  .cerror wid_Editor!=1
	.word assembler_wordlist-wh_LinkNt-1	; WID 2:
	  .cerror wid_Assembler!=2
	.word root_wordlist-wh_LinkNt-1		; WID 3:
	  .cerror wid_Root!=3


 WordHeader "Forth",NN ; ( -- )  Replace first WID in search order with Forth-Wordlist
  ; ## "forth"  auto  ANS search ext
  ; https://forth-standard.org/standard/search/FORTH
Forth:		lda #wid_Forth
		sta Search_OrderV+0
	WordEnd
		rts


 WordHeader "Definitions",NN ; ( -- )  Make first wordlist in search order the current wordlist
  ; ## "definitions" auto ANS search
Definitions:	lda Search_OrderV	; Transfer SEARCH_ORDER[0] to
		sta CurrentV		;   byte variable CURRENT.
	WordEnd
		rts


 WordHeader "WordList",NN ; ( -- wid )  Create new wordlist
  ; ## "wordlist" auto ANS search
  ; From pool of 8.
  ; https://forth-standard.org/standard/search/WORDLIST
  ; See the tutorial on Wordlists and the Search Order for
  ; more information.
WordList:
		lda Num_wordlistsV	; Get the current number of wordlists

		cmp #max_wordlists	; already at the max?
		bcc +
		lda #$100+err_TooManyWordlists	;   Print an error message
		jmp ThrowA
+
		inc Num_WordlistsV	; increment wordlist count
		jmp PushZA		; and put it on the stack.
	WordEnd


 WordHeader "Set-Current",0 ; ( wid -- )  Set the compilation wordlist
  ; ## "set-current" auto ANS search
  ; https://forth-standard.org/standard/search/SET-CURRENT
Set_Current:	jsr PopA	; pop wid
		sta CurrentV	; only the LSB is used.
	WordEnd
		rts

 WordHeader "Get-Current",NN ; ( -- wid )  Get the id of the compilation wordlist 
  ; ## "get-current" auto ANS search
  ; https://forth-standard.org/standard/search/GET-CURRENT
Get_Current:
		; This is a little different than some of the variables
		; in the user area as we want the value rather than
		; the address.
		lda CurrentV
		jmp PushZA	; CURRENT is a byte variable
	WordEnd


 WordHeader "Set-Order",NN ; ( wid_n .. wid_1 n -- )  Set the current search order
  ; ## "set-order" auto ANS search
  ; https://forth-standard.org/standard/search/SET-ORDER
Set_Order:
		lda DStack+1,x		; Test for -1 TOS
		bpl _start

		; Replace it with the default search order.
		inx			; Drop
		inx
		jsr Root_Wordlist	; wid
		jsr One			; Count is 1.

_start:		; Continue processing with ( forth-wordlist 1 -- )

		; Set #ORDER - the number of wordlists in the search order.
		jsr PopA	; pop count
		sta Num_orderV	; #ORDER is a byte variable.


		; Move the wordlist ids from the data stack to the search order.
		ldy #0
		beq _test

_loop:		; Move one wordlist id over into the search order.
		jsr PopA		; The search order is a byte array
		sta Search_OrderV,y	; so only save the LSB
		iny

_test:		; See if that was the last one to process (first in the list).
		cpy Num_orderV
		bne _loop
	WordEnd
		rts

 WordHeader "Get-Order",NN ; ( -- wid_n .. wid_1 n)  Get the current search order
  ; ## "get-order" auto ANS search
  ; https://forth-standard.org/standard/search/GET-ORDER
Get_Order:
		ldy Num_OrderV	; Get #ORDER - the number of wordlists in the search order.
		beq _done	; If zero, there are no wordlists.

_loop:
		lda Search_OrderV-1,y	; Put that wordlist id on the stack.
		jsr PushZA

		dey
		bne _loop		; See if that was the last one to process (first in the list).
_done:

		lda Num_OrderV		; Push the number of items
		jmp PushZA
	WordEnd


 WordHeader "Search-Wordlist",NN ; ( caddr u wid -- 0 | xt 1 | xt -1)  Search for a word in a wordlist
  ; ## "search-wordlist" auto ANS search
  ; caddr u = name string
  ; wid = wordlist id
  ; Also returns tmp1= nt
  ; https://forth-standard.org/standard/search/SEARCH_WORDLIST
Search_WordList:
		jsr PopA		; Pop wid
		pha

		jsr swl_prepare 	; ( 0 u )
		inx
		inx			; ( 0 )

		pla			; pop wid
		jsr swl_search_wordlist ; tmp1= nt of matching word
		beq _NotFound

	; The strings match.

		ldy #wh_Flags
		lda (tmp1),y
		and #FP+DB
		clc
		adc #wh_LinkNt-1
		adc tmp1+0		; TOS= xt
		sta DStack+0,x
		lda #0
		adc tmp1+1
		sta DStack+1,x

	;	ldy #Wh_Flags		; get flags
		lda (tmp1),y
		and #IM
		bne _immediate		; bit set, we're immediate

		jmp True		; We're not immediate, return -1

_immediate:	jmp One			; We're immediate, return 1


_NotFound:
					; NOS already zeroed
		rts
	WordEnd

.endif ; "wordlist"

 .endsection code


.if "block" in TALI_OPTIONAL_WORDS ;------------------------------------------------

 .section bss
; Block I/O vectors
BlockReadV: .word ?	; Vector to block reading routine
BlockWriteV: .word ?	; Vector to block writing routine

; Block buffer variables
BuffBlockNumV: .word ?	; Block number current in BlockBuffer
BuffStatusV: .word ?	; Status of BlockBuffer (bit 0 = used, bit 1 = dirty)
BlockBuffer: .fill 1024

 .endsection bss

 .section code

BlockInit: ; initialize block variables
		lda #0
		sta BuffStatusV

		lda #<Platform_Block_Read
		ldy #>Platform_Block_Read
		sta BlockReadV+0
		sty BlockReadV+1
		lda #<Platform_Block_Write
		ldy #>Platform_Block_Write
		sta BlockWriteV+0
		sty BlockWriteV+1

		rts

 .endsection code

.if "ramdrive" in TALI_OPTIONAL_WORDS

 .section bss
RamDriveV: .word ?	; ptr to RamDrive storage area
 .endsection bss

 .section code

.weak
; These labels allow this to assemble even if c65 is not the target platform.
; Because they are weak, they will be replaced when c65 is the target platform.
io_blk_status = 0
io_blk_action = 0
io_blk_number = 0
io_blk_buffer = 0
.endweak

 .if io_blk_status!=0 ; c65 simulator set up?

 WordHeader "Block_C65_Init",NN ; ( -- f )  Initialize c65 simulator block storage
  ; ## "block-c65-init"  auto  Tali block
  ; Set up block IO to read/write to/from c65 block file.
  ; Run simulator with a writable block file option
  ; e.g. `touch blocks.dat; c65/c65 -b blocks.dat -r taliforth-py65mon.bin`
  ; Returns true if c65 block storage is available and false otherwise.
block_c65_init:
                lda #<c65_blk_read
                ldy #>c65_blk_read
                sta BlockReadV+0
                sty BlockReadV+1

                lda #<c65_blk_write
                ldy #>c65_blk_write
                sta BlockWriteV+0
		sty BlockWriteV+1

                lda #$ff
                sta io_blk_status
                lda #0
                sta io_blk_action
                lda io_blk_status	; $0 if OK, $ff otherwise
                eor #$ff		; invert to forth true/false
                jmp PushAA		; push f


c65_blk_write: ; ( addr u -- )
		ldy #2			; write action code
                bne c65_blk_rw

c65_blk_read: ; ( addr u -- )
		ldy #1			; read action code
c65_blk_rw:     lda 0,x                 ; ( addr blk# )
                sta io_blk_number
                lda 1,x
                sta io_blk_number+1
                lda 2,x
                sta io_blk_buffer
                lda 3,x
                sta io_blk_buffer+1
                sty io_blk_action       ; trigger the r/w
                jmp Two_Drop		; clean up stack, return

 .endif ; io_blk_status ; c65 simulator set up?


 WordHeader "Block-RamDrive-Init",UF+NN ; ( u -- )  Create a ramdrive for blocks
  ; ## "block-ramdrive-init"  auto  Tali block
  ; Create a RAM drive, with the given number of
  ; blocks, in the dictionary along with setting up the block words to
  ; use it.  The read/write routines do not provide bounds checking.
  ; Expected use: `4 block-ramdrive-init` ( to create blocks 0-3 )
Block_RamDrive_Init:
		lda #10			; Calculate how many bytes are needed for numblocks blocks
		jsr PushZA
		jsr LShift

		jsr Here		; ( size addr )
		sta RamDriveV+0
		sty RamDriveV+1

		jsr Over
		jsr Allot		; Create ramdrive buffer
					; ( size addr )
		jsr Swap		; blank the buffer
		jsr Blank

		lda #<_read		; set block read vector
		ldy #>_read
		sta BlockReadV+0
		sty BlockReadV+1

		lda #<_write		; set block write vector
		ldy #>_write
		sta BlockWriteV+0
		sty BlockWriteV+1

		rts
	WordEnd

_read: ; : block-read-ramdrive"	; ( addr u -- )
		jsr _addr
		jsr Swap
		lda #0
		ldy #>1024
		jsr PushYA
		jmp Move

_write: ; : block-write-ramdrive" ; ( addr u -- )
		jsr _addr
		lda #0
		ldy #>1024
		jsr PushYA
		jmp Move

_addr: ; ( n -- addr )  point at ramdrive block
		lda #10
		jsr PushZA
		jsr LShift
		jsr RamDrive
		jmp Plus

 WordHeader "RamDrive",NN ; ( -- addr )  push address of RamDrive buffer
RamDrive:	lda RamDriveV+0
		ldy RamDriveV+1
		jmp PushYA
	WordEnd

 .endsection code

.endif  ; "ramdrive"

 .section code

 WordHeader "BuffStatus",NN ; ( -- addr )  Push address of variable holding buffer status
  ; ## "buffstatus"  auto	 Tali block
BuffStatus:	ldy #>BuffStatusV
		lda #<BuffStatusV
		jmp PushYA
	WordEnd


 WordHeader "BuffBlockNum",NN ; ( -- addr )  Push address of variable holding block in buffer
  ; ## "buffblocknum"  auto  Tali block
BuffBlockNum:	ldy #>BuffBlockNumV
		lda #<BuffBlockNumV
		jmp PushYA
	WordEnd


 WordHeader "BlkBuffer",NN ; ( -- addr )  Push address of block buffer
  ; ## "blkbuffer"  auto	Tali block
BlkBuffer:	ldy #>BlockBuffer
		lda #<BlockBuffer
		jmp PushYA
	WordEnd


 WordHeader "Scr",NN ; ( -- addr )  Push address of variable holding last screen listed
  ; ## "scr"  auto  ANS block ext
  ; https://forth-standard.org/standard/block/SCR
Scr:		ldy #>ScrV
		lda #<ScrV
		jmp PushYA
	WordEnd

 WordHeader "Blk",NN ; ( -- addr )  Push address of block # being interpreted
  ; ## "blk"  auto  ANS block
  ; https://forth-standard.org/standard/block/BLK
Blk:		ldy #>BlkV
		lda #<BlkV
		jmp PushYA
	WordEnd


 WordHeader "Block-Write",NN ; ( addr u -- )  Write a block to storage (deferred word)
  ; ## "block-write"  auto  Tali block
  ; BLOCK-WRITE is a vectored word that the user needs to override
  ; with their own version to write a block to storage.
  ; The stack parameters are ( buffer_address block# -- ).
Block_Write:	jmp (BlockWriteV)	; Execute the BLOCK-READ-VECTOR
	WordEnd


 WordHeader "Block-Write-Vector",NN ; ( -- addr )  Address of the block-write vector
  ; ## "block-write-vector"  auto	 Tali block
  ; Deferred words need the HC (Code Field) flag.
  ; """BLOCK-WRITE is a vectored word that the user needs to override
  ; with their own version to write a block to storage.
  ; This word gives the address of the vector so it can be replaced.
  ; """
Block_Write_Vector:
		ldy #>BlockWriteV
		lda #<BlockWriteV	; Get the BLOCK-WRITE-VECTOR address
		jmp PushYA
	WordEnd


 WordHeader "Block-Read",NN ; ( addr u -- )  Read a block from storage (deferred word)
  ; ## "block-read"  auto	 Tali block
  ; BLOCK-READ is a vectored word that the user needs to override
  ; with their own version to read a block from storage.
  ; The stack parameters are ( buffer_address block# -- ).
Block_Read:	jmp (BlockReadV)	; Execute the BLOCK-READ-VECTOR
	WordEnd


 WordHeader "Block-Read-Vector",NN ; ( -- addr )  Address of the block-read vector
  ; ## "block-read-vector"  auto	Tali block
  ; BLOCK-READ is a vectored word that the user needs to override
  ; with their own version to read a block from storage.
  ; This word gives the address of the vector so it can be replaced.
Block_Read_Vector:
		ldy #>BlockReadV
		lda #<BlockReadV	; Get the BLOCK-READ-VECTOR address
		jmp PushYA
	WordEnd


 WordHeader "Save-Buffers",0 ; ( -- )  Save all dirty buffers to storage
  ; ## "save-buffers"  tested  ANS block
  ; https://forth-standard.org/standard/block/SAVE-BUFFERS
Save_Buffers:
		; Check the buffer status
		lda BuffStatusV+0 ; Only bits 0 and 1 are used, so only
		cmp #3		; LSB is needed.
		bne _done	; Either not used or not dirty = done!

		; We need to save the block.
		jsr BlkBuffer
		jsr BuffBlockNum
		jsr Fetch
		jsr Block_Write

		; Mark the buffer as clean now.
		lda #1
		sta BuffStatusV+0
_done:
	WordEnd
		rts


 WordHeader "Block",0 ; ( u -- a-addr )  Fetch a block into a buffer
  ; ## "block"  auto  ANS block
  ; https://forth-standard.org/standard/block/BLOCK
Block:
		; See if the block requested is the same as the one we
		; currently have in the buffer.
		lda BuffBlockNumV+0	; check the LSB
		cmp DStack+0,x
		bne _not_in_buffer
		lda BuffBlockNumV+1	; check the MSB
		cmp DStack+1,x
		bne _not_in_buffer

		; The block is in the buffer. See if the buffer is in use.
		lda BuffStatusV+0
		and #1		; Check the in-use flag (bit 0)
		bne _done	; It's already in the buffer and in use.
				; _done will replace the block# with the
				; buffer address.
_not_in_buffer:
		; Check the buffer status
		lda BuffStatusV+0 ; Only bits 0 and 1 are used, so only
		cmp #3		; LSB is needed.
		bne _buffer_available ; Unused or not dirty = available

		; We need to save the block.
		jsr BlkBuffer
		jsr BuffBlockNum
		jsr Fetch
		jsr Block_Write

_buffer_available:
		; Save the block number.
		lda DStack+0,x
		sta BuffBlockNumV+0
		lda DStack+1,x
		sta BuffBlockNumV+1

		; Get the requested block.
		jsr BlkBuffer
		jsr Swap
		jsr Block_Read

		; Mark the buffer as clean and in-use.
		lda #1
		sta BuffStatusV+0

		; Make room on the stack for the return address.
		dex
		dex

_done:
		; It's in the buffer. Return the buffer address.
		lda #<BlockBuffer
		sta DStack+0,x
		lda #>BlockBuffer
		sta DStack+1,x
	WordEnd
		rts


 WordHeader "Update",0 ; ( -- )  Mark current block as dirty
  ; ## "update"  auto  ANS block
  ; https://forth-standard.org/standard/block/UPDATE
Update:
		; Turn on the dirty bit.
		lda BuffStatusV+0
		ora #2		; Turn on dirty flag (bit 2)
		sta BuffStatusV+0
	WordEnd
		rts


 WordHeader "Buffer",NN ; ( u -- a-addr )  Get a buffer for a block
  ; ## "buffer"  auto  ANS block
  ; https://forth-standard.org/standard/block/BUFFER
Buffer:
		; Check the buffer status
		lda BuffStatusV+0 ; Only bits 0 and 1 are used, so only
		cmp #3		; LSB is needed.
		bne _buffer_available ; Unused or not dirty = available

		; We need to save the block.
		jsr BlkBuffer
		jsr BuffBlockNum
		jsr Fetch
		jsr Block_Write

_buffer_available:
		; Save the block number.
		lda DStack+0,x
		sta BuffBlockNumV+0
		lda DStack+1,x
		sta BuffBlockNumV+1

		; Mark the buffer as clean and in-use.
		lda #1
		sta BuffStatusV+0

_done:
		; Return the buffer address.
		lda #<BlockBuffer
		sta DStack+0,x
		lda #>BlockBuffer
		sta DStack+1,x
	WordEnd
		rts


 WordHeader "Empty-Buffers",NN ; ( -- )  Empty all block buffers without saving
  ; ## "empty-buffers"  tested  ANS block ext
  ; https://forth-standard.org/standard/block/EMPTY-BUFFERS
Empty_Buffers:	lda #0		; Set the buffer status to empty.
		sta BuffStatusV+0 ; Only LSB is used.
	WordEnd
		rts


 WordHeader "Flush",NN ; ( -- )  Save dirty block buffers and empty buffers
  ; ## "flush"  auto  ANS block
  ; https://forth-standard.org/standard/block/FLUSH
Flush:		jsr Save_Buffers
		jmp Empty_Buffers
	WordEnd


 WordHeader "Load",UF ; ( scr# -- )  Load the Forth code in a screen/block
  ; ## "load"  auto  ANS block
  ; https://forth-standard.org/standard/block/LOAD
Load:
		jsr underflow_1

		lda BlkV+1		; Save the current value of BLK on the return stack.
		pha
		lda BlkV+0
		pha

		lda DStack+0,x		; Set BLK to the given block/screen number.
		sta BlkV+0
		lda DStack+1,x
		sta BlkV+1

		jsr Block		; Load that block into a buffer

		lda #<1024		; block length.
		ldy #>1024
		jsr PushYA

		; Jump to a special evaluate target. This bypasses the underflow
		; check and skips the zeroing of BLK.
		sec		; Set a flag to not zero BLK
		jsr load_evaluate

		; Restore the value of BLK from before the LOAD command.
		pla
		sta BlkV+0
		pla
		sta BlkV+1

		; If BLK is not zero, read it back into the buffer.
		; A still has MSB
		ora BlkV+0
		beq _done

		; Make sure we're pointing into the block again.
		lda BlkV+0
		ldy BlkV+1
		jsr PushYA
		jsr Block
		jsr PopYA	; Pop the buffer address.
		sta cib+0
		sty cib+1

_done:
	WordEnd
		rts


 WordHeader "Thru",UF+NN ; ( scr# scr# -- )  Load screens in the given range
  ; ## "thru"  tested  ANS block ext
  ; https://forth-standard.org/standard/block/THRU
Thru:
		jsr underflow_2

		; We need to loop here, and can't use the data stack
		; because the LOADed screens might use it.
		; We'll hold the limit and current index on the return stack.

		; Move the ending screen number to the return stack
		jsr To_R

_loop:
		lda DStack+1,x		; copy to return stack
		pha
		lda DStack+0,x
		pha

		jsr Load		; Load this screen.

		jsr R_From		; Get the screen # we just loaded.
		jsr One_plus		; increment

		; See if we just loaded the last screen.
		stx tmp1
		tsx
		txa
		tay
		ldx tmp1
		lda RStack+1,y
		cmp DStack+0,x
		lda RStack+2,y
		sbc DStack+1,x
		bcs _loop

		inx			; Drop index
		inx
		pla			; RDrop limit
		pla
	WordEnd
		rts


 WordHeader "List",NN ; ( scr# -- )  List the given screen
  ; ## "list"  tested  ANS block ext
  ; https://forth-standard.org/standard/block/LIST
List:		jsr PopYA	; Save the screen number
		sta ScrV+0
		sty ScrV+1

ListScr: ; entry from editor "L"
		; Load the current screen
		jsr Scr
		jsr Fetch
		jsr Block	; Get the current screen contents.

		jsr CR

		jsr SLiteral_Runtime
		  jmp +				; for SLiteral_Runtime
		  .text "Screen #"		; for SLiteral_Runtime
+
		jsr Type
		jsr Scr		; print screen number
		jsr Fetch
		lda #4		;    in 4 positions
		jsr U_Dot_R_A

		; The address of the buffer is currently on the stack.
		; Print 64 chars at a time. TYPE uses tmp1
		lda #0			; line #
_line_loop:
		pha
		jsr CR

		pla		; Print the line number
		pha
		jsr PushZA
		lda #2		;   in 2 positions
		jsr U_Dot_R_A
		jsr Space

		jsr Dup		; Print one line using the address on the stack.
		lda #64
		jsr PushZA
		jsr Type

		lda #64		; move address to the next line.
		jsr Plus_A

		pla		; Increment the line number
		clc
		adc #1
		cmp #16		; See if we are done.
		bne _line_loop

		inx		; Drop the address
		inx

		jmp CR
	WordEnd

 .endsection code

.endif ; "block"


 .section code

 WordHeader "Defer",NN ; ( "name" -- )  Create a placeholder for words by name
  ; ## "defer"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/DEFER
  ; Compile a name that can point to various xt by Defer! or Is .
Defer:
		jsr Header_Comma	; compile word header

		lda #<_undefined	; compile "jmp _undefined" (patched later)
		ldy #>_undefined
		jsr Jmp_Comma_YA

		jmp adjust_z		; set word length
	WordEnd

_undefined:  ; Error routine for unset DEFER
		lda #$100+err_Defer	; throw exception
		jmp ThrowA

 WordHeader "Defer!",NN ; ( xt2 xt1 -- )  Set defer word xt1 to execute xt2 
  ; ## "defer!"  auto  ANS core ext
  ; http://forth-standard.org/standard/core/DEFERStore
Defer_Store:	jsr PopYA
Defer_Store_YA:	sta tmp1+0
		sty tmp1+1

		; could check for valid DEFER  ( starts with $4c (JMP abs) )

		lda DStack+0,x		; DEFERs JMP abs operand= xt2
		ldy #1
		sta (tmp1),y
		lda DStack+1,x
		iny
		sta (tmp1),y

		jmp Drop		; Drop xt2
	WordEnd

 WordHeader "Defer@",NN ; ( xt1 -- xt2 )  Get the current XT in a defer word
  ; ## "defer@"  auto  ANS core ext
  ; http://forth-standard.org/standard/core/DEFERFetch
Defer_Fetch:	jsr PopYA	; pop xt1, check underflow
Defer_Fetch_YA:	sta tmp1+0
		sty tmp1+1

		; could check for valid DEFER  ( starts with $4c (JMP abs) )

		dex		; push DEFERs JMP abs operand
		dex
		ldy #1
		lda (tmp1),y
		sta DStack+0,x
		iny
		lda (tmp1),y
		sta DStack+1,x
		rts
	WordEnd

 WordHeader "Is",IM+NN ; ( xt "name" -- )  Set named DEFER word to execute xt
  ; ## "is"  auto	 ANS core ext
  ; http://forth-standard.org/standard/core/IS
Is:
		jsr Tick		; get xt of "name"

		; This is a state aware word with different behavior
		; when used while compiling vs interpreting.
		lda state		; Check STATE
		beq Defer_Store		; interpreting, put xt in the DEFER word

_compiling:
		jsr ldya_immed_comma	; compile LDY #; LDA # of the xt of "name"

		ldy #>Defer_Store_YA	; Postpone DEFER! by compiling a JSR to it.
		lda #<Defer_Store_YA
		jmp Jsr_Comma_YA
	WordEnd

 WordHeader "Action-Of",IM+NN ; ( "name" -- xt )  Get named deferred word's xt
  ; ## "action-of"  auto	ANS core ext
  ; http://forth-standard.org/standard/core/ACTION-OF
Action_Of:
		jsr Tick		; get xt of "name"

		; This is a state aware word with different behavior
		; when used while compiling vs interpreting.
		lda state		; Check STATE
		beq Defer_Fetch		; interpreting, get xt in the DEFER word

_compiling:
		jsr ldya_immed_comma	; compile LDY #; LDA # of the xt of "name"

		ldy #>Defer_Fetch_YA	; Postpone DEFER@ by compiling a JSR to it.
		lda #<Defer_Fetch_YA
		jmp Jsr_Comma_YA
	WordEnd


 WordHeader "UserAddr",NN ; ( -- addr )  Push address of base address of user variables
  ; ## "useraddr"	 tested	 Tali Forth
UserAddr:	ldy #>User0
		lda #<User0
		jmp PushYA
	WordEnd


 WordHeader "Buffer:",NN ; ( u "<name>" -- ; -- addr )  Create an uninitialized buffer
  ; ## "buffer:"	auto  ANS core ext
  ; https://forth-standard.org/standard/core/BUFFERColon
  ; Create a buffer of size u that puts its address on the stack
  ; when its name is used.
Buffer_Colon:	jsr Create
		jmp Allot
	WordEnd


 WordHeader "Case",IM+CO+NN ; (C: -- 0) ( -- )  Case flow control
  ; ## "case"  auto  ANS core ext
  ; http://forth-standard.org/standard/core/CASE
Case:
		jmp Zero	; init jmp fixup chain
	WordEnd

 WordHeader "EndCase",IM+CO+NN ; (C: case-sys -- ) ( x -- )  Case flow control
  ; ## "endcase"	auto  ANS core ext
  ; http://forth-standard.org/standard/core/ENDCASE
EndCase:
		; Postpone DROP to remove the item being checked.
		jsr Drop_Comma

		; There are a number of address (of branches that need their
		; jump addressed filled in with the address of right here).
		; Keep calling THEN to deal with them until we reach the
		; 0 that CASE put on the stack at the beginning.
_loop:
		lda DStack+1,x	; while addr fixup entries left
		beq _done

		jsr Then	;   fixup another one
		jmp _loop

_done:
		inx		; Drop the 0
		inx
	WordEnd
		rts

 WordHeader "Of",IM+CO+NN ; (C: -- of-sys) (x1 x2 -- |x1)  Case flow control
  ; ## "of"  auto	 ANS core ext
  ; http://forth-standard.org/standard/core/OF
Of:
		ldy #>_runtime		; Check if value is equal to this case.
		lda #<_runtime
		jsr Jsr_Comma_YA

		lda #$4c		; compile jmp abs
		jsr C_Comma_A
		jsr Here		; Put the origination address on the stack for else/then
					; Stuff zero in for the branch address right now.
					; THEN or ELSE will fix it later.
		jsr Zero
		jmp Comma
	WordEnd

_runtime: ; ( x1 x2 -- x1 )
		inx		; Drop x2
		inx
		lda DStack-2,x	; compare x1 with x2
		cmp DStack+0,x
		bne _NotEq
		lda DStack-1,x
		cmp DStack+1,x
		beq zbranch_run2 ; Drop x1 & return to after the jmp abs
_NotEq:		rts		; return to the jmp abs to next test


 WordHeader "EndOf",IM+CO+NN ; (C: case-sys1 of-sys1-- case-sys2) ( -- )  Case flow control
  ; ## "endof"  auto  ANS core ext
  ; http://forth-standard.org/standard/core/ENDOF
EndOf:		jmp Else
	WordEnd


 WordHeader "If",IM+CO+NN ; (C: -- orig) (flag -- )  IF flow control
  ; ## "if"  auto	 ANS core
  ; http://forth-standard.org/standard/core/IF
If:
If3:		jsr zbranch_jsr_comma	; Compile a 0BRANCH

zbranch_jmp0_comma:
		lda #$4c		; compile jmp abs
		jsr C_Comma_A
		jsr Here		; save ptr to address for else/then
		lda #0			; Stuff zero in for the branch address right now.
		tay			; THEN or ELSE will fix it later.
		jmp Comma_YA
	WordEnd


zbranch_jmp_comma = Jmp_Comma		; compile jmp abs


zbranch_jsr_comma: ; Compile a 0BRANCH runtime jsr
		ldy #>_runtime
		lda #<_runtime
		jmp Jsr_Comma_YA


_runtime: ; ( f -- )
	; In some Forths, this is called (0BRANCH).

		lda DStack+0,x		;flag is false?
		ora DStack+1,x
		beq zbranch_run_done
		; Flag is TRUE

zbranch_run2:	clc			; move RTS addr over the next jmp abs.
		pla
		adc #3
		bcc +
		tay
		pla
		adc #0
		pha
		tya
+		pha

zbranch_run_done:
		inx			; Drop f
		inx

		rts


 WordHeader "Then",IM+CO+NN ; (C: orig -- ) ( -- )  IF flow control
  ; ## "then"  auto  ANS core
  ; http://forth-standard.org/standard/core/THEN
Then:
		; Get the address to jump to.
		jsr Here

		; Stuff HERE in for the branch address back
		; at the IF or ELSE (origination address is on stack).
		jsr Swap
		jmp Store
	WordEnd


 WordHeader "Else",IM+CO+NN ; (C: orig -- orig) ( -- )  IF flow control
  ; ## "else"  auto  ANS core
  ; http://forth-standard.org/standard/core/ELSE
  ;
  ; The code is used by ENDOF
Else:
		jsr zbranch_jmp0_comma	; compile jmp 0, push addr of addr

		; Get the address to jump to (just after the
		; unconditional branch) for the IF to jump to
		; when false.
		jsr Here
		jsr Rot

		; Update the original if 0branch address.
		jmp Store
	WordEnd


;----------------------------------------------------
; BEGIN ... AGAIN
; BEGIN ... UNTIL
; BEGIN ... WHILE ... REPEAT

 WordHeader "Begin",NN+CO+IM ; ( -- addr )  Mark entry point for loop
  ; ## "begin"  auto  ANS core
  ; https://forth-standard.org/standard/core/BEGIN
Begin:		jsr Here	; remember the loop starting location
		lda #<Begin	; pairing marker
		jmp PushZA
	WordEnd


 WordHeader "Again",NN+CO+IM+UF ; ( addr -- ) Code backwards branch to address left by BEGIN
  ; ## "again"  tested  ANS core ext
  ; https://forth-standard.org/standard/core/AGAIN
Again:		jsr underflow_2
		lda #<Begin	; check pairing
		jsr QPairCtlA
		jmp Jmp_Comma	; Compile a jmp abs.
	WordEnd


 WordHeader "Until",IM+CO+NN ; (C: dest -- ) ( -- )  Loop flow control
  ; ## "until"  auto  ANS core
  ; http://forth-standard.org/standard/core/UNTIL
Until:		lda #<Begin		; check pairing
		jsr QPairCtlA
		jmp ZBranch_Comma	; The address to loop back to is on the stack.
	WordEnd


 WordHeader "While",IM+CO+NN ; ( C: dest -- orig dest ) ( x -- )  Loop flow control
  ; ## "while"  auto  ANS core
  ; http://forth-standard.org/standard/core/WHILE
While:		lda #<Begin	; check pairing
		jsr QPairCtlA
		jsr If3		; Compile a 0branch & jmp, push addr of addr
		jsr Swap	; Swap the two addresses on the stack.
		lda #<Begin	; pairing marker
		jmp PushZA
	WordEnd


 WordHeader "Repeat",IM+CO+NN ; (C: orig dest -- ) ( -- )  Loop flow control
  ; ## "repeat"  auto  ANS core
  ; http://forth-standard.org/standard/core/REPEAT
Repeat:		lda #<Begin	; check pairing
		jsr QPairCtlA
		jsr Jmp_Comma	; compile the jmp back
		jsr Here	; patch the WHILE jmp operand
		jsr Swap
		jmp Store
	WordEnd


ZBranch_Comma: ; compile 0 test
		lda #$e8		; compile inx; inx
		tay
		jsr Comma_YA	
		lda #$b5		; compile lda DStack-2,x
		ldy #DStack-2
		jsr Comma_YA
		lda #$15		; compile ora DStack-1,x
		ldy #DStack-1
		jsr Comma_YA

		lda #$f0		; BEQ
		bne Branch_CommaA

 WordHeader "Branch,",NN ; ( addr A -- )  compile short or long branch
		jsr PopA		; pop opcode to A
Branch_CommaA: ;  opcode in A, addr in TOS
		pha			; save branch opcode

		sec			; AY= displacement+2
		lda DStack+0,x
		sbc cp+0
		tay
		lda DStack+1,x
		sbc cp+1
		cmp #$ff		; offset maybe in range?
		beq _rev

_2byte:		pla			; restore branch opcode
		eor #$20		; reverse branch sense
		ldy #3			; compile Bcc *+5
		jsr Comma_YA
		jmp Jmp_Comma		; compile JMP abs

_rev:		cpy #$80+2		; offset in range?
		bcc _2byte
_1byte:		inx			; Drop address
		inx
		dey			; fix displacement
		dey
		pla			; restore branch opcode
		jmp Comma_YA		; compile Bcc


 WordHeader "Word",UF+NN ; ( char "name " -- caddr )  Parse input stream
  ; ## "word"  auto  ANS core
  ; https://forth-standard.org/standard/core/WORD
  ; Obsolete parsing word included for backwards compatibility only.
  ; Do not use this, use `PARSE` or `PARSE-NAME`. Skips leading delimiters
  ; and copies word to storage area for a maximum size of 255 bytes.
  ; Returns the result as a counted string (requires COUNT to convert
  ; to modern format), and inserts a space after the string. See "Forth
  ; Programmer's Handbook" 3rd edition p. 159 and
  ; http://www.forth200x.org/documents/html/rationale.html#rat:core:PARSE
  ; for discussions of why you shouldn't be using WORD anymore.
  ; Seriously, use PARSE-NAME.
Word:
		jsr underflow_1

		; Skip over leading delimiters - this is like PARSE-NAME,
		; but unlike PARSE
		ldy toin+0		; >IN
_DelimLoop:	cpy ciblen+0		; quit if end of input
		beq _found_char
		lda (cib),y
		cmp DStack+0,x		; ASCII of delimiter
		bne _found_char
		iny
		bne _DelimLoop

_found_char:
		sty toin+0		; Save index of where word starts

		jsr Parse		; The real work is done by parse
					; ( addr u )

		jsr PopA		; pop u
		sta tmp2+0
		jsr PopTmp1		; pop addr

		jsr Here		; Return caddr

		; Copy the modern ( addr u ) string format to obsolete
		; ( caddr ) format.
		lda tmp2+0
		ldy #0			; Save length of string
		beq _CopyStart
_CopyLoop:	lda (tmp1),y
		iny
_CopyStart:	jsr C_Comma_A
		cpy tmp2+0
		bcc _CopyLoop
	WordEnd
		rts


 WordHeader "(",IM+NN ; ( -- )  Discard input up to close paren ( comment )
  ; ## "("  auto	ANS core
  ; http://forth-standard.org/standard/core/p
Paren:		lda #')'		; separator
		jsr Parse_A		; Call parse.

		jmp Two_drop		; 2Drop the result.
	WordEnd


 WordHeader ".(",IM+NN ; ( -- )  Print input up to close paren .( comment )
  ; ## ".("  auto	 ANS core
  ; http://forth-standard.org/standard/core/Dotp
Dot_paren:	lda #')'
		jsr Parse_A

		jmp Type
	WordEnd


.if "environment?" in TALI_OPTIONAL_WORDS ;---------------------------------------

 WordHeader "Hash",NN ; ( addr u -- n )  Hash a string, case-insensitive
Hash:		jsr PopA		; save length
		sta tmp2+0
		lda DStack+0,x		; copy addr
		ldy DStack+1,x
		sta tmp1+0
		sty tmp1+1

		lda #$b3		; init hash
		sta DStack+0,x
		sta DStack+1,x
		ldy #0			; for each char
		beq _next

_loop:		lda (tmp1),y		;   get char
		cmp #'a'		;   uppercase
		bcc _3
		cmp #'z'+1
		bcs _3
		and #$df
_3:		asl DStack+0,x		;   add to hash
		rol DStack+1,x
		adc DStack+0,x
		sta DStack+0,x
		iny
_Next:		cpy tmp2+0
		bcc _loop
	WordEnd
		rts


 WordHeader "Environment?",NN ; ( addr u -- 0 | i*x true )  Return system information
  ; ## "environment?"  auto  ANS core
  ; https://forth-standard.org/standard/core/ENVIRONMENTq
Environment_Q:
		jsr Hash		; ( hash )

		ldy #0			; Y= table index
		beq _entry_test

_entry_next:	tya
		cpy #_table_dbl		; if double
		bcc +
		adc #6-4-1		;   skip double

+		adc #4			;   skip single
_3:		tay

_entry_test:	lda _Table+0,y		; match?
		beq _notfound
		cmp DStack+0,x
		bne _entry_next
		lda _Table+1,y
		cmp DStack+1,x
		bne _entry_next

			; we found the entry

		lda _Table+2,y
		sta DStack+0,x
		lda _Table+3,y
		sta DStack+1,x

		cpy #_table_dbl		; single or double data?
		bcc _single

		dex			;  push 2nd cell
		dex
		lda _Table+4,y 
		sta DStack+0,x
		lda _Table+5,y
		sta DStack+1,x
_single:

		jmp True		; return True


_notfound: ; no table entry matched
		lda #0			; return 0
		sta DStack+0,x
		sta DStack+1,x
		rts
	WordEnd


_Table: ; environment strings
	;     hash  value
	.word $e65e,$7fff	; "/COUNTED-STRING"
	.word $79BE,$ff		; "/HOLD"
	.word $3cb9,padoffset	; "/PAD"
	.word $818b,8		; "ADDRESS-UNIT-BITS"
	.word $fc57,0		; "FLOORED"	we have symmetric)
	.word $fb4f,$ff		; "MAX-CHAR"
	.word $7f56,$7fff	; "MAX-N"
	.word $7f5d,$ffff	; "MAX-U"
	.word $ce38,$80		; "RETURN-STACK-CELLS"
	.word $c0f2,DStack0/2	; "STACK-CELLS"
	.word $e336,9		; "WORDLISTS"
  .if "fp" in TALI_OPTIONAL_WORDS
				; Table 12.2 - Environmental query strings 
				;
				; String       Value data type  Constant?  Meaning
				; ------       ---------------  ---------  -------
	.word $f24a,$ffff	; "FLOATING"		flag	no	   floating-point word set present
;	.word $272f,0		; "FLOATING-EXT"	flag	no	   floating-point extensions word set present
	.word $9901,FDim	; "FLOATING-STACK"	n	yes	   If n = zero, floating-point numbers are 
	 			;						kept on the data stack; otherwise n is 
				;						the maximum depth of the separate 
				;						floating-point stack.
;	.word $f559,??		; "MAX-FLOAT"	;       r	yes        largest usable floating-point number
;	  .byte $ff,$ff,$ff,$7f,$7f
  .endif ; "fp"
_table_dbl = *-_Table	; These return a double-cell number
	.word $7f4c,$ffff,$7fff	; "MAX-D"
	.word $fefe,$ffff,$ffff	; "MAX-UD"
	.byte 0		; end of list

.endif ; "environment?"


 WordHeader "Dump",UF+NN ; ( addr u -- )  Display a memory region
  ; ## "dump"  tested  ANS tools
  ; https://forth-standard.org/standard/tools/DUMP
  ;
  ; DUMP's exact output is defined as "implementation dependent".
Dump:
		jsr underflow_2

		dex			; alloc work area
		dex
_row:
		jsr CR

		lda DStack+5,x		; print address number
		jsr C_Dot_Hex_A
		lda DStack+4,x
		jsr C_Dot_Hex_A

		jsr Space
		jsr Space

		; the index for the ASCII characters
		; that we print at the and of the hex block. We
		; start saving them at HERE (CP)
		lda #0
		sta DStack+0,x

_loop:					; ( addr u work )
		; if there are zero bytes left to display, we're done
		lda DStack+2,x
		ora DStack+3,x
		beq _all_printed

		; write as hex
		lda (DStack+4,x)
		jsr C_Dot_Hex_A
		jsr Space

		; Handle ASCII printing
		lda (DStack+4,x)
		jsr is_printable
		bcs _printable
		lda #'.'		 ; Print dot if not printable
_printable:
		ldy DStack+0,x
		sta (cp),y

		; extra space after eight bytes
		cpy #7
		bne +
		jsr Space
+
		inc DStack+4,x
		bne +
		inc DStack+5,x
+
		jsr NOS_One_Minus	; loop counter

		inc DStack+0,x
		lda DStack+0,x
		cmp #16
		bcc _loop		; next byte

		; Done with one line, print the ASCII version of these
		; characters
		jsr Space
		jsr dump_print_ascii

		jmp _row		; new row

_all_printed:
		; See if there are any ASCII characters in the buffer
		; left to print
		lda DStack+0,x
		beq _done

		; In theory, we could try to make the ASCII part line
		; up with the line before it. But that is a hassle (we
		; use three bytes for each missed hex entry, and
		; then there is the gap after eight entries) and it
		; makes it harder to read. We settle for one extra
		; space instead for the moment
		jsr Space
		jsr dump_print_ascii
_done:
		inx			; drop work area
		inx
		jmp Two_drop		; one byte less than 4x INX


dump_print_ascii:
		; Print the ASCII characters that we have saved from
		; HERE (CP) to HERE plus whatever is in TMP2.
		; We keep it inside the scope of DUMP.
		ldy #0
_loop:
		lda (cp),y
		jsr Emit_A
		iny

		; extra space after eight chars
		cpy #8
		bne +
		jsr Space
+
		tya
		cmp DStack+0,x
		bcc _loop

		rts
	WordEnd


 WordHeader "C.Hex",NN ; ( u -- )  Type byte as hex
C_Dot_Hex:	jsr PopA
C_Dot_Hex_A:	pha
		lsr		; convert high nibble first
		lsr
		lsr
		lsr
		jsr _nibble_to_ascii
		pla

_nibble_to_ascii: ; Convert lower nibble of A to hex and and EMIT it.
		and #$0F	; only use lower nibble
		cmp #9+1
		bcc +
		adc #6
+		adc #'0'
		jmp Emit_A
	WordEnd

 WordHeader ".Hex",NN ; ( u -- )  Type cell as hex
Dot_Hex:	lda DStack+1,x	; do hi byte
		jsr C_Dot_Hex_A
		jmp C_Dot_Hex	; do lo byte
	WordEnd


is_printable:
        ; """Given a character in A, check if it is a printable ASCII
        ; character in the range from $20 to $7E inclusive. Returns the
        ; result in the Carry Flag: 0 (clear) is not printable, 1 (set)
        ; is printable. Keeps A. See
        ; http://www.obelisk.me.uk/6502/algorithms.html for a
        ; discussion of various ways to do this
                cmp #AscSP              ; $20
                bcc _done
                cmp #$7F + 1             ; '~'
                bcs _failed

                sec
                rts

_failed:	clc
_done:		rts


;is_whitespace:
        ; Given a character in A, check if it is a whitespace
        ; character, that is, an ASCII value from 0 to 32 (where
        ; 32 is SPACE). Returns the result in the Carry Flag:
        ; 0 (clear) is no, it isn't whitespace, while 1 (set) means
        ; that it is whitespace. See PARSE and PARSE-NAME for
        ; a discussion of the uses. Does not change A or Y.

;               cmp #AscSP+1
;               bcs _failed

;               sec
;               rts

;_failed:	clc
;		rts


 WordHeader ".S",NN ; ( -- )  Non-destructive Print content of Data Stack
  ; ## ".s"  tested  ANS tools
  ; https://forth-standard.org/standard/tools/DotS
  ; Print content of Data Stack non-distructively. We follow the format
  ; of Gforth and print the number of elements first in brackets,
  ; followed by the Data Stack content (if any).
  ;
  ; Since this is for humans, we don't have to worry about speed.
Dot_s:
		; Print stack depth in brackets
		lda #'<'
		jsr Emit_A

		jsr Depth	; ( -- u )

		; print unsigned number without the trailing space
		jsr print_u

		lda #'>'
		jsr Emit_A
		jsr Space

		ldy #DStack0		; for each cell on the stack
_loop:		dey
		dey
		stx tmp1
		cpy tmp1
		bcc _done

		tya			; save index
		pha

		dex			; push stack[index]
		dex
		lda DStack+0,y
		sta DStack+0,x
		lda DStack+1,y
		sta DStack+1,x

		jsr Dot

		pla			; restore index
		tay
		bne _loop
_done:
	WordEnd
		rts


 WordHeader "Compare",UF+NN ; ( addr1 u1 addr2 u2 -- -1 | 0 | 1)  Compare two strings
  ; ## "compare"	 auto  ANS string
  ; https://forth-standard.org/standard/string/COMPARE
  ; Compare string1 (denoted by addr1 u1) to string2 (denoted by
  ; addr2 u2).  Return -1 if string1 < string2, 0 if string1 = string2
  ; and 1 if string1 > string2 (ASCIIbetical comparison).	 A string
  ; that entirely matches the beginning of the other string, but is
  ; shorter, is considered less than the longer string.
Compare:
		jsr underflow_4

		lda DStack+0,x		; tmp3+1= u2.lo
		sta tmp3+1
		lda DStack+2,x		; tmp2= addr2
		ldy DStack+3,x
		sta tmp2+0
		sty tmp2+1
		lda DStack+4,x		; tmp3+0= u1.lo
		sta tmp3+0
		lda DStack+6,x		; tmp1= addr1
		ldy DStack+7,x
		sta tmp1+0
		sty tmp1+1

		ldy #0		; for each char
_loop:
		cpy tmp3+0	; string1 empty?
		beq _str1
_L1:
		cpy tmp3+1	; string2 empty?
		beq _str2
_L2:
				; Both strings have at least one char left.
		lda (tmp1),y	; Are they different?
		cmp (tmp2),y
		bne _neq
		iny		; to next char
		bne _loop
		inc tmp1+1	; to next page
		inc tmp2+1
		bne _loop


_str1:		dec DStack+5,x	; string1 really empty?
		bpl _L1
				; String 1 is empty.
		cpy tmp3+1	; string2 empty?
		bne _less
		lda DStack+1,x
		bne _less
_equal:		lda #0		; Return 0
		beq _gt2

_neq:		bcs _greater
_less:		lda #$FF	; Return -1
		sta DStack+6,x
		bne _done

_str2:		dec DStack+1,x	; is string2 empty?
		bpl _L2
				; string2 is empty (but string1 is not)
_greater:	lda #1		; Return 1
_gt2:		sta DStack+6,x
		lda #0
_done:		sta DStack+7,x

		txa		; Remove all but the result from the stack.
		clc
		adc #6
		tax
	WordEnd
		rts


 WordHeader "Search",UF+NN ; ( addr1 u1 addr2 u2 -- addr3 u3 flag)  Search for a substring
  ; ## "search"	auto  ANS string
  ; https://forth-standard.org/standard/string/SEARCH
  ; Search for string2 (denoted by addr2 u2) in string1 (denoted by
  ; addr1 u1). If a match is found the flag will be true and
  ; addr3 will have the address of the start of the match and u3 will have
  ; the number of characters remaining from the match point to the end
  ; of the original string1. If a match is not found, the flag will be
  ; false and addr3 and u3 will be the original string1's addr1 and u1.
Search:
		jsr underflow_4

		; ANS says if the second string is a zero-length string it
		; automatically matches.
		lda DStack+0,x
		ora DStack+1,x
		bne _start_search

		; The second string is a zero length string.  Just remove
		; the second string and put a true flag.
		inx		; Drop u2
		inx
		lda #$FF	; overwrite addr2 with a true flag
		sta DStack+0,x
		sta DStack+1,x
		rts

_start_search:
		; Put an offset (starting at zero) on the stack.
		jsr Zero

_search_loop:
		; We stop (not found) when u2 + offset > u1
		; Calculate u2+offset into tmp1
		clc
		lda DStack+0,x
		adc DStack+2,x
		sta tmp1+0
		lda DStack+1,x
		adc DStack+3,x
		sta tmp1+1

		; Compare to u1. Start with the high byte
		cmp DStack+7,x
		bcc _init_comparison ; Obviously less
		bne _not_found

		; The upper address byte matched - check the lower byte
		; Load u1 first so we can use just a carry to check.
		lda DStack+6,x
		cmp tmp1
		bcs _init_comparison

_not_found:
		; The substring isn't in the main string.
		; Return just the main string and a false flag.
		inx		; Remove offset
		inx
		inx		; Remove u2
		inx
		lda #0
		sta DStack+0,x	; Turn addr2 into a false flag
		sta DStack+1,x
		rts

_init_comparison:
		; Use tmp1 to hold address in string 1.
		; Use tmp2 to hold address in string 2.
		; Use tmp3 to hold the number of characters left to check.

		; Compute the starting address in string 1
		; as addr1 + offset
		clc
		lda DStack+8,x
		adc DStack+0,x
		sta tmp1
		lda DStack+9,x
		adc DStack+1,x
		sta tmp1+1

		; The starting address in string 2 is just addr2.
		lda DStack+4,x
		sta tmp2
		lda DStack+5,x
		sta tmp2+1

		; The number of characters to check is u2.
		lda DStack+2,x
		sta tmp3+0
		lda DStack+3,x
		sta tmp3+1

_comparison_loop:
		; Check to see if the current characters match.
		ldy #0
		lda (tmp1),y
		cmp (tmp2),y
		beq _letters_match

		; One of the letters didn't match.
		; Increment the offset and try again.
		jsr One_plus
		jmp _search_loop

_letters_match:
		; The letters match.  Advance the pointers until the
		; count reaches zero.
		inc tmp1
		bne +
		inc tmp1+1
+
		inc tmp2
		bne +
		inc tmp2+1
+
		; Decrement the count of remaining letters to check.
		lda tmp3
		bne +
		dec tmp3+1
+
		dec tmp3

		; Check if we've reached zero.
		lda tmp3
		ora tmp3+1
		bne _comparison_loop ; Check the next letter

		; We've run out of letters and they all match!
		; Return (addr1+offset) (u1-offset) true
		; Add offset to addr1.
		clc
		lda DStack+0,x
		adc DStack+8,x
		sta DStack+8,x
		lda DStack+1,x
		adc DStack+9,x
		sta DStack+9,x

		; Subtract offset from u1.
		sec
		lda DStack+6,x
		sbc DStack+0,x
		sta DStack+6,x
		lda DStack+7,x
		sbc DStack+1,x
		sta DStack+7,x

		; Replace addr2, u2, and offset with a true flag.
		inx		; drop offset
		inx
		inx		; drop u2
		inx
		lda #$FF
		sta DStack+0,x	; Turn addr2 into a true flag.
		sta DStack+1,x
	WordEnd
		rts


 WordHeader "Marker",IM+NN ; ( "name" -- )  Create a deletion boundry word
  ; ## "marker"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/MARKER
  ; This word replaces FORGET in earlier Forths. Old entries are not
  ; actually deleted, but merely overwritten by restoring CP and wordlist heads.
  ; Run the named word at a later time to restore all of the wordlists
  ; to their state when the word was created with marker.	 Any words
  ; created after the marker (including the marker) will be forgotten.
  ;
  ; To do this, we want to end up with something that jumps to a
  ; run-time component with a link to the original CP and wordlist head values:
  ;
  ;	jsr _runtime
  ;	<Original CP LSB, MSB>
  ;	CURRENT (byte variable)
  ;	<All wordlists> (currently 12) (cell array)
  ;	<#ORDER> (byte variable)
  ;	<All search order> (currently 9) (byte array)
Marker:
		jsr Here		; Save original CP, which
					; after all is the whole point of this operation.

		jsr Header_Build	; compile a word header, but don't link into wordlist

		ldy #>_runtime		; compile JSR _runtime
		lda #<_runtime
		jsr Jsr_Comma_YA

		jsr Comma		; Append original CP

		; Add the user variables for the wordlists and search order.
		ldy #0			; Start at CURRENT
_loop:		lda CurrentV,y
		jsr C_Comma_A
		iny
		cpy #MarkEnd-CurrentV	; One past the end of the search order.
		bcc _loop

		jmp Header_Link		; link built header into current dictionary
	WordEnd


_runtime: ; Restore Dictionary and memory (CP) to where the were
	; when this marker was defined.
	; We arrive here with the return
	; address on the Return Stack in the usual 6502 format

		pla		; Pop the RTS address off the stack
		sta tmp1+0
		pla
		sta tmp1+1

		ldy #1		; start just aftet JSR _runtime
		lda (tmp1),y	; CP was stored first
		sta cp+0
		iny
		lda (tmp1),y
		sta cp+1
		iny

_rloop:		; Copy back on top of the wordlists and search order.
		lda (tmp1),y
		sta CurrentV-2-1,y
		iny
		cpy #MarkEnd-CurrentV+2+1 ; One past the end of the search order.
		bcc _rloop

		rts


 WordHeader "Words",NN ; ( -- )  Print known words from Dictionary
  ; ## "words"  tested  ANS tools
  ; https://forth-standard.org/standard/tools/WORDS
  ; This is pretty much only used at the command line so we can
  ; be slow and try to save space.
  ; Modified to list later search wordlists first.
Words:
		lda #$ff
		jsr PushZA		; Alloc space for line size & wordlist #
		dex			; Alloc space for a nt
		dex
_wordlist_next:				; ( wordlist# nt )
		jsr CR			; start wordlist on new line

		inc DStack+2,x		; step to next search wordlist
		ldy DStack+2,x		; Y= wordlist index
		cpy Num_OrderV
		bcs _wordslist_done	; ran out of search wordlist entries?

		; start with latest word in Dictionary
		lda Search_OrderV,y	; A= SEARCH-ORDER[Y]	Get the current wordlist id
		asl			; TOS= WORDLISTS[A]
		tay
		lda WordlistsV+0,y
		sta DStack+0,x
		lda WordlistsV+1,y
		sta DStack+1,x
		beq _wordlist_next	; end of list?
_word_loop:
		jsr Dup			; ( wordlist# nt nt )
		jsr Name_To_String	; ( wordlist# nt addr u )

		; Insert line break if we're about to go past the end of the
		; line
		lda DStack+7,x
		sec			; don't forget the space between words
		adc DStack+0,x
		cmp #MAX_LINE_LENGTH-1
		bcc +

		jsr CR

		lda DStack+0,x		; After going to next line, start
					; with length of this word.
+		sta DStack+7,x
		jsr Type		; ( wordlist# nt )

		jsr Space

		lda DStack+0,x
		sta tmp1+0
		lda DStack+1,x
		sta tmp1+1
		jsr LinkNext		; tmp1= next word nt in list
		lda tmp1+0
		sta DStack+0,x
		lda tmp1+1
		sta DStack+1,x
		bne _word_loop		; end of list?

		beq _wordlist_next

_wordslist_done:
		jmp Two_Drop
	WordEnd


 WordHeader "WordSize",NN ; ( nt -- u )  Get size of word in bytes
  ; ## "wordsize"	 auto  Tali Forth
  ; Given an word's name token (nt), return the size of the
  ; word's payload size in bytes (CFA plus PFA) in bytes.
  ; Probably does not count the final RTS.
WordSize:	jsr PopTmp1

		ldy #Wh_CodeLength
		lda (tmp1),y
		jmp PushZA
	WordEnd


 WordHeader "Aligned",0 ; ( addr -- addr )  Return the first aligned address
  ; ## "aligned"	auto  ANS core
  ; https://forth-standard.org/standard/core/ALIGNED
Aligned:
	WordEnd
		rts


 WordHeader "Align",0 ; ( -- )  Make sure CP is aligned on word size
  ; ## "align"  auto  ANS core
  ; https://forth-standard.org/standard/core/ALIGN
  ; On the 6502, this does nothing.
Align:
	WordEnd
		rts


 WordHeader "Output",NN ; ( -- addr )  Return the address of the EMIT vector address
  ; ## "output"  tested  Tali Forth
xt_output:
	; Return the address where the jump target for EMIT is stored (but
	; not the vector itself). By default, this will hold the value of
	; kernel_putc routine, but this can be changed by the user, hence this
	; routine.
		ldy #>output
		lda #<output
		jmp PushYA
	WordEnd


 WordHeader "Input",NN ; ( -- addr )  Return address of input vector
  ; ## "input" tested Tali Forth
xt_input:	ldy #>input
		lda #<input
		jmp PushYA
	WordEnd


 WordHeader "CR",NN ; ( -- )  Print a line feed
  ; ## "cr"  auto	 ANS core
  ; https://forth-standard.org/standard/core/CR
CR:
 .if "cr" in TALI_OPTION_CR_EOL
		lda #AscCR
		jsr Emit_A
  .endif
 .if "lf" in TALI_OPTION_CR_EOL
		lda #AscLF
		jsr Emit_A
 .endif
	WordEnd
		rts


 WordHeader "Page",NN ; ( -- )  Clear the screen
  ; ## "page"  tested  ANS facility
  ; https://forth-standard.org/standard/facility/PAGE
  ; Clears a page if supported by ANS terminal codes.
Page:		jsr SLiteral_Runtime
		  jmp +
		  .text AscESC,"[2J"	; ANSI clear screen
		  .text AscESC,"[1;1H"	; move cursor to top left of screen
+		jmp Type
	WordEnd


 WordHeader "At-XY",UF+NN ; ( ux uy -- )  Move cursor to position given
  ; ## "at-xy"  tested  ANS facility
  ; https://forth-standard.org/standard/facility/AT-XY
  ; On an ANSI compatible terminal, place cursor at row nx colum ny.
  ; ANSI code is ESC[<y>;<x>H
  ;
  ; Do not use U. to print the numbers because the
  ; trailing space will not work with xterm
At_XY:
		jsr underflow_2

		lda base	; Save the BASE
		pha
		jsr Decimal	; ANSI escape code values need to be in decimal.

		lda #AscESC
		jsr Emit_A
		lda #'['
		jsr Emit_A
		inc DStack+0,x	; AT-XY is zero based, but ANSI is 1 based
		jsr print_u
		lda #';'
		jsr Emit_A
		inc DStack+0,x	; AT-XY is zero based, but ANSI is 1 based
		jsr print_u
		lda #'H'
		jsr Emit_A

		pla		; Restore the base
		sta base
	WordEnd
		rts


 WordHeader "Pad",0 ; ( -- addr )  Return address of user scratchpad
  ; ## "pad"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/PAD
  ; Return address to a temporary area in free memory for user.
  ; It is located relative to
  ; the compile area pointer (CP) and therefore varies in position.
  ; This area is reserved for the user and not used by the system
Pad:		dex		; push cp+PadOffset
		dex
		clc
		lda cp+0
		adc #PadOffset
		sta DStack+0,x
		lda cp+1
		adc #0
		sta DStack+1,x
	WordEnd
		rts


 WordHeader "<#",0 ; ( -- )  Start number conversion
  ; ## "<#"  auto	 ANS core
  ; https://forth-standard.org/standard/core/num-start
  ; Start the process to create pictured numeric output.
  ;
  ; The new
  ; string is constructed from back to front, saving the new character
  ; at the beginning of the output string. Since we use PAD as a
  ; starting address and work backward (!), the string is constructed
  ; in the space between the end of the Dictionary (as defined by CP)
  ; and the PAD. This allows us to satisfy the ANS Forth condition that
  ; programs don't fool around with the PAD but still use its address.
Less_Number_Sign:
		lda #PadOffset		; init hold buffer index
		sta ToHold
	WordEnd
		rts


 WordHeader "#>",UF ; ( d -- addr u )  Finish pictured number conversion
  ; ## "#>"  auto	 ANS core
  ; https://forth-standard.org/standard/core/num-end
  ; Finish conversion of pictured number string, putting address and
  ; length on the Data Stack.
Number_Sign_Greater:
		jsr underflow_2		; double number

		clc			; addr= cp + ToHold
		lda cp+0
		adc ToHold
		sta DStack+2,x
		lda cp+1
		adc #0
		sta DStack+3,x

		sec			; u= PadOffset - ToHold
		lda #PadOffset
		sbc ToHold
		sta DStack+0,x
		lda #0
		sta DStack+1,x
	WordEnd
		rts


 WordHeader "Hold",0 ; ( char -- )  Insert character at current output
  ; ## "hold"  auto  ANS core
  ; https://forth-standard.org/standard/core/HOLD
  ; Prepend a character at the current position of a pictured numeric
  ; output string.
Hold:		jsr PopA
Hold_A:		dec ToHold
		ldy ToHold
		sta (cp),y
	WordEnd
		rts

; WordHeader "HoldS",NN ; ( c-addr u -- )  Insert string at current output
  ; ## "hold"  auto  ANS core
  ; https://forth-standard.org/standard/core/HOLDS
;HoldS:		jsr PopA
;		sta tmp2
;		jsr PopTmp1
;		jmp _8
;
;_2:		ldy DStack+0,x
;		lda (tmp1),y
;		jsr Hold_A
;_8:		dec DStack+0,x
;		bne _2
;		jmp Two_Drop
;	WordEnd

 WordHeader "#",UF+NN ; ( ud -- ud )  Add character to pictured output string
  ; ## "#"  auto	ANS core
  ; https://forth-standard.org/standard/core/num
  ; Add one char to the beginning of the pictured output string.
  ;
  ; Based on
  ; https://github.com/philburk/pforth/blob/master/fth/numberio.fth
  ; Forth code  BASE @ UD/MOD ROT 9 OVER < IF 7 + THEN [CHAR] 0 + HOLD ;
Number_Sign:
		jsr underflow_2		; double number

		; divide ud by base, quotient to ud, remainder to A
		lda #0			;   init remainder
		clc
		ldy #32+1		;   for each bit
_11:
		rol a			;   shift remainder

		cmp base		;   will it fit?
		bcc _27
		sbc base
_27:
		rol DStack+2,x		;   shift ud
		rol DStack+3,x
		rol DStack+0,x
		rol DStack+1,x

		dey
		bne _11

		; Convert the remainder to an ASCII character.
		cmp #9+1		; alternatively this could use s_abc_upper
		bcc +
		adc #7-1
+		adc #'0'
					; ( ud )
		bne Hold_A
	WordEnd


 WordHeader "#S",0 ; ( d -- d )  Convert all digits in pictured output
  ; ## "#s"  auto	 ANS core
  ; https://forth-standard.org/standard/core/numS
Number_Sign_S:
	;	jsr underflow_2	; Number_sign will check
_loop:
		jsr Number_sign	; convert a single number ("#")

		lda DStack+0,x	; until d is zero
		ora DStack+1,x
		ora DStack+2,x
		ora DStack+3,x
		bne _loop
	WordEnd
		rts


 WordHeader "Sign",NN ; ( n -- )  Add minus to pictured output
  ; ## "sign"  auto  ANS core
  ; https://forth-standard.org/standard/core/SIGN
Sign:		jsr PopYA
		tya		; test MSB of TOS
Sign_P: ; enter with N flag loaded
		bmi _minus
		rts

_minus:		lda #'-'	; add minus sign
		bne hold_a
	WordEnd


 WordHeader "Cleave",UF+NN ; ( addr u -- addr2 u2 addr1 u1 )  Split off word from string
  ; ## "cleave"  auto  Tali Forth
  ; Given a range of memory with words delimited by whitespace,return
  ; the first word at the top of the stack and the rest of the word
  ; following it.
  ;
  ; Example:
  ; s" w1 w2 w3" cleave  -> "w2 w3" "w1"
  ; s" w1" cleave	       -> "" "w1"
  ;
  ; Since it will be used in loops a lot, we want it to work in pure
  ; assembler and be as fast as we can make it. Calls PARSE-NAME so we
  ; strip leading delimiters.
Cleave:
		jsr underflow_2

		; We arrive here with ( addr u ). We need to strip any leading
		; spaces by hand: PARSE-NAME does do that, but it doesn't
		; remember how many spaces were stripped. This means we can't
		; calculate the length of the remainder. Fortunately, Tali
		; Forth has just the word we need for this:
		jsr Minus_leading	; -LEADING ( addr u )

		; The main part we can turn over to PARSE-NAME, except that we
		; have a string ( addr u ) and not stuff in the input buffer.
		; We get around this by cheating: We place ( addr u ) in the
		; input buffer and then call PARSE-NAME.
		jsr Input_To_R	; save old imput state

		lda DStack+0,x	; u is new ciblen
		sta ciblen
		lda DStack+1,x
		sta ciblen+1

		lda DStack+2,x	; addr is new cib
		sta cib
		lda DStack+3,x
		sta cib+1

		lda #0
		sta toin	; >IN pointer is zero
		sta toin+1

		; PARSE-NAME gives us back the substring of the first word
		jsr parse_name	; ( addr u addr-s u-s )

		; If we were given an empty string, then we're done. It's the
		; resposibility of the user to catch this as a sign to end the
		; any loop
		lda DStack+0,x
		ora DStack+1,x
		beq _done

		; Now we have to adjust the original string
		sec
		lda DStack+4,x	; LSB of original u
		sbc DStack+0,x
		sta DStack+4,x
		lda DStack+5,x	; MSB of original u
		sbc DStack+1,x
		sta DStack+5,x

		clc
		lda DStack+6,x	; LSB of original addr
		adc DStack+0,x
		sta DStack+6,x
		lda DStack+7,x	; MSB of original addr
		adc DStack+1,x
		sta DStack+7,x

		; There is one small problem: PARSE-NAME will probably have
		; left the string with the rest of the words with leading
		; delimiters. We use our magic -LEADING again
		jsr Two_swap		; ( addr-s u-s addr u )
		jsr Minus_leading
		jsr Two_swap		; ( addr u addr-s u-s )
_done:
		; Restore input
		jsr R_To_Input
	WordEnd
		rts


 WordHeader "HexStore",UF+NN ; ( addr1 u1 addr2 -- u2 )  Store a list of numbers
  ; ## "hexstore"	 auto  Tali
  ; Given a string addr1 u1 with numbers in the current base seperated
  ; by spaces, store the numbers at the address addr2, returning the
  ; number of elements. Non-number elements are skipped, an zero-length
  ; string produces a zero output.
Hexstore:
		jsr underflow_3

		jsr Dup		; Save copy of original address
		jsr Two_to_r		; ( addr1 u1 ) ( R: addr2 addr2 )

_loop:
		; Loop until string is totally consumed
		lda DStack+0,x
		ora DStack+1,x
		beq _done

		jsr Cleave		; ( addr1 u1 addr3 u3 ) ( R: addr2 addr2 )

		; Prepare the conversion of the number.
		jsr Two_to_r
		jsr Zero
		jsr Zero
		jsr Two_r_from	; ( addr1 u1 0 0 addr3 u3 ) ( R: addr2 addr2 )
		jsr To_Number	; ( addr1 u1 n n addr4 u4 ) ( R: addr2 addr2 )

		; If u4 is not zero, we have leftover chars and have to do
		; things differently
		lda DStack+0,x
		ora DStack+1,x
		bne _have_chars_left

		; Normal case, this number is all done
		jsr Two_drop		; ( addr1 u1 n n ) ( R: addr2 addr2 )
		jsr D_To_S		; ( addr1 u1 n ) ( R: addr2 addr2 )

		; Store the new value
		jsr R_Fetch		; ( addr1 u1 n addr2 ) ( R: addr2 addr2 )
		jsr C_Store		; ( addr1 u1 ) ( R: addr2 addr2 )

		; Increase counter
		jsr R_From		; R>
		jsr One_plus		; 1+
		jsr To_R		; >R ( addr1 u1 ) ( R: addr2+1 addr2 )
		jmp _loop

_have_chars_left:
		; Pathological case: Drop the rest of this number off the stack
		; and continue with the next word. Doesn't print a warning. We
		; need to drop four cells, that is, eight bytes
		txa
		clc
		adc #8
		tax
		bne _loop

_done:
		; Clean up return stack and calculate number of chars stored
		inx
		inx
		inx
		inx			; 2DROP

		jsr Two_r_from		; ( addr2+n addr2 )
		jsr Swap
		jmp Minus		; ( n )
	WordEnd


 WordHeader "Within",UF+NN ; ( n1 n2 n3 -- )  See if within a range
  ; ## "within"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/WITHIN
  ;
  ; This an assembler version of the ANS Forth implementation
  ; at https://forth-standard.org/standard/core/WITHIN which is
  ; OVER - >R - R> U<  note there is an alternative high-level version
  ; ROT TUCK > -ROT > INVERT AND
Within:
		jsr underflow_3

		jsr Over
		jsr Minus
		jsr To_R
		jsr Minus
		jsr R_From
		jmp U_Less_Than
	WordEnd


 WordHeader "\",IM+NN ; ( -- )  Ignore rest of line
  ; ## "\"  auto	ANS core ext
  ; https://forth-standard.org/standard/block/bs
  ; https://forth-standard.org/standard/core/bs
Backslash:
		lda BlkV+0		; interpreting a block?
		ora BlkV+1
		beq _not_block
			                ; We are in a block.

		; Was the "\" at the end of the line (so Parse advanced into the next line)?
		lda toin+0
		and #$3F
		cmp #2
		bcc _rts

                lda toin+0		; Move toin to next multiple of 64.
                and #$C0        	;   Clear lower bits to move to beginning of line.
                clc             	;   Add $40 (64 decimal) to move to next line.
                adc #$40
                sta toin+0
                bcc _rts
                inc toin+1
		rts

_not_block:
                lda ciblen+0
                sta toin+0
                lda ciblen+1
                sta toin+1
	WordEnd
_rts:		rts


 WordHeader "Move",NN+UF ; ( addr1 addr2 u -- )  Copy bytes, handle overlapping buffers
  ; ## "move"  auto  ANS core
  ; https://forth-standard.org/standard/core/MOVE
  ; Copy u "address units" from addr1 to addr2. Since our address
  ; units are bytes, this is just a front-end for CMOVE and CMOVE>. This
  ; is actually the only one of these three words that is in the CORE set.
Move:
		; We let CMOVE and CMOVE> check if there is underflow or
		; we've been told to copy zero bytes

		; compare MSB first
		lda DStack+3,x		; MSB of addr2
		cmp DStack+5,x		; MSB of addr1
		bne _ne
		; MSB were equal
		lda DStack+2,x		; LSB of addr2
		cmp DStack+4,x		; LSB of addr1
_ne:		bcc CMove
		bne CMove_up

		; addr1 & addr2 are the same, no copy needed

ThreeDrop:	txa		; drop three entries from Data Stack
		clc
		adc #6
		tax
	WordEnd
		rts

 WordHeader "CMove>",UF+NN ; ( add1 add2 u -- )  Copy bytes from high to low
  ; ## "cmove>"  auto  ANS string
  ; https://forth-standard.org/standard/string/CMOVEtop
  ; Based on code in Leventhal, Lance A. "6502 Assembly Language
  ; Routines", p. 201, where it is called "move right".
  ;
  ; There are no official tests for this word.
CMove_up:
		jsr underflow_3

		; Move destination address to where we can work with it
		lda DStack+2,x
		sta tmp2	; use tmp2 because easier to remember
		lda DStack+3,x
		clc
		adc DStack+1,x
		sta tmp2+1	; point to last page of destination

		; Move source address to where we can work with it
		lda DStack+4,x
		sta tmp1	; use tmp1 because easier to remember
		lda DStack+5,x
		clc
		adc DStack+1,x
		sta tmp1+1	; point to last page of source
		inc DStack+1,x	; allows us to use bne with dec DStack+1,x below

		; Move the last partial page first
		ldy DStack+0,x	; length of last page
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
		ldy #0
		lda (tmp1),y	; handle y = 0 separately
		sta (tmp2),y

_nopartial:
		dec tmp1+1	; back up to previous pages
		dec tmp2+1
		dec DStack+1,x
		bne _outerloop
_done:
		jmp ThreeDrop	; clear up the stack and leave
	WordEnd

Throw_Stack_14: jmp Throw_Stack


 WordHeader "CMove",UF+NN ; ( addr1 addr2 u -- )  Copy bytes going from low to high
  ; ## "cmove"  auto  ANS string
  ; https://forth-standard.org/standard/string/CMOVE
  ; Copy u bytes from addr1 to addr2, going low to high (addr2 is
  ; larger than addr1). Based on code in Leventhal, Lance A.
  ; "6502 Assembly Language Routines", p. 201, where it is called
  ; "move left".
  ;
  ; There are no official tests for this word.
CMove:
		jsr underflow_3

		; move destination address to where we can work with it
		lda DStack+2,x
		sta tmp2	; use tmp2 because easier to remember
		lda DStack+3,x
		sta tmp2+1

		; move source address to where we can work with it
		lda DStack+4,x
		sta tmp1	; use tmp1 because easier to remember
		lda DStack+5,x
		sta tmp1+1

		ldy #0
		lda DStack+1,x	; number of whole pages to move
		beq _dopartial

_page:
		lda (tmp1),y
		sta (tmp2),y
		iny
		bne _page

		inc tmp1+1
		inc tmp2+1
		dec DStack+1,x
		bne _page

_dopartial:
		lda DStack+0,x	; length of last page
		beq _done

_partial:
		lda (tmp1),y
		sta (tmp2),y
		iny

		dec DStack+0,x
		bne _partial

_done:		jmp ThreeDrop	; clear the stack
	WordEnd


 WordHeader "UM*",NN ; ( u1 u2 -- ud )  Multiply 16 x 16 -> 32  unsigned
  ; ## "um*"  auto  ANS core
  ; https://forth-standard.org/standard/core/UMTimes
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
  ;
  ; Consider switching to a table-supported version based on
  ; http://codebase64.org/doku.php?id=base:seriously_fast_multiplication
  ; http://codebase64.org/doku.php?id=magazines:chacking16#d_graphics_for_the_masseslib3d>
  ; http://forum.6502.org/viewtopic.php?p=205#p205
  ; http://forum.6502.org/viewtopic.php?f=9&t=689
UM_Star:
		DStackCheck 2,Throw_Stack_14

		; to eliminate clc inside the loop, the value at
		; tmp1 is reduced by 1 in advance
		clc		; tmp2= divsor-1
		lda DStack+0,x
		sbc #0
		sta tmp2+0
		lda DStack+1,x
		sbc #0
		sta tmp2+1
		bcc _zero	; is divisor zero?

		lda #0
		sta tmp1
		stx tmp3	; tested for exit from outer loop
		dex
		dex

_outer_loop:
		ldy #8		; counter inner loop
		lsr DStack+4,x	; think "DStack+2,x" then later "DStack+3,x"

_inner_loop:
		bcc _no_add
		sta tmp1+1	; save time, don't CLC
		lda tmp1
		adc tmp2
		sta tmp1
		lda tmp1+1
		adc tmp2+1

_no_add:
		ror
		ror tmp1
		ror DStack+4,x	; think "DStack+2,x" then later "DStack+3,x"

		dey
		bne _inner_loop ; go back for one more shift?

		inx
		cpx tmp3
		bne _outer_loop ; go back for eight more shifts?

		; all done, store high word of result
		sta DStack+1,x
		lda tmp1
		sta DStack+0,x
		rts

_zero:		lda #0
		sta DStack+2,x
		sta DStack+3,x
		rts
	WordEnd


 WordHeader "M*",NN ; ( n n -- d )  16 * 16 --> 32 signed
  ; ## "m*"  auto	 ANS core
  ; https://forth-standard.org/standard/core/MTimes
  ; Multiply two 16 bit numbers, producing a 32 bit result. All
  ; values are signed. Adapted from FIG Forth for Tali Forth.
M_Star:		DStackCheck 2,Throw_Stack_15

		; figure out the sign
		lda DStack+1,x	; MSB of n1
		eor DStack+3,x	; MSB of n2
		php

		; get the absolute value of both numbers so we can feed
		; them to UM*, which does the real work
		jsr Abs
		jsr Swap
		jsr Abs

		jsr UM_Star		; ( d )

		plp			; handle the sign
		bpl +

		jmp DNegate
+
	WordEnd
		rts

 WordHeader "*",0 ; ( n n -- n )  16*16 --> 16  signed or unsigned
  ; ## "*"  auto	ANS core
  ; https://forth-standard.org/standard/core/Times
  ; Multiply two signed 16 bit numbers, returning a 16 bit result.
Star:		; UM_Star will check for underflow
		jsr UM_Star
		inx		; D>S	drop hi cell
		inx
	WordEnd
		rts


 WordHeader "UM/Mod",NN ; ( ud u_div -- u_rem u_quot )  32/16 -> 16,16 division unsigned
  ; ## "um/mod"  auto  ANS core
  ; https://forth-standard.org/standard/core/UMDivMOD
  ; Divide double cell number by single cell number, returning the
  ; quotient as TOS and any remainder as NOS. All numbers are unsigned.
  ; This is the basic division operation all others use. Based on FIG
  ; Forth code, modified by Garth Wilson, see
  ; http://6502.org/source/integers/ummodfix/ummodfix.htm
UM_Slash_Mod:	DStackCheck 3,Throw_Stack_15

		lda DStack+0,x		; catch division by zero
		ora DStack+1,x
		beq _DivByZero

		ldy #16			; for 16 bits
_loop:
		rol DStack+4,x		;   rotate low cell of dividend one bit left
		rol DStack+5,x
		rol DStack+2,x		;   rotate high cell of dividend one bit left
		rol DStack+3,x
		bcs _subtract		;   overflow?

		lda DStack+2,x		;   dividend.hi >= divisor?
		cmp DStack+0,x
		lda DStack+3,x
		sbc DStack+1,x
		bcc _next

_subtract:	lda DStack+2,x		;   dividend.hi -= divisor
		sbc DStack+0,x
		sta DStack+2,x
		lda DStack+3,x
		sbc DStack+1,x
		sta DStack+3,x
		sec			;   it always fits

_next:		dey			;  next
		bne _loop
		rol DStack+4,x		; finish rotating quotient bit in
		rol DStack+5,x

		inx			; drop divisor
		inx

		jmp Swap

_DivByZero:	lda #$100+err_DivideBy0
		jmp ThrowA
	WordEnd

Throw_Stack_15: jmp Throw_Stack


; WordHeader "UD/Mod",NN ; ( ud u_div -- u_rem ud_quot )
  ; The following code is the ancient Forth word UD/MOD, which in
  ; various Forths (including Gforth) lives on under the hood,
  ; even though it's not an ANS standard word, it doesn't appear
  ; in the docs, it's only used here, and there are no tests for
  ; it. This is why we got rid of it
;UDSlashMod:	jsr To_R		; >r
;		jsr Zero		; 0
;		jsr R_Fetch		; r@
;		jsr UM_Slash_Mod	; um/mod
;		jsr Not_Rot		; rot rot
;		jsr R_From		; r>
;		jsr UM_Slash_Mod	; um/mod
;		jmp Rot			; rot
;	WordEnd


 WordHeader "SM/Rem",NN ; ( d n1 -- n2 n3 )  Symmetic signed division
  ; ## "sm/rem"  auto  ANS core
  ; https://forth-standard.org/standard/core/SMDivREM
  ; Symmetic signed division. Compare FM/MOD. Based on F-PC 3.6
  ; by Ulrich Hoffmann. See http://www.xlerb.de/uho/ansi.seq
SM_Slash_Rem:
		DStackCheck 3,Throw_Stack_15 ; contains double number

		; push MSB of high cell of d to Data Stack so we can check
		; its sign later
		lda DStack+3,x
		php

		; XOR the MSB of the high cell of d and n1 so we figure out
		; its sign later as well
		eor DStack+1,x
		php

		; Prepare division by getting absolute of n1 and d
		jsr Abs

		inx		; pretend we pushed n1 to R
		inx
		jsr DAbs
		dex
		dex

		jsr UM_Slash_Mod	; UM/MOD

		; if the XOR compiled above is negative, negate the
		; quotient (n3)
		plp
		bpl +
		jsr Negate
+
		; if d was negative, negate the remainder (n2)
		plp
		bpl _done

		inx		; pretend we pushed quotient to R
		inx
		jsr Negate
		dex
		dex

_done:
	WordEnd
		rts


 WordHeader "FM/Mod",NN ; ( d n1  -- rem n2 )  Floored signed division
  ; ## "fm/mod"  auto  ANS core
  ; https://forth-standard.org/standard/core/FMDivMOD
  ; Note that by default, Tali Forth uses SM/REM for most things.
  ;
  ; There are various ways to realize this. We follow EForth with
  ;    DUP 0< DUP >R  IF NEGATE >R DNEGATE R> THEN >R DUP
  ;    0<	 IF R@ + THEN  R> UM/MOD R> IF SWAP NEGATE SWAP THEN
  ; See (http://www.forth.org/eforth.html). However you can also
  ; go FM/MOD via SM/REM (http://www.figuk.plus.com/build/arith.htm):
  ;     DUP >R  SM/REM DUP 0< IF SWAP R> + SWAP 1+ ELSE  R> DROP THEN
FM_Slash_Mod:
		DStackCheck 3,Throw_Stack_15

		; if sign of n1 is negative, negate both n1 and d
		lda DStack+1,x	; MSB of n1
		php		;  save sign
		bpl +

		jsr Negate	; NEGATE
		inx
		inx
		jsr DNegate	; DNEGATE
		dex
		dex
+

		; If d is negative, add n1 to high cell of d
		lda DStack+3,x	; MSB of high word of d
		bpl +

		clc
		lda DStack+0,x	; LSB of n1
		adc DStack+2,x	; LSB of dh
		sta DStack+2,x
		lda DStack+1,x	; MSB of n1
		adc DStack+3,x	; MSB of dh
		sta DStack+3,x
+

		jsr UM_Slash_Mod	; ( d n1 -- rem n2 )

		; if n was negative, negate the result
		plp
		bpl +

		inx		; pretend that we SWAP
		inx
		jsr Negate
		dex
		dex
+
	WordEnd
		rts


 WordHeader "/Mod",NN ; ( n1 n_div -- n_rem n_quot )  Divide NOS by TOS signed with a remainder
  ; ## "/mod"  auto  ANS core
  ; https://forth-standard.org/standard/core/DivMOD
Slash_Mod:	jsr Dup			; ( n1 n_div n_div )
		ldy #0			; sign extend n1
		lda DStack+5,x
		bpl +
		dey
+		sty DStack+2,x
		sty DStack+3,x		; ( d1 n_div )

		jmp SM_Slash_Rem	; SM/REM
	WordEnd


 WordHeader "/",NN ; ( n1 n2 -- n )  Divide signed NOS by TOS
  ; ## "/"  auto	ANS core
  ; https://forth-standard.org/standard/core/Div
Slash:		jsr Slash_Mod
		jmp Nip		; Nip remainder
	WordEnd


 WordHeader "Mod",0 ; ( n1 n2 -- n )  Divide signed NOS by TOS and return the remainder
  ; ## "mod"  auto  ANS core
  ; https://forth-standard.org/standard/core/MOD
Mod:		jsr Slash_Mod

		inx		; Drop quotient
		inx
	WordEnd
		rts


 WordHeader "*/Mod",UF+NN ;  ( n1 n2 n3 -- n4 n5 )  n1 * n2 / n3 --> n-mod n
  ; ## "*/mod"  auto  ANS core
  ; https://forth-standard.org/standard/core/TimesDivMOD
  ; Multiply n1 by n2 producing the intermediate double-cell result d.
  ; Divide d by n3 producing the single-cell remainder n4 and the
  ; single-cell quotient n5.
Star_Slash_Mod:
		jsr underflow_3

		jsr To_R
		jsr M_Star
		jsr R_From
		jmp SM_Slash_Rem
	WordEnd


 WordHeader "*/",NN ; ( n1 n2 n3 -- n4 )  n1 * n2 / n3 -->  n
  ; ## "*/"  auto	 ANS core
  ; https://forth-standard.org/standard/core/TimesDiv
  ; Multiply n1 by n2 and divide by n3, returning the result
  ; without a remainder. This is */MOD without the mod.
  ;
  ; This word
  ; can be defined in Forth as : */  */MOD SWAP DROP ; which is
  ; pretty much what we do here
Star_Slash:
		; We let */MOD check for underflow
		jsr Star_Slash_Mod
		jmp Nip			; mod
	WordEnd


 WordHeader "M*/",NN ; ( d1 n1 n2 -- d2 )  Multiply signed d1 by n1 and divide by n2.
  ; ## "m*/"  auto  ANS double
  ; n2 may be negative.
  ; https://forth-standard.org/standard/double/MTimesDiv
  ; From All About FORTH, MVP-Forth, public domain,
  ; from this forth code which is modified slightly for Tali2:
  ; DDUP XOR SWAP ABS >R SWAP ABS >R OVER XOR ROT ROT DABS
  ; SWAP R@ UM* ROT R> UM* ROT 0 D+ R@ UM/MOD ROT ROT R> UM/MOD
  ; SWAP DROP SWAP ROT 0< if dnegate then
m_star_slash:
                jsr underflow_4

		lda DStack+1,x		; calc result sign
		eor DStack+3,x
		eor DStack+5,x
		php
                jsr Abs			; Abs
                jsr To_R		; >R		( d1 n1 ) ( R: sign abs(n2) )
                jsr Abs			; Abs
                jsr To_R		; >R		( d1 ) ( R: sign abs(n2) abs(n1) )
                jsr DAbs		; DAbs		( abs(d1) ) ( R: sign abs(n2) abs(n1) )

                jsr Swap		; Swap
                jsr R_Fetch		; R@
                jsr UM_Star		; UM*
                jsr Rot			; Rot
                jsr R_From		; R>
                jsr UM_Star		; UM*
                jsr Rot			; Rot
                jsr UMPlus		; 0 D+
                jsr R_Fetch		; R@
                jsr UM_Slash_Mod	; UM/Mod
                jsr Not_Rot		; Rot Rot
                jsr R_From		; R>
                jsr UM_Slash_Mod	; UM/Mod

                jsr Nip			; Swap Drop
                jsr Swap		; Swap
		plp
		bpl _rts		; ... 0< if ...
                jmp DNegate

_rts:		rts
	WordEnd


 WordHeader "Evaluate",NN ; ( addr u -- )  Evaluate a string as Forth
  ; ## "evaluate"	 auto  ANS core
  ; https://forth-standard.org/standard/block/EVALUATE
  ; https://forth-standard.org/standard/core/EVALUATE
  ; Set SOURCE-ID to -1, make addr u the input source, set >IN to zero.
  ; After processing the line, revert to old input source. We use this
  ; to compile high-level Forth words and user-defined words during
  ; start up and cold boot. In contrast to ACCEPT, we need to, uh,
  ; accept more than 255 characters here, even though it's a pain in
  ; the 8-bit.
Evaluate:
		clc			; signal to zero BLK.

load_evaluate: ; special entry point.
  ; Carry flags controls zeroing BLK.

		lda BlkV+1	; Save the current value of BLK on the return stack.
		pha
		lda BlkV+0
		pha

		bcs +		; See if we should zero BLK.
		
		lda #0		; Set BLK to zero.
		sta BlkV+0
		sta BlkV+1
+

		jsr Input_To_R	; Save the input state to the Return Stack

		lda #$ff	; SOURCE-ID= -1
		sta insrc+0
		sta insrc+1

		lda #0		; >IN= zero
		sta toin+0
		sta toin+1

		jsr PopYA	; ciblen= string length (u)
		sta ciblen+0
		sty ciblen+1

		jsr PopYA	; cib= string addr
		sta cib+0
		sty cib+1

		jsr interpret	; ( -- )

		jsr R_To_Input	; restore input state

		pla		; Restore BLK from the return stack.
		sta BlkV+0
		pla
		sta BlkV+1
	WordEnd
		rts


 .if 0  ; >Number has this inlined
 WordHeader "Digit?",UF+NN ; ( char -- u f | char f )  Convert ASCII char to number
  ; ## "digit?"  auto  Tali Forth
  ; Inspired by the pForth instruction DIGIT, see
  ; https://github.com/philburk/pforth/blob/master/fth/numberio.fth
  ; Rewritten from DIGIT>NUMBER in Tali Forth. Note in contrast to
  ; pForth, we get the base (radix) ourselves instead of having the
  ; user provide it. There is no standard name for this routine, which
  ; itself is not ANS; we use DIGIT? following pForth and Gforth.
Digit_Question:
		jsr underflow_1

		jsr Zero		; alloc room for the flag on the stack

		lda DStack+2,x		; get char
		cmp #'0'
		bcc _done		; failure flag already set
		cmp #'9'+1
		bcc _checkbase
		and #$df		; ASCII uppercase
		cmp #'A'
		bcc _done		; failure flag is already set
		sbc #7			; so 'A'=10 below
_checkbase:	sec			; finish conversion
		sbc #'0'
		cmp base		; must be < base
		bcs _done		; already have false flag

		; Found a legal number
		sta DStack+2,x		; put number in NOS
		dec DStack+0,x		; set success flag
		dec DStack+1,x
_done:
	WordEnd
		rts
  .endif


 WordHeader ">Number",UF+NN ; ( ud addr u -- ud addr u )  Continue convert a string to an integer
  ; ## ">number"	auto  ANS core
  ; https://forth-standard.org/standard/core/toNUMBER
  ; Convert a string to a double number. Logic here is based on the
  ; routine by Phil Burk of the same name in pForth, see
  ; https://github.com/philburk/pforth/blob/master/fth/numberio.fth
  ; for the original Forth code. We arrive here from NUMBER which has
  ; made sure that we don't have to deal with a sign and we don't have
  ; to deal with a dot as a last character that signalizes double -
  ; this should be a pure number string.
To_Number:
		jsr underflow_4

		lda DStack+0,x		; no chars left?
		beq _done
_Char_loop:
		lda (DStack+2,x)		; Get next character
		cmp #'0'		; convert to value (Digit_Question)
		bcc _done
		cmp #'9'+1
		bcc _digit
		and #$df		;   ASCII uppercase
		cmp #'A'
		bcc _done
		sbc #7 ;'A'-'0'-10	;   to make 'A'=10 below
_digit:		sec
		sbc #'0'
		cmp base		; must be < base
		bcs _done
		; Conversion was successful.

		pha			; Save char value

		; multiply ud by the radix from BASE.
		jsr Two_Over		;   copy ud to work (multiplicand)
		lda #0			;   zero ud (product)
		sta DStack+8,x
		sta DStack+9,x
		sta DStack+10,x
		sta DStack+11,x

		lda base		;   get multiplier
		bne _Mul_Test

_Mul_Add:	pha			;   save multiplier

		clc			;   ud += work
		lda DStack+2,x
		adc DStack+10,x
		sta DStack+10,x
		lda DStack+3,x
		adc DStack+11,x
		sta DStack+11,x
		lda DStack+0,x
		adc DStack+8,x
		sta DStack+8,x
		lda DStack+1,x
		adc DStack+9,x
		sta DStack+9,x

		pla			;   restore multiplier

_Mul_Shift:	asl DStack+2,x		;   work <<= 1
		rol DStack+3,x
		rol DStack+0,x
		rol DStack+1,x

_Mul_Test:	lsr a			;   shift multiplier
		bcs _Mul_Add
		bne _Mul_Shift

		inx			;   2Drop work
		inx
		inx
		inx

		pla			; recover value
		clc			; add to ud
		adc DStack+6,x
		sta DStack+6,x
		bcc +
		inc DStack+7,x
		bne +
		inc DStack+4,x
		bne +
		inc DStack+5,x
+

		jsr NOS_One_Plus	; consume the char

		dec DStack+0,x
		bne _Char_loop

_done:		; no chars left or we hit an unconvertable char
	WordEnd
		rts


 WordHeader "Number",UF+NN ; ( addr u -- u | d )  Convert a string to an integer
  ; ## "number"  auto  Tali Forth
  ; Convert a number string to a double or single cell number. This
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
  ; Number calls >NUMBER which in turn calls UM*,
  ; which uses tmp1, tmp2, and tmp3, so we can't use them here, which is
  ; a pain.
Number:
		jsr underflow_2

		; we keep the flags for sign and double in tmp4 because
		; we've run out of temporary variables
		lda #0
		sta tmp4+0	; flag for double
		sta tmp4+1	; flag for minus

		; Save the current base onto the stack.
		; This is done to handle constants in a different base
		; like #1234 and $ABCD and %0101
		lda base
		pha

		; Make a copy of the addr u in case we need to print an error message.
		jsr Two_dup		; ( addr u addr u )

		lda (DStack+2,x)	; get 1st char

		jsr _MinusCheck		; a minus sign?

		ldy #10			; decimal?
		cmp #'#'
		beq _base_changed

		ldy #16			; hex?
		cmp #'$'
		beq _base_changed

		ldy #2			; binary?
		cmp #'%'
		beq _base_changed

		cmp #$27		; ASCII for "'"
		bne _check_dot
		; Character constants should have a length of 3
		; and another single quote in position 3.
		lda DStack+0,x	; length=3 ?
		cmp #3
		bne _not_a_char
;		lda DStack+1,x
;		bne _not_a_char ; No compare needed to check for non-zero.
		; Compute location of last character
		; We know the string is 3 characters long, so last char
		; is known to be at offset +2.
		lda DStack+2,x	; LSB of address
		sta tmp5
		lda DStack+3,x
		sta tmp5+1
		ldy #2
		lda (tmp5),y
		cmp #$27	; ASCII for "'"
		bne _not_a_char
		dey		; The char we want is between the single quotes.
				; Grab the character and replace the string with just the char.
		lda (tmp5),y
		sta DStack+2,x
		lda #0
		sta DStack+3,x

		jmp _drop_original_string ; Single flag will drop the TOS for us.

_not_a_char:	; This label was just a bit too far away for a single bra from
		; the character checking code
		jmp _number_error

_base_changed:
		sty base	; Switch to the new base
		jsr NOS_One_Plus ; start one character later
		dec DStack+0,x	; decrease string length by one

		lda (DStack+2,x) ; Load the first char again

_check_dot:
		jsr _MinusCheck

		; If the last character is a dot, strip it off and set a
		; flag. We can use tmp5 as a temporary variable
		lda DStack+2,x	; LSB of address
		sta tmp5
		lda DStack+3,x
		sta tmp5+1

		ldy DStack+0,x	; Y now points to the first character after the string,
		dey		; but we need the last character

		lda (tmp5),y
		cmp #'.'
		bne _main

		; We have a dot, which means this is a double number.
		dec tmp4	; Flag the fact
		dec DStack+0,x	; reduce string length by one

_main:
		; Set up stack for subroutine jump to >NUMBER, which means
		; we have to go ( addr u addr u --> addr u ud addr u )

		jsr Two_Dup

		lda #0
		sta DStack+4,x	; clear ud
		sta DStack+5,x
		sta DStack+6,x
		sta DStack+7,x
				; ( addr u ud addr u )
		jsr To_Number	; ( ud addr u -- ud addr u )

		; test length of returned string, which should be zero
		lda DStack+0,x
		beq _all_converted

_number_error:
		; Something went wrong, we still have characters left over,
		; so we print an error and abort.

		jsr Two_drop	; >NUMBER modified addr u
		jsr Two_drop	; ud   (partially converted number)

		lda #'>'	; print the unknown
		jsr Emit_A	; word using the original addr u we saved at the beginning.
		jsr Type
		lda #'<'
		jsr Emit_A
		jsr Space

		pla		; restore base
		sta base

		lda #$100+err_UndefinedWord
		jmp ThrowA

_all_converted:
		; We can drop the string info
		inx		; Drop the current addr u
		inx
		inx
		inx
_drop_original_string:
		jsr TwoNip	 ; Drop the original addr u
				 ; (was saved for unknown word error message)

		; We have a double-cell number on the Data Stack that might
		; actually have a minus and might actually be single-cell
		lda tmp4	; flag for double
		beq _single

		; Set status bit 5 to indicate this is a double number
		lda #%00100000
		ora status
		sta status

		; This is a double cell number. If it had a minus, we'll have
		; to negate it
		lda tmp4+1
		beq _done	; no minus, all done

		jsr DNegate

		jmp _done

_single:
		; This is a single number
		inx		; convert ud to u
		inx

		; Clear status bit 5 to indicate this is a single number
		lda #$ff-%00100000
		and status
		sta status

		; If we had a minus, we'll have to negate it
		lda tmp4+1
		beq _done	; no minus, all done

		jsr Negate
_done:
		; Restore the base (in case it was changed by #/$/%)
		pla
		sta base
	WordEnd
		rts

_MinusCheck: ; If the first character is a minus, strip it off and set
		; the flag
		cmp #'-'		; a minus sign?
		bne +
		dec tmp4+1		; set flag
		jsr NOS_One_Plus	; start one character later
		dec DStack+0,x		; decrease string length by one
		lda (DStack+2,x)	; get next char
+		rts


 WordHeader "Hex",NN ; ( -- )  Set radix base to hexadecimal
  ; ## "hex"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/HEX
Hex:		lda #16
		bne decimal_a
	WordEnd

 WordHeader "Decimal",0 ; ( -- )  Set radix base to decimal
  ; ## "decimal"	auto  ANS core
  ; https://forth-standard.org/standard/core/DECIMAL
Decimal:	lda #10
decimal_a:	sta base+0
		lda #0
		sta base+1		; paranoid
	WordEnd
		rts

 WordHeader "Base",NN ; ( -- addr )  Push address of radix base to stack
  ; ## "base"  auto  ANS core
  ; https://forth-standard.org/standard/core/BASE
  ; The ANS Forth standard sees the base up to 36
		ldy #>base
		lda #<base
		jmp PushYA
	WordEnd

 WordHeader "Count",UF+NN ; ( c-addr -- addr u )  Convert counted character string to normal format
  ; ## "count"  auto  ANS core
  ; https://forth-standard.org/standard/core/COUNT
  ; Convert old-style character string to address-length pair. Note
  ; that the length of the string c-addr is stored in character length
  ; (8 bit), not cell length (16 bit). This is rarely used these days,
  ; though COUNT can also be used to step through a string character by
  ; character.
Count:
		jsr underflow_1

		lda (DStack+0,x)	; A= number of characters

		inc DStack+0,x		; move start address up by one
		bne +
		inc DStack+1,x
+
		jmp PushZA		; push number of characters, return
	WordEnd


 WordHeader "?PairCtlA",NN ; ( A n -- )  Check control structure pairing
QPairCtlA:	cmp DStack+0,x
		beq _8
		lda #$100+err_ControlMismatch
		jmp ThrowA

_8:		inx
		inx	; Drop n
		rts
	WordEnd


 .endsection code

;-----------------------------------------------------------------------
 .if 1 ; separate stack based DO...LOOP

 .section bss
DoLeave  .word ?	; head of leave addr patch chain
DoStkIndex: .byte ?
DoIndexL: .fill DoStkDim
DoIndexH: .fill DoStkDim
DoFufaL:  .fill DoStkDim
DoFufaH:  .fill DoStkDim
 .endsection bss
 .section code

 WordHeader "?Do",CO+IM+NN ; ( limit start -- )(R: -- limit start)  Conditional DO loop start
  ; ## "?do"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/qDO
Question_Do:
		jsr Do_Leave_Init

		lda #<_runtime
		ldy #>_runtime
		jsr Jsr_Comma_YA	; compile JSR _runtime

		lda #$d0		; compile BNE *+5
		ldy #3
		jsr Comma_YA

		jsr Leave		; compile JMP leave

		jmp Do_8		; finish
	WordEnd


_runtime: ; runtime routine for ?DO
		jsr Do_Runtime	; start normally (we assume TOS!=NOS)

		lda DoIndex+0	; Are TOS and NOS equal?
		beq _2
		rts		; return Z=0 to signal continue

_2:		lda DoIndex+1
		cmp #$80
		rts		; return Z


 WordHeader "Do",CO+IM+NN ; ( limit start -- )(R: -- limit start)  DO loop start
  ; ## "do"  auto	 ANS core
  ; https://forth-standard.org/standard/core/DO
  ;
  ; Compile-time part of DO. Could be realized in Forth as
  ;	: DO POSTPONE (DO) HERE ; IMMEDIATE COMPILE-ONLY
  ; but we do it in assembler for speed. To work with LEAVE, we compile
  ; a routine that pushes the end address to the Return Stack at run
  ; time. This is based on a suggestion by Garth Wilson, see
  ; the Control Flow section of the manual for details.
  ;
  ; Don't check for a stack underflow
Do:
		jsr Do_Leave_Init

		lda #<Do_runtime
		ldy #>Do_runtime
		jsr Jsr_Comma_YA	; compile JSR _runtime

Do_8:		jsr Here		; remember loop body start addr

		lda #<Do		; identifier
		jmp PushZA
					; ( saved_DoLeave loopback_Addr id )
	WordEnd


Do_Runtime: ; Runtime subroutine for DO
  ; Note that ANS loops quit when the
  ; boundry of limit-1 and limit is reached, a different mechanism than
  ; the FIG Forth loop (you can see which version you have by running
  ; a loop with start and limit as the same value, for instance
  ; 0 0 DO -- these will walk through the number space).
  ; We use a
  ; "fudge factor" for the limit that makes the Overflow Flag trip when
  ; it is reached; see http://forum.6502.org/viewtopic.php?f=9&t=2026
  ; for further discussion of this. The source given there for
  ; this idea is Laxen & Perry F83.
  ; This routine is called (DO) in some Forths.

		dec DoStkIndex		; alloc DO stack entry
		ldy DoStkIndex
		bmi _TooDeep

		lda DoIndex+0		; save previous DoIndex
		sta DoIndexL,y
		lda DoIndex+1
		sta DoIndexH,y

		sec			; fudge factor (FUFA)= $8000 - limit
		lda #0			;   the number that will trip the overflow flag
		sbc DStack+2,x
		sta DoFuFaL,y
		lda #$80
		sbc DStack+3,x
		sta DoFuFaH,y

		clc			; index= FUFA plus original index
		lda DStack+0,x
		adc DoFuFaL,y
		sta DoIndex+0
		lda DStack+1,x
		adc DoFuFaH,y
		sta DoIndex+1

		inx			; Drop orig index
		inx
		inx			; Drop orig limit
		inx

		rts			; return

_TooDeep:	lda #$100+err_DoLoop_TooDeep
		jsr ThrowA


 WordHeader "Loop",CO+IM+NN ; ( -- )  DO loop end
  ; ## "loop"  auto  ANS core
  ; https://forth-standard.org/standard/core/LOOP
  ; Compile-time part of LOOP.
Loop:
  .if 0
		lda #$e6		; compile INC DoIndex+0
		ldy #DoIndex+0
		jsr Comma_YA

		jsr Over
		lda #$d0		; compile BNE body
		jsr Branch_CommaA

		lda #<_Runtime2
		ldy #>_Runtime2
		jmp Plus_Loop_5
  .else
		lda #<_Runtime
		ldy #>_Runtime
		jmp Plus_Loop_5
  .endif
	WordEnd

_Runtime:
		inc DoIndex+0
		beq _Runtime2
		clv			; return V=0 to signal loop back
		rts

_Runtime2:	clc
		lda DoIndex+1		; handle carry from lo byte
		adc #1
		sta DoIndex+1
		rts			; return V to signal loop back


 WordHeader "+Loop",CO+IM+NN ; ( -- )  DO loop end
  ; ## "+loop"  auto  ANS core
  ; https://forth-standard.org/standard/core/PlusLOOP
  ;
  ; Compile-time part of +LOOP, also used for LOOP. Is usually
  ;	: +LOOP POSTPONE (+LOOP) , POSTPONE UNLOOP ; IMMEDIATE
  ;	COMPILE-ONLY
  ; in Forth. LOOP uses this routine as well. We jump here with the
  ; address for looping as TOS and the address for aborting the loop
  ; (LEAVE) as the second double-byte entry on the Return Stack (see
  ; DO and the Control Flow section of the manual for details).
Plus_Loop:
		lda #<Plus_Loop_Runtime
		ldy #>Plus_Loop_Runtime
Plus_Loop_5:	jsr Loop_End		; compile JSR _runtime, BVC back

		lda #<(Unloop-wh_LinkNt-1)
		ldy #>(Unloop-wh_LinkNt-1)
		jmp Compile_Comma_NT_YA ; compile Unloop, return
	WordEnd

Plus_Loop_Runtime:
		DStackCheck 1,Throw_Stack_12

		clc			; DoIndex += step
		lda DStack+0,x
		adc DoIndex+0
		sta DoIndex+0
		lda DStack+1,x
		adc DoIndex+1
		sta DoIndex+1
		inx			; Drop step
		inx
		rts			; return V=0 to signal loop back

Throw_Stack_12: jmp Throw_Stack


Loop_End: ; compile loop end; YA=runtime subroutine
					; ( saved_DoLeave body_addr id )
		jsr Jsr_Comma_YA	; compile JSR runtime

		lda #<Do		; check id
Loop_End_3:	jsr QPairCtlA
					; ( saved_DoLeave body_addr )
		lda #$50		; compile BVC body
		jsr Branch_CommaA
					; ( saved_DoLeave )
		ldy DoLeave+0		; for each leave addr entry
		lda DoLeave+1
		beq _p9
_p1:		sty tmp1+0		;   save entry pointer
		sta tmp1+1

		ldy #1			;   save link
		lda (tmp1),y
		pha
		dey
		lda (tmp1),y
		pha

		lda cp+0		;   patch addr
		sta (tmp1),y
		lda cp+1
		iny
		sta (tmp1),y

		pla			;   get saved link
		tay
		pla
		bne _p1
_p9:

		jsr PopYA		; restore DoLeave
		sta DoLeave+0
		sty DoLeave+1
		rts


 WordHeader "Unloop",CO ; ( -- )(R: n1 n2 n3 ---)  Drop DO loop control
  ; ## "unloop"  auto  ANS core
  ; https://forth-standard.org/standard/core/UNLOOP
Unloop:
		ldy DoStkIndex

		lda DoIndexL,y	; restore Index
		sta DoIndex+0
		lda DoIndexH,y
		sta DoIndex+1

		inc DoStkIndex	; drop Do stack entry
	WordEnd
		rts


 WordHeader "Leave",IM+NN+CO ; ( -- )  Leave DO/LOOP
  ; ## "leave"  auto  ANS core
  ; https://forth-standard.org/standard/core/LEAVE
  ; Note that this does not work with anything but a DO/LOOP in
  ; contrast to other versions such as discussed at
  ; http://blogs.msdn.com/b/ashleyf/archive/2011/02/06/loopty-do-i-loop.aspx
  ;
  ; See the Control Flow section in the manual for details of how this works.
Leave:
			; compile a JMP to leave addr, add to DoLeave chain
		lda #$4c	; JMP abs
		jsr C_Comma_A
		lda cp+1	; save cp
		pha
		lda cp+0
		pha
		lda DoLeave+0	; compile DoLeave link, patched later
		ldy DoLeave+1
		jsr Comma_YA
		pla		; update DoLeave
		sta DoLeave+0
		pla
		sta DoLeave+1
	WordEnd
		rts


Do_Leave_Init: ; ( -- saved_leave )
		lda DoLeave+0
		ldy DoLeave+1
		jsr PushYA	; save old leave head

		lda #0		; init
		sta DoLeave+1

		rts


 WordHeader "I",CO ; ( -- n )(R: n -- n)  Push DO loop index
  ; ## "i"  auto	ANS core
  ; https://forth-standard.org/standard/core/I
  ; Note that this is not the same as R@ ;
  ; see the Control Flow section of the manual for details.
I:		ldy DoStkIndex

		dex
		dex

		sec		; n= fudged index - fudge factor (FUFA)
		lda DoIndex+0
		sbc DoFuFaL,y
		sta DStack+0,x
		lda DoIndex+1
		sbc DoFuFaH,y
		sta DStack+1,x
	WordEnd
		rts


 WordHeader "J",CO ; ( -- n ) (R: n -- n )  Push second DO loop index
  ; ## "j"  auto	ANS core
  ; https://forth-standard.org/standard/core/J
  ; see the Control Flow section of the manual for more details.
J:		ldy DoStkIndex

		dex
		dex

		sec		; n= 2nd fudged index - 2nd fudge factor (FUFA)
		lda DoIndexL+0,y	; LSB
		sbc DoFufaL+1,y
		sta DStack+0,x
		lda DoIndexH+0,y	; MSB
		sbc DoFufaH+1,y
		sta DStack+1,x
	WordEnd
		rts

 .endsection code
 .endif ; separate stack based DO...LOOP

 .section code

 WordHeader 'Abort"',CO+IM+NN ; ( "string" -- )  If flag TOS is true, ABORT with message
  ; ## "abort""  tested  ANS core
  ; https://forth-standard.org/standard/exception/ABORTq
  ; https://forth-standard.org/standard/core/ABORTq
  ; Abort and print a string.
Abort_Quote:
		jsr S_Quote		; compile the string literal

		ldy #>_runtime		; compile JSR runtime, return
		lda #<_runtime
		jmp Jsr_Comma_YA
	WordEnd


_runtime: ; ( f addr u )
		lda DStack+4,x
		ora DStack+5,x
		bne _do_abort	; true?
				; we're done

		jmp ThreeDrop	; Drop three entries from the Data Stack

_do_abort:	; We're true
		jsr Type	; print string
		jsr CR		; We follow Gforth in going to a new line
		lda #$100+err_AbortQuote
		jmp ThrowA


 WordHeader "Abort",NN ; ( -- )  Reset the Data Stack and restart the CLI
  ; ## "abort"  tested  ANS core
  ; https://forth-standard.org/standard/exception/ABORT
  ; https://forth-standard.org/standard/core/ABORT
  ; We can jump here via
  ; subroutine if we want to because we are going to reset the 6502's
  ; stack pointer (the Return Stack) anyway during QUIT.
  ; It just changes where the exception shows it is thrown from.
  ; See also: Abort_Core
Abort:		lda #$100+err_Abort
		jmp ThrowA
	WordEnd


 WordHeader "PopA",NN ; ( n -- )  Pop TOS into A, with underflow check
PopA:		DStackCheck 1,Throw_Stack
		lda DStack+0,x		; pop TOS to A (1 byte)
		inx
		inx
	WordEnd
                rts

; WordHeader "PopA2",NN ; ( n1 n2 -- n1 )  Pop TOS into A, with underflow check for 2 params
PopA2:		DStackCheck 2,Throw_Stack
		lda DStack+0,x		; pop TOS to A (1 byte)
		inx
		inx
;	WordEnd
                rts

 WordHeader "PopYA",NN ; ( n -- )  Pop TOS into YA, with underflow check
PopYA:		DStackCheck 1,Throw_Stack
		lda DStack+0,x		; pop TOS to YA
		ldy DStack+1,x
		inx
		inx
	WordEnd
		rts


PopTmp1: ; ( n -- )  pop TOS to tmp1, with underflow check
		DStackCheck 1,Throw_Stack
		lda DStack+0,x	; PopYA
		ldy DStack+1,x
		inx
		inx
		sta tmp1+0
		sty tmp1+1
		rts

; Underflow test subroutines.
; We JSR to the label with the number of cells (not: bytes)
; required for the word. This flows into the exception handling code

 WordHeader "underflow_1",NN ;  Make sure we have at least one cell on the Data Stack
underflow_1:	DStackCheck 1,Throw_Stack
	WordEnd
		rts

 WordHeader "underflow_2",NN ;  Make sure we have at least two cells on the Data Stack
underflow_2:	DStackCheck 2,Throw_Stack
	WordEnd
                rts

; WordHeader "underflow_3",NN ;  Make sure we have at least three cells on the Data Stack
underflow_3:	DStackCheck 3,Throw_Stack
;	WordEnd
                rts

; WordHeader "underflow_4",NN ;  Make sure we have at least four cells on the Data Stack
underflow_4:	DStackCheck 4,Throw_Stack
;	WordEnd
                rts

Throw_Stack: ; overflow or underflow (from DStackCheck)
		bmi _over
		lda #$100+err_Stack_Underflow
		bne ThrowA

_over:		lda #$100+err_Stack_Overflow
		bne ThrowA

 .if "fp" in TALI_OPTIONAL_WORDS

Throw_FPStack: ; overflow or underflow (from FAllocX)
		bmi _over
		lda #$100+err_FPStackUnderflow
		bne ThrowA

_over:		lda #$100+err_FPStackOverflow
		bne ThrowA

Throw_FpOutOfRange:
		lda #$100+err_FpOutOfRange
		bne ThrowA
 .endif ; "fp"

 WordHeader "?Stack",NN ; ( -- )  Check stack bounds
QStack:		DStackCheck 0,Throw_Stack

		; could check return stack

 .if "fp" in TALI_OPTIONAL_WORDS
		ldy FIndex		; check floating point stack
		cpy #FDim+1
		bcs Throw_FPStack
  .endif
	WordEnd
		rts			; all OK


 WordHeader "Throw",NN ; ( n -- )  Throw an error
Throw:		jsr PopA		; pop n
ThrowA:		jsr Type_Exception_Text_A ; print the associated error string

		ldx #DStack0		; reset data stack (in case of underflow)

		; Exception frames & CATCH aren't implemented,
Abort_Core:	;   so we act like Abort in core
		jsr SLiteral_runtime
		  jmp +
		  .text "?",0," "	; signal an error to the simulator
+		jsr Type

		jsr R_From		; show return stack TOS
		jsr TypeSymbol
		jsr R_Fetch		; show return stack NOS
		jsr TypeSymbol

		jsr CR
Abort_Core2:
		lda #DoStkDim		; init do stack
		sta DoStkIndex
		jsr Empty_Stack		; empty the Data Stack & FP stack
		jmp Quit		; continue into QUIT.

Type_Exception_Text_A:
  ; input: A=error #
		sta tmp2+0	; save error code

		lda #<Exception_Text_List
		ldy #>Exception_Text_List	; for each table entry
		sta tmp3+0
		sty tmp3+1

_TestEntry:	ldy #0		;   code match?
		lda (tmp3),y
		beq _NotFound
		cmp tmp2+0
		beq _Found 

-		iny		;   step to end of entry
		lda (tmp3),y
		bne -
		tya		;   step to next entry
		sec
		adc tmp3+0
		sta tmp3+0
		bcc _TestEntry
		inc tmp3+1
		bne _TestEntry

_Found:
		ldy #1
;		lda (tmp3),y		; silent?
;		cmp #$ff
;		beq _Silent
		jmp Print_ASCIIZ_tmp3_no_lf

;_Silent:	rts

_NotFound:	ldy #$ff		; print code
		lda tmp2+0
		jsr PushYA
		jmp Dot
	WordEnd


 WordHeader "Empty-Stack",NN ; ( ? -- )  Empty data & FP stacks
Empty_Stack:	ldx #DStack0	; init data stack
.if "fp" in TALI_OPTIONAL_WORDS
		lda #FDim	; init FP stack
		sta FIndex
 .endif ; fp
	WordEnd
		rts


 WordHeader "Quit",NN ; ( -- ) Reset the input and get new input
  ; ## "quit"  tested  ANS core
  ; https://forth-standard.org/standard/core/QUIT
  ; Reset the input and start command loop
Quit:
		; Clear the Return Stack. This is a little screwed up
		; because the 6502 can only set the Return Stack via X,
		; which is our Data Stack pointer. The ANS specification
		; demands, however, that ABORT reset the Data Stack pointer
		txa		; Save the DStack that we just defined
		ldx #rsp0
		txs
		tax		; Restore the DStack. Dude, seriously.

		lda #0		; SOURCE-ID= zero (keyboard input)
		sta insrc+0
		sta insrc+1

		jsr Left_Bracket_NoCheck ; switch to interpret state
_get_line:
		lda #<buffer0	; input buffer, this is paranoid
		sta cib+0
		lda #>buffer0
		sta cib+1

	;	lda #0		; Size of current input buffer (CIB) is zero
	;	sta ciblen+0
	;	sta ciblen+1

		; Accept a line from the current input source.
		; This is how modern Forths do it.
		jsr Refill		; ( -- f )
		lda DStack+0,x
		bne +
		lda #$100+err_Refill	; REFILL returned a FALSE flag, something went wrong
		jmp ThrowA
+		inx			; drop the flag
		inx

		; Assume we have successfully accepted a string of input from
		; a source, with address cib and length of input in ciblen.

		; Main compile/execute routine
		jsr interpret

		; Display system prompt if all went well. If we're interpreting,
		; this is " ok", if we're compiling, it's " compiled". Note
		; space at beginning of the string.
		lda state
		bne _print_compiled

		lda #<str_ok
		ldy #>str_ok
		bne _print

_print_compiled: lda #<str_compiled	; "compile" string
		ldy #>str_compiled
_print:		jsr Print_ASCIIZ_YA

		; Awesome line, everybody! Now get the next one.
		jmp _get_line
	WordEnd			; no RTS required


Interpret: ; interpreter core
  ; called by EVALUATE and QUIT.
  ; Process one line only. Assumes that the address of name is in
  ; cib and the length of the whole input line string is in ciblen

                ; Normally we would use PARSE here with the SPACE character as
                ; a parameter (PARSE replaces WORD in modern Forths). However,
                ; Gforth's PARSE-NAME makes more sense as it uses spaces as
                ; delimiters per default and skips any leading spaces, which
                ; PARSE doesn't
_loop:
		jsr QStack		; check stack bounds

		jsr parse_name		; ( "string" -- addr u )
	;	jsr two_dup ;??debug
	;	jsr Type ;??debug
	;	jsr Space ; ??debug
                lda DStack+0,x		; empty line?
                beq _line_done

		; Go to FIND-NAME to see if this is a word we know. We have to
		; make a copy of the address in case it isn't a word we know and
		; we have to go see if it is a number
		jsr Two_dup		; ( addr u -- addr u addr u )
		jsr find_name		; ( addr u addr u -- addr u nt|0 )
		lda DStack+1,x		; word found?
		bne _got_name_token

		; let's see if this is a number.
                inx                     ; ( addr u 0 -- addr u )
                inx

                jsr Number           ; ( addr u -- u|d )
			; If the number conversion doesn't work, NUMBER will do the
			; complaining for us

                lda state		; interpreting?
                beq _loop		;   we're done

                lda #$20		; double cell number?
                bit status
                beq _single_number

		jsr Two_literal		; compile a double number
		jmp _loop

_single_number:	jsr Literal		; compile a single number
		jmp _loop

_got_name_token:			; ( addr u nt )
		jsr Nip
		jsr Nip			; ( nt )
		sta tmp1+0		; save a work copy of nt
		sty tmp1+1

		ldy #Wh_Flags		; get word flags, we'll need them shortly
		lda (tmp1),y		;    using saved nt

		ldy state		; interpreting or compiling?
		bne _compile
		; We are interpreting

		and #CO			; is the word COMPILE-ONLY?
		beq _execute
		lda #$100+err_CompileOnly ;   complain & quit
		jsr ThrowA

_execute:	jsr Name_To_Int		; ( nt -- xt )
		jsr Execute		; EXECUTE the xt that is TOS
                jmp _loop

_compile:	; We're compiling!
		and #IM			; is the word IMMEDIATE?
		bne _execute		;   IMMEDIATE word, execute now

		jsr Compile_Comma_NT	; Compile the nt into the Dictionary
		jmp _loop

_line_done:
		inx			; drop stuff from PARSE_NAME
		inx
		inx
		inx

		rts


 WordHeader "Immediate",NN ; ( -- )  Mark most recent word as IMMEDIATE
  ; ## "immediate"  auto	ANS core
  ; https://forth-standard.org/standard/core/IMMEDIATE
  ; Make sure the most recently defined word is immediate. Will only
  ; affect the last word in the dictionary. Note that if the word is
  ; defined in ROM, this will have no affect, but will not produce an
  ; error message.
Immediate:	lda #IM
SetFlag:	pha
		jsr current_to_dp
		ldy #Wh_Flags
		pla
		ora (dp),y
		sta (dp),y
	WordEnd
		rts

 WordHeader "Compile-only",NN ; ( -- )  Mark most recent word as COMPILE-ONLY
  ; ## "compile-only"  tested  Tali Forth
  ; Set the Compile Only flag (CO) of the most recently defined word.
  ;
  ; The alternative way to do this is to define a word
  ; ?COMPILE that makes sure  we're in compile mode
Compile_Only:	lda #CO
		bne SetFlag
	WordEnd

 WordHeader "never-native",NN ; ( -- )  Mark most recent word as never natively compiled
  ; ## "never-native"  auto  Tali Forth
Never_Native:	jsr current_to_dp
		ldy #Wh_Flags
		lda (dp),y
		ora #NN		; set NN flag
		and #$ff-AN	; clear AN flag
		sta (dp),y
	WordEnd
		rts

 WordHeader "always-native",NN ; ( -- )  Mark most recent word as always natively compiled
  ; ## "always-native"  auto  Tali Forth
Always_Native:	jsr current_to_dp
		ldy #Wh_Flags
		lda (dp),y
		ora #AN		; Make sure AN flag is set
		and #$ff-NN	; and NN flag is clear.
		sta (dp),y
	WordEnd
		rts

 WordHeader "allow-native",NN ; ( -- )  Mark most recent word to allow native compiling
  ; ## "allow-native"  auto  Tali Forth
Allow_Native:	jsr current_to_dp
		ldy #Wh_Flags	; offset for status byte
		lda (dp),y
		and #$ff-NN-AN	; AN and NN flag is clear.
		sta (dp),y
	WordEnd
		rts


 WordHeader "nc-limit",NN ; ( -- addr )  Variable: max # of bytes to inline
  ; ## "nc-limit"	 tested	 Tali Forth
		ldy #>nc_limit
		lda #<nc_limit
		jmp PushYA
	WordEnd

 WordHeader "strip-underflow",NN ; ( -- addr )  Variable: strip underflow flag
  ; ## "strip-underflow"	tested	Tali Forth
  ; `STRIP-UNDERFLOW` is a flag variable that determines if underflow
  ; checking should be removed during the compilation of new words.
  ; Default is false.
		ldy #>uf_strip
		lda #<uf_strip
		jmp PushYA
	WordEnd


 WordHeader "postpone",IM+CO+NN ; ( "name" -- )  Compile word, reducing IMMEDIATEness
  ; ## "postpone"	 auto	ANS core
  ; https://forth-standard.org/standard/core/POSTPONE
  ; Add the compilation behavior of a word to a new word at compile time.
  ; If the word that follows it is immediate, include
  ; it so that it will be compiled when the word being defined is
  ; itself used for a new word. Tricky, but very useful.
  ;
  ; Because POSTPONE expects a word (not an xt) in the input stream (not
  ; on the Data Stack). This means we cannot build words with
  ;   "jsr Postpone, jsr <word>" directly.
Postpone:
		jsr parse_name_check	; get name string
					; ( addr n )
		jsr find_name_check	; lookup name
					; ( nt | 0 )

					; tmp1 has a copy of nt

		ldy #Wh_Flags		; IMMEDIATE word?
		lda (tmp1),y		;    using saved nt
		and #IM
		beq _not_immediate

		; It is immediate
		jmp Compile_Comma_NT	; compile it as if it was not IMMEDIATE

_not_immediate:	; It is not an immediate word
		; We do "deferred compilation" by compiling ' <NAME> COMPILE,
		jsr LDYA_Immed_Comma		; compile LDA #; LDY # with nt of the word
		ldy #>Compile_Comma_NT_YA	; compile COMPILE,
		lda #<Compile_Comma_NT_YA
		jmp Jsr_Comma_YA
	WordEnd


 WordHeader "Recurse",CO+IM+NN ; ( -- )  Compile recursive call to word being defined
  ; ## "recurse"	auto  ANS core
  ; https://forth-standard.org/standard/core/RECURSE
  ; Compile a reference to the word that is being compiled.
Recurse:
		lda WorkWord+0
		ldy WorkWord+1
		jsr PushYA

		bit status		; does WorkWord contain xt or nt?
		bvc _xt

		ldy #wh_Flags		; is it Always-Native ?
		lda (WorkWord),y
		and #AN
		beq _NotAn
		lda #$100+err_InvalidRecursion
		jsr ThrowA
_NotAN:
		jsr Name_To_Int		; convert nt to xt
_xt:
		jmp Jsr_Comma		; compile JSR xt, return
	WordEnd


 WordHeader "Compile,",NN ; ( xt -- )  Compile xt
  ; ## "compile,"	 auto  ANS core ext
  ; https://forth-standard.org/standard/core/COMPILEComma
  ; Compile the given xt in the current word definition.
  ; Because we are using subroutine threading, we can't use
  ; , (COMMA) to compile new words the indirect-threaded way.
Compile_Comma:	
		jsr Dup			; ( xt xt )
		jsr Int_To_Name		; ( xt nt )	does a dictionary search, tmp1=nt
		inx			; drop nt
		inx
		lda tmp1+1
		bne Compile_Comma_NT_Tmp1
					; no word header available
		jmp Jsr_Comma		; compile jsr, return

Compile_Comma_NT: ; ( nt -- )
		jsr PopYA
Compile_Comma_NT_YA: ; ( YA=nt -- )
		sta tmp1+0		; tmp1= nt
		sty tmp1+1
Compile_Comma_NT_Tmp1:			; ( )
		jsr NameToIntTmp	; tmp2= xt
		lda tmp2+0
		ldy tmp2+1
		jsr PushYA		; ( xt )

		ldy #wh_CodeLength
		lda (tmp1),y
		jsr PushZA		; ( xt u )

		ldy #Wh_Flags		; save word flags
		lda (tmp1),y
		and #ST
		cmp #ST			; inline & strip RTS addr save/restore?
		beq _strip
		cmp #NN			; Never Native word?
		beq _jsr_opt
		cmp #AN			; Always Native word?
		beq _inline

		lda nc_limit+1		; wordsize<=nc_limit?
		bne _inline
		lda nc_limit+0
		cmp DStack+0,x
		beq _jsr_opt
		bcs _inline

_jsr_opt:
		lda uf_strip
		beq _jsr

		ldy #wh_Flags		; underflow strip & not stack strip?
		lda (tmp1),y
		and #UF
		bne _jsr

		lda #3			;   strip the underflow check
		jsr Nos_Plus_A

_jsr: ; Compile xt as a subroutine jump
		inx			; Drop len
		inx

		jmp Jsr_Comma		; compile jsr, return


_strip:		; --- SPECIAL CASE 1: PREVENT RETURN STACK THRASHINIG ---

		lda #6			;   Adjust xt: skip over the leading RTS addr save
		jsr Nos_Plus_A

		lda #$100-12		;   Adjust u: omit the leading RTS addr save & trailing RTS addr restore
		jsr minus_a

_inline: ; do native compile
		; We arrive here with the ( xt u )

		lda uf_strip+0		; user wants underflow stripping?
		beq +
		lda DStack+1,x		; this word contains underflow checking?
		and #UF
		beq +

		; --- SPECIAL CASE 2: REMOVE UNDERFLOW CHECKING ---

		lda #3			;   Adjust xt: Start after underflow check
		jsr Nos_Plus_A

		lda #$100-3		;   Adjust u: omit underflow check
		jsr minus_a
+
					; ( xt u )
		lda DStack+0,x		; compile code bytes
		beq _copy_end
_copy_loop:	lda (DStack+2,x)
		jsr C_Comma_A
		jsr NOS_One_Plus

		dec DStack+0,x
		bne _copy_loop
_copy_end:
		jmp Two_drop
	WordEnd


 WordHeader "[",IM+CO+NN ; ( -- )  Switch to interpret state
  ; ## "["  auto	ANS core
  ; https://forth-standard.org/standard/core/Bracket
Left_Bracket:	lda state+0		; Already in the interpret state?
		bne Left_Bracket_NoCheck
		lda #$100+err_AlreadyInterpreting
		jmp ThrowA

Left_Bracket_NoCheck:
		lda #0
Left_Bracket_3:	sta state+0
		sta state+1
	WordEnd
		rts

 WordHeader "]",IM+NN ; ( -- )  Switch to compile state
  ; ## "]"  auto	ANS core
  ; https://forth-standard.org/standard/right-bracket
Right_Bracket:
		lda state+0		; Already in the compile state?
		beq +
		lda #$100+err_AlreadyCompiling ;   complain and quit
		jmp ThrowA
+
		lda #$FF
		bne Left_Bracket_3
	WordEnd


 WordHeader "Literal",IM+CO+UF+NN ; ( n -- )  Compile code using TOS to push value at runtime
  ; ## "literal"	auto  ANS core
  ; https://forth-standard.org/standard/core/LITERAL
  ; Compile-only word to store TOS so that it is pushed on stack
  ; during runtime.
Literal:
		jsr underflow_1

		jsr LitCompile		; compile load regs, choose a runtime routine
		jmp Compile_Comma_NT_YA ; compile JSR runtime
	WordEnd

LitCompile: ; ( n -- ) compile code using TOS to push value at runtime
	; returns YA=nt for Compile_Comma_NT or or Jmp_Comma_NT_YA
		lda DStack+1,x		; hi byte zero?
		beq _ZByte

		jsr ldya_immed_comma	; compile "ldy #; lda #" using TOS
		lda #<(PushYA-wh_LinkNt-1) ; prepare for Compile_Comma_NT_YA or Jmp_Comma_NT_YA
		ldy #>(PushYA-wh_LinkNt-1)
		rts

_ZByte: ; zero-extended byte
		ldy DStack+0,x		; is it 0 ?
		beq _zero
	;	dey			; is it 1 ?
	;	beq _one
	;	dey			; is it 2 ?
	;	beq _two
		jsr lda_immed_comma	; compile "lda #" using TOS
		lda #<(PushZA-wh_LinkNt-1) ; prepare for Jsr_Comma_YA or Jmp_Comma_YA
		ldy #>(PushZA-wh_LinkNt-1)
		rts

_zero:		inx			; drop
		inx
		lda #<(Zero-wh_LinkNt-1)	; prepare for Jsr_Comma_YA or Jmp_Comma_YA
		ldy #>(Zero-wh_LinkNt-1)
		rts

;_one:		inx			; Drop
;		inx
;		lda #<(One-wh_LinkNt-1)	; prepare for Jsr_Comma_YA or Jmp_Comma_YA
;		ldy #>(One-wh_LinkNt-1)
;		rts

;_two:		inx			; Drop
;		inx
;		lda #<(Two-wh_LinkNt-1)	; prepare for Jsr_Comma_YA or Jmp_Comma_YA
;		ldy #>(Two-wh_LinkNt-1)
;		rts


 WordHeader "LDYA#,",NN ; ( n -- )  compile "ldy #msb; lda #lsb"
ldya_immed_comma:
		lda #$a0		; ldy #
		ldy DStack+1,x
		jsr Comma_YA

lda_immed_comma: ; ( n -- ) compile "lda #"
		lda #$a9		; lda #
		jsr C_Comma_A
		jmp C_Comma
	WordEnd

 WordHeader "PushYA",0 ; ( YA -- n )  Push YA to data stack
PushYA:		dex
		dex
		sta DStack+0,x
		sty DStack+1,x
	WordEnd
		rts

 WordHeader "True",0 ; ( -- f )  Push TRUE flag to Data Stack
  ; ## "true"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/TRUE
True:		lda #$FF
PushAA:		dex
		dex
		sta DStack+0,x
		sta DStack+1,x
	WordEnd
		rts

 WordHeader "False",NN ; ( -- f )  Push flag FALSE to Data Stack
  ; ## "false"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/FALSE
False:		lda #0
		beq PushZA
	WordEnd

 WordHeader "PushZA",0 ; ( A -- n )  Push zero-extended A to the data stack
PushZA:		dex
		dex
		sta DStack+0,x
		lda #0
		sta DStack+1,x
	WordEnd
		rts

 WordHeader "0",NN ; ( -- 0 )  Push 0 to Data Stack
  ; ## "0"  auto	Tali Forth
  ; This routine preserves Y.
Zero:		lda #0
		beq PushZA
	WordEnd

 WordHeader "1",NN ; ( -- n )  Push the number 1 to the Data Stack
  ; ## "1"  auto	Tali Forth
One:		lda #1
		bne PushZA
	WordEnd

 WordHeader "2",NN ; ( -- u )  Push the number 2 to stack
  ; ## "2"  auto	Tali Forth
Two:		lda #2
		bne PushZA
	WordEnd

 WordHeader "Bl",NN ; ( -- c )  Push ASCII value of SPACE to stack
  ; ## "bl"  auto	 ANS core
  ; https://forth-standard.org/standard/core/BL
Bl:		lda #AscSP
		bne PushZA
	WordEnd

 WordHeader ">In",NN ; ( -- addr )  Return address of the input pointer
  ; ## ">in"  auto  ANS core
  ; https://forth-standard.org/standard/core/toIN
		lda #ToIn
		jmp PushZA	; jmp to be a recognizable constant
	WordEnd

 WordHeader "State",NN ; ( -- addr )  Return the address of compilation state flag
  ; ## "state"  auto  ANS core
  ; https://forth-standard.org/standard/core/STATE
  ; STATE is true when in compilation state, false otherwise. Note
  ; we do not return the state itself, but only the address where
  ; it lives. The state should not be changed directly by the user; see
  ; http://forth.sourceforge.net/standard/dpans/dpans6.htm#6.1.2250
		lda #state
		jmp PushZA	; jmp to be a recognizable constant
	WordEnd

 WordHeader "dp",NN ; ( -- addr )  Variable: nt of current word
		lda #dp
		jmp PushZA	; jmp to be a recognizable constant
	WordEnd

 WordHeader "Tmp1",NN ; ( -- addr ) Variable: zp work area
		lda #tmp1
		jmp PushZA	; jmp to be a recognizable constant
	WordEnd


 WordHeader "2Literal",UF+IM+NN ; (C: d -- ) ( -- d)  Compile a literal double word
  ; ## "2literal"	 auto  ANS double
  ; https://forth-standard.org/standard/double/TwoLITERAL
Two_literal:
		jsr underflow_2 ; check double number

		jsr Swap
		jsr Literal	; do lo cell
		jmp Literal	; do hi cell
	WordEnd


 WordHeader "SLiteral",CO+IM+UF+NN ; ( addr u -- )( -- addr u )  Compile a string for runtime
  ; ## "sliteral" auto  ANS string
  ; https://forth-standard.org/standard/string/SLITERAL
  ; Add the runtime for an existing string.
SLiteral:
		jsr underflow_2

		jsr SLiteral_Start	; compile header
					; ( addr u herej )
		; We can't assume that ( addr u ) of the current string is in
		; a stable area (eg. already in the dictionary.)
		; Copy the string data into the dictionary.
		jmp _Move_Test
_Move_Loop:	lda (DStack+4,x)
		jsr C_Comma_A
		inc DStack+4,x
		bne +
		inc DStack+5,x
+
		jsr NOS_One_Minus

_Move_Test:	lda DStack+2,x
		ora DStack+3,x
		bne _Move_Loop

		jsr SLiteral_End
					; ( addr u )
		jmp Two_Drop		; clean up and leave
	WordEnd


SLiteral_Start: ; ( -- herej )  compile header

		lda #<SLiteral_runtime	; compile JSR SLiteral_Runtime
		ldy #>SLiteral_runtime
		jsr Jsr_Comma_YA

		jsr Here		; push addr of start of JMP

		jmp Jmp_Comma_YA	; compile JMP around string (dummy addr)


SLiteral_End: ; ( herej -- )  compile trailer
		jsr One_Plus
		lda cp+0		; Update the address of the jump to HERE.
		sta (DStack+0,x)
		jsr One_Plus
		lda cp+1
		sta (DStack+0,x)
		inx			; drop herej
		inx
		rts


SLiteral_Runtime: ; Push ( addr u ) of string to the Data Stack.
		stx tmp2	; save data stack index
		tsx
SLiteral_Run2:	lda RStack+1,x	; tmp1= RTS addr
		sta tmp1+0
		lda RStack+2,x
		sta tmp1+1
		ldx tmp2	; restore data stack index

		dex		; push string addr
		dex
		clc
		lda tmp1+0
		adc #3+1	;   just after the following JMP + correcting for JSR behavior
		sta DStack+0,x
		lda tmp1+1
		adc #0
		sta DStack+1,x

		dex		; push string length
		dex
		sec
		ldy #2		;   = string_end - string_addr
		lda (tmp1),y
		sbc DStack+2,x
		sta DStack+0,x
		iny
		lda (tmp1),y
		sbc DStack+3,x
		sta DStack+1,x

		rts


 WordHeader '."',CO+IM+NN ; ( "string" -- )  Print string in compiled word
  ; ## ".""  auto	 ANS core ext
  ; https://forth-standard.org/standard/core/Dotq
  ; Compile string that is printed during run time. ANS Forth wants
  ; this to be compile-only, even though everybody and their friend
  ; uses it for everything. We follow the book here, and recommend
  ; `.(` for general printing.
Dot_quote:
		jsr S_Quote		; compile the string literal

		ldy #>Type		; compile: print string
		lda #<Type
		jmp Jsr_Comma_YA
	WordEnd


; WordHeader 'C";,IM+NN ; ( "string" -- )( -- addr )  compile counted string literal
  ; ## "c""  ???  ANS core ext
  ; https://forth-standard.org/standard/core/Cq


 WordHeader 'S"',IM+NN ; ( "string" -- )( -- addr u )  Store string in memory
  ; ## "s""  auto	 ANS core
  ; https://forth-standard.org/standard/core/Sq
  ; Store address and length of string given, returning ( addr u ).
  ; ANS core claims this is compile-only, but the file set expands it
  ; to be interpreted, so it is a state-sensitive word, which in theory
  ; are evil. We follow general usage.
  ;
  ; Can also be realized as
  ;     : S" [CHAR] " PARSE POSTPONE SLITERAL ; IMMEDIATE
  ; but it is used so much we want it in code.
S_Quote:
		lda #0		; Don't handle escaped chars.

S_Quote_start: ; A=escaped chars flag
		pha			; save "handle escaped chars" flag

		lda state		; compiling?
		beq _15
		jsr SLiteral_Start	; compile SLiteral header
		jmp _19
_15:		jsr Here		; remember start addr
_19:					; ( startaddr|jmpaddr )

		pla			; init t2 flags
		jsr PushZA
		jsr Zero		; init t3 flags
					; ( startaddr|jmpaddr tmp2 tmp3 )

_savechars_loop:
		; Start saving the string into the dictionary up to the
		; ending double quote.

		lda toin+0		; LSB
		cmp ciblen+0
		lda toin+1		; MSB
		sbc ciblen+1
		bcc _input_fine		; input buffer empty?

		; Input buffer is empty.
		jsr Refill		; Refill it  ( -- f )
		lda DStack+0,x		; Check result of refill.
	;	ora DStack+1,x
		bne +
		lda #$100+err_refill	; Something when wrong with refill.
		jmp ThrowA
+		inx			; Drop the refill flag
		inx

		bne _savechars_loop	; jump back up to the empty check, just in
					; case refill gave us an empty buffer (eg. empty/blank line of
					; input)

_input_fine:	; There is at least one valid char to use.

		clc			; tmp1= A= the char at Cib+ToIn
		lda cib+0
		sta tmp1+0
		lda cib+1
		adc toin+1
		sta tmp1+1
		ldy ToIn
		lda (tmp1),y
		sta tmp1

		inc toin+0		; Move on to the next character.
		bne +
		inc toin+1
+
		ldy DStack+2,x		; handling escaped characters?
		bmi +
		jmp _regular_char
+
		; We are handling escaped characters.
		ldy DStack+3,x  		; already seen the backslash?
		bmi +
		jmp _not_escaped
+
		; We have seen a backslash (previous character).
		sty tmp1+1
		bit tmp1+1		; in the middle of a \x sequence ?
		bvs _check_esc_chars

		; We are in the middle of a \x sequence. Check to see if we
		; are on the first or second digit.
		lda #1
		bit tmp1+1
		bne _esc_x_second_digit

		; First digit.
		iny			; Adjust flag for second digit next time.
		sty DStack+3,x
		lda tmp1+0		; Get the char again.

		jsr convert_hex_value	; Convert to hex

		asl			; This is the upper nybble, so move it up.
		asl
		asl
		asl
		sta DStack+0,x		; Save it for later.
		jmp _next_character

_esc_x_second_digit: ; We are on the second hex digit of a \x sequence.

		lda #0			; Clear the escaped character flag
		sta DStack+3,x		;   (because we are handling it right here)

		lda tmp1+0		; Convert to hex
		jsr convert_hex_value
		ora DStack+0,x		; combine with value in t3

		jmp _save_character

_check_esc_chars:
		; Clear the escaped character flag (because we are
		; handling it right here)
		ldy #0
		sty DStack+3,x

		; Process the escaped character
		tay

		lda #AscBELL	      ; BEL (ASCII value 7)
		cpy #'a'
		beq _save_character

		lda #AscBS	      ; Backspace (ASCII value 8)
		cpy #'b'
		beq _save_character

		lda #AscESC	       ; ESC (ASCII value 27)
		cpy #'e'
		beq _save_character

		lda #AscFF	       ; FF (ASCII value 12)
		cpy #'f'
		beq _save_character

		lda #AscLF	       ; LF (ASCII value 10)
		cpy #'l'
		beq _save_character

;		lda #AscLF		; newline, impl. dependant, using LF (ASCII values 10)
		cpy #'n'
		beq _save_character

		; This one is not like the others because we save two
		; characters
		cpy #'m'
		bne +
		lda #AscCR		; CR/LF pair (ASCII values 13, 10)
		jsr C_Comma_A
		lda #AscLF
		bne _save_character
+

		lda #AscDQuote		; Double quote (ASCII value 34)
		cpy #'q'
		beq _save_character

;		lda #AscDQuote		; Double quote (ASCII value 34)
		cpy #AscDQuote
		beq _save_character

		lda #AscCR		; CR (ASCII value 13)
		cpy #'r'
		beq _save_character

		lda #AscHT		; Horizontal TAB (ASCII value 9)
		cpy #'t'
		beq _save_character

		lda #AscVT		; Vertical TAB (ASCII value 11)
		cpy #'v'
		beq _save_character

		lda #0			; NULL (ASCII value 0)
		cpy #'z'
		beq _save_character

_check_esc_x:
		cpy #'x'
		bne +
		; This one is difficult. We need to get the next TWO
		; characters (which might require a refill in the middle)
		; and combine them as two hex digits. We do this by
		; clearing bit 6 of t2+1 to indicate we are in a digit
		; and using bit 0 to keep track of which digit we are on.
		lda #$BE	; Clear bits 6 and 0
		sta DStack+3,x
		bne _next_character
+

		lda #AscBackslash	; Backslash (ASCII value 92)
		cpy #AscBackslash
		beq _save_character

		tya

_not_escaped:
		; Check for the backslash to see if we should escape
		; the next char.
		cmp #$5C	; The backslash char
		bne _regular_char

		; We found a backslash.	 Don't save anyhing, but set
		; a flag (in t2+1) to handle the next char. We don't
		; try to get the next char here as it may require a
		; refill of the input buffer.
		lda #$FF
		sta DStack+3,x
		bne _next_character

_regular_char:
		; Check if the current character is the end of the string.
		cmp #AscDQuote		; ASCII for "
		beq _found_string_end

_save_character:
		jsr C_Comma_A		; compile this character into the dictionary

_next_character:
		jmp _savechars_loop

_found_string_end:
					; ( startaddr|jmpaddr t2 t3 )
		inx			; Drop t3
		inx
					; ( startaddr|jmpaddr t2 )
		lda state		; What happens next depends on the state (which is bad, but
		bne _cmpl		; that's the way it works at the moment).

					; We are interpretating
					; (used for file calls, see https://forth-standard.org/standard/file/Sq . 

		sec			; TOS= length of string = Here - start_addr
		lda cp+0
		sbc DStack+2,x
		sta DStack+0,x		;   LSB
		lda cp+1
		sbc DStack+3,x
		sta DStack+1,x		;   MSB

		rts			; ( startaddr u )

_cmpl:					; We're compiling
		inx			; Drop t2
		inx
		jmp SLiteral_End	; ( jmpaddr )
	WordEnd


convert_hex_value: ; convert a character
	; from ASCII to the corresponding hex value, eg 'F'->15

		cmp #'A'
		bcc _digit

_alpha:		and #$1F		; Make it uppercase.
		adc #9-1		; gives value 10 for 'A'
		rts

_digit: ; It's 0-9
		sbc #'0'-1
		rts


 WordHeader 'S\"',IM+NN ; ( "string" -- )( -- addr u )  Store string in memory
  ; ## "s\""  auto  ANS core
  ; https://forth-standard.org/standard/core/Seq
  ; Store address and length of string given, returning ( addr u ).
  ; ANS core claims this is compile-only, but the file set expands it
  ; to be interpreted, so it is a state-sensitive word, which in theory
  ; are evil. We follow general usage. This is just like S" except
  ; that it allows for some special escaped characters.
S_Backslash_Quote:
		lda #$ff	; Do handle escaped chars.
		jmp S_Quote_start
	WordEnd


 WordHeader "LatestXt",NN ; ( -- xt )  Push most recent xt to the stack
  ; ## "latestxt"	 auto  Gforth
  ; http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Anonymous-Definitions.html
LatestXt:	jsr LatestNt	; ( nt )
		jmp Name_To_Int	; ( xt )
	WordEnd

 WordHeader "LatestNt",NN ; ( -- nt )  Push most recent nt to the stack
  ; ## "latestnt"	 auto  Tali Forth
  ; www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
  ; The Gforth version of this word is called LATEST
LatestNt:	jsr current_to_dp
		lda dp+0
		ldy dp+1
		jmp PushYA
	WordEnd


current_to_dp: ; dp = wordlists[current]
  ; Look up the current (compilation) dictionary pointer
  ; in the wordlist set and put it into the dp zero-page
  ; variable. Uses A and Y.
		lda CurrentV		; A= current wordlist wid

		asl			; dp= wordlists[A]
		tay
		lda WordlistsV+0,y
		sta dp+0
		lda WordlistsV+1,y
		sta dp+1

		rts


dp_to_current: ; wordlists[current] = dp
  ; Look up which wordlist is current and update its pointer
  ; with the value in dp. Uses A and Y.
		lda CurrentV		; A= current wordlist wid

                asl			; wordlists[A]=dp
                tay
                lda dp+0
		sta WordlistsV+0,y
                lda dp+1
                sta WordlistsV+1,y

                rts


 WordHeader "Parse-Name",NN ; ( "name" -- addr u )  Get a whitespaced delimited string from the input 
  ; ## "parse-name"  auto	 ANS core ext
  ; https://forth-standard.org/standard/core/PARSE-NAME
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
Parse_Name:
		; PARSE-NAME and PARSE must be able to handle input strings
		; > 255 chars, so we can't just put toin in Y.

		lda cib+0		; tmp2= cib+(toin & $ff00)
		sta tmp2+0
		clc
		lda cib+1
		adc toin+1
		sta tmp2+1
		ldy toin+0		; Y= toin & $00ff

_skip_loop:
		cpy ciblen+0		; more chars left?
		lda toin+1
		sbc ciblen+1
		bcs _empty_line

		lda (tmp2),y		; get cib[toin]

		cmp #AscSP+1		; is_whitespace
		bcs _char_found
					; Char is still whitespace

		iny			; increment toin
		sty toin+0
		bne _skip_loop
		inc toin+1
		inc tmp2+1
		bne _skip_loop

_empty_line:
		; Neither the ANS Forth nor the Gforth documentation say
		; what to return as an address if a string with only
		; spaces is given.
		dex			; junk address
		dex
		jmp Zero		; length=0

_char_found: ; at cib[toin]
		jsr Bl			; push delimiter char
		jmp Parse
	WordEnd


parse_name_check: ; Do Parse-Name & abort if nothing found
		jsr parse_name		; get character from string
					; ( addr u )
		lda DStack+0,x		; empty string?
		beq _empty
		rts

_empty:		lda #$100+err_UndefinedWord	; complain & abort
		jmp ThrowA


 WordHeader "Parse",NN ; ( "name" c -- addr u )  Get a delimited string from the input
  ; ## "parse"  tested  ANS core ext
  ; https://forth-standard.org/standard/core/PARSE
  ; Find word in input string delimited by character given. Do not
  ; skip leading delimiters -- this is the main difference to PARSE-NAME.
  ; PARSE and PARSE-NAME replace WORD in modern systems. ANS discussion
  ; http://www.forth200x.org/documents/html3/rationale.html#rat:core:PARSE
  ;
  ;
  ;     cib  cib+toin   cib+ciblen
  ;      v      v		   v
  ;     |###################|
  ;
  ;     |------>|	 toin (>IN)
  ;     |------------------->|  ciblen
  ;
  ; The input string is stored starting at the address in the Current
  ; Input Buffer (CIB), the length of which is in CIBLEN. While searching
  ; for the delimiter, TOIN (>IN) points to the where we currently are.
  ; Since PARSE does not skip leading delimiters, we assume we are on a
  ; useful string if there are any characters at all. As with
  ; PARSE-NAME, we must be able to handle strings with a length of
  ; 16-bit for EVALUTE, which is a pain on an 8-bit machine.
Parse:		jsr PopA		; pop c (check for underflow)
Parse_A:	sta tmp5		; save delimiter

		dex			; alloc space for addr
		dex
		dex			; alloc space for length
		dex

		clc			; addr= cib+toin
		lda toin+0
		adc cib+0
		sta DStack+2,x
		lda toin+1
		adc cib+1
		sta DStack+3,x

		ldy toin+0		; Y= toin & $00ff
		sty tmp1+0
		lda cib+0		; tmp2= cib + (toin & $ff00)
		sta tmp2+0
		clc
		lda toin+1		; tmp1= toin
		sta tmp1+1
		adc cib+1
		sta tmp2+1

		lda #0		; Initialize the offset we use to adjust EOL or found delimiter
		sta tmp5+1

_loop:
		cpy ciblen+0		; end of string?
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

		lda tmp5+0		; get delimiter
		cmp #AscSP		; is it whitespace?
		bne _not_whitespace

		lda (tmp2),y		; get cib[toin]
		cmp #AscSP+1		; we're looking for all whitespace
	;	jsr is_whitespace
		bcc _found_delimiter
		bcs _not_delimiter

_not_whitespace: cmp (tmp2),y		; compare delimter to cib[toin]
		beq _found_delimiter
_not_delimiter:
		iny			; next character
		bne _loop
		inc toin+1
		inc tmp2+1
		bne _loop

_found_delimiter:
		; Increase the offset: If we've found a delimiter, we want
		; TOIN to point to the character after it, not the delimiter
		; itself
		inc tmp5+1
_eol:
		sec			; length = toin - tmp1
		tya
		sbc tmp1+0
		sta DStack+0,x
		lda toin+1
		sbc tmp1+1
		sta DStack+1,x

		clc			; toin = toin + delimiter_offset
		tya
		adc tmp5+1
		sta toin+0
		bcc +
		inc toin+1
+
	WordEnd
		rts


 WordHeader "Execute-Parsing",UF+NN ; ( addr u xt -- )  Pass a string to a parsing word
  ; ## "execute-parsing"	auto  Gforth
  ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/The-Input-Stream.html
  ; Execute the parsing word defined by the execution token (xt) on the
  ; string as if it were passed on the command line. See the file
  ; tests/tali.fs for examples.
  ;
  ; Note that this word is coded completely
  ; different in its Gforth version, see the file execute-parsing.fs
  ; (in /usr/share/gforth/0.7.3/compat/ on Ubuntu 18.04 LTS) for details.
Execute_parsing:
		jsr underflow_3

		jsr Input_To_R		; save existing input for later
		jsr Not_Rot		; -ROT ( xt addr u )

		jsr PopYA		; TOS is new ciblen
		sta ciblen+0
		sty ciblen+1

		jsr PopYA		; NOS is new cib
		sta cib+0
		sty cib+1

		lda #0
		sta toin+0		; Set >IN to zero
		sta toin+1
					; ( xt )
		jsr Execute

		jsr R_To_Input		; restore
	WordEnd
		rts


 WordHeader "Source",NN ; ( -- addr u )  Return location and size of input buffer
  ; ## "source"  auto  ANS core
  ; https://forth-standard.org/standard/core/SOURCE
Source:
		lda cib+0	; push address
		ldy cib+1
		jsr PushYA

		lda ciblen+0	; push size, return
		ldy ciblen+1
		jmp PushYA
	WordEnd


 WordHeader "Source-Id",NN ; ( -- n )  Return source identifier
  ; ## "source-id"  tested  ANS core ext
  ; https://forth-standard.org/standard/core/SOURCE-ID
  ; Identify the
  ; input source unless it is a block (s. Conklin & Rather p. 156). This
  ; will give the input source: 0 is keyboard, -1 ($FFFF) is character
  ; string, and a text file gives the fileid.
Source_Id:	lda insrc+0
		ldy insrc+1
		jmp PushYA
	WordEnd


 WordHeader "Exit",AN+CO ; ( -- )  Return control to the calling word immediately
  ; ## "exit"  auto  ANS core
  ; https://forth-standard.org/standard/core/EXIT
  ; If we're in a loop, we need to UNLOOP first and get everything
  ; we we might have put on the Return Stack off as well.
Exit:
		rts		; keep before WordEnd so it gets inlined
	WordEnd			; never reached


 WordHeader ";",CO+IM+NN ; ( -- )  End compilation of new word
  ; ## ";"  auto	ANS core
  ; https://forth-standard.org/standard/core/Semi
  ; End the compilation of a new word into the Dictionary.
  ;
  ; When we
  ; enter this, WorkWord is pointing to the nt_ of this word in the
  ; Dictionary, DP to the previous word, and CP to the next free byte.
  ; A Forth definition would be (see "Starting Forth"):
  ; : POSTPONE EXIT  REVEAL POSTPONE ; [ ; IMMEDIATE  Following the
  ; practice of Gforth, we warn here if a word has been redefined.
Semicolon:
		jsr Left_Bracket	; switch to interpret state

		bit status		; is this a : word or a :NONAME word?
		bvs _colonword

		; This is a :NONAME word

		lda #$60		; compile an RTS
		jsr C_Comma_A

		lda WorkWord+0		; push xt, return
		ldy WorkWord+1
		jmp PushYA

_colonword:
		; Before we formally add the word to the Dictionary, we
		; check to see if it is already present, and if yes, we
		; warn the user.

		; See if word already in Dictionary.
		; (STATUS bit 7 will be high as Header_Build already
		;  checked for us.)
		bit status
		bpl _new_word	; Bit 7 is clear = new word
					; This word is already in the Dictionary

		; WorkWord points to the head of our new word
		; We can't use LATESTNT because we haven't added the new
		; word to the Dictionary yet
		lda WorkWord+0		; push our nt
		ldy WorkWord+1
		jsr PushYA
		jsr Name_To_String	; get our name string

		lda #<str_redefined	; string "redefined"
		ldy #>str_redefined
		jsr Print_ASCIIZ_YA_no_lf

		jsr Type		; print the ofWordEnding word.
		jsr Space

		; Clear bit 7 of status (so future words will print message
		; by defaut)
		lda #$ff-%10000000
		and status
		sta status

_new_word:
		jsr Header_Link		; finish linking into current dictionary

		jsr adjust_z		; fix word length

		lda #$60		; compile an RTS
		jmp C_Comma_A
	WordEnd


adjust_z: ; Update the word length in the header
;		jsr current_to_dp	; make sure dp is correct

		ldy #wh_Flags		; tmp1= xt
		lda (dp),y
		and #FP+DB
		clc
		adc #wh_LinkNt+1
		adc dp+0
		sta tmp1+0
		lda #0
		adc dp+1
		sta tmp1+1

		sec			; length= cp-xt
		lda cp+0
		sbc tmp1+0
		tay
		lda cp+1
		sbc tmp1+1
		beq _short		; length > $ff ?

				; length is >$ff
		ldy #Wh_Flags		; make it NN
		lda (dp),y
		and #$ff-AN
		ora #NN
		sta (dp),y

		ldy #$ff		; set max length for header
_short:		tya			; fill in length in header
		ldy #Wh_CodeLength
		sta (dp),y

		rts


 WordHeader ":",NN ; ( "name" -- )  Start compilation of a new word, with word header
  ; ## ":"  auto	ANS core
  ; https://forth-standard.org/standard/core/Colon
Colon:
		jsr Right_Bracket	; switch to compile state

		lda status
		ora #%01000000	; tell ";" and RECURSE this is a normal word
		ora #%10000000	; Tell Header_Build not to print warning for duplicate name.
		sta status

		; Header_Build will build the header, but not link it into the dictionary.
 		; This is so FIND-NAME etc won't find a half-finished word when
		; looking in the Dictionary.
		; The new nt is saved in WorkWord until we're finished with a SEMICOLON.
		jmp Header_Build	; compile word header (but don't link)
	WordEnd


 WordHeader ":NoName",NN ; ( -- )  Start compilation of a new word, no word header
  ; ## ":NONAME"	auto  ANS core
  ; https://forth-standard.org/standard/core/ColonNONAME
  ; Compile a word with no header.
  ;  ";" will put its xt on the stack.
Colon_NoName:
		jsr Right_Bracket	; switch to compile state

		lda #$ff-%01000000	; tell ";" and RECURSE this is
		and status		; a :NONAME word.
		sta status

		; Put cp (the xt for this word) in WorkWord. The flag above
		; lets both ";" and RECURSE know that is is an xt instead of an
		; nt and they will modify their behavior.
		lda cp+0
		sta WorkWord+0
		lda cp+1
		sta WorkWord+1
	WordEnd
		rts


 WordHeader "'",NN ; ( "name" -- xt )  Return a word's execution token (xt)
  ; ## "'"  auto	ANS core
  ; https://forth-standard.org/standard/core/Tick
Tick:		jsr Tick_Nt
		jmp Name_To_Int	; ( nt -- xt )
	WordEnd

; WordHeader "'Nt",NN ; ( "name" -- nt )  Find a word's nt
Tick_Nt:	jsr parse_name_check	; ( -- addr u )
		jmp find_name_check	; ( addr u -- nt )


 WordHeader "[']",CO+IM+NN ; ( -- )  Store xt of following word during compilation
  ; ## "[']"  auto  ANS core
  ; https://forth-standard.org/standard/core/BracketTick
Bracket_Tick:	jsr Tick
		jmp Literal
	WordEnd


 WordHeader "Find-Name",NN ; ( addr u -- nt|0 )  Get the name token for this name
  ; ## "find-name"  auto	Gforth
find_name:
  ; www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
  ; Given a string, find the Name Token (nt) of a word or return
  ; zero if the word is not in the dictionary. We use this instead of
  ; ancient FIND to look up words in the Dictionary passed by
  ; PARSE-NAME. Note this returns the nt, not the xt of a word like
  ; FIND. To convert, use NAME>INT. This is a Gforth word.
  ; FIND calls this word.

		jsr swl_prepare 	; setup for search
					; ( 0 u )
		lda #$ff		; for each wordlist in the wordlist search order.
		sta DStack+1,x
_wordlist_next: ; step to next wordlist in wordlists
		inc DStack+1,x
		ldy DStack+1,x
		cpy Num_OrderV		; at end of list?
		bcc _nextS
		bne _fail
		lda #wid_Root		; also try root wordlist
		bne _nextA

_nextS:		lda Search_OrderV,y	; A = search_order[Y]  get wordlist ID
_nextA:		jsr swl_search_wordlist
		beq _wordlist_next
					; found a match.
		lda tmp1+0		; NOS= nt
		sta DStack+2,x
		lda tmp1+1
		sta DStack+3,x

_fail:					; ( nt ? )
		inx			; Drop work cell
		inx
	WordEnd
		rts

find_name_check: ; ( addr u -- nt|0 )
		jsr find_name
		lda DStack+1,x		; check that we found a word
		beq _NotFound
		rts

_NotFound:	lda #$100+err_UndefinedWord	; complain & quit
		jmp ThrowA


swl_prepare: ; ( addr u -- 0 u )  prepare for swl_search_wordlist
		jsr underflow_2

		sec			; A= 0 - name start offset
		lda DStack+0,x
		sbc #wh_NameLastChar+1
		adc DStack+2,x		; tmp2= pattern nt
		sta tmp2+0
		lda #$ff
		adc DStack+3,x
		sta tmp2+1

		lda #wh_NameLastChar	; tmp4+0= starting name char index -1
		sec
		sbc DStack+0,x
		sta tmp4+0

		ldy #wh_NameLastChar	; tmp3+0= wh_HNL (hash & length)
		lda (tmp2),y
		.cerror wh_HNL_HashMask!=$e0 ; hard coded
		asl a
		asl a
		asl a
		asl a
		asl a
		ora DStack+0,x
		sta tmp3+0

		lda #0			; assuming failure
		sta DStack+2,x
		sta DStack+3,x

		rts


swl_search_wordlist:
  ; wordlist # in A
  ; returns Z=1 (not found)
  ;	 or Z=0 (found, tmp1=nt)
		stx tmp3+1		; save data stack index

		asl			; tmp1 = up->wordlists[A]
		tay
		ldx WordlistsV+0,y
		lda WordlistsV+1,y
		bne _word_3		;   not end-of-list?

_rts:		php
		ldx tmp3+1		; restore data stack index
		plp
		rts			; Z= end_of_list

_word_next: ; step to next word in wordlist
		ldy #wh_Flags		; what kind of LinkNt?
		lda (tmp1),y
		ldy #wh_LinkNt
		and #FP
		beq _LinkShort

_LinkLong:	lda (tmp1),y		; tmp1= tmp1->Wh_LinkNt_word
		tax
		iny
		lda (tmp1),y
_word_3:	sta tmp1+1
		stx tmp1+0
		bne _Test
		beq _rts

_LinkShort:	lda tmp1+0		; tmp1 -= tmp1->wh_LinkNt offset byte
		sec
		sbc (tmp1),y
		sta tmp1+0
		bcs +
		dec tmp1+1
+
		
_Test:		ldy #Wh_HashNameLen	; Are hash & name length the same?
		lda (tmp1),y
		cmp tmp3+0
		bne _word_next

		ldy tmp4+0		; Y= index of 1st char -1
_char_next:	iny			; to next char
		bmi _rts		; end of string?
		lda (tmp2),y		; char of pattern name
		eor (tmp1),y		; char of this word name
		beq _char_next		;   exact match?
		cmp #$20		;   only a case mismatch?
		bne _word_next
		lda (tmp2),y		;   verify it is alpha char
		and #$df
		sbc #'A'
		cmp #'Z'-'A'+1
		bcc _char_next
		bcs _word_next



 WordHeader "Find",NN ; ( caddr -- caddr 0 | xt 1 | xt -1 )  Find word in Dictionary
  ; ## "find"  auto  ANS core
  ; https://forth-standard.org/standard/search/FIND
  ; https://forth-standard.org/standard/core/FIND
  ; Included for backwards compatibility only, because it still
  ; can be found in so may examples. It should, however, be replaced
  ; by FIND-NAME. Counted string either returns address with a FALSE
  ; flag if not found in the Dictionary, or the xt with a flag to
  ; indicate if this is immediate or not. FIND is a wrapper around
  ; FIND-NAME, we get this all over with as quickly as possible. See
  ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Word-Lists.html
  ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
Find:
		lda DStack+1,x		; Save caddr in case conversion fails.
		pha
		lda DStack+0,x
		pha

		; Convert counted string address to modern format
		jsr Count		; ( caddr -- addr u )
		jsr find_name		; ( addr u -- nt | 0 )
					; tmp1= nt

		lda DStack+1,x		; word found?
		bne _found_word
					; No word found.

		pla			; restore caddr
		sta DStack+0,x
		pla
		sta DStack+1,x

		jmp False		; ( addr 0 )

_found_word:
		pla			; RDrop saved caddr
		pla
					; ( nt )
		jsr Name_To_Int		; convert the return values to FIND's format
					; ( xt )

		ldy #Wh_Flags		; get flags
		lda (tmp1),y
		and #IM
		bne _immediate

		jmp True		; We're not immediate, return -1

_immediate:	jmp One			; We're immediate, return 1
	WordEnd


 WordHeader "Int>Name",UF+NN ; ( xt -- nt )  Get name token from execution token
  ; ## "int>name"	 auto  Tali Forth
  ; www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
  ; This is called >NAME in Gforth, but we change it to
  ; INT>NAME to match NAME>INT
  ; Also returns nt in tmp1 and sets P.Z .
Int_To_Name:
		jsr underflow_1

		; Unfortunately, to make sure this xt has a header,
		; we have to walk through all of the wordlists until we find it.
		; This searches all of the wordlists in id order.

					; ( nt )

		lda #$100-2		; for each wordlist
		sta tmp3+1
_wordlist_next:
		ldy tmp3+1		; get next wordlist index
		iny
		iny
		sty tmp3+1
		cpy #(Num_OrderV-WordlistsV)/2
		bcs _fail
		lda WordlistsV+0,y
		sta tmp1+0
		lda WordlistsV+1,y
		sta tmp1+1
		bne _calc
		beq _wordlist_next

_word_next:	jsr LinkNext		; step to next word
		beq _wordlist_next

_calc:		jsr NameToIntTmp	; tmp2= xt

		lda tmp2+0
		cmp DStack+0,x		;  match?
		bne _word_next
		lda tmp2+1
		cmp DStack+1,x
		bne _word_next

		; We found it!
		lda tmp1+0
		sta DStack+0,x
		lda tmp1+1
		sta DStack+1,x
		rts			; return P.Z=0

_fail: ; We didn't find it in any of the wordlists.
		lda #0			; return a zero to indicate that we didn't find it.
		sta DStack+0,x
		sta DStack+1,x
		sta tmp1+1
		rts			; return P.Z=1
	WordEnd


 WordHeader "Name>Int",NN ; ( nt -- xt )  Convert Name Token to Execute Token
  ; ## "name>int"	 tested	 Gforth
  ; See https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
Name_To_Int:	jsr PopTmp1
		jsr NameToIntTmp
		lda tmp2+0
		ldy tmp2+1
		jmp PushYA
	WordEnd

NameToIntTmp:	; in: tmp1=nt
		; out: tmp2=xt

		ldy #wh_Flags
		lda (tmp1),y
		and #DB			; has XT ptr?
		bne _HasXtPtr

			; appended
		lda (tmp1),y		; A= header length
		and #DB+FP
		clc
		adc #wh_LinkNt+1
		adc tmp1+0		; TOS=tmp1+A
		sta tmp2+0
		lda #0
		adc tmp1+1
		sta tmp2+1
		rts

_HasXtPtr:	lda (tmp1),y		; get wh_Flags again
		ldy #wh_LinkNt+1
		and #FP			; has long LinkNt ?
		beq _short
		iny			;   skip extra byte
_short:		lda (tmp1),y		; tmp2= xt pointer
		sta tmp2+0
		iny
		lda (tmp1),y
		sta tmp2+1
		rts


 WordHeader "Name>String",UF+NN ; ( nt -- addr u )  Given a name token, return string of word
  ; ## "name>string"  tested  Gforth
  ; http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Name-token.html
Name_To_String:
		jsr underflow_1

		lda DStack+0,x		; tmp1= nt
		ldy DStack+1,x
		sta tmp1+0
		sty tmp1+1		; ( nt )

		ldy #Wh_HashNameLen
		lda (tmp1),y
		and #wh_HNL_NameLengthMask
		pha
		eor #$ff
		sec
		adc #Wh_NameLastChar+1	; calc string start offset
		jsr Plus_A		; calc string start addr

		pla			; push string length
		jmp PushZA
	WordEnd


 WordHeader ">Body",UF+NN ; ( xt -- addr )  Return a word's Param Field Area (PFA)
  ; ## ">body"  auto  ANS core
  ; https://forth-standard.org/standard/core/toBODY
  ; Given a word's execution token (xt), return the address of the
  ; start of that word's parameter field (PFA). This is defined as the
  ; address that HERE would return right after CREATE.
  ;
  ; This is a difficult word for STC Forths, because most words don't actually
  ; have a Code Field Area (CFA) to skip.
  ; We assume the user will only call this on a word with:
  ;   a CFA consisting of a jsr abs
  ;   and the PFA following.
To_Body:
		jsr underflow_1

		lda #3		; PFA is after the beginning JSR abs
		jmp Plus_A
	WordEnd


 WordHeader "Erase",NN ; ( addr u -- )  Fill memory region with zeros
  ; ## "erase"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/ERASE
  ; Note that ERASE works with "address" units (bytes), not cells.
Erase:		; FILL will check for underflow
		jsr Zero
		jmp Fill
	WordEnd

 WordHeader "Blank",NN ; ( addr u -- )  Fill memory region with spaces
  ; ## "blank"  auto  ANS string
  ; https://forth-standard.org/standard/string/BLANK
Blank:		; Fill will check for underflow.
		jsr Bl
		jmp Fill
	WordEnd

 WordHeader "Fill",UF+NN ; ( addr u char -- )  Fill a memory region with a byte
  ; ## "fill"  auto  ANS core
  ; https://forth-standard.org/standard/core/FILL
  ; Fill u bytes of memory with char starting at addr.
  ; Note that this works on bytes, not on cells.
  ; It is not defined what happens when we reach the end of the address space
Fill:
		jsr underflow_3

		lda DStack+4,x		; tmp1= address
		sta tmp1+0
		lda DStack+5,x
		sta tmp1+1

		lda DStack+2,x		; tmp2= counter.lo
		sta tmp2+0
		inc DStack+3,x		; so decrement & test for 0 works

		lda DStack+0,x		; A= fill byte
		ldy #0
_loop:		cpy tmp2+0		; done?
		beq _test2
_3:		sta (tmp1),y		; store a byte
		iny			; to next byte
		bne _loop
		inc tmp1+1		; increment addr page
		bne _loop

_test2:		dec DStack+3,x		; any more pages?
		bne _3

		jmp ThreeDrop	; Drop three cells off the Data Stack.
	WordEnd


 WordHeader "Variable",NN ; ( "name" -- )  Define a variable
  ; ## "variable"	 auto  ANS core
  ; https://forth-standard.org/standard/core/VARIABLE
  ; There are various Forth definitions for this word, such as
  ; `CREATE 1 CELLS ALLOT`  or  `CREATE 0 ,`  We use a variant of the
  ; second one so the variable is initialized to zero
Variable:	jsr Create		; compile word header & push PFA

		lda #0			; allot & initialize the variable's data
		tay
		jmp Comma_YA
	WordEnd

 WordHeader "2Variable",NN ; ( "name" -- )  Create a double word variable
  ; ## "2variable"  auto	ANS double
  ; https://forth-standard.org/standard/double/TwoVARIABLE
  ; The variable is initialized to zero.
Two_variable:	jsr Variable		; compile word header & push PFA & 1st cell of data
		jmp Comma_YA		; alloc & init 2nd cell of data
	WordEnd


 WordHeader "Constant",UF+NN ; ( n "name" -- )  Define a constant
  ; ## "constant"	 auto  ANS core
  ; https://forth-standard.org/standard/core/CONSTANT
Constant:
		jsr underflow_1

		jsr Header_Comma	; compile word header

		jsr LitCompile		; compile code to load registers, & pick a subroutine
		jsr Jmp_Comma_NT_YA	; compile code to JMP to the subroutine
		jmp adjust_z		; fix word length
	WordEnd


 WordHeader "2Constant",UF+NN ; (C: d "name" -- ) ( -- d )  Create a double word constant
  ; ## "2constant"  auto	ANS double
  ; https://forth-standard.org/standard/double/TwoCONSTANT
Two_constant:
		jsr underflow_2

		jsr Header_Comma	; compile word header
		jsr Swap
		jsr Literal		; compile push lo cell
		jsr LitCompile		; compile push hi cell, YA=exit routine
		jsr Jmp_Comma_NT_YA	; compile JMP from above
		jmp adjust_z		; fix word length
	WordEnd


 WordHeader "Value",UF+NN ; ( n "name" -- )  Define a value word
  ; ## "value"  auto  ANS core
  ; https://forth-standard.org/standard/core/VALUE
Value:
		jsr underflow_1

		jsr Header_Comma	; compile word header

		jsr ldya_immed_comma	; compile lda # & ldy #
					; always use ldya_immed_comma so TO knows the layout
		lda #<PushYA		; compile jmp PushYA
		ldy #>PushYA
		jsr Jmp_Comma_YA

		jmp adjust_z		; fix word length
	WordEnd


 WordHeader "2Value",UF+NN ; ( d "name" -- )  Define a 2Value word
  ; ## "2value"  auto  ANS double-number
  ; https://forth-standard.org/standard/double/TwoVALUE
TwoValue:
		jsr underflow_2

		jsr Header_Comma	; compile word header

		lda #<TwoValue_Runtime	; compile JSR TValue_Runtime
		ldy #>TwoValue_Runtime
		jsr Jsr_Comma_YA

		jsr adjust_z		; fix word length

		jmp Two_Comma		; compile the value, return
	WordEnd

TwoValue_Runtime:
		pla			; pop RTS addr
		sta tmp1+0
		pla
		sta tmp1+1
		ldy #1			; start at offset 1 (for RTS addr behavior)
		jmp Two_Fetch_Tmp1Y	; fetch the data, return


 WordHeader "To",NN+IM ; ( n "name" -- ) or ( "name")  Gives a new value to a VALUE or 2VALUE or FVALUE
  ; ## "to"  auto	 ANS core ext
  ; https://forth-standard.org/standard/core/TO
  ;
  ; Don't use this to redefine a CONSTANT - it may fail and is a no-no.
  ;
  ; Many variations:
  ;	A: VALUE or 2VALUE or FVALUE
  ;	B: interpreting or compiling.
To:
		; At this point, we don't know if we are interpreted or
		; compile, so we don't know if there is a value n on the stack,
		; so we can't do an underflow check yet
		jsr Tick		; We always need the xt of the target word
					; ( ? xt )
		lda DStack+0,x		; tmp1= xt
		ldy DStack+1,x
		sta tmp1+0
		sty tmp1+1

		ldy #0			; determine what type xt points at
		lda (tmp1),y
		cmp #$a0		;   LDY #
		bne _Test2

		; we're modifying a VALUE
		lda state		; check compile state
		beq _Value_interpret

		; Compile code to modify a VALUE

		lda #<PopYA		; compile jsr PopYA
		ldy #>PopYA
		jsr Jsr_Comma_YA

		lda #$8C		; compile STY xt+1
		jsr C_Comma_A
		jsr One_plus
		jsr Dup
		jsr Comma

		lda #$8D		; compile STA xt+3
		jsr C_Comma_A
		jsr Cell_Plus
		jmp Comma

_Value_interpret: ; We're interpreting, so we arrive here with n
		; on the stack. This is an annoying place to put
		; the underflow check because we can't
		; automatically strip it out
		jsr underflow_2		; ( n xt )

		inx			; Drop xt
		inx

		lda DStack+0,x		; LSB
		ldy #3			;   modify LDA # data
		sta (tmp1),y
		lda DStack+1,x		; MSB
		ldy #1			;   modify LDY # data
		sta (tmp1),y

		inx			; Drop n
		inx
		rts

_Test2:		cmp #$20		;   JSR abs ?
		bne _Err
		; it's a JSR abs
		lda #3			; advance xt over the JSR abs to the data
		jsr Plus_A
		ldy #1			; get JSR abs addr lo byte
		lda (tmp1),y
		cmp #<TwoValue_Runtime
		bne _Test3
		.cerror (<FValue_runtime)==(<TwoValue_Runtime) ; since we don't test hi byte

		; we're modifying a 2VALUE

		lda state		; check compile state
		beq _2Value_runtime

		; compile code to modify the 2VALUE
		jsr ldya_immed_comma	; compile LDY #; LDA #  of xt+3
		lda #<Two_Store_YA
		ldy #>Two_Store_YA
		jmp Jsr_Comma_YA	; compile JSR Two_Store_YA; return

_2Value_runtime: jmp Two_Store

_Test3:
  .if "fp" in TALI_OPTIONAL_WORDS
		cmp #<FValue_runtime
		bne _Err
		; we're modifying a FVALUE

		lda state		; check compile state
		beq _FValue_interpret

		; compile code to modify a FVALUE
		jsr ldya_immed_comma	; compile LDY #; LDA #  of xt+3
		lda #<FStore_YA
		ldy #>FStore_YA
		jmp Jsr_Comma_YA	; compile JSR FStore_YA; return

_FValue_interpret: jmp FStore
  .endif ; fp

_Err:		lda #$100+err_InvalidName ; unrecognized type.
		jsr ThrowA
	WordEnd


 WordHeader "DMax",NN ; ( d1 d2 -- d )  return highest, signed
  ; ANS double
  ; https://forth-standard.org/standard/double/DMAX
DMax:		lda #$80
		bne DMin3
	WordEnd

 WordHeader "DMin",NN ; ( d1 d2 -- d )  return lowest, signed
  ; ANS double
  ; https://forth-standard.org/standard/double/DMIN
DMin:		lda #0
DMin3:		sta tmp2	; save sign correction
		jsr underflow_4

		lda DStack+2,x	; compare
		cmp DStack+6,x
		lda DStack+3,x
		sbc DStack+7,x
		lda DStack+0,x
		sbc DStack+4,x
		lda DStack+1,x
		sbc DStack+5,x
		bvc +
		eor #$80	; fix sign
+		eor tmp2
		bmi TwoNip_nouf	; if negative, NOS is larger and needs to be kept
		jmp Two_Drop
	WordEnd


 WordHeader "2Nip",UF+NN ; ( db da -- da ) "Delete NOS, double"
TwoNip:		jsr underflow_4

TwoNip_NoUf:	lda DStack+0,x	; copy dTOS to dNOS
		sta DStack+4,x
		lda DStack+1,x
		sta DStack+5,x
		lda DStack+2,x
		sta DStack+6,x
		lda DStack+3,x
		sta DStack+7,x
		jmp Two_Drop
	WordEnd


 WordHeader "S>D",UF ; ( n -- d )  Convert single cell number to double cell
  ; ## "s>d"  auto  ANS core
  ; https://forth-standard.org/standard/core/StoD
S_To_D:
		jsr underflow_1

		ldy #0			; assume positive
		lda DStack+1,x		; test n
		bpl +
		dey			; make negative
+
		dex			; push new hi cell
		dex
		sty DStack+0,x
		sty DStack+1,x
	WordEnd
		rts


 WordHeader "D>S",UF ; ( d -- n )  Convert a double number to single
  ; ## "d>s"  auto  ANS double
  ; https://forth-standard.org/standard/double/DtoS
  ; Though this is basically just DROP, we keep it
  ; separate so we can test for underflow
D_To_S:
		jsr underflow_2

		inx		; Drop hi cell
		inx
	WordEnd
		rts


 WordHeader "D-",UF ; ( d1 d2 -- d )  Subtract two double-celled numbers
  ; ## "d-"  auto	 ANS double
  ; https://forth-standard.org/standard/double/DMinus
D_Minus:
		jsr underflow_4 ; two double numbers

		sec

		lda DStack+6,x	; LSB of lower word
		sbc DStack+2,x
		sta DStack+6,x

		lda DStack+7,x	; MSB of lower word
		sbc DStack+3,x
		sta DStack+7,x

		lda DStack+4,x	; LSB of upper word
		sbc DStack+0,x
		sta DStack+4,x

		lda DStack+5,x	; MSB of upper word
		sbc DStack+1,x
		sta DStack+5,x

		inx
		inx
		inx
		inx

	WordEnd
		rts


 WordHeader "D+",UF ; ( d1 d2 -- d )  Add two double-celled numbers
  ; ## "d+"  auto	 ANS double
  ; https://forth-standard.org/standard/double/DPlus
D_Plus:
		jsr underflow_4 ; two double numbers

		clc
		lda DStack+2,x	; LSB of lower word
		adc DStack+6,x
		sta DStack+6,x

		lda DStack+3,x	; MSB of lower word
		adc DStack+7,x
		sta DStack+7,x

		lda DStack+0,x	; LSB of upper word
		adc DStack+4,x
		sta DStack+4,x

		lda DStack+1,x	; MSB of upper word
		adc DStack+5,x
		sta DStack+5,x

		inx
		inx
		inx
		inx

	WordEnd
		rts


 WordHeader "D1+",UF ; ( d1 -- d2 )  increment a double
D1Plus:		jsr underflow_2

		inc DStack+2,x
		bne +
		inc DStack+3,x
		bne +
		inc DStack+0,x
		bne +
		inc DStack+1,x
+
	WordEnd
		rts


 WordHeader "D1-",UF ; ( d1 -- d2 )  Decrement a double
D1Minus:	jsr underflow_2

		lda DStack+2,x
		bne _1
		lda DStack+3,x
		bne _2
		lda DStack+0,x
		bne _3
		dec DStack+1,x
_3:		dec DStack+0,x
_2:		dec DStack+3,x
_1:		dec DStack+2,x
	WordEnd
		rts


 WordHeader "Allot",UF+NN ; ( n -- )  Reserve or release memory
  ; ## "allot"  auto  ANS core
  ; https://forth-standard.org/standard/core/ALLOT
  ; Reserve a certain number of bytes (not cells) or release them.
  ; If n = 0, do nothing. If n is negative, release n bytes, but only
  ; to the beginning of the Dictionary. If n is positive (the most
  ; common case), reserve n bytes, but not past the end of the
  ; Dictionary. See http://forth-standard.org/standard/core/ALLOT
Allot:
		jsr underflow_1

		clc			; adjust cp
		lda DStack+0,x
		adc cp+0
		sta cp+0
		tay			;   save lo byte
		lda DStack+1,x
		sta tmp1
		adc cp+1
		sta cp+1

		bit tmp1
		bmi _release

		; Wait, did we just grant more space than we have? This is
		; a check we only do here, not for other situations like C_Comma_A
		; where smaller amounts are reserved.

		bcs _pos_err		; carry from the add is bad

_last = cp_end-2*PadOffset
		cpy #<_last		; cp < cp_end ?
		sbc #>_last
		bcc _done		; we're fine.

_pos_err:	; Oops, that was too much, we're beyond the end of
		; legal Dictionary RAM. Reduce to max memory and report
		; an error
		lda #<_last
		sta cp+0
		lda #>_last
		sta cp+1

		lda #$100+err_Allocate
		jmp ThrowA

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
		; an error message.

		bcc _neg_err		; borrow indicates we've gone below 0

		; Second step, see if we've gone too far. We compare the new
		; CP on TOS (which, if we've really screwed up, might be
		; negative) with CP0. This is a signed comparison
		cpy #<cp0
		sbc #>cp0
		bcs _done

_neg_err:	; Yep, we're in trouble. 
		lda #<cp0		; Set CP to CP0
		sta cp+0
		lda #>cp0
		sta cp+1

		lda #<forth_dictionary_start	; set WordLists[Forth] to the first
		sta WordlistsV+0		; word in ROM
		lda #>forth_dictionary_start
		sta WordlistsV+1

		lda #$100+err_Free		; abort with an error
		jmp ThrowA

_done:
		inx			; Drop n
		inx
	WordEnd
		rts


 WordHeader "Header,",NN ; ( "name" -- )  Compile word header
  ; See the definition of WordHeader towards the top of this file for details on the header.
Header_Comma:
		jsr Header_Build

Header_Link: ; finish linking compiled word header into current dictionary
		lda WorkWord+0		; dp= WorkWord
		sta dp+0
		lda WorkWord+1
		sta dp+1
		jmp dp_to_current	; Update the CURRENT wordlist with the new DP.


Header_Build: ; compile word header, but don't link it into a wordlist yet

		jsr parse_name_check	; get name string, throw error if empty string
					; ( addr u )

		; We assume name lengths will be <255

		; Check to see if this name already exists in current wordlist.
		jsr Two_dup		; ( addr u addr u )
		jsr Get_Current
		jsr Search_WordList	; ( addr u xt f ) or ( addr u 0 )
		inx			; pop flag.
		inx
		lda DStack-2,x		; not found?
		beq _new_name		; We haven't seen this one before.

		; This name already exists.  ( addr u xt )
		inx			; Drop xt
		inx

		; See if we are supposed to print the message for it.
		bit status		; Check bit 7
		bpl _redefined_name	; Bit 7 is zero, so print the message.

		; We aren't supposed to print the redefined message ourselves,
		; but we should indicate that it is redefined (for ; to print
		; later).
		lda #$80		; Set bit 7 to indicate dup
		ora status
		sta status
		bne _process_name

_redefined_name:
		; Print the message that the name is redefined.
		lda #<str_redefined
		ldy #>str_redefined
		jsr Print_ASCIIZ_YA_no_lf

		jsr Two_dup		 ; ( addr u addr u )
		jsr Type
		jsr Space

		jmp _process_name

_new_name:
		lda #$7F		; Clear bit 0 of status to indicate new word.
		and status
		sta status

_process_name:
					; ( addr u )

		jsr Two_dup		; compile name string ending at wh_NameLastChar
		jsr Here
		jsr Swap
		jsr CMove
		jsr Dup			;   save length
		jsr Allot

		sec
		lda cp+0		; WorkWord= nt
		sbc #wh_NameLastChar+1
		sta WorkWord+0
		lda cp+1
		sbc #0
		sta WorkWord+1		

		ldy #wh_NameLastChar
		lda (WorkWord),y
		.cerror wh_HNL_HashMask!=$e0 ; hard coded
		asl a
		asl a
		asl a
		asl a
		asl a
		ora DStack+0,x
		sta DStack+0,x
		jsr C_Comma		; compile wh_HashNameLen

		inx			; drop name string addr
		inx

		lda #NN			; compile wh_Flags
		jsr C_Comma_A

		lda #3			; compile wh_CodeLength (temporary value, see adjust_z)
		jsr C_Comma_A

		jsr current_to_dp	; Get the CURRENT dictionary pointer.
		sec
		lda WorkWord+0
		sbc dp+0
		tay
		lda WorkWord+1
		sbc dp+1
		beq _LinkShort

		ldy #wh_Flags		; set FP flag
		lda (WorkWord),y
		ora #FP
		sta (WorkWord),y

		lda dp+0		; compile wh_LinkNt ptr word
		ldy dp+1
		jmp Comma_YA

_LinkShort:	tya			; compile wh_LinkNt offset byte
		jmp C_Comma_A
	WordEnd


 WordHeader "Create",NN ; ( "name" -- )  Create Dictionary entry for 'name'
  ; ## "create"  auto  ANS core
  ; https://forth-standard.org/standard/core/CREATE
Create:
		jsr Header_Comma	; compile word header

		lda #<DoVar		; compile JSR DoVar
		ldy #>DoVar
		jsr Jsr_Comma_YA

		jmp adjust_z
	WordEnd


DoVar:	;  Push the address of the PFA onto the stack.
	; This is called with JSR so we can pick up the address
	; of the PFA off the 6502's stack.
	; This is the default routine installed with CREATE.
		pla		; Pull the return address off the machine's stack
		clc		;   +1 because of the way the JSR works
		adc #1
		dex		; push on data stack
		dex
		sta DStack+0,x
		pla
		adc #0
		sta DStack+1,x

		rts		; takes us to the original caller of the
				; routine that itself called DOVAR.


 WordHeader "Does>",CO+IM+NN ; ( -- )  Add payload when defining new words
  ; ## "does>"  auto  ANS core
  ; https://forth-standard.org/standard/core/DOES
  ; Create the payload for defining new defining words. See
  ; http://www.bradrodriguez.com/papers/moving3.htm and
  ; the Developer Guide in the manual for a discussion of
  ; DOES>'s internal workings. This uses tmp1 and tmp2.
Does:
		ldy #>_runtime		; compile JSR _runtime
		lda #<_runtime
		jsr Jsr_Comma_YA

		; compile a subroutine jump to DODOES. In traditional
		; terms, this is the Code Field Area (CFA) of the new
		; word
		lda #$68		; compile PLA  to pop RTS addr (PFA-1) to AY
		ldy #$a8		;    & TAY
		jsr Comma_YA
		lda #$68		; compile PLA
		jsr C_Comma_A
		ldy #>_DoDoes		; compile JSR _DoDoes
		lda #<_DoDoes
		jmp Jsr_Comma_YA
	WordEnd

_runtime: ; Runtime portion of DOES>.
	; We don't
	; jump to DODOES directly because we need to work our magic with
	; the return addresses.
	; This routine is also known as "(DOES)" in other Forths

		; CREATE has also already modified the DP to point to the new
		; word. We have no idea which instructions followed the CREATE
		; command if there is a DOES> so the CP could point anywhere
		; by now.
		jsr current_to_dp	; Grab the DP from the CURRENT wordlist.
		lda dp+0		; tmp1= dp
		ldy dp+1
		sta tmp1+0
		sty tmp1+1

		jsr NameToIntTmp	; tmp2= xt

				; Replace the DOVAR address with our own
				;   addr of CFA's JSR
		clc
		pla		; RTS addr LSB
		adc #1		;   +1 for JSR bahavior
		ldy #1
		sta (tmp2),y
		pla		; RTS addr MSB
		adc #0
		iny
		sta (tmp2),y

		; Since we removed the return address that brought us here, we
		; go back to whatever the main routine was. Otherwise, we would
		; smash into the subroutine jump to DODOES.
		rts


_DoDoes: ; Execute the runtime portion of DOES>.
	; See DOES> and docs/create-does.txt for details and
	; http://www.bradrodriguez.com/papers/moving3.htm

		; RTS addr (PFA-1) is already in AY from inline code

		iny		; +1 for JSR behavior
		bne +
		clc
		adc #1
+
		jmp PushAY


 WordHeader "Unused",0 ; ( -- u )  Return size of space available to Dictionary
  ; ## "unused"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/UNUSED
  ; UNUSED only includes dictionary space.
Unused:
_last = cp_end-2*padoffset
		dex
		dex
		sec
		lda #<_last
		sbc cp+0
		sta DStack+0,x
		lda #>_last
		sbc cp+1
		sta DStack+1,x
	WordEnd
		rts


 WordHeader "Depth",NN ; ( -- u )  Get number of cells (not bytes) used by stack
  ; ## "depth"  auto  ANS core
  ; https://forth-standard.org/standard/core/DEPTH
Depth:
		lda #DStack0	; A= DStack0 - X
		stx tmp4
		sec
		sbc tmp4

		lsr		; divide by two because each cell is two bytes

		jmp PushZA
	WordEnd


 WordHeader "Key",NN ; ( -- char )  Get one character from the input
  ; ## "key"  tested  ANS core
  ; https://forth-standard.org/standard/core/KEY
  ; Get a single character of input from the vectored
  ; input without echoing.
Key:		jsr key_a		; returns char in A
		jmp PushZA
	WordEnd

Key_A: ; returns char in A
	; We use this routine internally
	; to avoid manipulating the Data Stack when we just want a
	; character
		jmp (input)		; JSR/RTS


 WordHeader "Key?",NN ; ( -- f )  Is a key available?
  ; https://forth-standard.org/standard/facility/KEYq
KeyQ:		jsr KeyQ_A
		jmp PushZA

KeyQ_A:		jmp (HaveKey)


 WordHeader "Refill",NN ; ( -- f )  Refill the input buffer
  ; ## "refill"  tested  ANS core ext
  ; https://forth-standard.org/standard/block/REFILL
  ; https://forth-standard.org/standard/core/REFILL
  ; Attempt to fill the input buffer from the input source, returning
  ; a true flag if successful. When the input source is the user input
  ; device, attempt to receive input into the terminal input buffer. If
  ; successful, make the result the input buffer, set >IN to zero, and
  ; return true. Receipt of a line containing no characters is considered
  ; successful. If there is no input available from the current input
  ; source, return false. When the input source is a string from EVALUATE,
  ; return false and perform no other action." See
  ; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/The-Input-Stream.html
  ; and Conklin & Rather p. 156. Note we don't have to care about blocks
  ; because REFILL is never used on blocks - Tali is able to evaluate the
  ; entire block as a 1024 byte string.
Refill:
		lda #0			; show empty in case of error
		sta ciblen+0
		sta ciblen+1

		; Get input source from SOURCE-ID. This is an
		; optimized version of a subroutine jump to SOURCE-ID
		lda insrc		; cheat: We only check LSB
		bne _src_not_kbd

		; SOURCE-ID of zero means we're getting stuff from the keyboard
		; with ACCEPT, which wants the address of the current input
		; buffer NOS and the max number of characters to accept TOS

		lda cib+0		; address of CIB is NOS
		ldy cib+1
		jsr PushYA

		lda #bsize		; max number of chars is TOS
		jsr PushZA		;  cheat: We only accept max 255

		jsr Accept		; ( addr n1 -- n2)

		; ACCEPT returns the number of characters accepted, which
		; belong in CIBLEN
		lda DStack+0,x
		sta ciblen+0
		lda DStack+1,x
		sta ciblen+1		; though we only accept 255 chars

		; make >IN point to beginning of buffer
		lda #0
		sta toin+0
		sta toin+1

		lda #$FF		; overwrite with TRUE flag
		sta DStack+0,x
		sta DStack+1,x

		rts

_src_not_kbd:
		; If SOURCE-ID doesn't return a zero, it must be a string in
		; memory or a file (remember, no blocks in this version).
		; If source is a string, we were given the flag -1 ($FFFF)
		cmp #$ff
		bne _src_not_string

		; Simply return FALSE flag as per specification
		jmp False

_src_not_string:
		; Since we don't have blocks, this must mean that we are trying
		; to read from a file. However, we don't have files yet, so we
		; report an error and jump to ABORT.
		lda #$100+err_Unsupported
		jsr ThrowA
	WordEnd


 WordHeader "Accept",UF+NN ; ( addr n -- n )  Receive a string of characters from the keyboard
  ; ## "accept"  auto  ANS core
  ; https://forth-standard.org/standard/core/ACCEPT
  ; Receive a string of at most n1 characters, placing them at
  ; addr. Return the actual number of characters as n2. Characters
  ; are echoed as they are received. ACCEPT is called by REFILL in
  ; modern Forths.
Accept:
		jsr underflow_2

		lda DStack+0,x		; Abort if we were asked to receive 0 chars
		ora DStack+1,x
		bne _not_zero

		inx			; drop n
		inx
		sta DStack+0,x		; replace addr with 0
		sta DStack+1,x

		jmp accept_done

_not_zero:
		lda DStack+0,x	; number of chars to get in tmp2 ...
		sta tmp2
		lda #0
		sta tmp2+1	; ... but we only accept max 255 chars

		lda DStack+2,x	; address of buffer is NOS, to tmp1
		sta tmp1
		lda DStack+3,x
		sta tmp1+1

		inx
		inx

		ldy #0

		; Select the next history buffer. Clear bit 3 first (so overflow
		; from bit 2 to 3 is OK)
		lda status
		and #$f7

		; Increment the buffer number (overflow from 7 to 0 OK)
		clc
		adc #1

		; Set bit 3 for detecting if CTRL-n has been pressed the first
		; time. This bit will be cleared on the first CTRL-n or CTRL-p
		; received and won't be used to calculate the history buffer
		; offset.
		ora #$08
		sta status

accept_loop:
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

		; BACKSPACE and DEL do the same thing for the moment
		cmp #AscBS
		beq _backspace
		cmp #AscDEL	; (CTRL-h)
		beq _backspace

		; Check for CTRL-p and CTRL-n to recall input history
		cmp #AscCP
		beq _ctrl_p
		cmp #AscCN
		beq _ctrl_n

		; That's enough for now. Save and echo character.
		sta (tmp1),y
		iny

		; Emit_A sidesteps all the fooling around with the Data Stack
		jsr Emit_A

		cpy tmp2	; reached character limit?
		bne accept_loop	      ; fall through if buffer limit reached
		beq _buffer_full

_eol:
		jsr Space	; print final space

_buffer_full:
		; REFILL updates ciblen and toin, we don't need to do it here
		sty DStack+0,x	; Y contains number of chars accepted already
		lda #0
		sta DStack+1,x		; we only accept 256 chars

		jmp accept_done

_backspace:
		; Handle backspace and delete kex, which currently do the same
		; thing
		cpy #0		; buffer empty?
		bne +

		lda #AscBELL	; complain and don't delete beyond the start of line
		jsr Emit_A
		iny
+
		dey
		lda #AscBS	; move back one
		jsr Emit_A
		jsr Space	; print a space (rubout)
		lda #AscBS	; move back over space
		jsr Emit_A

		jmp accept_loop

_ctrl_p:
		; CTRL-p was pressed. Recall the previous input buffer.

		; Select the previous buffer
		lda status

		; Check for 0 (need to wrap back to 7)
		and #7
		bne _ctrl_p_dec

		; We need to wrap back to 7.
		lda status
		ora #7
		sta status
		bne _recall_history

_ctrl_p_dec:
		; It's safe to decrement the buffer index directly.
		dec status
		jmp _recall_history

_ctrl_n:
		; CTRL-n was pressed. Recall the next input buffer. Select
		; the next buffer Check bit 3. If it's set, this is the first
		; time CTRL-n has been pressed and we should select the CURRENT
		; history buffer.
		lda #$8
		bit status
		bne _recall_history

		; This isn't the first time CTRL-n has been pressed, select the
		; next history buffer. Clear bit 3 first (so overflow is OK)
		lda status
		and #$f7

		; Increment the buffer number (overflow from 7 to 0 OK)
		clc
		adc #1

		; Bit 3 (if it got set by going from buffer 7 to 0) will
		; be cleared below.
		sta status

		; Falls through to _recall_history

_recall_history:
		; Clear bit 3 (first time ctrl-n recall) bit in status
		lda #$ff-%00001000
		and status
		sta status
		jsr accept_total_recall

		; tmp3 now has the address of the previous history buffer.
		; First byte of buffer is length. Clear the line by sending
		; CR, Y spaces, then CR.
		lda #AscCR
		jsr Emit_A

input_clear:
		cpy #0
		beq input_cleared

		jsr Space
		dey
		jmp input_clear

input_cleared:
		lda #AscCR
		jsr Emit_A

		; Save the history length byte into histinfo+1
		; ldy #0	; Y is already 0 by clearing the line.
		lda (tmp3),y
		sta status+1

		; Increment the tmp3 pointer so we can use ,y addressing
		; on both tmp1 (the input buffer) and tmp3 (the history
		; buffer)
		inc tmp3
		bne +		; Increment the upper byte on carry.
		inc tmp3+1
+
		; Copy the history buffer into the input buffer,
		; sending the characters to the output as we go.
		lda #AscCR
		jsr Emit_A

_history_loop:
		; See if we have reached the end of the history buffer.
		cpy status+1
		bne +
		jmp accept_loop	      ; Needs a long jump
+
		; See if we have reached the end of the input buffer.
		; (only comparing to lower byte as we currently limit
		; to 255 characters max)
		cpy tmp2
		beq _hist_filled_buffer

		; Copy a character and echo.
		lda (tmp3),y
		sta (tmp1),y
		jsr Emit_A

		; Move to the next character.
		iny
		bne _history_loop

_hist_filled_buffer:
		; We don't want a history recall to EOL our buffer,
		; so back up one character and return to editing.
		dey
		jmp accept_loop

accept_done:
		; Copy the input buffer into the currently
		; selected history buffer.
		jsr accept_total_recall
		sta status+1

		; Also save it in the first buffer byte.
		ldy #0
		sta (tmp3),y

		; Move path the count to the data bytes
		inc tmp3
		bne +		; Increment the upper byte on carry.
		inc tmp3+1
+
		; Copy the characters from the input buffer to the
		; history buffer.

_save_history_loop:
		cpy status+1
		beq _save_history_done

		lda (tmp1),y
		sta (tmp3),y
		iny
		bne _save_history_loop

_save_history_done:
	WordEnd
		rts

accept_total_recall:
	; Internal subroutine for ACCEPT that recalls history entry

		; Generate the address of the buffer in tmp3. Start with the
		; base address.
		lda #<hist_buff
		sta tmp3
		lda #>hist_buff
		sta tmp3+1

		; This is a bit annoying as some bits go into each byte.
		; .....xxx gets put into address like ......xx x.......
		lda status
		ror
		and #3
		clc
		adc tmp3+1
		sta tmp3+1

		lda status
		ror		; Rotate through carry into msb.
		ror
		and #$80
		clc
		adc tmp3
		sta tmp3
		bcc +		; Increment the upper byte on carry.
		inc tmp3+1
+
		; Save the current length of the input buffer in
		; histinfo+1 temporarily.  Reduce to 127 if larger.
		tya
		cmp #$80
		bcc +
		lda #$7F
+
		rts


 WordHeader "Input>R",NN|ST ; ( -- ) ( R: -- n1 n2 n3 n4 )  Save input state to the Return Stack
  ; ## "input>r"	tested	Tali Forth
  ; Save the current input state as defined by insrc, cib, ciblen, and
  ; toin to the Return Stack. Used by EVALUATE.
  ; Like https://forth-standard.org/standard/core/SAVE-INPUT
  ;
  ; The naive way of doing
  ; this is to push each two-byte variable to the stack in the form of
  ;
  ;	lda insrc
  ;	pha
  ;	lda insrc+1
  ;	pha
  ;
  ; for a total of 24 byte of instruction in one direction and later
  ; a further 24 bytes to reverse the process. We shorten this at the
  ; cost of some speed by assuming the four variables are grouped
  ; together on the Zero Page and start with insrc (see definitions.asm
  ; for details). The reverse operation is r_to_input. These words must
  ; be flagged as Never Native. Uses tmp1
Input_To_R:
		; We arrive here with the return address on the top of the
		; 6502's stack.
		pla			; move it out of the way
		sta tmp1+0
		pla
		sta tmp1+1

		; This assumes that insrc is the first of eight bytes and
		; toin+1 the last in the sequence we want to save.
		.cerror InSrc+2!=Cib
		.cerror Cib+2!=CibLen
		.cerror CibLen+2!=ToIn
		ldy #7
_loop:		lda InSrc,y	; insrc+7 is toin+1
		pha
		dey
		bpl _loop

		lda tmp1+1		; Restore address for return jump
		pha
		lda tmp1+0
		pha
	WordEnd
		rts


 WordHeader "R>Input",ST ; ( -- ) ( R: n1 n2 n3 n4 -- )  Restore input state from Return Stack
  ; ## "r>input"	tested	Tali Forth
  ; Restore the current input state as defined by insrc, cib, ciblen,
  ; and toin from the Return Stack.
  ;
  ; See INPUT_TO_R for a discussion of this word. Uses tmp1
  ; like https://forth-standard.org/standard/core/RESTORE-INPUT
R_To_Input:
		; We arrive here with the return address on the top of the
		; 6502's stack.
		pla		 ; move RTS addr out of the way
		sta tmp1+0
		pla
		sta tmp1+1

		; This assumes that insrc is the first of eight bytes and
		; toin+1 the last in the sequence we want to save from the Zero
		; Page. Since we went in reverse order, insrc is now on the top
		; of the Return Stack.
		ldy #0
_loop:		pla
		sta InSrc,y ; also cib ciblen toin
		iny
		cpy #8
		bne _loop

		lda tmp1+1	; Restore RTS address
		pha
		lda tmp1+0
		pha
	WordEnd
		rts


 WordHeader "IKey",NN ; ( -- n )  get integer from console
  ; Like BASIC's INPUT for 1 integer.
IKey:	;	lda Base+0	; save base
	;	pha
	;	jsr Decimal	; set base to decimal

_again:		jsr Zero
		jsr Zero	; ( ud )

		jsr Here
		jsr Dup		; get text
		lda #16
		jsr PushZA
		jsr Accept	; ( ud addr len )

		lda (DStack+2,x) ; negative?
		cmp #'-'
		php		;  save sign
		bne +
		jsr NOS_One_Plus ;   eat the '-'
		jsr One_Minus
+
		jsr To_Number ; ( ud addr u -- ud addr u )  Continue convert a string to an integer
		inx		; Drop len
		inx
		inx		; Drop addr
		inx
		inx		; UD>S
		inx
		lda DStack-6,x	; string all consumed?
		beq _ok

_err:		inx		; Drop u
		inx
		plp		; RDrop saved sign
		jsr Space	; prompt again
		lda #'?'
		jsr Emit_A
		jsr Space
		jmp _again	; try it again

_ok:		plp		; apply sign
		bne +
		jsr Negate
+
	;	pla		; restore base
	;	sta Base

		rts
	WordEnd


 WordHeader "Bounds",UF ; ( addr u -- addr+u addr )  Prepare address for looping
  ; ## "bounds"  auto  Gforth
  ; http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Memory-Blocks.html
  ; Given a string, return the correct Data Stack parameters for
  ; a DO/LOOP loop over its characters. This is realized as
  ; OVER + SWAP in Forth, but we do it a lot faster in assembler
Bounds:
		jsr underflow_2

		clc
		lda DStack+2,x		; LSB addr
		tay
		adc DStack+0,x		; LSB u
		sta DStack+2,x		; LSB addr+u
		sty DStack+0,x

		lda DStack+3,x		; MSB addr
		tay
		adc DStack+1,x		; MSB u
		sta DStack+3,x		; MSB addr+u
		sty DStack+1,x
	WordEnd
		rts


 WordHeader "Spaces",UF+NN ; ( n -- )  Print a number of spaces
  ; ## "spaces"  auto  ANS core
  ; https://forth-standard.org/standard/core/SPACES
  ; ANS says this word takes a signed value but prints no spaces for negative values.
 Spaces:
		jsr underflow_1

		jmp _test

_loop:
		jsr Space		; print a space

_test:		dec DStack+0,x		; decrement & test
		bpl _loop

		inx			; Drop
		inx
	WordEnd
		rts


 WordHeader "-Trailing",UF+NN ; ( addr u1 -- addr u2 )  Remove trailing spaces
  ; ## "-trailing"  auto	ANS string
  ; https://forth-standard.org/standard/string/MinusTRAILING
  ; Remove trailing spaces
Minus_trailing:
		jsr underflow_2

		lda DStack+2,x		; tmp1= addr + (u1 & $ff00)
		sta tmp1+0
		lda DStack+3,x
		clc
		adc DStack+1,x
		sta tmp1+1

		ldy DStack+0,x
_loop:		dey			; back 1 char
		cpy #$ff
		bne +
		dec tmp1+1		;   back 1 page
		dec DStack+1,x
		bmi _done
+

		lda (tmp1),y		; if blank, keep going
		cmp #AscSP
		beq _loop

_done:		iny			; forward 1 char
		bne +
		inc DStack+1,x
+		sty DStack+0,x		
	WordEnd
		rts		


 WordHeader "-Leading",UF+NN ; ( addr1 u1 -- addr2 u2 )  Remove leading spaces
  ; ## "-leading"	 auto  Tali String
  ; Remove leading whitespace. This is the reverse of -TRAILING
Minus_leading:
		jsr underflow_2

_loop:
		lda DStack+0,x		; chars left?
		ora DStack+1,x
		beq _done

		lda (DStack+2,x)	; get first character
		cmp #AscSP+1		;   is_whitespace
		bcs _done

		; It's whitespace, move one down
		jsr NOS_One_Plus
		jsr One_Minus_NoUF

		jmp _loop
_done:
	WordEnd
		rts


 WordHeader "/String",UF+NN ; ( addr u n -- addr u )  Shorten string by n
  ; ## "/string"	auto  ANS string
  ; https://forth-standard.org/standard/string/DivSTRING
Slash_String:	DStackCheck 3,Throw_Stack_20

		clc		; addr += n
		lda DStack+0,x
		adc DStack+4,x
		sta DStack+4,x
		lda DStack+1,x
		adc DStack+5,x
		sta DStack+5,x

		sec		; u -= n
		lda DStack+2,x
		sbc DStack+0,x
		sta DStack+2,x
		lda DStack+3,x
		sbc DStack+1,x
		sta DStack+3,x

		inx		; Drop n
		inx
	WordEnd
		rts


Throw_Stack_20: jmp Throw_Stack


 WordHeader "2Drop",UF ; ( n n -- )  Drop TOS and NOS
  ; ## "2drop"  auto  ANS core
  ; https://forth-standard.org/standard/core/TwoDROP
Two_drop:
		jsr underflow_2

		inx
		inx
		inx
		inx

	WordEnd
		rts


 WordHeader "2Swap",NN ; ( n1 n2 n3 n4 -- n3 n4 n1 n1 )  Exchange two double words
  ; ## "2swap"  auto  ANS core
  ; https://forth-standard.org/standard/core/TwoSWAP
Two_Swap:	DStackCheck 4,Throw_Stack_20

		stx tmp1
		dex
		dex
		dex
		dex
_loop:		inx
		lda DStack+3,x	; 3 <-> 7
		ldy DStack+7,x
		sta DStack+7,x
		sty DStack+3,x
		cpx tmp1
		bcc _loop
	WordEnd
		rts


 WordHeader "2Over",UF+NN ; ( d1 d2 -- d1 d2 d1 )  Copy double word NOS to TOS
  ; ## "2over"  auto  ANS core
  ; https://forth-standard.org/standard/core/TwoOVER
Two_over:	DStackCheck 4,Throw_Stack_20

		ldy #4
_loop:		dex
		lda DStack+8,x
		sta DStack+0,x
		dey
		bne _loop
	WordEnd
		rts


 WordHeader "2!",UF ; ( n1 n2 addr -- )  Store two numbers at given address
  ; ## "2!"  auto	 ANS core
  ; https://forth-standard.org/standard/core/TwoStore
  ; Stores so n2 goes to addr and n1 to the next consecutive cell.
Two_Store:
		jsr underflow_3

		jsr PopYA
Two_Store_YA:	sta tmp1+0	; save addr
		sty tmp1+1

		lda DStack+0,x	; copy MSB
		ldy #0
		sta (tmp1),y
		lda DStack+1,x	; copy next
		iny
		sta (tmp1),y
		lda DStack+2,x	; copy next
		iny
		sta (tmp1),y
		lda DStack+3,x	; copy MSB
		iny
		sta (tmp1),y

		inx		; 2Drop
		inx
		inx
		inx
	WordEnd
		rts


 WordHeader "2@",0 ; ( addr -- n1 n2 )  Fetch the cell pair n1 n2 stored at addr
  ; ## "2@"  auto	 ANS core
  ; https://forth-standard.org/standard/core/TwoFetch
  ; Note n2 stored at addr and n1 in the next cell -- in our case,
  ; the next two bytes. This is equvalent to  `DUP CELL+ @ SWAP @`
Two_fetch:	jsr PopYA
Two_Fetch_YA:	sta tmp1+0	; save addr
		sty tmp1+1
		ldy #0
Two_Fetch_Tmp1Y:
		dex
		dex
		dex
		dex
		lda (tmp1),y	; copy LSB
		sta DStack+0,x
		iny		; copy next
		lda (tmp1),y
		sta DStack+1,x
		iny		; copy next
		lda (tmp1),y
		sta DStack+2,x
		iny		; copy next
		lda (tmp1),y
		sta DStack+3,x
	WordEnd
		rts


 WordHeader "D@",NN ; ( addr -- d )  fetch double, XINU format
DFetch:		jsr PopYA
DFetchYA:	sta tmp1+0	; save addr
		sty tmp1+1

		dex
		dex
		dex
		dex
		ldy #0
		lda (tmp1),y
		sta DStack+2,x
		iny
		lda (tmp1),y
		sta DStack+3,x
		iny
		lda (tmp1),y
		sta DStack+0,x
		iny
		lda (tmp1),y
		sta DStack+1,x
	WordEnd
		rts

 WordHeader "D!",NN ; ( d addr -- )  store double, XINU format
DStore:		jsr PopYA
DStoreYA:	sta tmp1+0	; save addr
		sty tmp1+1

		lda DStack+2,x	; LSB
		ldy #0
		sta (tmp1),y
		lda DStack+3,x
		iny
		sta (tmp1),y
		lda DStack+0,x
		iny
		sta (tmp1),y
		lda DStack+1,x	; MSB
		iny
		sta (tmp1),y

		jmp Two_Drop	; also check underflow, return
	WordEnd


 WordHeader "2R@",CO+NN ; ( -- n n )  Copy top two entries from Return Stack
  ; ## "2r@"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/TwoRFetch
  ;
  ; This is R> R> 2DUP >R >R SWAP but we can do it a lot faster in
  ; assembler. We use trickery to access the elements on the Return
  ; Stack instead of pulling the return address first and storing
  ; it somewhere else like for 2R> and 2>R.
Two_r_fetch:
		txa		; Y= return stack index
		tsx
		stx tmp1
		ldy tmp1
		tax

		dex		; make room on the Data Stack
		dex
		dex
		dex

		; The Return Stack addreses RStack+1 are occupied by
		; the return address for this word. This is a whole lot
		; easier on the 65816
		lda RStack+3,y	; LSB of top entry
		sta DStack+0,x
		lda RStack+4,y	; MSB of top entry
		sta DStack+1,x
		lda RStack+5,y	; LSB of bottom entry
		sta DStack+2,x
		lda RStack+6,y	; MSB of bottom entry
		sta DStack+3,x
	WordEnd
		rts


 WordHeader "2R>",CO+ST ; ( -- n1 n2 ) (R: n1 n2 -- )  Move two cells from Return Stack
  ; ## "2r>"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/TwoRfrom
  ; Pull top two entries from Return Stack.
  ;
  ; Is the same as
  ; R> R> SWAP. As with R>, the problem with the is word is that
  ; the top value on the ReturnStack for a STC Forth is the
  ; return address, which we need to get out of the way first.
  ; Native compile needs to be handled as a special case.
Two_r_from:
		pla			; save the return address
		sta tmp1+0
		pla
		sta tmp1+1

		; --- CUT HERE FOR NATIVE COMPILING ---

		dex			; make room on stack
		dex
		dex
		dex

		; In theory, we should test for underflow on the Return
		; Stack. However, given the traffic there with an STC
		; Forth, we've already crashed if there is a problem.

		; now we can access the data
		pla			; LSB
		sta DStack+0,x
		pla			; MSB
		sta DStack+1,x

		pla			; LSB
		sta DStack+2,x
		pla			; MSB
		sta DStack+3,x

		; --- CUT HERE FOR NATIVE COMPILING ---

		lda tmp1+1		; restore return address
		pha
		lda tmp1+0
		pha
	WordEnd
		rts


 WordHeader "2>R",CO+UF+ST ; ( n1 n2 -- )(R: -- n1 n2)  Move two cells to Return Stack
  ; ## "2>r"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/TwotoR
  ; Push top two entries to Return Stack.
  ;
  ; The same as SWAP >R >R
  ; except that if we jumped here, the return address will be in the
  ; way. May not be natively compiled unless we're clever and use
  ; special routines.
Two_to_r:
		pla		; save the return address
		sta tmp1+0
		pla
		sta tmp1+1

		; --- CUT HERE FOR NATIVE CODING ---

		jsr underflow_2

		; now we can move the data
		lda DStack+3,x	; MSB
		pha
		lda DStack+2,x	; LSB
		pha

		lda DStack+1,x	; MSB
		pha
		lda DStack+0,x	; LSB
		pha

		inx
		inx
		inx
		inx

		; --- CUT HERE FOR NATIVE CODING ---

		lda tmp1+1	; restore return address
		pha
		lda tmp1+0
		pha
	WordEnd
		rts


 WordHeader "Invert",UF ; ( n1 -- n2 )  Bitwise complement
  ; ## "invert"  auto  ANS core
  ; https://forth-standard.org/standard/core/INVERT
Invert:
		jsr underflow_1

		lda #$FF
		eor DStack+0,x	; LSB
		sta DStack+0,x

		lda #$FF
		eor DStack+1,x	; MSB
		sta DStack+1,x
	WordEnd
		rts


 WordHeader "Negate",UF ; ( n1 -- n2 )  Two's complement
  ; ## "negate"  auto  ANS core
  ; https://forth-standard.org/standard/core/NEGATE
Negate:
		jsr underflow_1

Negate3:	sec
Negate4:	lda #0
		sbc DStack+0,x	; LSB
		sta DStack+0,x
		lda #0
		sbc DStack+1,x	; MSB
		sta DStack+1,x
	WordEnd
		rts


 WordHeader "Abs",NN ; ( n -- u )  Return absolute value of a number
  ; ## "abs"  auto  ANS core
  ; https://forth-standard.org/standard/core/ABS
Abs:		DStackCheck 1,Throw_Stack_17

		lda DStack+1,x	; n negative?
		bmi Negate3
	WordEnd
		rts

Throw_Stack_17: jmp Throw_Stack


 WordHeader "DNegate",NN ; ( d -- d )  2s complement double cell number
  ; ## "dnegate"	auto  ANS double
  ; https://forth-standard.org/standard/double/DNEGATE
DNegate:	DStackCheck 2,Throw_Stack_17 ; double number

DNegate3:	sec
		lda #0
		sbc DStack+2,x	; LSB of low cell
		sta DStack+2,x
		lda #0
		sbc DStack+3,x	; MSB of low cell
		sta DStack+3,x

		jmp Negate4
	WordEnd


 WordHeader "DAbs",NN ; ( d -- d )  Return the absolute value of a double
  ; ## "dabs"  auto  ANS double
  ; https://forth-standard.org/standard/double/DABS
DAbs:		DStackCheck 2,Throw_Stack_17 ; double number

		lda DStack+1,x	; d negative?
		bmi DNegate3
	WordEnd
		rts


 WordHeader "D<>",NN ; ( d1 d2 -- f )  return d1 != d2
DNEq:		jsr DEqual
		jmp ZEqA
	WordEnd

 WordHeader "D=",NN ; ( d1 d2 -- f )  return d1 == d2
  ; https://forth-standard.org/standard/double/DEqual
DEqual:		lda DStack+6,x
		cmp DStack+2,x
		bne False3
		lda DStack+7,x
		cmp DStack+3,x
		bne False3
		lda DStack+0,x
		cmp DStack+4,x
		bne False3
		lda DStack+1,x
		cmp DStack+5,x
		bne False3
		beq True3
	WordEnd
		
 WordHeader "DU>",NN ; ( ud1 ud2 -- f )  return d1 > d2 (unsigned)
DUGt:		jsr DGtSub
		bcc True3
		bcs False3
	WordEnd

 WordHeader "DU<=",NN ; ( ud1 ud2 -- f )  return d1 <= d2 (unsigned)
DULe:		jsr DGtSub
		bcs True3
		bcc False3
	WordEnd

 WordHeader "DU>=",NN ; ( d1 d2 -- f )  Return d1 >= d2 (unsigned)
DUGe:		jsr DLessSub
		bcs True3
		bcc False3
	WordEnd

 WordHeader "DU<",NN ; ( ud1 ud2 -- f )  return d1 < d2 (unsigned)
  ; https://forth-standard.org/standard/double/DUless
DULess:		jsr DLessSub
		bcc True3

False3:		lda #0
		beq DReturn3
	WordEnd

Throw_Stack_09: jmp Throw_Stack

 WordHeader "D>=",NN ; ( d1 d2 -- f ) return d1 >= d2
DGEq:		jsr DLessSub
		bvs DLess3
DGEq3:		bpl True3
		bmi False3
	WordEnd

  WordHeader "D<",NN ; ( d1 d2 -- f )  return d1 < d2 (signed)
  ; https://forth-standard.org/standard/double/Dless
DLess:		jsr DLessSub
		bvs DGEq3
DLess3:		bpl False3
True3:		lda #$ff
DReturn3:	DStackCheck 4,Throw_Stack_09
		sta DStack+6,x
		sta DStack+7,x
		inx			; preserve A, don't use ThreeDrop
		inx
		inx
		inx
		inx
		inx
		rts
	WordEnd

DLessSub:	lda DStack+6,x
		cmp DStack+2,x
		lda DStack+7,x
		sbc DStack+3,x
		lda DStack+4,x
		sbc DStack+0,x
		lda DStack+5,x
		sbc DStack+1,x
		rts

 WordHeader "D<=",NN ; ( d1 d2 -- f )  Return d1 <= d2
DLe:		jsr DGtSub
		bvs DGt3
DLe3:		bpl True3
		bmi False3
	WordEnd

 WordHeader "D>",NN ; ( d1 d2 -- f )  Return d1 > d2
DGt:		jsr DGtSub
		bvs DLe3
DGt3:		bmi True3
		bpl False3
	WordEnd

DGtSub:		lda DStack+2,x
		cmp DStack+6,x
		lda DStack+3,x
		sbc DStack+7,x
		lda DStack+0,x
		sbc DStack+4,x
		lda DStack+1,x
		sbc DStack+5,x
		rts


 WordHeader "D0<=",NN ; ( d -- f )  Return d <= 0
D0Le:		lda DStack+1,x	; test sign
		bmi True1
		bpl D0Equal
	WordEnd

 WordHeader "D0>",NN ; ( d -- f )  Return d < 0
D0Gt:		lda DStack+1,x	; test sign
		bpl D0Ne
		jmp False1

 WordHeader "D0<>",NN ; ( d -- f )  Return d != 0
D0Ne:		jsr D0EqSub
		bne True1
		beq False1
	WordEnd

 WordHeader "D0=",NN ; ( d -- f )  return d == 0
  ; https://forth-standard.org/standard/double/DZeroEqual
D0Equal:	jsr D0EqSub
		bne False1
		beq True1
	WordEnd

D0EqSub:	lda DStack+2,x
		ora DStack+3,x
		ora DStack+0,x
		ora DStack+1,x
		rts

 WordHeader "D0<",NN ; ( d -- f )  return D < 0
  ; https://forth-standard.org/standard/double/DZeroless
D0Less:		lda DStack+1,x	; test sign
		bmi True1
		bpl False1
	WordEnd

 WordHeader "D0>=",NN ; ( d -- f )  Return d >= 0
D0Ge:		lda DStack+1,x	; test sign
		bpl True1
		bmi False1
	WordEnd

Throw_Stack_08: jmp Throw_Stack


WordHeader "=",NN ; ( n1 n2 -- f )  Return n1 == n2
  ; ## "="  auto	ANS core
  ; https://forth-standard.org/standard/core/Equal
Equal:		lda DStack+0,x		; LSB
		cmp DStack+2,x
		bne False1
		lda DStack+1,x		; MSB
		cmp DStack+3,x
		bne False1

True1:		lda #$ff	; return TRUE
Return1:	DStackCheck 2,Throw_Stack_08
		inx		; Drop 
		inx
		sta DStack+0,x	; store f
		sta DStack+1,x
		rts
	WordEnd

 WordHeader "<>",NN ; ( n1 n2 -- f )  return n1 != n2
  ; ## "<>"  auto	 ANS core ext
  ; https://forth-standard.org/standard/core/ne
Not_Equals:	lda DStack+0,x		; LSB
		cmp DStack+2,x
		bne True1
		lda DStack+1,x		; MSB
		cmp DStack+3,x
		bne True1

False1:		lda #0		; return FALSE
		beq Return1
	WordEnd


 WordHeader "<",NN ; ( n1 n2 -- f )  return n1 < n2
  ; ## "<"  auto	ANS core
  ; https://forth-standard.org/standard/core/less
Less_Than:	lda DStack+2,x	; compare
		cmp DStack+0,x
		lda DStack+3,x
		sbc DStack+1,x
		bvc _c
		eor #$80	; fix sign
_c:		bpl False1
		bmi True1
	WordEnd

 WordHeader ">=",NN ; ( n1 n2 -- f )  return n1 > n2
Ge:		jsr Less_Than
		jmp ZEqA
	WordEnd

 WordHeader "U<",NN ; ( u1 u2 -- f )  Return u1 < u2 (unsigned)
  ; ## "u<"  auto	 ANS core
  ; https://forth-standard.org/standard/core/Uless
U_Less_Than:	lda DStack+2,x
		cmp DStack+0,x
		lda DStack+3,x
		sbc DStack+1,x
		bcs False1
		bcc True1
	WordEnd

 WordHeader "U>",NN ; ( u1 u2 -- f )  return u1 > u2 (unsigned)
  ; ## "u>"  auto	 ANS core ext
  ; https://forth-standard.org/standard/core/Umore
U_Greater_Than:	lda DStack+0,x
		cmp DStack+2,x
		lda DStack+1,x
		sbc DStack+3,x
		bcs False1
		bcc True1
	WordEnd


 WordHeader ">",NN ; ( n1 n2 -- f )  return n1 > n2
  ; ## ">"  auto	ANS core
  ; https://forth-standard.org/standard/core/more
Greater_Than:	lda DStack+0,x	; compare
		cmp DStack+2,x
		lda DStack+1,x
		sbc DStack+3,x
		bvc _c
		eor #$80	; fix sign
_c:		bpl False1
		jmp True1
	WordEnd

 WordHeader "<=",NN ; ( n1 n2 -- f )  return n1 < n2
Le:		jsr Greater_Than
		jmp ZEqA
	WordEnd

 WordHeader "U>=",NN ; ( u1 u2 -- f )  Return u1 >= u2 (unsigned)
UGe:		jsr U_Less_Than
		jmp ZEqA
	WordEnd

 WordHeader "U<=",NN ; ( u1 u2 -- f )  Return u1 <= u2 (unsigned)
ULe:		jsr U_Greater_Than
		jmp ZEqA
	WordEnd

Throw_Stack_11: jmp Throw_Stack


 WordHeader "0=",NN ; ( n -- f )  Return n == 0
  ; ## "0="  auto	 ANS core
  ; https://forth-standard.org/standard/core/ZeroEqual
Zero_Equal:	lda DStack+1,x
ZEq3:		ora DStack+0,x
		bne False0
		beq True0
	WordEnd

ZEqA: ; complement boolean, value in A
		eor #$ff
		sta DStack+0,x
		sta DStack+1,x
		rts

 WordHeader "0<>",NN ; ( n -- f )  Return n != 0
  ; ## "0<>"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/Zerone
Zero_Unequal:	lda DStack+1,x
ZNe3:		ora DStack+0,x
		beq False0

True0:		lda #$ff	; return TRUE
Return0:	sta DStack+0,x
		sta DStack+1,x
		DStackCheck 1,Throw_Stack_05
		rts
	WordEnd

 WordHeader "0>",NN ; ( n -- f )  Return n > 0
  ; ## "0>"  auto	 ANS core ext
  ; https://forth-standard.org/standard/core/Zeromore
Zero_Greater:	lda DStack+1,x	; MSB
		bpl ZNe3	; >= 0 ?

False0:		lda #0		; return FALSE
		beq Return0
	WordEnd

 WordHeader "0<=",NN ; ( n -- f )  return n <= 0
ZLe:		lda DStack+1,x	; < 0 ?
		bmi True0
		bpl ZEq3
	WordEnd

 WordHeader "0>=",NN ; ( n -- f )  return n >= 0
ZGe:		lda DStack+1,x
		bpl True0
		bmi False0
	WordEnd

 WordHeader "0<",NN ; ( n -- f )  return n < 0
  ; ## "0<"  auto	 ANS core
  ; https://forth-standard.org/standard/core/Zeroless
Zero_Less:	lda DStack+1,x	; MSB
		bpl False0
		bmi True0
	WordEnd


Throw_Stack_05: jmp Throw_Stack


 WordHeader "Min",UF+NN ; ( n1 n2 -- n )  Keep smaller of two signed numbers
  ; ## "min"  auto  ANS core
  ; https://forth-standard.org/standard/core/MIN
  ; See http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
Min:		DStackCheck 2,Throw_Stack_05

		lda DStack+0,x	; compare n1 & n2; sets V & N but not Z
		cmp DStack+2,x
		lda DStack+1,x
		sbc DStack+3,x
		bvs Max_3	; if overflow, the sign is backwards
Min_3:		bmi Nip_NoUf	; if negative, NOS is larger and needs to be dumped
		inx		; Drop n2
		inx
	WordEnd
		rts

 WordHeader "Max",NN ; ( n1 n2 -- n )  Keep larger of two signed numbers
  ; ## "max"  auto  ANS core
  ; https://forth-standard.org/standard/core/MAX
  ; See also
  ;   http://6502.org/tutorials/compare_instructions.html and
  ;   http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
Max:		DStackCheck 2,Throw_Stack_05

		lda DStack+0,x	; Compare n1 & n2; sets V & N but not Z
		cmp DStack+2,x
		lda DStack+1,x
		sbc DStack+3,x
		bvs Min_3	; if overflow, the sign is backwards
Max_3:		bpl Nip_NoUf	; if negative, NOS is larger and needs to be kept
		inx		; Drop n2
		inx
	WordEnd
		rts


 WordHeader "Nip",UF ; ( n1 n2 -- n2 )  Delete NOS
  ; ## "nip"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/NIP
Nip:
		jsr underflow_2

Nip_NoUf:	lda DStack+0,x	; PopYA
		ldy DStack+1,x
		inx
		inx
		sta DStack+0,x	; store over n1
		sty DStack+1,x
	WordEnd
		rts


 WordHeader "Pick",0 ; ( n n u -- n n n )  Move element u of the stack to TOS
  ; ## "pick"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/PICK
  ; Take the u-th element out of the stack and put it on TOS,
  ; overwriting the original TOS. 0 PICK is equivalent to DUP, 1 PICK to
  ; OVER. Note that using PICK is considered poor coding form. Also note
  ; that FIG Forth has a different behavior for PICK than ANS Forth.
Pick:
		; Checking for underflow is difficult because it depends on
		; which element we want to grab. We could probably figure
		; something out, but it wouldn't work with underflow stripping
		; Since using PICK is considered poor form anyway, we just
		; leave it as it is
		asl DStack+0,x	; we assume u < 128 (stack is small)
		txa
		adc DStack+0,x
		tay

		lda DStack+2,y
		sta DStack+0,x
		lda DStack+3,y
		sta DStack+1,x
	WordEnd
		rts


 WordHeader "Char",NN ; ( "c" -- u )  Convert character to ASCII value
  ; ## "char"  auto  ANS core
  ; https://forth-standard.org/standard/core/CHAR
Char:
		; get character from string, returns ( addr u )
		jsr parse_name_check

		inx		; Drop number of characters, leave addr
		inx
		jmp C_Fetch	; get character ( C@ )
	WordEnd


 WordHeader "[Char]",CO+IM+NN ; ( "c" -- )  Compile character literal
  ; ## "[char]"  auto  ANS core
  ; https://forth-standard.org/standard/core/BracketCHAR
  ; Compile the ASCII value of a character as a literal. This is an
  ; immediate, compile-only word.
  ;
  ; A definition given in
  ; http://forth-standard.org/standard/implement is
  ; : [CHAR]  CHAR POSTPONE LITERAL ; IMMEDIATE
Bracket_Char:	jsr Char
		jmp Literal
	WordEnd


 WordHeader "Char+",NN ; ( addr -- addr+1 )  Add the size of a character unit to address
  ; ## "char+"  auto  ANS core
  ; https://forth-standard.org/standard/core/CHARPlus
Char_Plus:	jmp One_Plus
	WordEnd


 WordHeader "Chars",AN ; ( n -- n )  Number of bytes that n chars need
  ; ## "chars"  auto  ANS core
  ; https://forth-standard.org/standard/core/CHARS
  ; Return how many address units n chars are.
  ; On 6502, these are equal & this word does nothing.
  ; It is included for compatibility with other Forth versions
Chars:
		; We assume the next consumer of n will detect overflow.
		; jsr underflow_1
	WordEnd
		rts


 WordHeader "Cells",UF ; ( u1 -- u2 )  Convert cells to size in bytes
  ; ## "cells"  auto  ANS core
  ; https://forth-standard.org/standard/core/CELLS
Cells:		jsr underflow_1

		asl DStack+0,x		; 2*
		rol DStack+1,x
	WordEnd
		rts


 WordHeader "Cell+",UF ; ( u -- u )  Add cell size in bytes
  ; ## "cell+"  auto  ANS core
  ; https://forth-standard.org/standard/core/CELLPlus
  ; Add the number of bytes ("address units") that one cell needs.
  ; Since this is an 8 bit machine with 16 bit cells, we add two bytes.
Cell_Plus:	jsr underflow_1

Cell_Plus_NoUf:	lda #2		; our cells are 2 bytes

Plus_A: ; ( n A -- n+A ) add unsigned A to TOS
		clc
		adc DStack+0,x
		sta DStack+0,x
		bcc +
		inc DStack+1,x
+
	WordEnd
		rts


Nos_Plus_A: ; add unsigned A to NOS
		clc
		adc DStack+2,x
		sta DStack+2,x
		bcc +
		inc DStack+3,x
+		rts


 WordHeader "Here",NN ; ( -- addr )  Push Compiler Pointer on Data Stack
  ; ## "here"  auto  ANS core
  ; https://forth-standard.org/standard/core/HERE
Here:		lda cp+0
		ldy cp+1
		jmp PushYA
	WordEnd


 WordHeader "1-",UF ; ( u -- u-1 )  Decrement TOS
  ; ## "1-"  auto	 ANS core
  ; https://forth-standard.org/standard/core/OneMinus
One_Minus:	jsr underflow_1
One_Minus_NoUf:
		lda DStack+0,x
		bne +
		dec DStack+1,x
+		dec DStack+0,x
	WordEnd
		rts


NOS_One_Minus: ; ( u1 u2 -- u1-1 u2 )  Decrement NOS
		lda DStack+2,x
		bne +
		dec DStack+3,x
+		dec DStack+2,x
		rts


Minus_A: ; ( n1 A -- n2 ) add negative extended A to TOS
		clc
		adc DStack+0,x
		sta DStack+0,x
		bcs +
		dec DStack+1,x
+		rts


 WordHeader "1+",UF ; ( u -- u+1 )  Increase TOS by one
  ; ## "1+"  auto	 ANS core
  ; https://forth-standard.org/standard/core/OnePlus
One_Plus:	jsr underflow_1

		inc DStack+0,x
		bne +
		inc DStack+1,x
+
	WordEnd
		rts


NOS_One_Plus: ; ( n1 n2 -- n1+1 n2 )  Increment NOS
		inc DStack+2,x
		bne +
		inc DStack+3,x
+		rts


 WordHeader "UM+",0 ; ( d1 u -- d2 )  Return d1 + u  (unsigned u)
UMPlus:		jsr PopYA

		clc		; add n to d1.lo
		adc DStack+2,x
		sta DStack+2,x
		tya
		adc DStack+3,x
		sta DStack+3,x
		bcc +		; propagate carry thru d1.hi
		inc DStack+0,x
		bne +
		inc DStack+1,x
+
	WordEnd
		rts

 WordHeader "M+",NN ; ( d1 n -- d2 )  Return d1 + n
  ; https://forth-standard.org/standard/double/MPlus
MPlus:		jsr UMPlus

		tya		; if n negative
		bmi One_Minus_NoUf ;   decrement d1.hi
	WordEnd
		rts


 WordHeader "D2*",UF ; ( d1 -- d2 )  Shift double left 1 bit
  ; https://forth-standard.org/standard/double/DTwoTimes
D2Star:		jsr underflow_2

		asl DStack+2,x
		rol DStack+3,x
		rol DStack+0,x
		rol DStack+1,x
	WordEnd
		rts

 WordHeader "D2/",UF ; ( d1 -- d2 )  Shift double right 1 bit, signed
  ; https://forth-standard.org/standard/double/DTwoDiv
D2Slash:	jsr underflow_2

		lda DStack+1,x		; setup for sign-extended shift right
		asl a
D2SlashU:	ror DStack+1,x
		ror DStack+0,x
		ror DStack+3,x
		ror DStack+2,x
	WordEnd
		rts

 WordHeader "UD2/",UF+NN ; ( ud1 -- ud2 )  Shift double right 1 bit, unsigned
DU2Slash:	jsr underflow_2

		clc
		bcc D2SlashU
	WordEnd


 WordHeader "2*",UF ; ( n -- n )  Multiply TOS by two
  ; ## "2*"  auto	 ANS core
  ; https://forth-standard.org/standard/core/TwoTimes
Two_Star:
		jsr underflow_1

		asl DStack+0,x
		rol DStack+1,x
	WordEnd
		rts


 WordHeader "2/",UF ; ( n1 -- n )  Divide TOS by two
  ; ## "2/"  auto	 ANS core
  ; """https://forth-standard.org/standard/core/TwoDiv"""
Two_Slash:
		jsr underflow_1

		lda DStack+1,x		; load sign into carry, for signed shift
		asl
		ror DStack+1,x
		ror DStack+0,x
	WordEnd
		rts


 WordHeader "U2/",UF ; ( u1 -- u )  Divide TOS by two (unsigned)
UTwo_Slash:
		jsr underflow_1

		lsr DStack+1,x
		ror DStack+0,x
	WordEnd
		rts


 WordHeader "DRShift",0 ; ( ud1 u -- ud2 )  shift double right u bits
DRShift:	jsr PopA	; pop u
DRShiftA:	tay
		beq _9
		lda DStack+1,x
_2:		lsr a
		ror DStack+0,x
		ror DStack+3,x
		ror DStack+2,x
		dey
		bne _2
		sta DStack+1,x
_9:
	WordEnd
		rts


 WordHeader "RShift",0 ; ( x u -- x )  Shift TOS to the right
  ; ## "rshift"  auto  ANS core
  ; https://forth-standard.org/standard/core/RSHIFT
RShift:		jsr PopA2	; pop u, check for 2 params
RShift_A:	tay		; get shift count
		beq _done
		lda DStack+1,x
_loop:		lsr a
		ror DStack+0,x
		dey
		bne _loop
		sta DStack+1,x
_done:
	WordEnd
		rts


 WordHeader "LShift",0 ; ( x u -- u )  Shift TOS left
  ; ## "lshift"  auto  ANS core
  ; https://forth-standard.org/standard/core/LSHIFT
LShift:		jsr PopA2	; pop u, check for 2 params
LShift_A:	tay		; get shift count
		beq _done
		lda DStack+1,x
_loop:		asl DStack+0,x
		rol a
		dey
		bne _loop
		sta DStack+1,x
_done:
	WordEnd
		rts


 WordHeader "And",UF ; ( n1 n2 -- n )  Bitwise AND TOS and NOS
  ; ## "and"  auto  ANS core
  ; https://forth-standard.org/standard/core/AND
And2:
		jsr underflow_2

		lda DStack+0,x
		and DStack+2,x
		sta DStack+2,x

		lda DStack+1,x
		and DStack+3,x
		sta DStack+3,x

		inx		; Drop n2
		inx
	WordEnd
		rts


 WordHeader "Or",UF ; ( n1 n2 -- n )  Bitwise OR TOS and NOS
  ; ## "or"  auto	 ANS core
  ; https://forth-standard.org/standard/core/OR
Or:
		jsr underflow_2

		lda DStack+0,x
		ora DStack+2,x
		sta DStack+2,x

		lda DStack+1,x
		ora DStack+3,x
		sta DStack+3,x

		inx		; Drop n2
		inx
	WordEnd
		rts


 WordHeader "Xor",UF ; ( n1 n2 -- n )  Bitwise XOR TOS and NOS
  ; ## "xor"  auto  ANS core
  ; https://forth-standard.org/standard/core/XOR
Xor:
		jsr underflow_2

		lda DStack+0,x
		eor DStack+2,x
		sta DStack+2,x

		lda DStack+1,x
		eor DStack+3,x
		sta DStack+3,x

		inx		; Drop n2
		inx
	WordEnd
		rts


 WordHeader "+",UF ; ( n1 n2 -- n )  Add TOS to NOS
  ; ## "+"  auto	ANS core
  ; https://forth-standard.org/standard/core/Plus
Plus:
		jsr underflow_2

		clc
		lda DStack+0,x		; LSB
		adc DStack+2,x
		sta DStack+2,x
		lda DStack+1,x		; MSB. No CLC, conserve carry bit
		adc DStack+3,x
		sta DStack+3,x

		inx
		inx
	WordEnd
		rts


 WordHeader "-",UF ; ( n1 n2 -- n )  Subtract TOS from NOS
  ; ## "-"  auto	ANS core
  ; https://forth-standard.org/standard/core/Minus
Minus:
		jsr underflow_2

		sec
		lda DStack+2,x	; LSB
		sbc DStack+0,x
		sta DStack+2,x
		lda DStack+3,x	; MSB
		sbc DStack+1,x
		sta DStack+3,x

		inx		; Drop n2
		inx
	WordEnd
		rts


 WordHeader ".",NN ; ( n -- )  Print signed single
  ; ## "."  auto	ANS core
  ; https://forth-standard.org/standard/core/d
Dot:		lda DStack+1,x		; ( n )	save sign
		php
		jsr Abs			; ( u )
		jsr Zero		; ( ud )	u>d  cvt u to ud
		jmp fmt_d3
	WordEnd


 WordHeader "D.",NN ; ( d -- )  Print signed double
  ; ## "d."  tested  ANS double
  ; http://forth-standard.org/standard/double/Dd
D_Dot:		lda DStack+1,x		; save sign
		php
		jsr DAbs
fmt_d3:		jsr Less_Number_Sign	; ( ud )	start formatting
		jsr Number_sign_s	; ( ud )	do all digits
		plp			; ( ud )	do sign
		jsr Sign_P		; ( ud )
		jsr Number_sign_greater	; ( addr u )	end formatting
		jsr Type
		jmp Space
	WordEnd


 WordHeader "U.",UF+NN ; ( u -- )  Print TOS as unsigned number
  ; ## "u."  tested  ANS core
  ; https://forth-standard.org/standard/core/Ud
U_Dot:		jsr underflow_1
		jsr print_u
		jmp Space
	WordEnd


 WordHeader "UD.",UF+NN ; ( ud -- )  Print double as unsigned
  ; ## "ud."  auto  Tali double
UD_Dot:		jsr underflow_2 ; double number
		jsr print_ud
		jmp Space
	WordEnd


print_u: ; ( u -- )  Print, without trailing space
		jsr Zero			; convert to ud

print_ud: ; ( ud -- )  Print, without trailing space
		jsr Less_Number_Sign		; <#	start formatting
		jsr Number_sign_s		; #S	do all digits
		jsr Number_sign_greater		; #>	end formatting
		jmp Type


 WordHeader "U.R",NN ; ( u u -- )  Print NOS as unsigned number right-justified with TOS width
  ; ## "u.r"  tested  ANS core ext
  ; https://forth-standard.org/standard/core/UDotR
U_Dot_R:	jsr PopA		; save field width
U_Dot_R_A:	pha
		jsr Zero		; u>d  cvt u to ud
		jmp fmt_udr3
	WordEnd


 WordHeader "UD.R",NN ; ( ud u -- )  Print unsigned double right-justified u wide
  ; ## "ud.r"  auto  Tali double
UD_Dot_R:	jsr PopA			; save field width
UD_Dot_R_A:	pha
fmt_udr3:	jsr Less_Number_Sign		; start formatted
		jsr Number_sign_s		; do all digits
fmt_r:		jsr Number_sign_greater		; finish formatted
		pla				; recover field width
		sec				; do leading spaces
		sbc DStack+0,x
		bcc +
		jsr PushZA
		jsr Spaces
+		jmp Type			; type formatted
	WordEnd


 WordHeader ".R",NN ; ( n u -- )  Print NOS as unsigned number in TOS width
  ; ## ".r"  tested  ANS core ext
  ; https://forth-standard.org/standard/core/DotR
Dot_R:		jsr PopA		; save field width
Dot_R_A:	pha
		lda DStack+1,x		; save sign
		php
		jsr Abs
		jsr Zero		; u>d  cvt u to ud
		jmp fmt_dr3
	WordEnd


 WordHeader "D.R",NN ; ( d u -- )  Print double right-justified u wide
  ; ## "d.r"  tested  ANS double
  ; http://forth-standard.org/standard/double/DDotR"""
D_Dot_R:	jsr PopA		; save field width
D_Dot_R_A:	pha
		lda DStack+1,x		; save sign
		php
		jsr DAbs
fmt_dr3:	jsr Less_Number_Sign	; start formatted output
		jsr Number_sign_s	; do all digits
		plp			; do the sign
		jsr Sign_P
		jmp fmt_r
	WordEnd


 WordHeader "?",NN ; ( addr -- )  Print content of a variable
  ; ## "?"  tested  ANS tools
  ; https://forth-standard.org/standard/tools/q
  ;
  ; Only used interactively. Since humans are so slow, we
  ; save size and just go for the subroutine jumps
Question:
		; FETCH takes care of underflow check
		jsr Fetch
		jmp Dot
	WordEnd


 WordHeader "2Dup",UF ; ( n1 n2 -- n1 n2 n1 n2 )  Duplicate top two stack elements
  ; ## "2dup"  auto  ANS core
  ; https://forth-standard.org/standard/core/TwoDUP
Two_Dup:
		jsr underflow_2

		dex
		dex
		dex
		dex

		lda DStack+4,x	; TOS
		sta DStack+0,x
		lda DStack+5,x
		sta DStack+1,x

		lda DStack+6,x	; NOS
		sta DStack+2,x
		lda DStack+7,x
		sta DStack+3,x
	WordEnd
		rts


 WordHeader "Tuck",UF ; ( b a -- a b a )  Copy TOS below NOS
  ; ## "tuck"  auto  ANS core ext
  ; https://forth-standard.org/standard/core/TUCK
Tuck:
		jsr underflow_2

		dex
		dex

		ldy DStack+4,x	; LSB
		lda DStack+2,x
		sta DStack+4,x
		sty DStack+2,x
		sta DStack+0,x

		ldy DStack+5,x	; MSB
		lda DStack+3,x
		sta DStack+5,x
		sty DStack+3,x
		sta DStack+1,x
	WordEnd
		rts


 WordHeader "C,",NN ; ( c -- )  Append one byte/char in memory
  ; ## "c,"  auto	 ANS core
  ; https://forth-standard.org/standard/core/CComma
C_Comma:	jsr PopA	; pop c, with underflow check
C_Comma_A: ; A=byte
		; We preserve Y.
		dex
		sty DStack+0,x	; save Y
		ldy #0		; store A
		sta (cp),y
		inc cp+0	; increment cp
		bne +
		inc cp+1
+
		ldy DStack+0,x	; restore Y
		inx
	WordEnd
		rts


 WordHeader ",",NN ; ( n -- )  Append one cell in memory
  ; ## ","  auto	ANS core
  ; https://forth-standard.org/standard/core/Comma
  ; Store TOS at current place in memory.
  ;
  ; Since 6502 doesn't care, we don't have any alignment issues.
Comma:		jsr PopYA	; pop n, with underflow check
Comma_YA:	; compile a word in YA (little-endian)
		jsr C_Comma_A	; compile LSB
		tya		; compile MSB
		jmp C_Comma_A
	WordEnd


 WordHeader "2,",NN ; ( d -- )  Append 2 cells in memory
Two_Comma:	jsr Comma
		jmp Comma
	WordEnd


; WordHeader "Drop,",NN ; ( -- )  Compile inline Drop
Drop_Comma:	lda #$e8	;inx
		tay
		bne Comma_YA
;	WordEnd

 WordHeader "Jsr,",NN ; ( addr -- )  compile JSR abs
Jsr_Comma:	jsr PopYA	; pop addr (optimize can skip)
Jsr_Comma_YA:	jsr PushYA	; push addr
		lda #$20	; JSR abs opcode
Jsr_Comma_3:	jsr C_Comma_A
		jmp Comma	; compile addr
	WordEnd

 WordHeader "Jmp,",NN ; ( addr -- )  compile JMP abs
Jmp_Comma:	jsr PopYA	; pop addr (optimize can skip)
Jmp_Comma_YA:	jsr PushYA	; push addr
		lda #$4c	; JMP abs opcode
		bne Jsr_Comma_3

Jmp_Comma_NT_YA: ; ( YA=nt -- )
		jsr PushYA
		jsr Name_To_Int	; convert nt to xt
		jmp Jmp_Comma
	WordEnd


 WordHeader "C@",UF ; ( addr -- c )  Get a character/byte from given address
  ; ## "c@"  auto	 ANS core
  ; https://forth-standard.org/standard/core/CFetch
C_Fetch:	jsr underflow_1

		lda (DStack+0,x)
		sta DStack+0,x
		lda #0
		sta DStack+1,x	; zero MSB
	WordEnd
		rts


 WordHeader "C!",UF ; ( c addr -- )  Store character at address given
  ; ## "c!"  auto	 ANS core
  ; https://forth-standard.org/standard/core/CStore
C_Store:	jsr underflow_2

		lda DStack+2,x
		sta (DStack+0,x)

		inx
		inx
		inx
		inx
	WordEnd
		rts


 WordHeader "1+!",0 ; ( addr -- )  increment cell at addr
OnePlusStore:
		lda #1

		clc
		adc (DStack+0,x)	; increment lo byte
		sta (DStack+0,x)
		bcc _7			; if carry

		inc DStack+0,x		;   point at hi byte
		bne +
		inc DStack+1,x
+
		lda (DStack+0,x)	;   increment hi byte
		adc #0
		sta (Dstack+0,x)
_7:
		inx			; Drop addr
		inx
	WordEnd
		rts 


 WordHeader "+!",UF+NN ; ( n addr -- )  Add number to value at given address
  ; ## "+!"  auto	 ANS core
  ; https://forth-standard.org/standard/core/PlusStore
Plus_store:
		jsr underflow_2

		clc
		lda DStack+2,x
		adc (DStack+0,x)
		sta (DStack+0,x)

		inc DStack+0,x
		bne +
		inc DStack+1,x
+
		lda DStack+3,x
		adc (DStack+0,x)
		sta (DStack+0,x)

		jmp Two_Drop
	WordEnd


 WordHeader "Bell",NN ; ( -- )  Emit ASCII Bell
  ; ## "bell"  tested  Tali Forth
Bell:		lda #7		; ASCII value for BELL
		bne Emit_A
	WordEnd


 WordHeader "Emit",NN ; ( char -- )  Print character to current output
  ; ## "emit"  auto  ANS core
  ; https://forth-standard.org/standard/core/EMIT
  ; Run-time default for EMIT. The user can revector this by changing
  ; the value of the OUTPUT variable. We ignore the MSB completely, and
  ; do not check to see if we have been given a valid ASCII character.
Emit:		jsr PopA		; pop char, with underflow check
Emit_A: ; print char in A
		jmp (output)		; JSR/RTS
	WordEnd


 WordHeader "Space",NN ; ( -- )  Print a single space
  ; ## "space"  auto  ANS core
  ; https://forth-standard.org/standard/core/SPACE
Space:		lda #AscSP
		bne Emit_A
	WordEnd


 WordHeader "Type",UF+NN ; ( addr u -- )  Print string
  ; ## "type"  auto  ANS core
  ; https://forth-standard.org/standard/core/TYPE
  ; Works through EMIT to allow OUTPUT revectoring.
Type:
		jsr underflow_2
		jmp _test

_loop:
		dec DStack+0,x		; finish length decrement

		lda (DStack+2,x)	; Send the current character
		jsr Emit_A

		jsr NOS_One_Plus	; increment address

_test:		lda DStack+0,x		; decrement length & test for <0
		bne _loop
		dec DStack+1,x
		bpl _loop

		jmp Two_drop
	WordEnd


Print_ASCIIZ_YA_no_lf: ; Print zero-terminated string at YA, no newline
  ; Uses tmp3 & Y .
		sta tmp3+0		; save string address
		sty tmp3+1

		ldy #0
Print_ASCIIZ_tmp3_no_lf:
		lda (tmp3),y
		beq _done		; end of string?

		jsr emit_a
		iny
		bne Print_ASCIIZ_tmp3_no_lf
_done:
		rts


Print_ASCIIZ_YA: ; Print zero-terminated string at YA, with newline
		jsr Print_ASCIIZ_YA_no_lf
                jmp CR


 WordHeader "Execute",NN ; ( xt -- )  Jump to word using execution token
  ; ## "execute"	auto  ANS core
  ; https://forth-standard.org/standard/core/EXECUTE
Execute:	DStackCheck 1,Throw_Stack_03

		lda DStack+1,x	; addr for RTI
		pha
		lda DStack+0,x
		pha
		inx		; drop addr
		inx
		php		; flags for RTI
		rti
	WordEnd


 WordHeader "2Rot",NN ; ( da db dc -- db dc da )  Rotate first three stack entries downwards
  ; ## "2rot"  auto  ANS double
  ; https://forth-standard.org/standard/double/TwoROT
TwoRot:		DStackCheck 6,Throw_Stack_03

		stx tmp1+0
		inx		; do 4 times
		inx
		inx
		inx
-		dex
		ldy DStack+8,x	; do a byte
		lda DStack+4,x
		sta DStack+8,x
		lda DStack+0,x
		sta DStack+4,x
		sty DStack+0,x
		cpx tmp1+0
		bne -
	WordEnd
		rts

Throw_Stack_03: jmp Throw_Stack

; WordHeader "Roll",NN ; ( ... u -- ... )  Rotate u+1 items on the top of the stack
  ; https://forth-standard.org/standard/core/ROLL

 WordHeader "Rot",NN ; ( a b c -- b c a )  Rotate first three stack entries downwards
  ; ## "rot"  auto  ANS core
  ; https://forth-standard.org/standard/core/ROT
  ; Remember "R for 'Revolution'" - the bottom entry comes out
  ; on top!
Rot:		DStackCheck 3,Throw_Stack_03

		ldy DStack+5,x	; do MSB
		lda DStack+3,x
		sta DStack+5,x
		lda DStack+1,x
		sta DStack+3,x
		sty DStack+1,x

		ldy DStack+4,x	; do LSB
		lda DStack+2,x
		sta DStack+4,x
		lda DStack+0,x
		sta DStack+2,x
		sty DStack+0,x
	WordEnd
		rts


 WordHeader "-Rot",NN ; ( a b c -- c a b )  Rotate upwards
  ; ## "-rot"  auto  Gforth
  ; http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Data-stack.html
Not_Rot:	DStackCheck 3,Throw_Stack_03

		ldy DStack+1,x	; do MSB
		lda DStack+3,x
		sta DStack+1,x
		lda DStack+5,x
		sta DStack+3,x
		sty DStack+5,x

		ldy DStack+0,x	; do LSB
		lda DStack+2,x
		sta DStack+0,x
		lda DStack+4,x
		sta DStack+2,x
		sty DStack+4,x
	WordEnd
		rts

 WordHeader "@",NN ; ( addr -- n )  Push cell content from memory to stack
  ; ## "@"  auto	ANS core
  ; https://forth-standard.org/standard/core/Fetch
Fetch:		DStackCheck 1,Throw_Stack_03

		lda (DStack+0,x)		; LSB
		tay

		inc DStack+0,x
		bne +
		inc DStack+1,x
+
		lda (DStack+0,x)		; MSB
		sta DStack+1,x
		sty DStack+0,x
	WordEnd
		rts


 WordHeader "!",NN ; ( n addr -- )  Store TOS in memory
  ; ## "!"  auto	ANS core
  ; https://forth-standard.org/standard/core/Store
Store:		DStackCheck 2,Throw_Stack_03

		lda DStack+2,x	; LSB
		sta (DStack+0,x)

		inc DStack+0,x
		bne +
		inc DStack+1,x
+
		lda DStack+3,x	; MSB
		sta (DStack+0,x)

		inx		; 2Drop
		inx
		inx
		inx
	WordEnd
		rts

 WordHeader "0!",0 ; ( addr -- )  Store 0 in memory
  ; Roughly STZ addr
ZStore:		jsr PopTmp1	; pop addr, check underflow
		lda #0
		tay		; clear LSB
		sta (tmp1),y
		iny		; clear MSB
		sta (tmp1),y
	WordEnd
		rts


 WordHeader ">R",CO+ST ; ( n -- )(R: -- n)  Move TOS to the Return Stack
  ; ## ">r"  auto	 ANS core
  ; native is special case
  ; https://forth-standard.org/standard/core/toR
  ; This word is handled differently for native and for
  ; subroutine coding, see `COMPILE,`.
To_R:
		pla		; move the RTS address out of the way
		sta tmp5+0
		pla
		sta tmp5+1

		; --- CUT HERE FOR NATIVE CODING ---

		; We check for underflow in the second step, so we can
		; strip off the stack thrashing for native compiling first
		jsr underflow_1

		; now we can do the actual work
		lda DStack+1,x	; MSB
		pha
		lda DStack+0,x	; LSB
		pha

		inx
		inx

		; --- CUT HERE FOR NATIVE CODING ---

		lda tmp5+1	; move the RTS address back in
		pha
		lda tmp5+0
		pha
	WordEnd
		rts


 WordHeader "R>",CO+ST ; ( -- n )(R: n --)  Move top of Return Stack to TOS
  ; ## "r>"  auto	 ANS core
  ; native is special case
  ; https://forth-standard.org/standard/core/Rfrom
  ; Move Top of Return Stack to Top of Data Stack.
  ;
  ; We have to move
  ; the RTS address out of the way first. This word is handled
  ; differently for native and and subroutine compilation, see COMPILE,
  ; This is a compile-only word
R_From:
		; This is ommitted when natively compiled.
		pla		; Move the RTS addr out of the way
		sta tmp5+0
		pla
		sta tmp5+1

		; --- CUT FOR NATIVE CODING ---

		dex
		dex
		pla		; LSB
		sta DStack+0,x
		pla		; MSB
		sta DStack+1,x

		; --- CUT FOR NATIVE CODING ---

		; This is ommitted when natively compiled.
		lda tmp5+1	; Restore the RTS addr
		pha
		lda tmp5+0
		pha
	WordEnd
		rts


 WordHeader "R@",NN+CO ; ( -- n )  Get copy of top of Return Stack
  ; ## "r@"  auto	 ANS core
  ; https://forth-standard.org/standard/core/RFetch
  ; This word is Compile Only in Tali Forth, though Gforth has it
  ; work normally as well
R_Fetch:
		stx tmp1	; save data stack index
		tsx		; X= return stack index
		lda RStack+3,x
		ldy RStack+4,x
		ldx tmp1	; restore data stack index

		jmp PushYA
	WordEnd


 WordHeader "Over",UF ; ( b a -- b a b )  Copy NOS to TOS
  ; ## "over"  auto  ANS core
  ; https://forth-standard.org/standard/core/OVER
Over:
		jsr underflow_2

		lda DStack+2,x	; LSB
		ldy DStack+3,x	; MSB

		dex		; PushYA
		dex
		sta DStack+0,x
		sty DStack+1,x
	WordEnd
		rts


 WordHeader "?Dup",UF+NN ; ( n -- 0 | n n )  Duplicate TOS if non-zero
  ; ## "?dup"  auto  ANS core
  ; https://forth-standard.org/standard/core/qDUP
Question_Dup:
		jsr underflow_1

		lda DStack+0,x	; Check if TOS is zero
		ora DStack+1,x
		bne Dup_NoUf
	WordEnd
		rts

 WordHeader "Dup",UF ; ( u -- u u )  Duplicate TOS
  ; ## "dup"  auto  ANS core
  ; https://forth-standard.org/standard/core/DUP
Dup:
		jsr underflow_1

Dup_NoUf:	lda DStack+0,x	; LSB
		ldy DStack+1,x	; MSB

		dex
		dex
		sta DStack+0,x
		sty DStack+1,x
	WordEnd
		rts


PushAY: ; ( AY -- n )
		dex
		dex
		sty DStack+0,x
		sta DStack+1,x
		rts


 WordHeader "Swap",UF ; ( b a -- a b )  Exchange TOS and NOS
  ; ## "swap"  auto  ANS core
  ; https://forth-standard.org/standard/core/SWAP
Swap:
		jsr underflow_2

		lda DStack+0,x	; do LSB
		ldy DStack+2,x
		sta DStack+2,x
		sty DStack+0,x

		lda DStack+1,x	; do MSB
		ldy DStack+3,x
		sta DStack+3,x
		sty DStack+1,x
	WordEnd
		rts


 WordHeader "Drop",UF ; ( u -- )  Discard top entry on Data Stack
  ; ## "drop"  auto  ANS core
  ; https://forth-standard.org/standard/core/DROP
Drop:
		jsr underflow_1

		inx
		inx
	WordEnd
		rts


forth_dictionary_start = WordListLink ; END of FORTH-WORDLIST


; ROOT-WORDLIST---------------------------------------------------------------
WordListLink .var 0
.if "wordlist" in TALI_OPTIONAL_WORDS

  ; This is a short wordlist that has just the words needed to
  ; set the wordlists. These words have entries in the
  ; FORTH-WORDLIST as well.

 WordHeader "Words",NN
		jmp Words
	WordEnd

 WordHeader "Forth-Wordlist",NN
		jmp Forth_WordList
	WordEnd

 WordHeader "Forth",NN
		jmp Forth
	WordEnd

 WordHeader "Set-Order",NN
		jmp Set_Order
	WordEnd

.endif

root_dictionary_start = WordListLink ; END of ROOT-WORDLIST


; ASSEMBLER-WORDLIST-------------------------------------------------------------------

;assembler:		; used to calculate size of assembler code
WordListLink .var 0
.if "assembler" in TALI_OPTIONAL_WORDS

; This is the built-in assembler for Tali Forth 2.
; Once the assembler wordlist is included with
;
;	assembler-wordlist >order
;
; the opcodes are available as normal Forth words. The format is Simpler
; Assembler Notation (SAN) which separates the opcode completely from the
; operand (see https://github.com/scotws/SAN). In this case, the operand is
; entered before the opcode in the postfix Forth notation (for example, "200
; lda.#"). See the assembler documenation in the manual for more detail.

; The code here was originally used in A Typist's Assembler for the 6502
; (tasm65c02), see https://github.com/scotws/tasm65c02 for the standalone
; version. Tasm65c02 is in the public domain.

;------ assembler word processor routines:

asm_r: ; opcode follows JSR, 1byte relative on param stack
	pla		; pop RTS addr
	tay
	pla
	jsr asm_op	; compile opcode
;	jsr Here	; calc displacement
;	jsr One_plus
;	jsr Minus
	lda DStack+1,x	; check range
	beq _plus
	cmp #$ff
	beq _minus
_err:	lda #$100+err_OutOfRange
	jmp ThrowA

_plus:	lda DStack+0,x
	bmi _err
	bpl _store

_minus:	lda DStack+0,x
	bpl _err
_store:	jmp C_Comma


asm_1: ; opcode follows JSR, 1byte on param stack
	pla		; pop RTS addr
	tay
	pla
	jsr asm_op	; compile opcode
	lda DStack+1,x	; compile operand
	beq _store	;   unsigned byte?
	cmp #$ff	;   signed byte?
	bne _err
_store:	jmp C_Comma

_err:	lda #$100+err_OutOfRange
	jmp ThrowA


asm_2: ; opcode follows JSR, 2byte on param stack
	pla		; pop RTS addr
	tay
	pla
	jsr asm_op	; compile opcode
	jmp Comma	; compile operand


asm_0: ; opcode follows JSR, no additional bytes
	pla		; pop RTS addr
	tay
	pla
asm_op:	sty tmp1+0	; save RTS addr
	sta tmp1+1
	ldy #1		; get opcode byte
	lda (tmp1),y
	jmp C_Comma_A	; compile opcode

; MNEMONICS--------------------------------------------------------

; The assembler instructions are realized as individual Forth words.

Inst .macro name, opcode, processor ; compile assembler instruction word
    WordHeader \name,NN
	jsr \processor
      WordEnd
	.byte \opcode
  .endmacro

 Inst "adc.#"	,$69,asm_1 ; ADC #nn
 Inst "adc.x"	,$7d,asm_2 ; ADC nnnn,X
 Inst "adc.y"	,$79,asm_2 ; ADC nnnn,Y
 Inst "adc.z"	,$65,asm_1 ; ADC nn
; Inst "adc.zi"	,$72,asm_1 ; ADC (nn)
 Inst "adc.ziy"	,$71,asm_1 ; ADC (nn),Y
 Inst "adc.zx"	,$75,asm_1 ; ADC nn,X
 Inst "adc.zxi"	,$61,asm_1 ; ADC (nn,X)
 Inst "and."	,$2d,asm_2 ; AND nnnn ; not "and" because of conflicts with Forth word
 Inst "and.#"	,$29,asm_1 ; AND #nn
 Inst "and.x"	,$3d,asm_2 ; AND nnnn,X
 Inst "and.y"	,$39,asm_2 ; AND nnnn,Y
 Inst "and.z"	,$25,asm_1 ; AND nn
; Inst "and.zi"	,$32,asm_1 ; AND (nn)
 Inst "and.ziy"	,$31,asm_1 ; AND (nn),Y
 Inst "and.zx"	,$35,asm_1 ; AND nn,X
 Inst "and.zxi"	,$21,asm_1 ; AND (nn,X)
 Inst "asl"	,$0e,asm_2 ; ASL nnnn
 Inst "asl.a"	,$0a,asm_0 ; ASL
 Inst "asl.x"	,$1e,asm_2 ; ASL nnnn,X
 Inst "asl.z"	,$06,asm_1 ; ASL nn
 Inst "asl.zx"	,$16,asm_1 ; ASL nn,X
 Inst "bcc"	,$90,asm_r ; BCC
 Inst "bcs"	,$b0,asm_r ; BCS
 Inst "beq"	,$f0,asm_r ; BEQ
 Inst "bit"	,$2c,asm_2 ; BIT nnnn
; Inst "bit.#"	,$89,asm_1 ; BIT #nn
; Inst "bit.x"	,$3c,asm_2 ; BIT nnnn,X
 Inst "bit.z"	,$24,asm_1 ; BIT nn
; Inst "bit.zx"	,$34,asm_1 ; BIT nn,X
 Inst "bmi"	,$30,asm_r ; BMI
 Inst "bne"	,$d0,asm_r ; BNE
 Inst "bpl"	,$10,asm_r ; BPL
; Inst "bra"	,$80,asm_r ; BRA
 Inst "brk"	,$00,asm_1 ; BRK
 Inst "bvc"	,$50,asm_r ; BVC
 Inst "bvs"	,$70,asm_r ; BVS
 Inst "clc"	,$18,asm_0 ; CLC
 Inst "cld"	,$d8,asm_0 ; CLD
 Inst "cli"	,$58,asm_0 ; CLI
 Inst "clv"	,$b8,asm_0 ; CLV
 Inst "cmp"	,$cd,asm_2 ; CMP nnnn
 Inst "cmp.#"	,$c9,asm_1 ; CMP #nn
 Inst "cmp.x"	,$dd,asm_2 ; CMP nnnn,X
 Inst "cmp.y"	,$d9,asm_2 ; CMP nnnn,Y
 Inst "cmp.z"	,$c5,asm_1 ; CMP nn
; Inst "cmp.zi"	,$d2,asm_1 ; CMP (nn)
 Inst "cmp.ziy"	,$d1,asm_1 ; CMP (nn),Y
 Inst "cmp.zx"	,$d5,asm_1 ; CMP nn,X
 Inst "cmp.zxi"	,$c1,asm_1 ; CMP (nn,X)
 Inst "cpx"	,$ec,asm_2 ; CPX nnnn
 Inst "cpx.#"	,$e0,asm_1 ; CPX #nn
 Inst "cpx.z"	,$e4,asm_1 ; CPX nn
 Inst "cpy"	,$cc,asm_2 ; CPY nnnn
 Inst "cpy.#"	,$c0,asm_1 ; CPY #nn
 Inst "cpy.z"	,$c4,asm_1 ; CPY nn
 Inst "dec"	,$ce,asm_2 ; DEC nnnn
 Inst "dec.a"	,$3a,asm_0 ; DEC
 Inst "dec.x"	,$de,asm_2 ; DEC nnnn,X
 Inst "dec.z"	,$c6,asm_1 ; DEC nn
 Inst "dec.zx"	,$d6,asm_1 ; DEC nn,X
 Inst "dex"	,$ca,asm_0 ; DEX
 Inst "dey"	,$88,asm_0 ; DEY
 Inst "eor"	,$4d,asm_2 ; EOR nnnn
 Inst "eor.#"	,$49,asm_1 ; EOR #nn
 Inst "eor.x"	,$5d,asm_2 ; EOR nnnn,X
 Inst "eor.y"	,$59,asm_2 ; EOR nnnn,Y
 Inst "eor.z"	,$45,asm_1 ; EOR nn
; Inst "eor.zi"	,$52,asm_1 ; EOR (nn)
 Inst "eor.ziy"	,$51,asm_1 ; EOR (nn),Y
 Inst "eor.zx"	,$55,asm_1 ; EOR nn,X
 Inst "eor.zxi"	,$41,asm_1 ; EOR (nn,X)
 Inst "inc"	,$ee,asm_2 ; INC nnnn
 Inst "inc.a"	,$1a,asm_0 ; INC
 Inst "inc.x"	,$fe,asm_2 ; INC nnnn,X
 Inst "inc.z"	,$e6,asm_1 ; INC nn
 Inst "inc.zx"	,$f6,asm_1 ; INC nn,X
 Inst "inx"	,$e8,asm_0 ; INX
 Inst "iny"	,$c8,asm_0 ; INY
 Inst "jmp"	,$4c,asm_2 ; JMP nnnn
 Inst "jmp.i"	,$6c,asm_2 ; JMP (nnnn)
; Inst "jmp.xi"	,$7c,asm_2 ; JMP (nnnn,X)
 Inst "jsr"	,$20,asm_2 ; JSR nnnn
 Inst "lda"	,$ad,asm_2 ; LDA nnnn
 Inst "lda.#"	,$a9,asm_1 ; Inst nn
 Inst "lda.x"	,$bd,asm_2 ; LDA nnnn,X
 Inst "lda.y"	,$b9,asm_2 ; LDA nnnn,Y
 Inst "lda.z"	,$a5,asm_1 ; LDA nn
; Inst "lda.zi"	,$b2,asm_1 ; LDA (nn)
 Inst "lda.ziy"	,$b1,asm_1 ; LDA (nn),Y
 Inst "lda.zx"	,$b5,asm_1 ; LDA nn,X
 Inst "lda.zxi"	,$a1,asm_1 ; LDA (nn,X)
 Inst "ldx"	,$ae,asm_2 ; LDX nnnn
 Inst "ldx.#"	,$a2,asm_1 ; LDX #nn
 Inst "ldx.y"	,$be,asm_2 ; LDX nnnn,Y
 Inst "ldx.z"	,$a6,asm_1 ; LDX nn
 Inst "ldx.zy"	,$b6,asm_1 ; LDX nn,Y
 Inst "ldy"	,$ac,asm_2 ; LDY nnnn
 Inst "ldy.#"	,$a0,asm_1 ; LDY #nn
 Inst "ldy.x"	,$bc,asm_2 ; LDY nnnn,X
 Inst "ldy.z"	,$a4,asm_1 ; LDY nn
 Inst "ldy.zx"	,$b4,asm_1 ; LDY nn,X
 Inst "lsr"	,$4e,asm_2 ; LSR nnnn
 Inst "lsr.a"	,$4a,asm_0 ; LSR
 Inst "lsr.x"	,$5e,asm_2 ; LSR nnnn,X
 Inst "lsr.z"	,$46,asm_1 ; LSR nn
 Inst "lsr.zx"	,$56,asm_1 ; LSR nn,X
 Inst "nop"	,$ea,asm_0 ; NOP
 Inst "ora"	,$0d,asm_2 ; ORA nnnn
 Inst "ora.#"	,$09,asm_1 ; ORA #nn
 Inst "ora.x"	,$1d,asm_2 ; ORA nnnn,X
 Inst "ora.y"	,$19,asm_2 ; ORA nnnn,Y
 Inst "ora.z"	,$05,asm_1 ; ORA nn
; Inst "ora.zi"	,$12,asm_1 ; ORA (nn)
 Inst "ora.ziy"	,$11,asm_1 ; ORA (nn),Y
 Inst "ora.zx"	,$15,asm_1 ; ORA nn,X
 Inst "ora.zxi"	,$01,asm_1 ; ORA (nn,X)
 Inst "pha"	,$48,asm_0 ; PHA
 Inst "php"	,$08,asm_0 ; PHP
; Inst "phx"	,$da,asm_0 ; PHX
; Inst "phy"	,$5a,asm_0 ; PHY
 Inst "pla"	,$68,asm_0 ; PLA
 Inst "plp"	,$28,asm_0 ; PLP
; Inst "plx"	,$fa,asm_0 ; PLX
; Inst "ply"	,$7a,asm_0 ; PLY
 Inst "rol"	,$2e,asm_2 ; ROL nnnn
 Inst "rol.a"	,$2a,asm_0 ; ROL
 Inst "rol.x"	,$3e,asm_2 ; ROL nnnn,X
 Inst "rol.z"	,$26,asm_1 ; ROL nn
 Inst "rol.zx"	,$36,asm_1 ; ROL nn,X
 Inst "ror"	,$6e,asm_2 ; ROR nnnn
 Inst "ror.a"	,$6a,asm_0 ; ROR
 Inst "ror.x"	,$7e,asm_2 ; ROR nnnn,X
 Inst "ror.z"	,$66,asm_1 ; ROR nn
 Inst "ror.zx"	,$76,asm_1 ; ROR nn,X
 Inst "rti"	,$40,asm_0 ; RTI
 Inst "rts"	,$60,asm_0 ; RTS
 Inst "sbc"	,$ed,asm_2 ; SBC nnnn
 Inst "sbc.#"	,$e9,asm_1 ; SBC #nn
 Inst "sbc.x"	,$fd,asm_2 ; SBC nnnn,X
 Inst "sbc.y"	,$f9,asm_2 ; SBC nnnn,Y
 Inst "sbc.z"	,$e5,asm_1 ; SBC nn
; Inst "sbc.zi"	,$f2,asm_1 ; SBC (nn)
 Inst "sbc.ziy"	,$f1,asm_1 ; SBC (nn),Y
 Inst "sbc.zx"	,$f5,asm_1 ; SBC nn,X
 Inst "sbc.zxi"	,$e1,asm_1 ; SBC (nn,X)
 Inst "sec"	,$38,asm_0 ; SEC
 Inst "sed"	,$f8,asm_0 ; SED
 Inst "sei"	,$78,asm_0 ; SEI
 Inst "sta"	,$8d,asm_2 ; STA nnnn
 Inst "sta.x"	,$9d,asm_2 ; STA nnnn,X
 Inst "sta.y"	,$99,asm_2 ; STA nnnn,Y
 Inst "sta.z"	,$85,asm_1 ; STA nn
; Inst "sta.zi"	,$92,asm_1 ; STA (nn)
 Inst "sta.ziy"	,$91,asm_1 ; STA (nn),Y
 Inst "sta.zx"	,$95,asm_1 ; STA nn,X
 Inst "sta.zxi"	,$81,asm_1 ; STA (nn,X)
 Inst "stx"	,$8e,asm_2 ; STX nnnn
 Inst "stx.z"	,$86,asm_1 ; STX nn
 Inst "stx.zy"	,$96,asm_1 ; STX nn,Y
 Inst "sty"	,$8c,asm_2 ; STY nnnn
 Inst "sty.z"	,$84,asm_1 ; STY nn
 Inst "sty.zx"	,$94,asm_1 ; STY nn,X
; Inst "stz"	,$9c,asm_2 ; STZ nnnn
; Inst "stz.x"	,$9e,asm_2 ; STZ nnnn,X
; Inst "stz.z"	,$64,asm_1 ; STZ nn
; Inst "stz.zx"	,$74,asm_1 ; STZ nn,X
 Inst "tax"	,$aa,asm_0 ; TAX
 Inst "tay"	,$a8,asm_0 ; TAY
; Inst "trb"	,$1c,asm_2 ; TRB nnnn
; Inst "trb.z"	,$14,asm_1 ; TRB nn
; Inst "tsb"	,$0c,asm_2 ; TSB nnnn
; Inst "tsb.z"	,$04,asm_1 ; TSB nn
 Inst "tsx"	,$BA,asm_0 ; TSX
 Inst "txa"	,$8a,asm_0 ; TXA
 Inst "txs"	,$9a,asm_0 ; TXS
 Inst "tya"	,$98,asm_0 ; TYA

asm_table = WordListLink ; head of instruction word list for disassembler


; Assembler pseudo-instructions, directives and macros


 WordHeader "push-a",IM+NN ; ( A -- n )  Push unsigned A onto data stack
  ; Puts the content of the 6502 Accumulator on the Forth
  ; data stack as the TOS.
		lda #<PushZA
		ldy #>PushZA
		jmp Jsr_Comma_YA
	WordEnd


 WordHeader "-->",IM+NN ; ( -- addr )	 Save address
		jmp Here
	WordEnd


 WordHeader "<j",IM ; ( addr -- addr )  Calc absolute address
  ; (back jump) is a dummy instruction (syntactic sugar) to
  ; make clear that the JMP or JSR instructions are using the address that had
  ; been placed on the stack by "-->" (the "arrow" directive).

	WordEnd
		rts


 WordHeader "<b",IM+NN ; ( addr -- n )  Calc displacement
  ; The "<B" directive (back branch) takes an address that was placed on the Data
  ; Stack by the anonymous label directive "-->" (the "arrow") and the current
  ; address (via HERE) to calculate a backward branch offset. This is then stored
  ; by a following branch instruction.
asm_back_branch:
		; We arrive here with ( addr-l ) of the label on the stack and
		; then subtract the current address
		jsr Here		; ( addr-l addr-h )
		jsr Minus		; ( offset )

		; We subtract two more because of the branch instruction itself
		lda #$fe
		jmp Minus_A
	WordEnd


 WordHeader "DisAsm",NN ; ( addr n -- )  Disassemble a block of 6502 code.
  ; Using assembler wordlist.
  ; Convert a segment of memory to assembler output.
  ; This produces Simpler Assembly Notation (SAN) code
  ; see the section on The Disassembler in the manual for more details.
DisAsm:
		jsr Bounds		; ( addr_end addr )
_instr: ; instruction loop
		jsr underflow_2

		lda DStack+0,x		; addr >= addr_end?
		cmp DStack+2,x
		lda DStack+1,x
		sbc DStack+3,x
		bcc +
		jmp Two_drop		; discard parms & return
+
		jsr CR			; new line
		jsr Dup			; print address
		jsr Dot_Hex
		jsr Space
		lda (DStack+0,x)	; print opcode byte
		jsr _print_a

		lda (DStack+0,x)	; save opcode
		sta tmp3
		jsr One_plus

		lda #<asm_table		; for each asm opcode word
		ldy #>asm_table
		sta tmp1+0
		sty tmp1+1
		bne _WTest

_WNext:		jsr LinkNext		;   next entry
		beq _unknown

_WTest:		jsr NameToIntTmp	; tmp2= xt
		ldy #3			;   opcode match?
		lda (tmp2),y
		cmp tmp3
		bne _WNext
				; found matching assembler opcode word

		lda tmp3		; save opcode
		pha

		lda tmp1+1		; save its nt
		pha
		lda tmp1+0
		pha

		ldy #1			; get processor routine addr lo byte from JSR
		lda (tmp2),y
		cmp #<asm_0		; goto disasm processor
		beq _c0
		cmp #<asm_1
		beq _c1
		cmp #<asm_2
		beq _c2
		cmp #<asm_r
		beq _cr

_unknown: ; no opcode word found
		jsr _tab		; to source area
		lda #'?'
		jsr Emit_A
		jmp _instr


_c0: ; no operands
		jsr _tab		; to source area
		pla			; get opcode word nt
		tay
		pla
		jsr _print_opcode
		pla			; discard opcode
		jmp _instr


_cr: ; 1byte relative operand
		jsr _get_byte		; get displacement byte
		ldy #0			; sign extend displacement
		ora #0
		bpl +
		dey
+		jsr PushYA
		jsr Dup			; save for TypeSymbol
					; ( addr_end addr dest dest )
		jsr _tab		; to source area
		jsr Dot			; print dest address
		pla			; get opcode word nt
		tay
		pla
		jsr _print_opcode
		pla			; discard opcode
		jsr Over		; calc dest addr
		jsr Plus
		jsr TypeSymbol		; print dest addr symbolic
		jmp _instr


_c1: ; 1byte immediate or direct operand
		jsr _get_byte		; get operand
		jsr PushZA
		jsr _tab		; to source area
		jsr Dup
		jsr U_Dot		; print operand
		pla			; get opcode word nt
		tay
		pla
		jsr _print_opcode
		pla			; discard opcode
		jsr TypeSymbol
		jmp _instr


_c2: ; 2byte absolute operand
		jsr _get_byte		; get operand lo byte
		pha
		jsr _get_byte		; get operand hi byte

		dex			; push operand bytes
		dex
		sta DStack+1,x
		pla
		sta DStack+0,x
					; ( addr_end addr operand )
		jsr _tab		; to source area
		jsr Dup
		jsr U_Dot		; print operand
		pla			; AY= opcode word nt
		tay
		pla
		jsr _print_opcode

		jsr Dup
		jsr TypeSymbol		; print operand as symbol

					; ( addr_end addr operand )

		pla			; get opcode
		cmp #$20		;  JSR
		beq _jsr
_jdrop:		inx			; Drop operand
		inx
		jmp _instr		; ( addr_end addr )

_2drop:		inx			; Drop jsr_nt
		inx
		bne _jdrop

_jsr: ; it's a jsr
		; ( addr_end addr operand )

		jsr PopYA

		cmp #<sliteral_runtime	; string literal?
		bne _not_sliteral
		cpy #>sliteral_runtime
		bne _not_sliteral

		jsr CR
		jsr Dup			; ( addr_end addr addr )
		jsr Dot_Hex
		jsr Space
		jsr Dup			; ( addr_end ? addr )
		jsr _get_byte		; get JMP opcode
		jsr _get_byte		; get JMP addr lo byte
		sta DStack+2,x
		jsr _get_byte		; get JMP addr hi byte
		sta DStack+3,x		; ( addr_end string_end addr )
		jsr CR
		jsr Dup
		jsr Dot_Hex
		jsr Space

_slit_11:	lda DStack+0,x		; done with string data?
		cmp DStack+2,x
		lda DStack+1,x
		sbc DStack+3,x
		bcs _slit_19

		jsr _get_byte
		jmp _slit_11
_slit_19:
		jsr Nip			; ( addr_end addr )

		jmp _instr

_not_sliteral:

;		cmp #<two_literal_runtime	; string literal?
;		bne _not_2literal
;		cpy #>two_literal_runtime
;		bne _not_2literal

;_not_2literal:
		jmp _instr


;-------------------------

_tab: ; move over to disassembly area
		lda #9
		jmp Emit_A

_print_opcode: ; print opcode from word header
	; AY = opcode word nt
		jsr PushAY		; save opcode word nt
		jsr Space
		jsr Name_To_String	; get name string
		jmp Type		; type word name


_get_byte_silent: ; get next instruction stream byte
		lda (DStack+0,x)
		jmp One_plus

_get_byte: ; get next instruction stream byte to A
		jsr _get_byte_silent
		pha
		jsr _print_a
		pla
		rts


_print_a:
		jsr PushZA
		jsr C_Dot_Hex
		jmp Space


.endif ; "assembler"
assembler_dictionary_start = WordListLink ; END of ASSEMBLER-WORDLIST


; EDITOR-WORDLIST----------------------------------------------------------------
WordListLink .var 0	; start wordlist

.if "editor" in TALI_OPTIONAL_WORDS && "block" in TALI_OPTIONAL_WORDS


 WordHeader "l",NN ; ( -- )  List the current screen
  ; ## "l"  tested  Tali Editor
Editor_l:	jmp ListScr
	WordEnd


Editor_Screen_Helper: ; ( scr# -- addr )  Get block in buffer, return addr of buffer
  ; Used by both enter-screen and erase-screen
  ; to get a buffer for the given screen number and set SCR to
  ; the given screen number.  This word is not in the dictionary.
		jsr Dup
		jsr Scr
		jsr Store
		jmp Buffer


 WordHeader "enter-screen",NN ; ( scr# -- )  Enter all lines for given screen
  ; ## "enter-screen"  auto  Tali Editor
Editor_Enter_Screen:
		; Set the variable SCR and get a buffer for the
		; given screen number.
		jsr Editor_Screen_Helper

					; buffer address gets reused as work area

		lda #0			; Overwrite the lines one at a time.
_loop:		sta DStack+0,x

		jsr PushZA		; Put the current line number on the stack.

		jsr Editor_o		; prompt for overwrite.

		inc DStack+0,x		; Move on to the next line.
		lda DStack+0,x
		cmp #16
		bcc _loop

		inx			; Drop work area
		inx
	WordEnd
		rts


 WordHeader "line",NN ; ( line# -- c-addr )  Turn a line number into address in current screen
  ; ## "line"  tested  Tali Editor
Editor_line:
;		jsr underflow_1

		lda #6		; Multiply the TOS by 64 (chars/line) to compute offset.
		jsr LShift_A	; *64 is same as left shift 6 times.

		; Load the current screen into a buffer
		; and get the buffer address
		jsr Scr
		jsr Fetch
		jsr Block

		; Add the offset to the buffer base address.
		jmp Plus
	WordEnd


 WordHeader "erase-screen",NN ; ( scr# -- )  Erase all lines for given screen
  ; ## "erase-screen"  tested  Tali Editor
Editor_Erase_Screen:
		; Set the variable SCR and get a buffer for the
		; given screen number.
		jsr Editor_Screen_Helper

		lda #<1024
		ldy #>1024
		jsr PushYA
		jsr Blank		; Erase the entire block (fill with spaces).

		jmp Update		; Mark buffer as updated.
	WordEnd


 WordHeader "el",NN ; ( line# -- )  Erase the given line number
  ; ## "el"  tested  Tali Editor
Editor_el:
		; Turn the line number into buffer offset.
		; This also loads the block into the buffer if it's
		; not there for some reason.
		jsr Editor_line

		lda #64			;(# of chars/line)
		jsr PushZA
		jsr Blank		; Fill with spaces.

		jmp Update		; Mark buffer as updated.
	WordEnd


 WordHeader "o",NN ; ( line# -- )  Overwrite the given line
  ; ## "o"  tested  Tali Editor
Editor_o:
		; Print prompt
		jsr CR
		jsr Dup
		lda #2
		jsr U_Dot_R_A
		jsr Space
		lda #'*'
		jsr Emit_A
		jsr Space

		; Accept new input (directly into the buffer)
		jsr Editor_line
		jsr Dup		; Save a copy of the line address for later.
		lda #64		; chars/line
		jsr PushZA
		jsr Accept

		; Fill the rest with spaces.
		; Stack is currently ( line_address numchars_from_accept )
		jsr Dup
		jsr Not_Rot
		jsr Plus
		lda #64		; chars/line
		jsr PushZA
		jsr Rot
		jsr Minus
		jsr Blank

		; Mark buffer as updated.
		jmp Update
	WordEnd

.endif ; editor & block

editor_dictionary_start = WordListLink ; END of EDITOR-WORDLIST




.include "strings.asm"          ; Strings, including error messages


 .endsection code


; END
