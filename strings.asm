; Strings for Tali Forth 2
; Scot W. Stevenson <scot.stevenson@gmail.com>
; First version: 01. Apr 2016 (for Liara Forth)
; This version: 31. May 2024

; This file is included by taliforth.asm

; ## GENERAL STRINGS

; All general strings must be zero-terminated

str_ok:         .text " ok", 0         ; note space at beginning
str_compiled:   .text " compiled", 0   ; note space at beginning
str_redefined:  .text "redefined ", 0  ; note space at end

str_wid_asm:    .text "Assembler ", 0  ; Wordlist ID 2, note space at end
str_wid_editor: .text "Editor ", 0     ; Wordlist ID 1, note space at end
str_wid_forth:  .text "Forth ", 0      ; Wordlist ID 0, note space at end
str_wid_root:   .text "Root ", 0       ; Wordlist ID 3, note space at end

str_see_flags:  .text "flags:", 0
str_see_nt:     .text "nt: $", 0
str_see_xt:     .text "xt: $", 0
str_see_size:   .text "size: #", 0

;str_disasm_lit: .text "LITERAL ", 0
;str_disasm_sdc: .text "STACK DEPTH CHECK", 0
;str_disasm_bra: .text "BRANCH ",0


;s_abc_lower:  .text "0123456789abcdefghijklmnopqrstuvwxyz"
;s_abc_upper:  .text "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

   
; ## ERROR STRINGS

; If the string texts are changed, the test suite must be as well
; They're changed!

Exception_Text_List:
  ; Each entry consist of:
  ;    1 byte error code (0 terminates list)
  ;    0 terminated ASCII text
	.text $100+err_Abort,0
	.text $100+err_AbortQuote,0
	.text $100+err_Stack_Overflow,"Stack Overflow",0
	.text $100+err_Stack_Underflow,"Stack Underflow",0
;	.text $100+err_Return_Stack_Overflow,"Return stack overflow",0
;	.text $100+err_Return_Stack_Underflow,"Return stack underflow",0
	.text $100+err_DoLoop_TooDeep,"do-loops nested too deeply during execution",0
;	.text $100+err_Dictionary_Overflow,"Dictionary overflow",0
;	.text $100+err_InvalidMemoryAddr,"invalid memory address",0
	.text $100+err_DivideBy0,"Divide by 0",0
	.text $100+err_OutOfRange,"out of range",0
	.text $100+err_ArgTypeMismatch,"argument type mismatch",0
	.text $100+err_UndefinedWord,"Undefined word",0
	.text $100+err_CompileOnly,"Interpreting a Compile-only word",0
;	.text $100+err_Forget,"invalid FORGET",0
;	.text $100+err_EmptyName,"attempt to use zero-length string as a name",0
;	.text $100+err_PicStringOverflow,"pictured numeric output string overflow",0
;	.text $100+err_ParsedStringOverflow,"parsed string overflow",0
;	.text $100+err_NameTooLong,"definition name too long",0
;	.text $100+err_WriteToRO,"write to a read-only location",0
	.text $100+err_Unsupported,"unsupported operation",0 ;  (e.g., AT-XY on a too-dumb terminal)
	.text $100+err_ControlMismatch,"control structure mismatch",0
;	.text $100+err_AddrAlignment,"address alignment",0
;	.text $100+err_InvalidNumericArg,"invalid numeric argument",0
;	.text $100+err_ReturnStackImbalance,"return stack imbalance",0
;	.text $100+err_LoopParmUnavailable,"loop parameters unavailable",0
;	.text $100+err_InvalidRecursion,"invalid recursion",0
;	.text $100+err_UserInterrupt,"user interrupt",0
;	.text $100+err_CompilerNesting,"compiler nesting",0
;	.text $100+err_Obsolete,"obsolescent feature",0
;	.text $100+err_NoBody,">BODY used on non-CREATEd definition",0
	.text $100+err_InvalidName,"invalid name",0 ; argument (e.g., TO name)
	.text $100+err_BlockRead,"block read",0
	.text $100+err_BlockWrite,"block write",0
;	.text $100+err_InvalidBlock,"invalid block number",0
;	.text $100+err_InvalidFilePosition,"invalid file position",0
;	.text $100+err_FileIO,"file I/O",0 ; exception
;	.text $100+err_FileMissing,"non-existent file",0
;	.text $100+err_EndOfFile,"unexpected end of file",0
;	.text $100+err_InvalidBase,"invalid BASE",0 ; for floating point conversion
;	.text $100+err_LossOfPrecision,"loss of precision",0
	.text $100+err_FPDivideBy0,"FP divide by zero",0
	.text $100+err_FPOutOfRange,"FP result out of range",0
	.text $100+err_FPStackOverflow,"FP stack overflow",0
	.text $100+err_FPStackUnderflow,"FP stack underflow",0
	.text $100+err_FPInvalidArg,"FP invalid argument",0
;	.text $100+err_CompileWordsDeleted,"compilation word list deleted",0
;	.text $100+err_PostponeInvalid,"invalid POSTPONE",0
;	.text $100+err_SearchOrderOverflow,"search-order overflow",0
;	.text $100+err_SearchOrderUnderflow,"search-order underflow",0
;	.text $100+err_CompileWordlistChanged,"compilation word list changed",0
;	.text $100+err_ControlStackOverflow,"control-flow stack overflow",0
;	.text $100+err_ExceptionStackOverflow,"exception stack overflow",0
	.text $100+err_FPUnderflow,"FP underflow",0
;	.text $100+err_FPFault,"FP unidentified fault",0
;	.text $100+err_Quit,"QUIT",0
;	.text $100+err_ConsoleIO,"exception in sending or receiving a character",0
;	.text $100+err_BracketIf,"[IF], [ELSE], or [THEN]",0 ; exception
	.text $100+err_Allocate,"ALLOCATE",0
	.text $100+err_Free,"FREE",0
;	.text $100+err_Resize,"RESIZE",0
;	.text $100+err_CloseFile,"CLOSE-FILE",0
;	.text $100+err_CreateFile,"CREATE-FILE",0
;	.text $100+err_DeleteFile,"DELETE-FILE",0
;	.text $100+err_FilePosition,"FILE-POSITION",0
;	.text $100+err_FileSize,"FILE-SIZE",0
;	.text $100+err_FileStatus,"FILE-STATUS",0
;	.text $100+err_FlushFile,"FLUSH-FILE",0
;	.text $100+err_OpenFile,"OPEN-FILE",0
;	.text $100+err_ReadFile,"READ-FILE",0
;	.text $100+err_ReadLine,"READ-LINE",0
;	.text $100+err_RenameFile,"RENAME-FILE",0
;	.text $100+err_RepositionFile,"REPOSITION-FILE",0
;	.text $100+err_ResizeFile,"RESIZE-FILE",0
;	.text $100+err_WriteFile,"WRITE-FILE",0
;	.text $100+err_WriteLine,"WRITE-LINE",0
;	.text $100+err_MalformedXChar,"Malformed xchar",0
;	.text $100+err_Substitute,"SUBSTITUTE",0
;	.text $100+err_Replaces,"REPLACES",0

	.text $100+err_Refill,"Refill",0
	.text $100+err_Defer,"Defer not set",0
	.text $100+err_AlreadyInterpreting,"Already interpreting",0
	.text $100+err_AlreadyCompiling,"Already compiling",0

;	.text $100+err_BadSource??,"Illegal SOURCE-ID during REFILL", 0

	.text $100+err_TooManyWordlists,"No wordlists available",0
	.text 0 ; end of list

; END
