	IFND _FRAMEWORKLIBRARY_I
_FRAMEWORKLIBRARY_I SET 1

*****************************************************************************

; Name			: IntroLibrary.i
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com)
; Description		: Shared library structures.
; Date last edited	: 24/05/2019

; CPU Required		: MC68000 or better
; ChipSet Required	: OCS or better
				
*****************************************************************************
*****************************************************************************

;	RSRESET
;FW_IRQHANDLER_LEV3_FRAMEODD:		rs.w	1
;FW_IRQHANDLER_LEV3_FRAMESYNC:		rs.w	1
;FW_IRQHANDLER_LEV3_CODE:		rs.l	1

LIB_SIN_Q14_1024W_NUMWORDS = 1024
LIB_SIN_Q14_1024W_OFFSET_MASK = LIB_SIN_Q14_1024W_NUMWORDS-1

LIB_SIN_Q15_1024W_NUMWORDS = 1024
LIB_SIN_Q15_1024W_OFFSET_MASK = LIB_SIN_Q15_1024W_NUMWORDS-1

LIB_ARJM7_BUFFER_SIZE = 11312
LIB_PACKFIRE_BUFFER_SIZE = 15980

; KingCon BOB and image structures
	RSRESET
LIB_BOBTABLE_WIDTHINWORDS:	rs.w	1	;Width of the BOB in words (see bob width notes below)
LIB_BOBTABLE_NUMSPRITES:	equ	LIB_BOBTABLE_WIDTHINWORDS
LIB_BOBTABLE_HEIGHT:		rs.w	1	;pixels
LIB_BOBTABLE_WIDTH:		rs.w	1	;pixels
LIB_BOBTABLE_OFFSET:		rs.l	1	;offset into BPL data
LIB_BOBTABLE_ANCHORX:		rs.w	1	;??
LIB_BOBTABLE_ANCHORY:		rs.w	1	;??
LIB_BOBTABLE_SIZEOF:		rs.b	0	;Just an assembler value to indicate size of structure


*****************************************************************************

	ENDC				; _FRAMEWORKLIBRARY_I