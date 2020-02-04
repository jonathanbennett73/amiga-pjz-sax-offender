	IFND _MIA_CONTROLLERSCRIPT
_MIA_CONTROLLERSCRIPT SET 1
	ELSE
_MIA_CONTROLLERSCRIPT SET _MIA_CONTROLLERSCRIPT+1
	ENDC

*****************************************************************************
*****************************************************************************
*****************************************************************************

;	SECTION	IntroFramework_PublicData,DATA	;Public data section for variables
	
*****************************************************************************


*****************************************************************************

FX_END_FLAG		= 	$ff
FX_PAUSE_FLAG		=	$f0
FX_SCRIPTJMP_FLAG	=	$f1
FX_PALETTE_FLAG		=	$f2

FX_PALETTE	MACRO
		dc.w	FX_PALETTE_FLAG
		dc.w	\1		;speed
		dc.l	\2		;new palette
		ENDM

FX_PAUSE	MACRO
		dc.w	FX_PAUSE_FLAG
		dc.w	\1		;frames to pause
		ENDM

FX_END		MACRO
		dc.w	FX_END_FLAG
		ENDM

FX_SCRIPTJMP	MACRO
		dc.w	FX_SCRIPTJMP_FLAG
		dc.l	\1		;new script address
		ENDM


*****************************************************************************


*****************************************************************************

MIA_ControllerScript:
	FX_PAUSE		32767
	
	FX_END

