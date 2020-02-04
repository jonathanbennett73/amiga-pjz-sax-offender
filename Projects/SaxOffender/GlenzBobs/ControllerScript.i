	IFND _VEC_CONTROLLERSCRIPT
_VEC_CONTROLLERSCRIPT SET 1
	ELSE
_VEC_CONTROLLERSCRIPT SET _VEC_CONTROLLERSCRIPT+1
	ENDC

*****************************************************************************
*****************************************************************************
*****************************************************************************

;	SECTION	IntroFramework_PublicData,DATA	;Public data section for variables
	
*****************************************************************************

; Objects
	INCLUDE "Obj_Glenz24.i"
	INCLUDE "Obj_Cube6.i"

*****************************************************************************

FX_END_FLAG			= 	0
FX_LOAD_FLAG			=	1
FX_PAUSE_FLAG			=	2
FX_MORPH_FLAG			=	3
FX_PALETTE_FLAG			=	4
FX_CLONE_ROTATION_FLAG		=	5
FX_CHANGE_ROT_DELTA_FLAG	=	6
FX_CHANGE_ROT_FLAG		=	7
FX_HORIZSINE_FLAG		=	8
FX_MOVE_FLAG			=	9
FX_SCRIPTJMP_FLAG		=	10
FX_MOVE2_FLAG			=	11
FX_NEXT_PHASE_FLAG		=	12

FX_MOVE	MACRO
		dc.w	FX_MOVE_FLAG
		dc.w	\1		;speed
		dc.w	\2,\3,\4	;new coords
		ENDM

FX_MORPH	MACRO
		dc.w	FX_MORPH_FLAG
		dc.w	\1		;speed
		dc.l	\2		;new pts
		ENDM

FX_PALETTE	MACRO
		dc.w	FX_PALETTE_FLAG
		dc.w	\1		;speed
		dc.l	\2		;new palette
		ENDM

FX_LOAD	MACRO
		dc.w	FX_LOAD_FLAG
		dc.l	\1		;new object info
		ENDM

FX_CLONE_ROTATION	MACRO
		dc.w	FX_CLONE_ROTATION_FLAG
		dc.l	\1		;new object info
		ENDM

FX_PAUSE	MACRO
		dc.w	FX_PAUSE_FLAG
		dc.w	\1		;frames to pause
		ENDM

FX_FLASH	MACRO
		dc.w	FX_FLASH_FLAG
		ENDM

FX_END	MACRO
		dc.w	FX_END_FLAG
		ENDM

FX_CHANGE_ROT	MACRO
		dc.w	FX_CHANGE_ROT_FLAG
		dc.w	\1,\2,\3,\4
		ENDM

FX_CHANGE_ROT_DELTA	MACRO
		dc.w	FX_CHANGE_ROT_DELTA_FLAG
		dc.w	\1,\2,\3
		ENDM

FX_HORIZSINE	MACRO
		dc.w	FX_HORIZSINE_FLAG
		dc.w	\1,\2	;speed1,step1
		ENDM

FX_SCRIPTJMP	MACRO
		dc.w	FX_SCRIPTJMP_FLAG
		dc.l	\1		;new script address
		ENDM

FX_NEXT_PHASE	MACRO
		dc.w	FX_NEXT_PHASE_FLAG
		ENDM

*****************************************************************************

; For glenz we need colors:
; color01 = off white    
; color02 = dark trans
; color05 = pure white    (+col03 and 07 for overlapping vectors)
; color06 = light trans

; For these palettes we use colors 8-15 to indicate what the reflection 
; colours should be.

PAL_B_BDR = $002	;General border/reflection colour
PAL_B_BKG = $113	;BG1, general main background color until the end of routine


; Default palette loaded at startup
PAL_Default:		dc.w	PAL_B_BKG,PAL_B_BKG,PAL_B_BKG,PAL_B_BKG,PAL_B_BKG,PAL_B_BKG,PAL_B_BKG,PAL_B_BKG
			dc.w	PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR

PAL_Default2:		dc.w	PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR
			dc.w	PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR,PAL_B_BDR

PAL_CubeBlue:		dc.w	PAL_B_BKG,$99f,$66f,$99f,$33f,$99f,$66f,$99f
			dc.w	PAL_B_BDR,$55b,$22b,$55b,$00b,$55b,$22b,$55b

PAL_GlenzRed:		dc.w	PAL_B_BKG,$eef,$800,$fff,$0,$fff,$f00,$fff
			dc.w	PAL_B_BDR,$aab,$400,$0,$0,$bbb,$b00,$ddd

PAL_GlenzYellow:	dc.w	PAL_B_BKG,$eef,$880,$fff,$0,$fff,$ff0,$fff
			dc.w	PAL_B_BDR,$aab,$440,$ddd,$0,$bbb,$bb0,$ddd

PAL_GlenzPink:		dc.w	PAL_B_BKG,$eef,$808,$fff,$0,$fff,$f0f,$fff
			dc.w	PAL_B_BDR,$aab,$404,$ddd,$0,$bbb,$b0b,$ddd

PAL_GlenzGreen:		dc.w	PAL_B_BKG,$eef,$080,$fff,$0,$fff,$0f0,$fff
			dc.w	PAL_B_BDR,$aab,$040,$ddd,$0,$bbb,$0b0,$ddd

PAL_GlenzPurple:	dc.w	PAL_B_BKG,$eef,$306,$fff,$0,$fff,$63d,$fff
			dc.w	PAL_B_BDR,$ccd,$104,$ddd,$0,$ddd,$41b,$ddd

PAL_GlenzOrange:	dc.w	PAL_B_BKG,$eef,$840,$fff,$0,$fff,$f80,$fff
			dc.w	PAL_B_BDR,$aab,$400,$ddd,$0,$bbb,$b40,$ddd

PAL_GlenzBlue:		dc.w	PAL_B_BKG,$eef,$149,$fff,$0,$fff,$58d,$fff
			dc.w	PAL_B_BDR,$ccd,$016,$ddd,$0,$ddd,$25a,$ddd

PAL_GlenzCyan:		dc.w	PAL_B_BKG,$eef,$088,$fff,$0,$fff,$0ff,$fff
			dc.w	PAL_B_BDR,$ccd,$044,$ddd,$0,$ddd,$0bb,$ddd



*****************************************************************************

ControllerScript:

BOBDISTNEAR set 1150		;32pix
;BOBDISTNEAR set 500		;48pix

BOBDISTFAR set 2500
;BOBDISTFAR set 1150
BOBMOVESPEED set 16
BOBMOVEDELAY set ((BOBDISTFAR-BOBDISTNEAR)/BOBMOVESPEED)

	; Initial palette, rotation, position, everything must be done in
	;P0_Init because of the way we will precalc.
	FX_PAUSE		35

	FX_MOVE			BOBMOVESPEED,0,0,BOBDISTNEAR
	FX_PALETTE		4,PAL_CubeBlue
	FX_PAUSE		BOBMOVEDELAY

	FX_MOVE			BOBMOVESPEED,0,0,BOBDISTFAR
	FX_PAUSE		BOBMOVEDELAY
	FX_LOAD			Obj_Glenz24_Info
	FX_MORPH		0,Obj_Glenz24_Pts_Cube
	FX_MOVE			0,0,0,BOBDISTFAR	;DO NOT REMOVE, new object
	FX_CLONE_ROTATION	Obj_Cube6_Info
	FX_MOVE			BOBMOVESPEED,0,0,BOBDISTNEAR
	FX_PAUSE		BOBMOVEDELAY
	FX_PALETTE		8,PAL_GlenzBlue
	FX_CHANGE_ROT_DELTA	-16,-3,-5
	FX_MOVE			BOBMOVESPEED,0,0,BOBDISTNEAR
	FX_PAUSE		BOBMOVEDELAY

	; Let build up slightly
	FX_PAUSE		BOBMOVEDELAY
	FX_MORPH		1,Obj_Glenz24_Pts_ClassicSphere
	FX_PALETTE		8,PAL_GlenzPurple
	FX_PAUSE		BOBMOVEDELAY

	;Pyramid
	FX_MOVE			BOBMOVESPEED,0,0,BOBDISTFAR
	FX_PAUSE		BOBMOVEDELAY
	FX_PALETTE		8,PAL_GlenzOrange
	FX_MORPH		1,Obj_Glenz24_Pts_Pyramid2	
	FX_CHANGE_ROT_DELTA	3,-20,5
	FX_MOVE			BOBMOVESPEED,0,0,BOBDISTNEAR
	FX_PAUSE		BOBMOVEDELAY

	; Let build up slightly
	FX_PAUSE		BOBMOVEDELAY
	FX_PALETTE		8,PAL_GlenzPink

	;Final
	FX_MOVE			BOBMOVESPEED,0,0,BOBDISTFAR
	FX_PAUSE		BOBMOVEDELAY
	FX_PALETTE		8,PAL_GlenzOrange
	FX_MORPH		1,Obj_Glenz24_Pts_ClassicSphere	
	FX_CHANGE_ROT_DELTA	-16,3,4
	FX_MOVE			BOBMOVESPEED,0,0,BOBDISTNEAR
	FX_PAUSE		BOBMOVEDELAY

	; Let build up slightly
	FX_PAUSE		BOBMOVEDELAY
	FX_PALETTE		8,PAL_GlenzRed
	FX_PAUSE		BOBMOVEDELAY
	;FX_PAUSE		BOBMOVEDELAY
	FX_PALETTE		8,PAL_GlenzPurple
	FX_PAUSE		BOBMOVEDELAY

	;Exit
	FX_MOVE			BOBMOVESPEED,0,0,BOBDISTFAR
	FX_PAUSE		BOBMOVEDELAY
	FX_PALETTE		4,PAL_Default
	FX_PAUSE		4*15
	FX_PALETTE		1,PAL_Default2
	FX_PAUSE		1*15
	FX_PALETTE		1,PAL_AllBlack
	FX_PAUSE		1*15

	FX_PAUSE		1

	FX_END

