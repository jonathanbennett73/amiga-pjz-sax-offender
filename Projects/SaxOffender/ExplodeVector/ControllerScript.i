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
	INCLUDE "Obj_ExplodeCube6.i"

*****************************************************************************

FX_END_FLAG			= 	0
FX_FLASH_FLAG			=	1
FX_LOAD_FLAG			=	2
FX_PAUSE_FLAG			=	3
FX_MORPH_FLAG			=	4
FX_PALETTE_FLAG			=	5
FX_CLONE_ROTATION_FLAG		=	6
FX_CHANGE_ROT_DELTA_FLAG	=	7
FX_CHANGE_ROT_FLAG		=	8
FX_HORIZSINE_FLAG		=	9
FX_MOVE_FLAG			=	10
FX_SCRIPTJMP_FLAG		=	11
FX_ZOOM_FLAG			=	12
FX_MOVE2_FLAG			=	13
FX_NEXT_PHASE_FLAG		=	14
FX_LIGHTSOURCE_FLAG		=	15

FX_MOVE	MACRO
		dc.w	FX_MOVE_FLAG
		dc.w	\1		;speed
		dc.w	\2,\3,\4	;new coords
		ENDM

FX_MOVE2	MACRO
		dc.w	FX_MOVE2_FLAG
		dc.w	\1		;speed
		dc.w	\2,\3,\4	;new coords
		ENDM

FX_ZOOM	MACRO
		dc.w	FX_ZOOM_FLAG
		dc.w	\1		;speed
		dc.w	\2		;new zoom (0-1023)
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

FX_END	MACRO
		dc.w	FX_END_FLAG
		ENDM

FX_NEXT_PHASE	MACRO
		dc.w	FX_NEXT_PHASE_FLAG
		ENDM

FX_CHANGE_ROT	MACRO
		dc.w	FX_CHANGE_ROT_FLAG
		dc.w	\1,\2,\3,\4	;speed,x,y,z
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

FX_LIGHTSOURCE	MACRO
		dc.w	FX_LIGHTSOURCE_FLAG
		dc.w	\1		;frames wait for (local timing)
		ENDM

*****************************************************************************

; For glenz we need colors:
; color01 = off white    
; color02 = dark trans
; color05 = pure white    
; color06 = light trans

; For these palettes we use colors 8-15 to indicate what the reflection 
; colours should be.

PAL_B_TIN = $000	;Transition in background (color00 of previous routine)
PAL_B_TOUT_BKG = $002	;Transition out background
PAL_B_TOUT_FACE = $113	;Transition out main color (face color)
PAL_B_TOUT_BDR = $002	;Transition out border colour
PAL_B_BKG = $002		;BG1, general background color until the end of routine


;For explode vector use the first "dark" color as the top and bottom border color.
PAL_Default:		dc.w	PAL_B_BKG,$0,$0,$0,$0,$0,$0,$0
			dc.w	0,$0,$0,$0,$0,$0,$0,$0

PAL_SolidPurple		dc.w	PAL_B_BKG,$63d,$63d,$0,$0,$63d,$306,$0
			dc.w	0,$0,$0,$0,$0,$0,$0,$0

PAL_ExplodeRed:		dc.w	PAL_B_BKG,COLOR_NEON_RED,COLOR_NEON_RED_LIGHT,COLOR_NEON_RED_DARK,$000,$000,$000,$000
			dc.w	0,$0,$0,$0,$0,$0,$0,$0

PAL_ExplodeOrange:	dc.w	PAL_B_BKG,COLOR_NEON_ORANGE,COLOR_NEON_ORANGE_LIGHT,COLOR_NEON_ORANGE_DARK,$000,$000,$000,$000
			dc.w	0,$0,$0,$0,$0,$0,$0,$0

PAL_ExplodePink:		dc.w	PAL_B_BKG,COLOR_NEON_PINK,COLOR_NEON_PINK_LIGHT,COLOR_NEON_PINK_DARK,$000,$000,$000,$000
			dc.w	0,$0,$0,$0,$0,$0,$0,$0

PAL_ExplodePurple:	dc.w	PAL_B_BKG,COLOR_NEON_PURPLE,COLOR_NEON_PURPLE_LIGHT,COLOR_NEON_PURPLE_DARK,$000,$000,$000,$000
			dc.w	0,$0,$0,$0,$0,$0,$0,$0

PAL_ExplodeYellow:	dc.w	PAL_B_BKG,COLOR_NEON_YELLOW,COLOR_NEON_YELLOW_LIGHT,COLOR_NEON_YELLOW_DARK,$000,$000,$000,$000
			dc.w	0,$0,$0,$0,$0,$0,$0,$0

PAL_ExplodeBlue:	dc.w	PAL_B_BKG,$149,$259,$369,$000,$000,$000,$000
			dc.w	PAL_B_TOUT_BDR,$0,$0,$0,$0,$0,$0,$0

PAL_ExitPurple:		dc.w	PAL_B_TOUT_BKG,PAL_B_TOUT_FACE,$259,$369,$000,$000,$000,$000	;col1 is front face
			dc.w	PAL_B_TOUT_BDR,$0,$0,$0,$0,$0,$0,$0

PAL_PJZ1:		dc.w	PAL_B_TIN,$556,$556,$556,$7aa,$699,$588,$000
			dc.w	$0,$0,$0,$0,$0,$0,$0,$0


*****************************************************************************

ControllerScript:
	;FX_LOAD			Obj_ExplodeCube6_Info
	;FX_PALETTE		0,PAL_ExitPurple
	;FX_CHANGE_ROT_DELTA	1,5,3
	;FX_PAUSE		32767

	;Start with cube at max zoom
	FX_LOAD			Obj_ExplodeCube6_Info
	FX_PALETTE		0,PAL_PJZ1
	FX_CHANGE_ROT_DELTA	1,3,5
	FX_MOVE			0,0,0,-50
	FX_ZOOM			0,ZOOM_MAX
	FX_HORIZSINE		4,0
	FX_LIGHTSOURCE		1

	;FX_SCRIPTJMP		.test

	;Unzoom into center
	FX_PAUSE		1
	FX_ZOOM			1,0
	FX_PALETTE		16,PAL_ExplodePurple
	FX_PAUSE		ZOOM_MAX

	FX_PALETTE		4,PAL_ExplodePink
	FX_PAUSE		4*15


	;Move to top right	
	FX_MOVE2		16,196,128,256
	FX_PALETTE		4,PAL_ExplodeYellow

	FX_PAUSE		100
	FX_PALETTE		8,PAL_ExplodeOrange
	FX_MOVE2		8,0,0,-100
	FX_ZOOM			4,ZOOM_MAX/2

	FX_PAUSE		100
	FX_MOVE2		8,-196,-128,256
	FX_ZOOM			1,0

	FX_PAUSE		100
	FX_PALETTE		8,PAL_ExplodePink
	FX_MOVE2		8,0,0,0
	FX_ZOOM			1,0

	FX_PAUSE		75

	FX_ZOOM			1,ZOOM_MAX
	FX_PAUSE		ZOOM_MAX


.test	FX_PALETTE		8,PAL_ExplodeBlue
	FX_CHANGE_ROT		0,0,0,0		;Reset rotation for consistency
	FX_CHANGE_ROT_DELTA	5,3,1
	FX_LIGHTSOURCE		0

	FX_ZOOM			1,0
	FX_PAUSE		ZOOM_MAX-100

	FX_MOVE2		8,0,0,384
	FX_MORPH		1,Obj_ExplodeCube6_Pts_Huge
	FX_PAUSE		100
	FX_CHANGE_ROT		2,0,0,0
	FX_PAUSE		165

	FX_MOVE2		16,0,0,-16
	FX_PAUSE		40
	FX_PALETTE		3,PAL_ExitPurple
	FX_PAUSE		40
	FX_NEXT_PHASE
	FX_PAUSE		18

	FX_END

