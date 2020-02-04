	IFND _DOT_CONTROLLERSCRIPT
_DOT_CONTROLLERSCRIPT SET 1
	ELSE
_DOT_CONTROLLERSCRIPT SET _DOT_CONTROLLERSCRIPT+1
	ENDC

*****************************************************************************

;	SECTION	IntroFramework_PublicData,DATA	;Public data section for variables
	EVEN

*****************************************************************************

;Anim format (image or palette)
	RSRESET
DOT_ANIM_NUMFRAMES:		rs.w	1	;how many frames in this anim, 1+
DOT_ANIM_CURFRAME:		rs.w	1	;current frame, starts at 0
DOT_ANIM_FRAMEDELAY:		rs.w	1	;how long to pause between each frame
DOT_ANIM_FRAMEDELAYCOUNT:	rs.w	1	;count down
DOT_ANIM_FRAMESECTIONPTR:	rs.l	1	;ptr to first frame section (increment by 4 each frame)
DOT_ANIM_FRAMESTART:		rs.l	1	;ptr to first frame

DOT_BPL_BYTEWIDTH = 6	;48 pixels, Will be aligned 16pixels

DOT_BPL_List:
	dc.l DOT_BPL_Walk_0,DOT_CHK_Walk_0
	dc.l DOT_BPL_Walk_1,DOT_CHK_Walk_1
	dc.l DOT_BPL_Walk_2,DOT_CHK_Walk_2
	dc.l DOT_BPL_Walk_3,DOT_CHK_Walk_3
	dc.l DOT_BPL_Walk_4,DOT_CHK_Walk_4
	dc.l DOT_BPL_Walk_5,DOT_CHK_Walk_5
	dc.l DOT_BPL_Walk_6,DOT_CHK_Walk_6
	dc.l DOT_BPL_Walk_7,DOT_CHK_Walk_7

	dc.l DOT_BPL_Circle1_0,DOT_CHK_Circle1_0
	dc.l DOT_BPL_Circle1_1,DOT_CHK_Circle1_1
	dc.l DOT_BPL_Circle1_2,DOT_CHK_Circle1_2
	dc.l DOT_BPL_Circle1_3,DOT_CHK_Circle1_3
	dc.l DOT_BPL_Circle1_4,DOT_CHK_Circle1_4
	dc.l DOT_BPL_Circle1_5,DOT_CHK_Circle1_5
	dc.l DOT_BPL_Circle1_6,DOT_CHK_Circle1_6
	dc.l DOT_BPL_Circle1_7,DOT_CHK_Circle1_7

	dc.l DOT_BPL_Pulse_0,DOT_CHK_Pulse_0
	dc.l DOT_BPL_Pulse_1,DOT_CHK_Pulse_1
	dc.l DOT_BPL_Pulse_2,DOT_CHK_Pulse_2
	dc.l DOT_BPL_Pulse_3,DOT_CHK_Pulse_3
	dc.l DOT_BPL_Pulse_4,DOT_CHK_Pulse_4
	dc.l DOT_BPL_Pulse_5,DOT_CHK_Pulse_5
	dc.l DOT_BPL_Pulse_6,DOT_CHK_Pulse_6
	dc.l DOT_BPL_Pulse_7,DOT_CHK_Pulse_7	
DOT_BPL_List_End:

DOT_PAL_List:
	dc.l DOT_PAL_Input_Walk_0,DOT_PAL_Output_Walk_0
	dc.l DOT_PAL_Input_Circle1_0,DOT_PAL_Output_Circle1_0
	dc.l DOT_PAL_Input_Pulse_0,DOT_PAL_Output_Pulse_0
	dc.l DOT_PAL_Input_Pulse_1,DOT_PAL_Output_Pulse_1
	dc.l DOT_PAL_Input_Pulse_2,DOT_PAL_Output_Pulse_2
DOT_PAL_List_End:


; A blank frame
DOT_CHK_Blank:
	dcb.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS,0

DOT_PAL_Output_Blank_0: ;output 16 cols
	dcb.w	DOT_OUTPUT_NUMCOLS,$000

DOT_Anim_CHK_Blank_1_8:
	dc.w	1			;num frames
	dc.w	0			;cur frame
	dc.w	8			;frame delay
	dc.w	0			;cur count
	dc.l	0			;frame section ptr
	dc.l	DOT_CHK_Blank

DOT_Anim_PAL_Blank_1_8:
	dc.w	1			;num frames
	dc.w	0			;cur frame
	dc.w	8			;frame delay
	dc.w	0			;cur count
	dc.l	0			;frame section ptr
	dc.l	DOT_PAL_Output_Blank_0

*****************************************************************************

FX_END_FLAG			= 	$ff
FX_PAUSE_FLAG			=	$f0
FX_PALETTE_FLAG			=	$f1
FX_SCRIPTJMP_FLAG		=	$f2
FX_LOAD_ANIM_IMAGE_FLAG		=	$f3
FX_LOAD_ANIM_PAL_FLAG		=	$f4
FX_SET_SINE_XY_SPEED_FLAG	=	$f5

FX_PALETTE	MACRO
		dc.w	FX_PALETTE_FLAG
		dc.w	\1		;speed
		dc.l	\2		;new palette
		ENDM

FX_PAUSE	MACRO
		dc.w	FX_PAUSE_FLAG
		dc.w	\1		;frames to pause
		ENDM

FX_END	MACRO
		dc.w	FX_END_FLAG
		ENDM

FX_SCRIPTJMP	MACRO
		dc.w	FX_SCRIPTJMP_FLAG
		dc.l	\1		;new script address
		ENDM

FX_LOAD_ANIM_IMAGE	MACRO
		dc.w	FX_LOAD_ANIM_IMAGE_FLAG
		dc.l	\1		;new image anim
		ENDM

FX_LOAD_ANIM_PAL	MACRO
		dc.w	FX_LOAD_ANIM_PAL_FLAG
		dc.l	\1		;new pal anim
		ENDM

FX_SET_SINE_XY_SPEED	MACRO
		dc.w	FX_SET_SINE_XY_SPEED_FLAG
		dc.w	\1,\2,\3,\4	;new speed (must be even)
					;x speed, x step, y speed, y step
		ENDM

*****************************************************************************

ControllerScript:
	;FX_PALETTE		0,PAL_AllWhite

	;FX_SET_SINE_XY_SPEED	2,2,-2,-2

	FX_LOAD_ANIM_IMAGE	DOT_Anim_CHK_Pulse_16_8
	FX_PALETTE		2,DOT_PAL_Output_Pulse_0
	FX_PAUSE		(16*8*2)-1	;16 frames, 8 delay, 2 times
	FX_LOAD_ANIM_IMAGE	DOT_Anim_CHK_Blank_1_8
	FX_PAUSE		2

	FX_LOAD_ANIM_IMAGE	DOT_Anim_CHK_Pulse2_16_8
	FX_LOAD_ANIM_PAL	DOT_Anim_PAL_Pulse2_16_8
	FX_PAUSE		(16*8*2)-1	;16 frames, 4 delay, 2 times
	FX_LOAD_ANIM_IMAGE	DOT_Anim_CHK_Blank_1_8
	FX_PALETTE		0,PAL_AllBlack
	FX_PAUSE		2

	FX_LOAD_ANIM_IMAGE	DOT_Anim_CHK_Walk
	FX_PALETTE		4,DOT_PAL_Output_Walk_0
	FX_PAUSE		400
	
	FX_PALETTE		1,PAL_AllBlack
	FX_PAUSE		16

	FX_LOAD_ANIM_IMAGE	DOT_Anim_CHK_Circle1
	FX_PALETTE		1,DOT_PAL_Output_Circle1_0
	FX_PAUSE		150

	FX_PALETTE		1,PAL_AllBlack
	FX_PAUSE		16

	FX_LOAD_ANIM_IMAGE	DOT_Anim_CHK_Blank_1_8
	FX_PAUSE		2

	FX_LOAD_ANIM_IMAGE	DOT_Anim_CHK_Pulse4_8_8
	FX_PALETTE		0,DOT_PAL_Output_Pulse_1
	;FX_PAUSE		(8*8*2)-1	;8 frames, 8 delay, 1 times
	FX_PAUSE		(8*8)-1		;8 frames, 8 delay, 1 times

	FX_PALETTE		0,PAL_AllBlack
	FX_LOAD_ANIM_IMAGE	DOT_Anim_CHK_Blank_1_8
	FX_PAUSE		2		;2 frames to ensure black (double buf)
	FX_END


*****************************************************************************

DOT_BPL_Walk_0:	
	INCBIN "AssetsConverted/Dotflag_Walk_Anim_40x32x2_0_inter.BPL"
DOT_BPL_Walk_1:	
	INCBIN "AssetsConverted/Dotflag_Walk_Anim_40x32x2_1_inter.BPL"
DOT_BPL_Walk_2:	
	INCBIN "AssetsConverted/Dotflag_Walk_Anim_40x32x2_2_inter.BPL"
DOT_BPL_Walk_3:	
	INCBIN "AssetsConverted/Dotflag_Walk_Anim_40x32x2_3_inter.BPL"
DOT_BPL_Walk_4:	
	INCBIN "AssetsConverted/Dotflag_Walk_Anim_40x32x2_4_inter.BPL"
DOT_BPL_Walk_5:	
	INCBIN "AssetsConverted/Dotflag_Walk_Anim_40x32x2_5_inter.BPL"
DOT_BPL_Walk_6:	
	INCBIN "AssetsConverted/Dotflag_Walk_Anim_40x32x2_6_inter.BPL"
DOT_BPL_Walk_7:	
	INCBIN "AssetsConverted/Dotflag_Walk_Anim_40x32x2_7_inter.BPL"
DOT_PAL_Input_Walk_0:
	INCBIN "AssetsConverted/Dotflag_Walk_Anim_40x32x2_0_inter.PAL"

DOT_CHK_Walk_0:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Walk_1:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Walk_2:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Walk_3:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Walk_4:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Walk_5:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Walk_6:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Walk_7:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_PAL_Output_Walk_0: ;output 16 cols
	ds.w	DOT_OUTPUT_NUMCOLS	

DOT_Anim_CHK_Walk:
	dc.w	8			;num frames
	dc.w	0			;cur frame
	dc.w	8			;frame delay
	dc.w	0			;cur count
	dc.l	0			;frame section ptr
	dc.l	DOT_CHK_Walk_0
	dc.l	DOT_CHK_Walk_1
	dc.l	DOT_CHK_Walk_2
	dc.l	DOT_CHK_Walk_3
	dc.l	DOT_CHK_Walk_4
	dc.l	DOT_CHK_Walk_5
	dc.l	DOT_CHK_Walk_6
	dc.l	DOT_CHK_Walk_7

*****************************************************************************

DOT_BPL_Circle1_0:	
	INCBIN "AssetsConverted/Dotflag_Circle1_Anim_40x32x2_0_inter.BPL"
DOT_BPL_Circle1_1:	
	INCBIN "AssetsConverted/Dotflag_Circle1_Anim_40x32x2_1_inter.BPL"
DOT_BPL_Circle1_2:	
	INCBIN "AssetsConverted/Dotflag_Circle1_Anim_40x32x2_2_inter.BPL"
DOT_BPL_Circle1_3:	
	INCBIN "AssetsConverted/Dotflag_Circle1_Anim_40x32x2_3_inter.BPL"
DOT_BPL_Circle1_4:	
	INCBIN "AssetsConverted/Dotflag_Circle1_Anim_40x32x2_4_inter.BPL"
DOT_BPL_Circle1_5:	
	INCBIN "AssetsConverted/Dotflag_Circle1_Anim_40x32x2_5_inter.BPL"
DOT_BPL_Circle1_6:	
	INCBIN "AssetsConverted/Dotflag_Circle1_Anim_40x32x2_6_inter.BPL"
DOT_BPL_Circle1_7:	
	INCBIN "AssetsConverted/Dotflag_Circle1_Anim_40x32x2_7_inter.BPL"
DOT_PAL_Input_Circle1_0:
	INCBIN "AssetsConverted/Dotflag_Circle1_Anim_40x32x2_0_inter.PAL"

DOT_CHK_Circle1_0:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Circle1_1:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Circle1_2:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Circle1_3:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Circle1_4:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Circle1_5:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Circle1_6:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Circle1_7:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_PAL_Output_Circle1_0: ;output 16 cols
	ds.w	DOT_OUTPUT_NUMCOLS	

DOT_Anim_CHK_Circle1:
	dc.w	8			;num frames
	dc.w	0			;cur frame
	dc.w	8			;frame delay
	dc.w	0			;cur count
	dc.l	0			;frame section ptr
	dc.l	DOT_CHK_Circle1_0
	dc.l	DOT_CHK_Circle1_1
	dc.l	DOT_CHK_Circle1_2
	dc.l	DOT_CHK_Circle1_3
	dc.l	DOT_CHK_Circle1_4
	dc.l	DOT_CHK_Circle1_5
	dc.l	DOT_CHK_Circle1_6
	dc.l	DOT_CHK_Circle1_7

*****************************************************************************

DOT_BPL_Pulse_0:	
	INCBIN "AssetsConverted/Dotflag_Pulse_Anim_40x32x2_0_inter.BPL"
DOT_BPL_Pulse_1:	
	INCBIN "AssetsConverted/Dotflag_Pulse_Anim_40x32x2_1_inter.BPL"
DOT_BPL_Pulse_2:	
	INCBIN "AssetsConverted/Dotflag_Pulse_Anim_40x32x2_2_inter.BPL"
DOT_BPL_Pulse_3:	
	INCBIN "AssetsConverted/Dotflag_Pulse_Anim_40x32x2_3_inter.BPL"
DOT_BPL_Pulse_4:	
	INCBIN "AssetsConverted/Dotflag_Pulse_Anim_40x32x2_4_inter.BPL"
DOT_BPL_Pulse_5:	
	INCBIN "AssetsConverted/Dotflag_Pulse_Anim_40x32x2_5_inter.BPL"
DOT_BPL_Pulse_6:	
	INCBIN "AssetsConverted/Dotflag_Pulse_Anim_40x32x2_6_inter.BPL"
DOT_BPL_Pulse_7:	
	INCBIN "AssetsConverted/Dotflag_Pulse_Anim_40x32x2_7_inter.BPL"

DOT_PAL_Input_Pulse_0:
	INCBIN "AssetsConverted/Dotflag_Pulse_Anim_40x32x2_0_inter.PAL"
DOT_PAL_Input_Pulse_1:
	dc.w	$000,$f09,$ee1,$70d,0,0,0,0,0,0,0,0,0,0,0,0	;16
DOT_PAL_Input_Pulse_2:
	dc.w	$000,$8f3,$f90,$f00,0,0,0,0,0,0,0,0,0,0,0,0	;16

DOT_CHK_Pulse_0:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Pulse_1:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Pulse_2:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Pulse_3:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Pulse_4:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Pulse_5:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Pulse_6:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS
DOT_CHK_Pulse_7:
	ds.w	DOT_NUM_X_BYTES*DOT_NUM_Y_DOTS

DOT_PAL_Output_Pulse_0: ;output 16 cols
	ds.w	DOT_OUTPUT_NUMCOLS	
DOT_PAL_Output_Pulse_1: ;output 16 cols
	ds.w	DOT_OUTPUT_NUMCOLS	
DOT_PAL_Output_Pulse_2: ;output 16 cols
	ds.w	DOT_OUTPUT_NUMCOLS

DOT_Anim_CHK_Pulse_16_8:
	dc.w	16			;num frames
	dc.w	0			;cur frame
	dc.w	8			;frame delay
	dc.w	0			;cur count
	dc.l	0			;frame section ptr
	dc.l	DOT_CHK_Pulse_0
	dc.l	DOT_CHK_Pulse_1
	dc.l	DOT_CHK_Pulse_2
	dc.l	DOT_CHK_Pulse_3
	dc.l	DOT_CHK_Pulse_4
	dc.l	DOT_CHK_Pulse_5
	dc.l	DOT_CHK_Pulse_6
	dc.l	DOT_CHK_Pulse_7
	dcb.l	8,DOT_CHK_Blank		;8 blank frames

DOT_Anim_CHK_Pulse2_16_8:
	dc.w	16			;num frames
	dc.w	0			;cur frame
	dc.w	8			;frame delay
	dc.w	0			;cur count
	dc.l	0			;frame section ptr
	dc.l	DOT_CHK_Pulse_0
	dc.l	DOT_CHK_Pulse_1
	dc.l	DOT_CHK_Pulse_2
	dc.l	DOT_CHK_Pulse_3
	dc.l	DOT_CHK_Pulse_4
	dc.l	DOT_CHK_Pulse_5
	dc.l	DOT_CHK_Pulse_6
	dc.l	DOT_CHK_Pulse_7
	dc.l	DOT_CHK_Pulse_0
	dc.l	DOT_CHK_Pulse_1
	dc.l	DOT_CHK_Pulse_2
	dc.l	DOT_CHK_Pulse_3
	dc.l	DOT_CHK_Pulse_4
	dc.l	DOT_CHK_Pulse_5
	dc.l	DOT_CHK_Pulse_6
	dc.l	DOT_CHK_Pulse_7

DOT_Anim_CHK_Pulse3_16_4:
	dc.w	16			;num frames
	dc.w	0			;cur frame
	dc.w	4			;frame delay
	dc.w	0			;cur count
	dc.l	0			;frame section ptr
	dc.l	DOT_CHK_Pulse_0
	dc.l	DOT_CHK_Pulse_1
	dc.l	DOT_CHK_Pulse_2
	dc.l	DOT_CHK_Pulse_3
	dc.l	DOT_CHK_Pulse_4
	dc.l	DOT_CHK_Pulse_5
	dc.l	DOT_CHK_Pulse_6
	dc.l	DOT_CHK_Pulse_7
	dc.l	DOT_CHK_Pulse_0
	dc.l	DOT_CHK_Pulse_1
	dc.l	DOT_CHK_Pulse_2
	dc.l	DOT_CHK_Pulse_3
	dc.l	DOT_CHK_Pulse_4
	dc.l	DOT_CHK_Pulse_5
	dc.l	DOT_CHK_Pulse_6
	dc.l	DOT_CHK_Pulse_7	

DOT_Anim_CHK_Pulse4_8_8:
	dc.w	8			;num frames
	dc.w	0			;cur frame
	dc.w	8			;frame delay
	dc.w	0			;cur count
	dc.l	0			;frame section ptr
	dc.l	DOT_CHK_Pulse_7
	dc.l	DOT_CHK_Pulse_6
	dc.l	DOT_CHK_Pulse_5
	dc.l	DOT_CHK_Pulse_4
	dc.l	DOT_CHK_Pulse_3
	dc.l	DOT_CHK_Pulse_2
	dc.l	DOT_CHK_Pulse_1
	dc.l	DOT_CHK_Pulse_0

DOT_Anim_PAL_Pulse2_16_8:
	dc.w	16			;num frames
	dc.w	0			;cur frame
	dc.w	8			;frame delay
	dc.w	0			;cur count
	dc.l	0			;frame section ptr
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2

DOT_Anim_PAL_Pulse3_16_4:
	dc.w	16			;num frames
	dc.w	0			;cur frame
	dc.w	4			;frame delay
	dc.w	0			;cur count
	dc.l	0			;frame section ptr
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_1
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2
	dc.l	DOT_PAL_Output_Pulse_2	

*****************************************************************************

