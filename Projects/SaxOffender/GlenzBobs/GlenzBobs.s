*****************************************************************************

; Name			: Vector.s
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com)
; Description		: Vector routine.
; Date last edited	: 04/02/2020
				
*****************************************************************************

RasterTest set 0	;color00 timing bar, 0=off, 1=overall, 2=show blitwaits

	IFEQ	RasterTest
tmpcolor00 set color00	;use tmpcolor00 in place of color00 everywhere so timing is easier
	ELSE
tmpcolor00 set $1fe	;dummy
	ENDC

*****************************************************************************

	INCLUDE "hardware/custom.i"
	INCLUDE "hardware/intbits.i"	
	INCLUDE "hardware/dmabits.i"

	INCLUDE "../IntroConfig.i"
	INCLUDE	"../Framework/CustomExtra.i"
	INCLUDE "../Framework/CustomMacros.i"
	INCLUDE "../Framework/IntroLibrary.i"

; Additional external symbols 
	xref	FW_CheckUserQuitSignal_A6
	xref	FW_ClearBuffer_BlitCPU_A6
	xref	FW_ClearBuffer_CPU
	xref	FW_CPUClearBuffer
	xref	FW_InitCopperBplPtrs
	xref	FW_InitCopperColsFromPalette
	xref	FW_LineDraw_Scratch
	xref	FW_SetBaseCopperAndLev3Irq_A6
	xref	FW_SetBaseLev3Irq
	xref	FW_SetCopperAndLev3Irq_A6
	xref	FW_SetLev3Irq
	xref	FW_WaitFrame
	xref	FW_WaitRaster_A6
	xref 	FW_VBlankProxy

	xref	LIB_RGB12_Interpolate_Fast_Palette
	xref	LIB_SIN_Q15_1024W_Table
	xref	LIB_COS_Q15_1024W_Table

	xref	FW_Chip_Buffer_1
	xref	FW_Chip_Buffer_1_End


*****************************************************************************

	SECTION	GlenzBobs_PublicCode,CODE	;Code section in Public memory

*****************************************************************************

; Photon:
; Best practice is to start with DDF 38,d0 and DIW 2c81,2cc1 and modify these 
; symmetrically and always in whole 16px steps for compatibility.
; Note DDF start of less than 30 and you start to lose sprites.

*** Changeable Parameters For Display ***

*** Display Window ***
DIW_V			=	$2c	;Hardware Vstart ($2c normal, $24 overscan)
DIW_H			=	$81	;Hardware Hstart ($81 normal, $71 overscan)
DIW_WIDTH		=	320	;Pixels		 (multiple of 16, 320 normal, 352 overscan)
DIW_HEIGHT		=	256	;Lines		 (256 normal PAL, 272 overscan)

*** Data Fetch ***
MEMORYFETCHMODE		=	0		;0 (OCS),1 or 3 
DDF_H			=	$81		;Hardware Hstart ($81 normal, $71 overscan)
DDF_WIDTH		=	320		;Pixels		 (320 normal pal, 352 overscan)
DDF_BYTEWIDTH		=	DDF_WIDTH/8	;Bytes
DDF_WORDWIDTH		=	DDF_BYTEWIDTH/2	;Words

;Extra DDF for horizontal scroll at left

*****************************************************************************

*** Non-changable parameters for display - Automatically calculated from above ***
DIW_VSTART		=	(DIW_V&$ff)<<8
DIW_VSTOP		=	((DIW_V+DIW_HEIGHT)&$ff)<<8
DIW_HSTART		=	DIW_H&$ff
DIW_HSTOP		=	(DIW_HSTART+DIW_WIDTH)&$ff
DIW_START		=	DIW_VSTART!DIW_HSTART
DIW_STOP		= 	DIW_VSTOP!DIW_HSTOP

	IFEQ	MEMORYFETCHMODE
DDF_INCREMENT	=	1
	ELSE
DDF_INCREMENT	=	(MEMORYFETCHMODE+1)&$fffe
	ENDC	

DDF_START		=	(DDF_H/2)-8
DDF_STOP		=	DDF_START+((DDF_WORDWIDTH-DDF_INCREMENT)*8)

*****************************************************************************

*** Screen Definitions ***

; Relection size, this will be added at top and bottom (two instances)
REFL_HEIGHT		=	32

;The visible screen area and back buffer. We use a full height screen as 
;we will draw bobs all the way into the reflection and easier to calculate.
BPL_BUF_WIDTH		=	320
BPL_BUF_BYTEWIDTH	=	BPL_BUF_WIDTH/8
BPL_BUF_WORDWIDTH	=	BPL_BUF_BYTEWIDTH/2
BPL_BUF_HEIGHT		=	256			;Rule of thirds
BPL_BUF_NUMPLANES	=	3			;3bpl/8 cols
BPL_BUF_NUMCOLS 	= 	(1<<BPL_BUF_NUMPLANES)
BPL_BUF_SIZE		=	BPL_BUF_BYTEWIDTH*BPL_BUF_HEIGHT
BPL_BUF_TOTALSIZE	=	BPL_BUF_SIZE*BPL_BUF_NUMPLANES

; We only need to concern ourselves with clearing/showing the main area REFL_HEIGHT in
; Szie of main area (for clearing) is REFL_HEIGHT*2 smaller than buffer
BPL_BUF_MAIN_OFFSET	=	REFL_HEIGHT*BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES
BPL_BUF_MAIN_HEIGHT	=	BPL_BUF_HEIGHT-(REFL_HEIGHT*2)
BPL_BUF_MAIN_SIZE	=	BPL_BUF_BYTEWIDTH*BPL_BUF_MAIN_HEIGHT
BPL_BUF_MAIN_TOTALSIZE	=	BPL_BUF_MAIN_SIZE*BPL_BUF_NUMPLANES

;BPL is normal interleaved
BPL_BPLMOD		=	(BPL_BUF_BYTEWIDTH-DDF_BYTEWIDTH)+(BPL_BUF_BYTEWIDTH*(BPL_BUF_NUMPLANES-1))

; For perfect reflection (interleaved):
; Modulo is added at DDFStop so need to set before then.
; Before DDFStart at last line of display set modulo to repeat that line
; -P0_DDF_BYTEWIDTH
; Then, before DDFStart on first line of reflection
; (-(BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES))-P0_DDF_BYTEWIDTH
BPL_BPLMOD_REPTLINE	=	-DDF_BYTEWIDTH
BPL_BPLMOD_REPTPREVLINE	=	(-(BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES))-DDF_BYTEWIDTH
BPL_BPLMOD_REPT2LINE	=	BPL_BPLMOD_REPTPREVLINE-(BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES)

SCANLINE_EOF		=	DIW_V+DIW_HEIGHT	; Safe to starting clearing after this scanline

; Vertical scanline to start the reflection
DIW_V_REFL_TOP		=	DIW_V
DIW_V_MAIN		=	DIW_V+REFL_HEIGHT
DIW_V_REFL_BOT		=	(DIW_V+BPL_BUF_HEIGHT)-REFL_HEIGHT

*****************************************************************************

VEC_CLIPPING		=	0	; Slighty quicker with no clipping
VEC_CLIPCHECK		=	0	; Will hard crash if clipping
VEC_DRAW_BOUNDING	=	0	; Draw bounding box for fill/clearing

VEC_BOB_WIDTH		=	32
VEC_BOB_BYTEWIDTH	=	VEC_BOB_WIDTH/8		;Bytes
VEC_BOB_WORDWIDTH	=	VEC_BOB_BYTEWIDTH/2	;Words
VEC_BOB_HEIGHT		=	32
VEC_BOB_NUMPLANES	=	3
VEC_BOB_SIZE		=	VEC_BOB_BYTEWIDTH*VEC_BOB_HEIGHT
VEC_BOB_TOTALSIZE	=	VEC_BOB_SIZE*VEC_BOB_NUMPLANES ; Each frame is 4*32*3 = 384 bytes

;256 Frames = 256x384 = 98304, 96KB
;Front+back buffers = 320x224 = 52KB
;=140KB
BOB_NUMBOBS		=	18
BOB_NUMFRAMES		=	256	;Power of two for easy masking 16,32,64,128
BOB_FRAME_SPACING	=	BOB_NUMFRAMES/BOB_NUMBOBS
BOB_FRAMEMASK		=	BOB_NUMFRAMES-1

; Number of frames before all BOBs are ready to show
BOB_NUMFRAMES_PRECALC 	=	BOB_NUMFRAMES-BOB_FRAME_SPACING

*****************************************************************************

PAL_NUMCOLS_MAIN	= 8		; number of main colour entries in our palettes
PAL_NUMCOLS_DARK	= 8		; number of dark/refl cols
PAL_NUMCOLS_ALL		= (PAL_NUMCOLS_MAIN+PAL_NUMCOLS_DARK)

*****************************************************************************

VEC_XOFFSET		=	VEC_BOB_WIDTH/2
VEC_YOFFSET		=	VEC_BOB_HEIGHT/2
VEC_ZOFFSET		=	256

VEC_LINEDRAW3_CLIPMINX	=	0
VEC_LINEDRAW3_CLIPMAXX	=	VEC_BOB_WIDTH-1
VEC_LINEDRAW3_CLIPMINY	=	0
VEC_LINEDRAW3_CLIPMAXY	=	VEC_BOB_HEIGHT-1


*****************************************************************************
* Start the effect (usually used for setting up the copper)
* This is called each time the effect is started
* IN:		a0(script ptr)
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

	IFEQ _INTROWRAPPER
	xdef	SubPartStart
SubPartStart:	
	ENDC

	xdef	VECBOB_Start
VECBOB_Start:
	movem.l	d2-d7/a2-a6,-(sp)	;save
	lea	ControllerScript(pc),a0

	lea	_custom,a6
	lea	Controller_Info(pc),a5

	;Save script pointer
	move.l	a0,CTRL_SCRIPT_PTR(a5)

	; Run intro startup precalc if not already done (only runs once)
	bsr.s	SubPartPreCalc_IntroStart	;T:None

	;Start the P0 routine irq and copper
	bsr.s	P0_Init			;I:a5-a6, T:d0-d1/a0-a1

	; Continue with main loop outside the irq
	bsr	P0_MainLoop		;I:a5-a6, T:d0-d1/a0-a1

	;May want to do various things here. Leave copperlist active, but use
	;default lev3 irq is common so that easy to transition.
	jsr	FW_SetBaseLev3Irq

	movem.l	(sp)+,d2-d7/a2-a6	;restore
	rts


*****************************************************************************
* To be called during INTRO startup to do any permanent precalc required.
* i.e. the buffers used are unique for this effect.
* CPU ONLY - No Blitter
* IN:		
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

	;Only export one symbol depending if in demo or standalone mode
	IFEQ _INTROWRAPPER
	xdef	SubPartPreCalc_IntroStart
	ELSE
	xdef	VECBOB_PreCalc_IntroStart
	ENDC
	
SubPartPreCalc_IntroStart:	
VECBOB_PreCalc_IntroStart:
	tst.w	Controller_Info+CTRL_PRECALC_INTROSTART_DONE
	bne.s	.exit			;already done

	movem.l	d2-d7/a2-a6,-(sp)	;save

	;Start

	; Multiply sine values by screen widths
	bsr	Calc_PreMult_SineForScreen

	; Simple mult/height tables
	bsr	Calc_PreMult_ScreenHeight
	bsr	Calc_PreMult_BOBHeight

	;End

	move.w	#1,Controller_Info+CTRL_PRECALC_INTROSTART_DONE

	movem.l	(sp)+,d2-d7/a2-a6	;restore
.exit:
	rts


*****************************************************************************
* Start the effect. Any precalc/slow ops should be done in the precalc code.
* Will also call PreCalc routes if not already been done (for safety)
* IN:		a6, _custom
*		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

P0_Init:
	movem.l	d2-d7/a2-a4,-(sp)

	;Reset anything that needs resetting
	moveq	#0,d0
	move.w	d0,CTRL_PHASE(a5)
	move.w	d0,CTRL_FINISHED(a5)
	move.w	d0,CTRL_PAUSE_COUNTER(a5)
	move.w	d0,CTRL_FRAME_COUNT(a5)
	move.w	d0,CTRL_PALETTE_ACTIVE(a5)
	move.w	d0,CTRL_ROT_CHANGE_ACTIVE(a5)
	move.w	d0,CTRL_MOVE_ACTIVE(a5)
	move.w	#1,CTRL_FRAMES_MIN(a5)

	;Reset all frame lookup to have invalid fills
	lea	BOB_FrameLookup(pc),a2
	move.w	#BOB_NUMFRAMES-1,d0
.frames:
	clr.l	BOB_FRAME_FILL_BOB_PTR(a2)
	lea	BOB_FRAME_SIZEOF(a2),a2
	dbf	d0,.frames

	;We have to make sure all vector stuff like palette/pos/rot is 
	;correct before we start due to all the precalc we do.
	; Load default palette
	;lea	PAL_AllBlack(pc),a0
	lea	PAL_Default(pc),a0
	moveq	#0,d0
	bsr	Controller_FX_Palette	;a1,a2,d0
	
	; Preload first object
	lea	Obj_Cube6_Info(pc),a0
	bsr	Controller_FX_Load		;a1/a2

	; Set initial rotation to 3,5,9
	move.w	#3,d0
	move.w	#5,d1
	move.w	#9,d2
	bsr	Controller_FX_Change_Rot_Delta

	; Set initial position
	move.w	#0,d0
	move.w	#0,d1
	move.w	#0,d2
	move.w	#BOBDISTFAR,d3
	bsr	Controller_FX_Move

	; Clear all screen buffers
	bsr	Clear_ScreenBuffers_A6		;I:a6

	;load current palette (default) into work CL
	bsr	P0_CL_InitPtrs			;I:a6, T:d0-d2/a0-a1
	bsr	Write_Palette_To_Copper		;I:a5, T:a0-a2

	;Swap buffers and load copper for next frame
	bsr	ScreenBufferSwap	;d0-d1/a0

	;Initialise our new copper and irq
	move.l	CL_Phys_Ptr(pc),a0
	lea	P0_Lev3Irq(pc),a1
	jsr	FW_SetCopperAndLev3Irq_A6

	movem.l	(sp)+,d2-d7/a2-a4
	rts


*****************************************************************************
* Runs the effect.
* IN:		a6, _custom
*		a5, Controller_Info	
* OUT:		
* TRASHED:	d0-d1/a0-a1
*
*****************************************************************************

P0_MainLoop:
	movem.l	d2-d7/a2-a4,-(sp)
	clr.w	.count				;Reset precalc count

.Mainloop1:
	tst.w	CTRL_FINISHED(a5)		;Check if script ended itself
	bne.s	.exit

	lea	Controller_Info(pc),a5		;All routines must preserve
	bsr	BOB_Update_FrameLookup		;T:d0/a0

	bsr	Calc_Rotation_Matrix_For_Object	;I:a5, T:d0-d7/a0-a2
	bsr 	RotateAndPerspective		;I:a5, T:d0-d7/a0-a4
	bsr	Calc_Visible_Faces		;I:a5, T:d0-d7/a0-a4
	; Based on current rotated+pers pts, used for fills/clrs
	; Note must be before and in same frame (screen address) as DrawObject/Fill
	bsr	Calc_BOB_Fill_Bounding		;T:d0-d7/a0-a2
	bsr	Clear_BOB_Buffer_CPU		;T:d0-d7/a0-a4
	;bsr	Clear_BOB_Bounded_A6
	bsr	DrawObject			;I:a6, T:d0-d7/a0-a4
	;bsr	Fill_BOB			;I:a6, T:a0
	bsr	Fill_BOB_Bounded_A6		;I:a6, T:d0-d/a0


	; If frame counter is >= num bob frames then move to next phase
	addq.w	#1,.count
	move.w	.count(pc),d0
	cmp.w	#BOB_NUMFRAMES_PRECALC,d0
	blt.s	.stillprecalc
	lea	P1_Lev3Irq(pc),a0
	jsr	FW_SetLev3Irq			;Switch to main routine
	bsr	P1_MainLoop
	bra.s	.exit

.stillprecalc:
	jsr	FW_CheckUserQuitSignal_A6	;I:a6, O:d0, T:d0
	beq.s	.Mainloop1

	;If user initiated section/intro exit then load default CL/Irq
	;So that sprites/screen is blanked and next section will not have weird fx
	jsr	FW_SetBaseCopperAndLev3Irq_A6

.exit
	movem.l	(sp)+,d2-d7/a2-a4
	rts

.count:	
	dc.w	0


*****************************************************************************
; P0 is just reading script commands and doing colors. During this time we
; are using the outside-VBL to pre-calculate BOBs as quickly as possible.
; The controller script is set to pause long enough on an A500 before 
; switching to the main P1 routine.
P0_Lev3Irq:				;Blank template VERTB/COP interrupt
	TIMERON
	movem.l	d0-d7/a0-a6,-(sp)

	lea	_custom,a6
	lea	Controller_Info(pc),a5		;All routines must preserve

	jsr	FW_VBlankProxy			;TRASHES ALL, Update framecounter, play music

	; Read new script lines and perform
	addq.w	#1,CTRL_FRAME_COUNT(a5)		;Update local frame count
	bsr	Controller_ReadCommands		;I:a5
	bsr	Controller_Perform		;I:a5

	bsr	Write_Palette_To_Copper		;T:d0/a0-a2

	bsr	ScreenBufferSwap		;T:d0-d1/a0

	;Reset interrupt
	IFNE FW_IRQ_TYPE_COPPER
		moveq	#INTF_COPER,d0
	ELSE
		moveq	#INTF_VERTB,d0
	ENDC
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)			;A4000 compat

	movem.l	(sp)+,d0-d7/a0-a6
	TIMEROFF
	rte


*****************************************************************************
* Runs the effect.
* IN:		a6, _custom
*		a5, Controller_Info	
* OUT:		
* TRASHED:	d0-d1/a0-a1
*
*****************************************************************************

P1_MainLoop:
	movem.l	d2-d7/a2-a4,-(sp)

.Mainloop1:
	tst.w	CTRL_FINISHED(a5)	;Check if script ended itself
	bne.s	.exit

	jsr	FW_CheckUserQuitSignal_A6	;I:a6, O:d0, T:d0
	beq.s	.Mainloop1

	;If user initiated section/intro exit then load default CL/Irq
	;So that sprites/screen is blanked and next section will not have weird fx
	jsr	FW_SetBaseCopperAndLev3Irq_A6

.exit
	movem.l	(sp)+,d2-d7/a2-a4
	rts


*****************************************************************************

P1_Lev3Irq:				;Blank template VERTB/COP interrupt
	TIMERON
	movem.l	d0-d7/a0-a6,-(sp)

	lea	_custom,a6
	lea	Controller_Info(pc),a5		;All routines must preserve

	;Use full blitter clear as Pretracker uses up 99% of CPU here
	;bsr	Clear_WorkBuffer_BlitterCPU_A6	;entire screen
	bsr	Clear_WorkBuffer_Blitter_A6	;entire screen
	jsr	FW_VBlankProxy			;T:d0-d7/a0-a4

	bsr	BOB_Update_FrameLookup		;T:d0/a0
	bsr	BOB_Update_BOBs			;T:d1/d3-d7/a0-a1

	; Read new script lines and perform
	addq.w	#1,CTRL_FRAME_COUNT(a5)	;Update local frame count
	bsr	Controller_ReadCommands		;I:a5
	bsr	Controller_Perform		;I:a5

	bsr	Calc_Rotation_Matrix_For_Object	;I:a5, T:d0-d7/a0-a2
	bsr 	RotateAndPerspective		;I:a5, T:d0-d7/a0-a4
	bsr	Calc_Visible_Faces		;I:a5, T:d0-d7/a0-a4
	; Based on current rotated+pers pts, used for fills/clrs
	; Note must be before and in same frame (screen address) as DrawObject/Fill
	bsr	Calc_BOB_Fill_Bounding		;T:d0-d7/a0-a2

	IFNE	VEC_CLIPCHECK
		bsr	ClipCheck	;Causes hang to show clip issues
	ENDC

	bsr	Clear_BOB_Buffer_CPU	;T:d0-d7/a0-a4
	bsr	Write_Palette_To_Copper	;T:d0/a0-a2

	; End of clearscreen blitter zone, tune so as much cpu as possible 

	;bsr	Clear_BOB_Bounded_A6
	bsr	DrawObject		;I:a6, T:d0-d7/a0-a4
	;bsr	Fill_BOB		;I:a6, T:a0
	bsr	Fill_BOB_Bounded_A6	;I:a6, T:d0-d3/a0

.finishdrawing:
	bsr	BOB_Draw_BOBs		;d0-d3/a0-a4

	bsr	ScreenBufferSwap	;T:d0-d1/a0

	;Reset interrupt
	IFNE FW_IRQ_TYPE_COPPER
		moveq	#INTF_COPER,d0
	ELSE
		moveq	#INTF_VERTB,d0
	ENDC
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)			;A4000 compat

	movem.l	(sp)+,d0-d7/a0-a6
	WAITBLIT_NASTY			;Blitter nasty on while we finish off any blits
	TIMEROFF
	rte


*****************************************************************************
* Inits ptrs in the copper list.
* IN:		
* OUT:		
* TRASHED:	d0-d2/a0-a1
*****************************************************************************

P0_CL_InitPtrs:

	;Setup items the same in front/back copper lists

	; Copper list buffers - copy screen list into 2nd buffer for doublebuffering
	lea	P0_CL_Phys,a0	;source
	lea	P0_CL_Log1,a1		;dest
	move.w	#(P0_CL_SIZE/2)-1,d0	;size in words
.copy:
	move.w	(a0)+,(a1)+
	dbf	d0,.copy

	;
	;Front buffer copper BPL pointers
	;
	lea	P0_CL_Phys,a1
	move.l	a1,CL_Phys_Ptr
	move.l	#BPL_Phys,BPL_Phys_Ptr

	;Main area, starts REFL_HEIGHT into the buffer
	lea	P0_CL_BPL_OFFSET(a1),a0	;copper bpl pointer block
	moveq	#BPL_BUF_NUMPLANES,d0
	move.l	#BPL_Phys+(REFL_HEIGHT*BPL_BUF_NUMPLANES*BPL_BUF_BYTEWIDTH),d1
	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;I:d0-d2/a0, T:d0-d1/a0

	;Reflection area starts REFL_HEIGHT*2 into the area
	lea	P0_CL_BPL_REFL_TOP_OFFSET(a1),a0	;copper bpl pointer block
	moveq	#BPL_BUF_NUMPLANES,d0
	move.l	#BPL_Phys+(REFL_HEIGHT*2*BPL_BUF_NUMPLANES*BPL_BUF_BYTEWIDTH),d1
	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;I:d0-d2/a0, T:d0-d1/a0


	;
	;Back buffer copper BPL pointers
	;
	lea	P0_CL_Log1,a1
	move.l	a1,CL_Log1_Ptr
	move.l	#BPL_Log1,BPL_Log1_Ptr

	;Main area
	lea	P0_CL_BPL_OFFSET(a1),a0	;copper bpl pointer block
	moveq	#BPL_BUF_NUMPLANES,d0
	move.l	#BPL_Log1+(REFL_HEIGHT*BPL_BUF_NUMPLANES*BPL_BUF_BYTEWIDTH),d1
	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	;Reflection area starts REFL_HEIGHT*2 into the area
	lea	P0_CL_BPL_REFL_TOP_OFFSET(a1),a0	;copper bpl pointer block
	moveq	#BPL_BUF_NUMPLANES,d0
	move.l	#BPL_Log1+(REFL_HEIGHT*2*BPL_BUF_NUMPLANES*BPL_BUF_BYTEWIDTH),d1
	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;I:d0-d2/a0, T:d0-d1/a0


	rts


*****************************************************************************

; Screen buffer and copper ptrs, must stay in this order as accessed as a group
BPL_Phys_Ptr:		dc.l	0
BPL_Log1_Ptr:		dc.l	0
CL_Phys_Ptr:		dc.l	0
CL_Log1_Ptr:		dc.l	0


*****************************************************************************
* Swaps the copperlist, screen and clr/fill pointers and activates the CL
* for the next frame. 
* NOTE: Call before vblank so new copper takes effect next frame.
* IN:		a6, _custom
* OUT:		
* TRASHED:	d0-d1/a0
*****************************************************************************

ScreenBufferSwap:

	; Swap screen buffer details
	lea	BPL_Phys_Ptr(pc),a0
	movem.l	(a0),d0-d1
	move.l	d1,(a0)+
	move.l	d0,(a0)

	; Swap copper buffer details
	lea	CL_Phys_Ptr(pc),a0
	movem.l	(a0),d0-d1
	move.l	d1,(a0)+
	move.l	d0,(a0)

	; and activate
	move.l 	d1,cop1lch(a6)		; Active NEXT frame

	rts


*****************************************************************************
* Creates table of screen height * screenwidth bytes (non-interleaved) or
* screen height * (screenwidth bytes * num bitplanes) (interleaved)
* Use for one-time setup
* IN:		
* OUT:		
* TRASHED:	d0-d3/a0
*****************************************************************************

Calc_PreMult_ScreenHeight:

	;interleaved
	lea	Mult_SCR_Height_ByteWidth_NumPlanes(pc),a0
	move.w	#BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES,d1

.calc	;d1 = byte width per line
	moveq	#0,d0
	move.w	#BPL_BUF_HEIGHT-1,d3
.loop
	move.w	d0,d2
	mulu	d1,d2
	move.w	d2,(a0)+
	addq.w	#1,d0
	dbf	d3,.loop

	rts

Calc_PreMult_BOBHeight:

	;interleaved
	lea	Mult_BOB_Height_ByteWidth_NumPlanes(pc),a0
	move.w	#VEC_BOB_BYTEWIDTH*VEC_BOB_NUMPLANES,d1

.calc	;d1 = byte width per line
	moveq	#0,d0
	move.w	#VEC_BOB_HEIGHT-1,d7
.loop
	move.w	d0,d2
	mulu	d1,d2
	move.w	d2,(a0)+
	addq.w	#1,d0
	dbf	d7,.loop

	rts

*****************************************************************************
* Takes the Y sine list and premultiples the values by the screen width
* IN:		
* OUT:		
* TRASHED:	
*****************************************************************************

Calc_PreMult_SineForScreen:	

	lea	BOB_Sine_Y_PreMult(pc),a1
	move.w	#BOB_SINE_NUMENTRIES-1,d7
.loop:
	move.w	(a1),d0
	mulu.w 	#BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES,d0
	move.w 	d0,(a1)+
	dbf	d7,.loop

	rts


*****************************************************************************
* Clears the current BOB buffer prior to drawing.
* IN:		a6, _custom
* OUT:		
* TRASHED:	d0/a0
*****************************************************************************

	IFNE	0
Clear_BOB_Buffer_Blit_A6:

	; Blitter
	;lea  BPL_BOBS_Log,a0		; memory to clear
	move.l	BOB_Frame_Screen_Ptr(pc),a0	;memory to clear

	WAITBLIT_NASTY_A6
	move.l	#$01000000,bltcon0(a6)
	move.l	a0,bltdpth(a6)
	move.w	#0,bltdmod(a6)
	move.w	#((VEC_BOB_HEIGHT*VEC_BOB_NUMPLANES)*64)+VEC_BOB_WORDWIDTH,bltsize(a6)
	; Max height = 1024, made wordwidth = 64
	
	rts
	ENDC


*****************************************************************************
* Clears the current BOB buffer prior to drawing with the CPU so that
* it can be done during blitter clear screen.
* IN:		a6, custom
* OUT:		
* TRASHED:	d0-d7/a0-a4
*****************************************************************************

Clear_BOB_Buffer_CPU:
	move.l	BOB_Frame_Screen_Ptr(pc),a0	;memory to clear
	move.l	#VEC_BOB_TOTALSIZE/2,d0		;size in words
	jsr	FW_ClearBuffer_CPU		;I:d0/a0, T:d0-d7/a0-a4

	rts


*****************************************************************************
* Clears the entire work buffer screen. Every frame.
* IN:		a6, _custom
* OUT:		
* TRASHED:	d0-d7/a0-a4
*****************************************************************************

	IFNE	0
Clear_WorkBuffer_BlitterCPU_A6:

	; We don't need to clear the first REFL_HEIGHT lines
	; Size is minus REFL_HEIGHT*2
	move.l  BPL_Log1_Ptr(pc),a0		; memory to clear
	lea	BPL_BUF_MAIN_OFFSET(a0),a0
	move.w	#BPL_BUF_MAIN_TOTALSIZE/2,d0	;size in words
	move.w	#64000,d1
	jsr	FW_ClearBuffer_BlitCPU_A6	;I:d0-d1/a0, T:d0-d7/a0-a4
	rts
	ENDC


*****************************************************************************
* Clears the entire work buffer screen. Every frame.
* Clears with the blitter only.
* IN:		A6(_custom/$dff000)
* OUT:		
* TRASHED:	d0-d7/a0-a5
*****************************************************************************

Clear_WorkBuffer_Blitter_A6:
	
	; We don't need to clear the first REFL_HEIGHT lines
	; Size is minus REFL_HEIGHT*2

	move.l  BPL_Log1_Ptr(pc),a0
	lea	BPL_BUF_MAIN_OFFSET(a0),a0
	WAITBLIT_NASTY_A6
	move.l	#$01000000,bltcon0(a6)
	move.l	a0,bltdpth(a6)
	move.w	#0,bltdmod(a6)
	move.w	#((BPL_BUF_MAIN_HEIGHT*BPL_BUF_NUMPLANES)*64)+BPL_BUF_WORDWIDTH,bltsize(a6)
	; Max height = 1024, made wordwidth = 64

	rts


*****************************************************************************
* Clears entire front and back buffer. Done once at startup to ensure clean
* environment.
* IN:		a6, _custom
* OUT:		
* TRASHED:	None
*****************************************************************************

Clear_ScreenBuffers_A6:
	WAITBLIT_A6
	move.l	#$01000000,bltcon0(a6)
	move.w	#0,bltdmod(a6)
	move.l	#BPL_Phys,bltdpth(a6)
	move.w	#((BPL_BUF_HEIGHT*BPL_BUF_NUMPLANES)*64)+BPL_BUF_WORDWIDTH,bltsize(a6)

	WAITBLIT_A6
	move.l	#BPL_Log1,bltdpth(a6)
	move.w	#((BPL_BUF_HEIGHT*BPL_BUF_NUMPLANES)*64)+BPL_BUF_WORDWIDTH,bltsize(a6)

	rts


*****************************************************************************
* Fills a bounded area of the screen.
* IN:		a6, _custom
* OUT:		
* TRASHED:	a0
*****************************************************************************

	IFNE	0
Fill_BOB:
	; our Blitter source has to be the last word on the last bitplane (desending mode)
	; For line 100
	; 100 * (ScreenByteWidth * NumBitplanes)
	; + (ScreenByteWidth * (NumBitplanes-1))
	; + rightx in bytes

	;move.l	#BPL_BOBS_Log,a0
	move.l	BOB_Frame_Screen_Ptr(pc),a0

	lea	(VEC_BOB_HEIGHT*VEC_BOB_BYTEWIDTH*VEC_BOB_NUMPLANES)-2(a0),a0	;last word

	WAITBLIT_NASTY_A6
	move.l	#$09f00012,bltcon0(a6)	; Descending, exclusive fill mode
	move.l	#-1,bltafwm(a6)
	move.w	#0,bltamod(a6)
	move.w	#0,bltdmod(a6)
	move.l	a0,bltapth(a6)
	move.l	a0,bltdpth(a6)
	move.w	#((VEC_BOB_HEIGHT*VEC_BOB_NUMPLANES)*64)+VEC_BOB_WORDWIDTH,bltsize(a6)
.exit:
	rts
	ENDC


*****************************************************************************
* Fills a bounded area of the screen.
* IN:		a6, _custom
* OUT:		
* TRASHED:	d0-d3/a0
*****************************************************************************

Fill_BOB_Bounded_A6:
	; We only fill the box that we've drawn in.
	move.l	BOB_FrameLookup_Ptr(pc),a0

	; Get start address of bottom right corner (fulls are in descending mode so match with clear)
	; If the value is 0 then no valid fill was done, so skip the clear as well - happens at
	; start of routine
;	move.l	BOB_FRAME_FILL_BPLPTR(a0),d0	;current screen to fill
	move.l	BOB_FRAME_FILL_BOB_PTR(a0),d0	;offset into screen
	beq.s	.exit

	move.w	BOB_FRAME_FILL_BLTSIZE(a0),d2	;completed bltsize in d4
	move.w	BOB_FRAME_FILL_BYTEMODULO(a0),d1

	lea 	bltapth(a6),a0
	moveq	#-1,d3
	WAITBLIT_NASTY_A6
	move.l	#$09f00012,bltcon0(a6)	; Descending, exclusive fill mode
	move.l	d3,bltafwm(a6)
	move.w	d1,bltamod(a6)
	move.w	d1,bltdmod(a6)
	move.l	d0,(a0)+	; bltapth
	move.l	d0,(a0)+	; bltdpth
	move.w	d2,(a0)		; bltsize
.exit:
	rts


*****************************************************************************
* Clears the work buffer screen. Only clears the bounded area that was drawn in.
* IN:		a6, _custom
* OUT:		
* TRASHED:	d0-d2/a0
*****************************************************************************

	IFNE	0
Clear_BOB_Bounded_A6:
	
	; We only fill the box that we've drawn in. 
	move.l	BOB_FrameLookup_Ptr(pc),a0

	; Get start address of bottom right corner (fills are in descending mode so match with clear)
	; If the value is 0 then no valid fill was done, so skip the clear as well - happens at
	; start of routine
	move.l	BOB_FRAME_FILL_BOB_PTR(a0),d0
	beq.s	.exit

	move.w	BOB_FRAME_FILL_BLTSIZE(a0),d2	;completed bltsize in d4
	move.w	BOB_FRAME_FILL_BYTEMODULO(a0),d1

	lea 	bltdpth(a6),a0
	WAITBLIT_NASTY_A6
	move.l	#$01000002,bltcon0(a6)	;desending to match fill routine calculations
	move.w	d1,bltdmod(a6)
	move.l	d0,(a0)+	; bltdpth
	move.w	d2,(a0)		; bltsize
.exit:
	rts
	ENDC

*****************************************************************************

	RSRESET
CTRL_PAUSE_COUNTER:		rs.w 1		;Pause counter, 0=running
CTRL_SCRIPT_PTR:		rs.l 1		;Script Ptr
CTRL_FINISHED:			rs.w 1		;1 if quitting
CTRL_PRECALC_INTROSTART_DONE	rs.w 1		;1 if intro start precalc done
CTRL_PHASE:			rs.w 1		;Current phase
CTRL_FRAME_COUNT:		rs.w 1		;Local (effect) frame counter
CTRL_PALETTE_LOAD_FLAG		rs.w 1		;set to >1 to force palette load
CTRL_FRAMES_MIN			rs.w 1		;Minimum frames to run (to avoid skipping between 1 and 2+ frames)
CTRL_OBJECTINFO_PTR:		rs.l 1		;Current object
CTRL_PALETTE_ACTIVE:		rs.w 1		;Palette change active
CTRL_PALETTE_PTR:		rs.l 1		;src Palette ptr (16 words of colors)
CTRL_PALETTE_COUNTER:		rs.w 1		;Palette counter, speed
CTRL_PALETTE_SPEED:		rs.w 1		;How often to update, higher is slower, 0 = instant
CTRL_PALETTE_STEP		rs.w 1		;Current step to interpolate between current color and final 0-15
CTRL_ROT_CHANGE_ACTIVE:		rs.w 1		;Rotation change active,final x,y,z,speed
CTRL_ROT_CHANGE_SPEED:		rs.w 1
CTRL_ROT_CHANGE_X:		rs.w 1
CTRL_ROT_CHANGE_Y:		rs.w 1
CTRL_ROT_CHANGE_Z:		rs.w 1
CTRL_MOVE_ACTIVE:		rs.w 1		;move active, final x,y,z,speed
CTRL_MOVE_SPEED:		rs.w 1
CTRL_MOVE_X:			rs.w 1
CTRL_MOVE_Y:			rs.w 1
CTRL_MOVE_Z:			rs.w 1

CTRL_SIZE:			rs.w 0

	EVEN
Controller_Info:
	dcb.b	CTRL_SIZE,0
	EVEN


*****************************************************************************
* Runs the controller script.
* Note the commands are read until a FX_PAUSE command is reached. So beware
* of hogging the CPU with too many commands at once.
* IN:		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d7/a0-a4
*****************************************************************************

Controller_ReadCommands:

	;Time to get a new command from the script? Subtract each frame until 0
	move.w	CTRL_PAUSE_COUNTER(a5),d0
	bne	.pausing

	tst.w	CTRL_FINISHED(a5)
	bne.s	.exit

	; Get current script pointer
.readcmd:	
	move.l	CTRL_SCRIPT_PTR(a5),a4
.loop:
	move.w	(a4)+,d0

	; subroutines need to preserve a4-a6
	cmpi.w	#FX_PALETTE_FLAG,d0
	beq.s	.fx_pallete

	cmpi.w	#FX_SCRIPTJMP_FLAG,d0
	beq.s	.fx_scriptjmp

	cmp.w	#FX_NEXT_PHASE_FLAG,d0
	beq	.fx_next_phase

	cmpi.w	#FX_PAUSE_FLAG,d0
	beq.s	.fx_pause

	cmpi.w	#FX_LOAD_FLAG,d0
	beq.s	.fx_load

	cmpi.w	#FX_MORPH_FLAG,d0
	beq.s	.fx_morph

	cmp.w	#FX_CLONE_ROTATION_FLAG,d0
	beq.s	.fx_clone_rotation

	cmp.w	#FX_CHANGE_ROT_DELTA_FLAG,d0
	beq.s	.fx_change_rot_delta

	cmp.w	#FX_CHANGE_ROT_FLAG,d0
	beq.s	.fx_change_rot

	cmp.w	#FX_MOVE_FLAG,d0
	beq.s	.fx_move

	;assume end of script, don't save ptr
	move.w	#1,CTRL_FINISHED(a5)	;exit
.exit:
	rts

.pausing:
	subq.w	#1,CTRL_PAUSE_COUNTER(a5)
	beq.s	.readcmd
	rts

.fx_pause:
	move.w	(a4)+,d0
	move.w	d0,CTRL_PAUSE_COUNTER(a5)
	move.l	a4,CTRL_SCRIPT_PTR(a5)
	rts				;exit when starting pause

.fx_scriptjmp:
	move.l	(a4)+,a4		;New script
	move.l	a4,CTRL_SCRIPT_PTR(a5)
	bra.s	.loop

.fx_pallete:
	move.w	(a4)+,d0		;Speed
	move.l	(a4)+,a0		;New pallete
	bsr	Controller_FX_Palette	
	bra.s	.loop

.fx_next_phase:
	addq.w	#1,CTRL_PHASE(a5)
	move.l	a4,CTRL_SCRIPT_PTR(a5)
	lea	P1_Lev3Irq(pc),a0
	jsr	FW_SetLev3Irq		;Switch to main routine
	rts

.fx_load:
	move.l	(a4)+,a0		;New object
	bsr	Controller_FX_Load
	bra	.loop

.fx_morph:
	move.w	(a4)+,d0		;Speed
	move.l	(a4)+,a0		;New points
	bsr	Controller_FX_Morph
	bra	.loop

.fx_clone_rotation:
	move.l	(a4)+,a0		;Object to clone from
	bsr	Controller_FX_Clone_Rotation
	bra	.loop

.fx_change_rot_delta:
	movem.w	(a4)+,d0-d2
	bsr	Controller_FX_Change_Rot_Delta
	bra	.loop

.fx_change_rot:
	movem.w	(a4)+,d0-d3		;speed,x,y,z
	bsr	Controller_FX_Change_Rot
	bra	.loop

.fx_move:
	movem.w	(a4)+,d0-d3		;speed,x,y,z
	bsr	Controller_FX_Move
	bra	.loop


*****************************************************************************
* Performs any time-based controller routines.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d7/a0-a4
*****************************************************************************

Controller_Perform:

	move.l	CTRL_OBJECTINFO_PTR(a5),a0	;Current object

	; Update current angles
	movem.w	VEC_OBJ_THETA_X(a0),d0-d5
	; current d0-d2, delta d3-d5

	; have to increase the rotation speed if not a one frame vector
	; so that 1 frame, 2 frame,3 frame vectors move at same apparent speed
	move.w	CTRL_FRAMES_MIN(a5),d7
	subq.w	#1,d7				; fix for dbf
	move.w	#LIB_SIN_Q15_1024W_OFFSET_MASK,d6
.thetaloop
	add.w	d3,d0
	add.w	d4,d1
	add.w	d5,d2
	dbf	d7,.thetaloop

	and.w	d6,d0				;ensure in range
	and.w	d6,d1
	and.w	d6,d2
	movem.w d0-d2,VEC_OBJ_THETA_X(a0)	;save new values

.morph:
	;move.l	CTRL_OBJECTINFO_PTR(a5),a0
	tst.w	VEC_OBJ_MORPH_ACTIVE(a0)
	beq.s	.pal
	bsr	Controller_FX_Morph_Perform	;I:a5, T:d0-d7/a1/a2
.pal:
	tst.w	CTRL_PALETTE_ACTIVE(a5)
	beq.s	.rot
	bsr	Controller_FX_Palette_Perform	;I:a5, T:d0-d7/a0-a2
.rot:
	tst.w	CTRL_ROT_CHANGE_ACTIVE(a5)
	beq.s	.move
	bsr	Controller_FX_Change_Rot_Perform	;I:a5, T:d0-d7/a0
.move:
	tst.w	CTRL_MOVE_ACTIVE(a5)
	beq.s	.exit
	bsr	Controller_FX_Move_Perform	;I:a5, T:d0-d6/a0

.exit:
	rts


*****************************************************************************
* Sets up the pallet change process.
* IN:		a5, Controller_Info
*		a0, new pallete
*		d0, speed
* OUT:		
* TRASHED:	d0/a0/a1
*****************************************************************************

Controller_FX_Palette:
	; If speed is 0 just instastransform
	tst.w	d0
	bne.s	.palette

	move.w	d0,CTRL_PALETTE_ACTIVE(a5)	;disable change, d0 is zero here
	move.w	#CL_NUM_BUFFERS,CTRL_PALETTE_LOAD_FLAG(a5)	;request copper loads immediately twice for double buffer CL issues
	lea	PAL_Current(pc),a1
	
	REPT PAL_NUMCOLS_ALL/2			;/2 for number of longs
	move.l	(a0)+,(a1)+
	ENDR

	rts

.palette:
	move.l	a0,CTRL_PALETTE_PTR(a5)		; supplied pallete now the master
	move.w	d0,CTRL_PALETTE_COUNTER(a5)	; Setup counter and speed
	move.w	d0,CTRL_PALETTE_SPEED(a5)
	
	moveq	#1,d0				; Initial step is 1 (we run 1-15)
	move.w	d0,CTRL_PALETTE_STEP(a5)
	move.w	d0,CTRL_PALETTE_ACTIVE(a5)	; Set pallete flag to 1
	
	lea	PAL_Current(pc),a0		;current active colors
	lea	PAL_Current_Src(pc),a1		;store original active colors

	REPT PAL_NUMCOLS_ALL/2			;/2 for number of longs
	move.l	(a0)+,(a1)+
	ENDR

	rts


*****************************************************************************
* Performs the pallete change.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d7/a0-a2
*****************************************************************************

Controller_FX_Palette_Perform:
	;check counter
	subq.w	#1,CTRL_PALETTE_COUNTER(a5)
	bgt.s	.exit			;1 or greater we skip

	;Reset counter for next time
	move.w	CTRL_PALETTE_SPEED(a5),CTRL_PALETTE_COUNTER(a5)

	lea	PAL_Current_Src(pc),a0		;starting colors
	move.l	CTRL_PALETTE_PTR(a5),a1		;final colors
	lea	PAL_Current(pc),a2		;active colors
	moveq	#PAL_NUMCOLS_ALL,d0		;number of colours to touch
	move.w	CTRL_PALETTE_STEP(a5),d1	;step

	;In, d0=numcols, d1=step, a0-a2 palettes
	;Out, d1=step
	;trashes d0/d2-d7,a0-a2
	jsr	LIB_RGB12_Interpolate_Fast_Palette
	
	;Request copper load this palette twice for double buffer cl issues
	move.w	#CL_NUM_BUFFERS,CTRL_PALETTE_LOAD_FLAG(a5)

	; Increase step
	addq.w	#1,d1			;increase step
	move.w	d1,CTRL_PALETTE_STEP(a5)
	cmpi.w	#16,d1			;Was this the final step?
	blt.s	.exit	

	clr.w	CTRL_PALETTE_ACTIVE(a5)	;finish routine
.exit:
	rts


*****************************************************************************
* Loads a new object.
* IN:		a5, Controller_Info
*		a0, new object info (Obj_Glenz24_Info for example)
* OUT:		
* TRASHED:	d0-d3/a0-a2
*****************************************************************************

Controller_FX_Load:
	; Test if new object has been initialised already, if so skip
	; the inintial face/pts copy and just change current pointer
	tst.w	VEC_OBJ_INITIALIZED(a0)
	bne.s	.changecurrent	

	; Points - ASSUMPTION: Points buffer must be large enough to hold!
	move.l	VEC_OBJ_PTS_PTR(a0),a1
	move.l 	VEC_OBJ_PTSINITIAL_PTR(a0),a2

	; Copy the initial points into the pts buffer
	move.w	(a2)+,d3	; num points-1 
	move.w	d3,(a1)+
.copypts
	movem.w	(a2)+,d0-d2
	movem.w	d0-d2,(a1)
	addq.l	#6,a1
	dbf	d3,.copypts

	; Mark as initialised
	move.w	#1,VEC_OBJ_INITIALIZED(a0)

.changecurrent:
	; Change current object
	move.l	a0,CTRL_OBJECTINFO_PTR(a5)

	;Update frame target based on object
	move.w VEC_OBJ_NUMFRAMES(a0),CTRL_FRAMES_MIN(a5)

	rts


*****************************************************************************
* Copies the rotation from one object to another.
* IN:		a5, Controller_Info
*		a0, object definition to clone (Obj_Glenz24_Info for example)
* OUT:		
* TRASHED:	d0-d2/a1
*****************************************************************************

Controller_FX_Clone_Rotation:
	move.l	CTRL_OBJECTINFO_PTR(a5),a1	;Current object

	movem.w	VEC_OBJ_THETA_X(a0),d0-d2
	movem.w	d0-d2,VEC_OBJ_THETA_X(a1)

	movem.w	VEC_OBJ_THETA_DX(a0),d0-d2
	movem.w	d0-d2,VEC_OBJ_THETA_DX(a1)

	rts


*****************************************************************************
* Changes rot delta.
* IN:		a5, Controller_Info
*		d0-d2, new x,y,z
* OUT:		
* TRASHED:	a0
*****************************************************************************

Controller_FX_Change_Rot_Delta:

	move.l	CTRL_OBJECTINFO_PTR(a5),a0	;Current object

	movem.w	d0-d2,VEC_OBJ_THETA_DX(a0)

	rts


*****************************************************************************
* Changes rot values. Will do over time is speed >0
* IN:		a5, Controller_Info
* 		d0-d3, speed,new x,y,z
* OUT:		
* TRASHED:	d0-d3/a0
*****************************************************************************

Controller_FX_Change_Rot:
	move.l	CTRL_OBJECTINFO_PTR(a5),a0	;Current object

	tst.w	d0
	bne.s	.slow
	move.w	#0,CTRL_ROT_CHANGE_ACTIVE(a5)	;disable change
	movem.w	d1-d3,VEC_OBJ_THETA_X(a0)		;change rot
	rts

.slow:
	move.w	#1,CTRL_ROT_CHANGE_ACTIVE(a5)	;enable change
	movem.w	d0-d3,CTRL_ROT_CHANGE_SPEED(a5)	;store params in controller info

	; If doing slow transform also have to zero rot delta
	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2

	movem.w	d0-d2,VEC_OBJ_THETA_DX(a0)

	rts


*****************************************************************************
* Performs rot change
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d7/a0
*****************************************************************************

Controller_FX_Change_Rot_Perform:
	move.l	CTRL_OBJECTINFO_PTR(a5),a0	;Current object

	movem.w	CTRL_ROT_CHANGE_X(a5),d0-d2
	movem.w	VEC_OBJ_THETA_X(a0),d3-d5
	
	;TODO: fix this lame code, should be working out the best direction

	moveq   #0,d6			;flag

	move.w	CTRL_ROT_CHANGE_SPEED(a5),d7
.x	cmp.w	d3,d0
	beq.s	.y
	moveq	#1,d6			;set flag
	addq.w	#1,d3
	andi.w	#LIB_SIN_Q15_1024W_OFFSET_MASK,d3
	dbf	d7,.x

	move.w	CTRL_ROT_CHANGE_SPEED(a5),d7
.y	cmp.w	d4,d1
	beq.s	.z
	moveq	#1,d6			;set flag
	addq.w	#1,d4
	andi.w	#LIB_SIN_Q15_1024W_OFFSET_MASK,d4
	dbf	d7,.y

	move.w	CTRL_ROT_CHANGE_SPEED(a5),d7
.z	cmp.w	d5,d2
	beq.s	.q
	moveq	#1,d6			;set flag
	addq.w	#1,d5
	andi.w	#LIB_SIN_Q15_1024W_OFFSET_MASK,d5
	dbf	d7,.z

.q
	tst.w	d6
	bne.s	.bye
	clr.w	CTRL_ROT_CHANGE_ACTIVE(a5)	;finished
.bye
	;store changes
	movem.w	d3-d5,VEC_OBJ_THETA_X(a0)

	rts


*****************************************************************************
* Moves. Will do over time is speed >0
* IN:		a5, Controller_Info
* 		d0-d3, speed,new x,y,z
* OUT:		
* TRASHED:	d0-d3
*****************************************************************************

Controller_FX_Move:
	move.l	CTRL_OBJECTINFO_PTR(a5),a0	;Current object

	;clr.w	CTRL_MOVE2_ACTIVE(a5)	;disable change move2

	tst.w	d0			;instant?
	bne.s	.slow
	move.w	d0,CTRL_MOVE_ACTIVE(a5)	;d0=0, disable change
	movem.w	d1-d3,VEC_OBJ_POSX(a0)	;change pos
	rts

.slow:
	move.w	#1,CTRL_MOVE_ACTIVE(a5)	;enable change
	movem.w	d0-d3,CTRL_MOVE_SPEED(a5)	;store params in controller info

	rts


*****************************************************************************
* Performs move change
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d6/a0
*****************************************************************************

Controller_FX_Move_Perform:
	move.l	CTRL_OBJECTINFO_PTR(a5),a0	;Current object

	moveq	#0,d6				;flag

	move.w	CTRL_MOVE_X(a5),d1	;final value
	move.w	VEC_OBJ_POSX(a0),d0	;current value
	move.w	CTRL_MOVE_SPEED(a5),d2	;speed
	bsr.s	Move_Coord_Towards
	move.w	d0,VEC_OBJ_POSX(a0)	;save value
	cmp.w	d0,d1
	sne.b	d6			;set to $ff if ne
.y:
	move.w	CTRL_MOVE_Y(a5),d1	;final value
	move.w	VEC_OBJ_POSY(a0),d0	;current value
	move.w	CTRL_MOVE_SPEED(a5),d2	;speed
	bsr.s	Move_Coord_Towards
	move.w	d0,VEC_OBJ_POSY(a0)	;save value
	cmp.w	d0,d1
	sne.b	d6			;set to $ff if ne
.z:
	move.w	CTRL_MOVE_Z(a5),d1	;final value
	move.w	VEC_OBJ_POSZ(a0),d0	;current value
	move.w	CTRL_MOVE_SPEED(a5),d2	;speed
	bsr.s	Move_Coord_Towards
	move.w	d0,VEC_OBJ_POSZ(a0)	;save value
	cmp.w	d0,d1
	sne.b	d6			;set to $ff if ne

	tst.b	d6
	bne.s	.exit			;test flag, if not zero not finished
	clr.w	CTRL_MOVE_ACTIVE(a5)	;finished
.exit:
	rts


*****************************************************************************
* Moves a coordinate towards a value at given speed.
* IN:		d0, current value
*		d1, target value
*		d2, speed
* OUT:		d0, new value
* TRASHED:	
*****************************************************************************

Move_Coord_Towards:
	cmp.w	d1,d0
	beq.s	.next			; already there
	bgt.s	.greater
.less:
	add.w	d2,d0			;add the speed
	cmp.w	d1,d0			;how about now?
	ble.s	.next			;still moving in right direction
.less_now_greater:
	move.w	d1,d0			;overshot, set value to final
	bra.s	.next

.greater:
	sub.w	d2,d0			;sub the speed
	cmp.w	d1,d0			;how about now?
	bge.s	.next			;still moving in right direction
.greater_now_less:
	move.w	d1,d0			;overshot, set value to final
.next
	rts


*****************************************************************************
* Sets up the morph process.
* IN:		a5, Controller_Info
*		a0, new pts
*		d0, speed
* OUT:		
* TRASHED:	d0-d3/a1-a2
*****************************************************************************

Controller_FX_Morph:
	move.l	CTRL_OBJECTINFO_PTR(a5),a2	;Current object

	; set new initial points which the morph will use
	move.l	a0,VEC_OBJ_PTSINITIAL_PTR(a2)

	; If speed is 0 just instastransform
	tst.w	d0
	bne.s	.morph

	clr.w	VEC_OBJ_MORPH_ACTIVE(a2)	;disable morph

	move.l	VEC_OBJ_PTS_PTR(a2),a1

	move.w	(a0)+,d3	; num points-1 
	move.w	d3,(a1)+
.copypts
	movem.w	(a0)+,d0-d2
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	move.w	d2,(a1)+
	dbf	d3,.copypts

	rts

.morph:
	; Setup counter 
	move.w	d0,VEC_OBJ_MORPH_COUNTER(a2)
	move.w	d0,VEC_OBJ_MORPH_SPEED(a2)

	; Set morph flag
	move.w	#1,VEC_OBJ_MORPH_ACTIVE(a2)

	rts


*****************************************************************************
* Performs the morph.
* IN:		
* OUT:
* TRASHED:	d0-d7/a1/a2
*****************************************************************************

Controller_FX_Morph_Perform:
	move.l	CTRL_OBJECTINFO_PTR(a5),a0	;Current object

	;check counter
	subq.w	#1,VEC_OBJ_MORPH_COUNTER(a0)
	bgt.s	.exit			;1 or greater we skip

	;new counter for next time
	move.w	VEC_OBJ_MORPH_SPEED(a0),VEC_OBJ_MORPH_COUNTER(a0)

	move.l	VEC_OBJ_PTSINITIAL_PTR(a0),a1
	move.l	VEC_OBJ_PTS_PTR(a0),a2
	moveq	#0,d6	

	;ASSERT: Num points in current and new buffers are the same.
	move.w	(a1)+,d7	; num points-1 
	addq.l	#2,a2		; skip num points
.morphpts:
	movem.w	(a1)+,d0-d2	;x,y,z
	movem.w	(a2),d3-d5	;x,y,z

	cmp	d0,d3
	beq.s	.xequal
	ble.s	.xless
	subq.w	#1,d3
	moveq	#1,d6		;flag
	bra.s	.xequal
.xless:		
	addq.w	#1,d3
	moveq	#1,d6		;flag
.xequal:
	cmp	d1,d4
	beq.s	.yequal
	ble.s	.yless
	subq.w	#1,d4
	moveq	#1,d6		;flag
	bra.s	.yequal
.yless:		
	addq.w	#1,d4
	moveq	#1,d6		;flag
.yequal:
	cmp	d2,d5
	beq.s	.zequal
	ble.s	.zless
	subq.w	#1,d5
	moveq	#1,d6		;flag
	bra.s	.zequal
.zless:		
	addq.w	#1,d5
	moveq	#1,d6		;flag
.zequal:

	; save pts
	move.w	d3,(a2)+
	move.w	d4,(a2)+
	move.w	d5,(a2)+
	dbf	d7,.morphpts

	; Reset morph flag if no changes made
	tst.w	d6
	bne.s	.exit

	clr.w	VEC_OBJ_MORPH_ACTIVE(a0)

.exit:
	rts


*****************************************************************************

; Master palette poked into the copperlist each frame
PAL_Current:		dcb.w	PAL_NUMCOLS_MAIN,0	;main colours
			dcb.w	PAL_NUMCOLS_DARK,0	;dark/reflection colours

; This holds the old source palette used during transitions in FX_PALETTE. The
; source value is interpolated from this value to the destination value + step size.
PAL_Current_Src:	dcb.w	PAL_NUMCOLS_MAIN,0	;main colours
			dcb.w	PAL_NUMCOLS_DARK,0	;dark/reflection colours

; All black and white palettes used for fades/lightsources
PAL_AllBlack:		dcb.w	PAL_NUMCOLS_MAIN,0	;main colours
			dcb.w	PAL_NUMCOLS_DARK,0	;dark/reflection colours

PAL_AllWhite:		dcb.w	PAL_NUMCOLS_MAIN,$fff	;main colours
			dcb.w	PAL_NUMCOLS_DARK,$fff	;dark/reflection colours

	RSRESET
PAL_MAIN:		rs.w	PAL_NUMCOLS_MAIN
PAL_MAIN_SIZEOF:	rs.w	0
PAL_DARK:		rs.w	PAL_NUMCOLS_DARK
PAL_SIZEOF:		rs.w	0


*****************************************************************************
* Loads the current colors into the current copperlist
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0/a0-a3
*****************************************************************************

Write_Palette_To_Copper:
	;Load flag may be > 1 if using double buffered CL or changing between CLs
	tst.w	CTRL_PALETTE_LOAD_FLAG(a5)
	beq	.exit
	subq.w	#1,CTRL_PALETTE_LOAD_FLAG(a5)

	lea	PAL_Current(pc),a0
	move.l	CL_Log1_Ptr(pc),a1

	;Normal colors
	lea	P0_CL_COL_OFFSET+2(a1),a2	
	REPT	BPL_BUF_NUMCOLS
	move.w	(a0)+,(a2)
	addq.l	#4,a2			;next color
	ENDR

	lea 	(a0),a3			;Save dark color pointer

	;Reflection colors - top
	lea	P0_CL_COL_REFL_TOP_OFFSET+2(a1),a2	
	REPT	BPL_BUF_NUMCOLS
	move.w	(a0)+,(a2)
	addq.l	#4,a2			;next color
	ENDR

	;Reflection colors - bot
	lea	P0_CL_COL_REFL_BOT_OFFSET+2(a1),a2	
	REPT	BPL_BUF_NUMCOLS
	move.w	(a3)+,(a2)
	addq.l	#4,a2			;next color
	ENDR

.exit:
	rts


*****************************************************************************
* Blits a bob onto the screen.
* environment.
* IN:		a6, _custom
*		a0, bob
*		a1, screen
*		d0, x
*		d1, y
* OUT:		
* TRASHED:	d0/a0/a1	
*****************************************************************************

	IFNE 0
VEC_Blit_BOB_XY_A6:
	lea	Mult_SCR_Height_ByteWidth_NumPlanes,a2
	add.w	d1,d1			;y value, access table in words
	add.w	(a2,d1.w),a1		;add y value to screen adr

	ext.l	d0			;ensure top word clear
	ror.l	#4,d0			;hiword of d0 contains shift in highest nibble
	add.w	d0,d0			;loword d0 contains byte offset into screen 
	add.w	d0,a1			;add byte offset to y address
	swap	d0			;d0 word now contains shift value
	or.w	#BLT_SRC_ABD+(BLT_A|BLT_B),d0	;D=A or B = $dfc
	swap	d0			;d0=bltcon0 and bltcon1
	clr.w	d0			;bltcon1=0

	WAITBLIT_NASTY_A6
	move.l	d0,bltcon0(a6)
	move.l	#$ffff0000,bltafwm(a6)	;mask last word as it's not there
	move.w	#BPL_BUF_BYTEWIDTH-(VEC_BOB_BYTEWIDTH+2),bltbmod(a6)
	move.w	#-2,bltamod(a6)		;We are blitting an extra word that doesn't exist
	move.w	#BPL_BUF_BYTEWIDTH-(VEC_BOB_BYTEWIDTH+2),bltdmod(a6)
	move.l	a1,bltbpth(a6)
	move.l	a0,bltapth(a6)
	move.l	a1,bltdpth(a6)
	move.w	#((VEC_BOB_HEIGHT*VEC_BOB_NUMPLANES)*64)+VEC_BOB_WORDWIDTH+1,bltsize(a6)

	rts
	ENDC


*****************************************************************************
* Updates the current bob frame lookup and stores easy access pointers.
* IN:	
* OUT:		
* TRASHED:	d0/a0
*****************************************************************************

BOB_Update_FrameLookup:

	;Update the address of the screen to clear/draw/fill the new bob
	move.w	BOB_Frame_Num(pc),d0
	addq.w	#1,d0
	andi.w	#BOB_FRAMEMASK,d0
	move.w	d0,BOB_Frame_Num
	lea	BOB_FrameLookup(pc),a0
	mulu.w	#BOB_FRAME_SIZEOF,d0
	lea	(a0,d0.w),a0
	move.l	a0,BOB_FrameLookup_Ptr	;Save current frame lookup for later
	move.l	BOB_FRAME_FILL_BPLPTR(a0),BOB_Frame_Screen_Ptr	;save address for other routines

	rts


*****************************************************************************
* Creates the bob draw list. CPU routine to be used during blit/screen clear
* IN:		
* OUT:		
* TRASHED:	d1/d3-d7/a0-a1
*****************************************************************************

BOB_Update_BOBs:
	;Get sine speed and steps
	lea	BOB_Sine_X(pc),a0
	lea	BOBSnake_DrawList_X(pc),a1			;X coords

	move.w	#BOB_SINE_OFFSET_MASK,d5	;Used to keep the offset in range

	; Sine 1
	move.w	BOBSnake_Sine1_X_Offset(pc),d6		;Offset in words
	move.w	BOBSnake_Sine1_X_Speed(pc),d3		;Get speed (movement per frame)
	add.w	d3,d3					;Offset in words
	add.w	d3,d6
	and.w	d5,d6					;Ensure in range
	move.w	d6,BOBSnake_Sine1_X_Offset		;Save for next frame
	move.w	BOBSnake_Sine1_X_Step(pc),d3		;Get step (movement per pixel)
	add.w	d3,d3					;Offset in words

	; Sine 2
	move.w	BOBSnake_Sine2_X_Offset(pc),d7		;Offset in words
	move.w	BOBSnake_Sine2_X_Speed(pc),d4		;Get speed (movement per frame)
	add.w	d4,d4					;Offset in words
	add.w	d4,d7
	and.w	d5,d7					;Ensure in range
	move.w	d7,BOBSnake_Sine2_X_Offset		;Save for next frame
	move.w	BOBSnake_Sine2_X_Step(pc),d4		;Get step (movement per pixel)
	add.w	d4,d4					;Offset in words

	REPT BOB_NUMBOBS
	;move.w	#BOB_NUMBOBS-1,d0
;.loopx
	move.w 	(a0,d6.w),d1		;sine 1
	add.w	(a0,d7.w),d1		;add sine 2

	move.w	d1,(a1)+
	;addq.l	#4,a1			; Next set of coords

	add.w	d3,d6			;Increase offsets and mask to 0-2047 (1024 sine entries, in words)
	add.w	d4,d7
	and.w	d5,d6
	and.w	d5,d7

	;dbf	d0,.loopx
	ENDR

	;Get sine speed and steps
	lea	BOB_Sine_Y_PreMult(pc),a0			;This list has the Y premultiplied by Screen_ByteWidth*NumPlanes
	lea	BOBSnake_DrawList_Y(pc),a1			; Y coords

	;move.w	#BOB_SINE_OFFSET_MASK,d5		;Used to keep the offset in range

	; Sine 1
	move.w	BOBSnake_Sine1_Y_Offset(pc),d6		;Offset in words
	move.w	BOBSnake_Sine1_Y_Speed(pc),d3		;Get speed (movement per frame)
	add.w	d3,d3					;Offset in words
	add.w	d3,d6
	and.w	d5,d6					;Ensure in range
	move.w	d6,BOBSnake_Sine1_Y_Offset		;Save for next frame
	move.w	BOBSnake_Sine1_Y_Step(pc),d3		;Get step (movement per pixel)
	add.w	d3,d3					;Offset in words

	; Sine 2
	move.w	BOBSnake_Sine2_Y_Offset(pc),d7		;Offset in words
	move.w	BOBSnake_Sine2_Y_Speed(pc),d4		;Get speed (movement per frame)
	add.w	d4,d4					;Offset in words
	add.w	d4,d7
	and.w	d5,d7					;Ensure in range
	move.w	d7,BOBSnake_Sine2_Y_Offset		;Save for next frame
	move.w	BOBSnake_Sine2_Y_Step(pc),d4		;Get step (movement per pixel)
	add.w	d4,d4					;Offset in words

	REPT BOB_NUMBOBS
	;move.w	#BOB_NUMBOBS-1,d0
;.loopy
	move.w 	(a0,d6.w),d1		;sine 1
	add.w	(a0,d7.w),d1		;add sine 2

	move.w	d1,(a1)+
	;addq.l	#4,a1			; Next set of coords

	add.w	d3,d6		;Increase offsets and mask to 0-2047 (1024 sine entries, in words)
	add.w	d4,d7
	and.w	d5,d6
	and.w	d5,d7

	;dbf	d0,.loopy
	ENDR


	rts


*****************************************************************************
* Draws the previously plotted BOBs.
* IN:		a6, _custom
* OUT:		
* TRASHED:	d0-d3/a0-a4
*****************************************************************************

BOB_Draw_BOBs:
	move.l	a5,-(sp)

	lea	BOBSnake_DrawList_X(pc),a2	; x 
	lea	BOBSnake_DrawList_Y(pc),a3	; y*screen_bytewidth values
	moveq	#BOB_NUMBOBS-1,d3

	move.l	BPL_Log1_Ptr(pc),a4	;save
	move.w	BOB_Frame_Num(pc),d2

	; Our BOB is 32 pixels wide, but we need to shift right by 16
	; to position it. So blit it an extra word wide and mask off the result.
	; Need to have -2 in modulos to accomodate this.
	; We are going to use the height details to speed up a little. Too
	; tricky to use the x attributes.
	WAITBLIT_NASTY_A6
	move.l	#$ffff0000,bltafwm(a6)	;mask last word as it's not there
	move.w	#BPL_BUF_BYTEWIDTH-(VEC_BOB_BYTEWIDTH+2),bltbmod(a6)
	move.w	#-2,bltamod(a6)		;We are blitting an extra word that doesn't exist
	move.w	#BPL_BUF_BYTEWIDTH-(VEC_BOB_BYTEWIDTH+2),bltdmod(a6)	
.loop
	andi.w	#BOB_FRAMEMASK,d2	;ensure in range
	move.w	d2,d0
	mulu	#BOB_FRAME_SIZEOF,d0	;offset
	lea	BOB_FrameLookup(pc),a5
	lea	(a5,d0.w),a5		;addr of frame structure
	tst.l	BOB_FRAME_FILL_BOB_PTR(a5)
	beq.s	.skipdraw

	move.l	BOB_FRAME_FILL_BPLPTR(a5),a0		;Addr of bob frame
	

	move.w	(a2)+,d0		;x in pixels
	move.l	a4,a1			;screen adr
	add.w	(a3)+,a1		;add to scr, y is premult by screen width

	ext.l	d0			;ensure top word clear
	ror.l	#4,d0			;hiword of d0 contains shift in highest nibble
	add.w	d0,d0			;loword d0 contains byte offset into screen 
	add.w	d0,a1			;add byte offset to y address
	swap	d0			;d0 word now contains shift value
	or.w	#BLT_SRC_ABD+(BLT_A|BLT_B),d0	; Simple A or B
	swap	d0			;d0=bltcon0 and bltcon1
	clr.w	d0			;bltcon1=0, clear only bottom word

	;Get precalced offsets and sizes
	add.l	BOB_FRAME_COPY_SCR_OFFSET(a5),a1
	move.l	BOB_FRAME_COPY_BOB_PTR(a5),a0
	move.w	BOB_FRAME_COPY_BLTSIZE(a5),d1

	WAITBLIT_NASTY_A6

	move.l	d0,bltcon0(a6)
	move.l	a1,bltbpth(a6)
	move.l	a0,bltapth(a6)
	move.l	a1,bltdpth(a6)
	;move.w	#((VEC_BOB_HEIGHT*VEC_BOB_NUMPLANES)*64)+VEC_BOB_WORDWIDTH+1,bltsize(a6)
	move.w	d1,bltsize(a6)
.skipdraw:
	; Next bob uses a different frame
	subi.w	#BOB_FRAME_SPACING,d2
	dbf	d3,.loop


	move.l	(sp)+,a5
	rts


*****************************************************************************


;Rotation stuff for matrix

;Object rotation

VEC_MROT11 = 0
VEC_MROT12 = 2
VEC_MROT13 = 4
VEC_MROT21 = 6
VEC_MROT22 = 8
VEC_MROT23 = 10
VEC_MROT31 = 12
VEC_MROT32 = 14
VEC_MROT33 = 16

VEC_MatrixRotObject:
	ds.w	1			;0
	ds.w	1			;2
	ds.w	1			;4

	ds.w	1			;6
	ds.w	1			;8
	ds.w	1			;10

	ds.w	1			;12
	ds.w	1			;14
	ds.w	1			;16


*****************************************************************************
* Precreates rotation matrix for the given object.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d7/a0-a2
*****************************************************************************

Calc_Rotation_Matrix_For_Object:
	; Get current angles
	move.l	CTRL_OBJECTINFO_PTR(a5),a0	;Current object
	movem.w	VEC_OBJ_THETA_X(a0),d0-d2	;angles
	lea	VEC_MatrixRotObject(pc),a0	;adr of matrix
	bra	Calc_Rotation_Matrix		;init matrix

	;rts


*****************************************************************************
* Creates a rotation matrix. General purpose routine for angles and a given matrix.
* IN:		d0-d2, x,y,z thetas
*		a0, Adr of rotation matrix
* OUT:
* TRASHED:	d0-d7/a0-a2
*****************************************************************************

Calc_Rotation_Matrix:

	add.w	d0,d0			;double for access in words
	add.w	d1,d1
	add.w	d2,d2

	lea	LIB_SIN_Q15_1024W_Table,a1		;Sine
	lea	LIB_COS_Q15_1024W_Table,a2		;Cosine
	
	move.w	(a1,d0.w),d7
	move.w	(a2,d0.w),d6
	move.w	(a1,d1.w),d5
	move.w	(a2,d1.w),d4
	move.w	(a1,d2.w),d3
	move.w	(a2,d2.w),d2

;all thetas have been taken care of - now init rot matrix

	;SinX = d7 , CosX = d6
	;SinY = d5 , CosY = d4
	;SinZ = d3 , CosZ = d2


; 3D composite rotation in ZYX order

; ( CosYCosZ		    -CosYSinZ		    SinY      )	 1
; ( SinXSinYCosZ+CosXSinZ   -SinXSinYSinZ+CosXCosZ  -SinXCosY )  2
; ( -CosXSinYCosZ+SinXSinZ  CosXSinYSinZ+SinXCosZ   CosXCosY  )  3
;             1                       2                 3

;rot11
	move.w	d4,d0			;cosY
	muls	d2,d0			;cosYcosZ

	add.l	d0,d0
	swap	d0			;/32768
	
	move.w	d0,(a0)+		;Rot11

;rot12
	move.w	d4,d0			;cosY
	neg.w	d0			;-CosY
	muls	d3,d0			;-cosYsinZ

	add.l	d0,d0
	swap	d0			;/32768

	move.w	d0,(a0)+		;Rot12

;rot13
	move.w	d5,(a0)+		;Rot13   (SinY)

;rot21
	move.w	d7,d0			;sinX
	muls	d5,d0     		;sinXsinY	

	add.l	d0,d0
	swap	d0			;/32768

	muls	d2,d0			;sinXsinYCosZ

	add.l	d0,d0
	swap	d0			;/32768

	move.w	d6,d1			;cosX
	muls	d3,d1			;cosXSinZ

	add.l	d1,d1
	swap	d1			;/32768

	add.w	d1,d0
	move.w	d0,(a0)+		;Rot21

;rot22
	move.w	d7,d0			;sinX
	neg.w	d0			;-sinX
	muls	d5,d0			;-sinXsinY

	add.l	d0,d0
	swap	d0			;/32768

	muls	d3,d0			;-sinXsinYsinZ

	add.l	d0,d0
	swap	d0			;/32768

	move.w	d6,d1			;cosX
	muls	d2,d1			;cosXCosZ

	add.l	d1,d1
	swap	d1			;/32768

	add.w	d1,d0
	move.w	d0,(a0)+		;Rot22

;rot23
	move.w	d7,d0			;sinX
	neg.w	d0			;-sinX
	muls	d4,d0			;-sinXcosY

	add.l	d0,d0
	swap	d0			;/32768

	move.w	d0,(a0)+		;Rot23

;rot31
	move.w	d6,d0			;cosX
	neg.w	d0			;-cosX
	muls	d5,d0			;-cosXsinY

	add.l	d0,d0
	swap	d0			;/32768

	muls	d2,d0			;-cosXsinYcosZ

	add.l	d0,d0
	swap	d0			;/32768

	move.w	d7,d1			;sinX
	muls	d3,d1			;sinXsinZ

	add.l	d1,d1
	swap	d1			;/32768

	add.w	d1,d0
	move.w	d0,(a0)+		;Rot31

;rot32
	move.w	d6,d0			;cosX
	muls	d5,d0			;cosXsinY	

	add.l	d0,d0
	swap	d0			;/32768

	muls	d3,d0			;cosXsinYsinZ

	add.l	d0,d0
	swap	d0			;/32768

	move.w	d7,d1			;sinX
	muls	d2,d1			;sinXcosZ

	add.l	d1,d1
	swap	d1			;/32768

	add.w	d1,d0
	move.w	d0,(a0)+		;Rot32

;rot33
	move.w	d6,d0			;cosX
	muls	d4,d0			;cosXcosY

	add.l	d0,d0
	swap	d0			;/32768

	move.w	d0,(a0)+		;Rot33
	
;rotation matrix is done

	rts


*****************************************************************************
* Rotates an object.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d7/a0-a4
*****************************************************************************
	
	IFNE	0
Rotate:
	move.l	a5,-(sp)
	move.l	a6,-(sp)

	move.l	CTRL_OBJECTINFO_PTR(a5),a0	;Current object

	movem.w	VEC_OBJ_POSX(a0),a4-a6	;get object position
	move.l	VEC_OBJ_PTS_PTR(a0),a0	;Unrotated pts

	lea	RotXYZpts(pc),a1
	lea	VEC_MatrixRotObject(pc),a2

	move.w	(a0)+,d7		;number of points
	move.w	d7,(a1)+

.rotloop
	movem.w	(a0)+,d0-d2

	move.l	a2,a3			;save a2

	move.w	d0,d4
	move.w	d1,d5
	move.w	d2,d6

	muls	(a3)+,d4		;rot11
	muls	(a3)+,d5		;rot12
	muls	(a3)+,d6		;rot13
	add.l	d4,d5
	add.l	d5,d6
	add.l	d6,d6			;*2 and swap = divide by 32768
	swap	d6			;d6=new x coord
	
	add.w	a4,d6			;add x pos
	move.w	d6,(a1)+


	move.w	d0,d4
	move.w	d1,d5
	move.w	d2,d6

	muls	(a3)+,d4		;rot21
	muls	(a3)+,d5		;rot22
	muls	(a3)+,d6		;rot23
	add.l	d4,d5
	add.l	d5,d6
	add.l	d6,d6
	swap	d6			;d6=new y coord

	add.w	a5,d6			;add y pos
	move.w	d6,(a1)+


	muls	(a3)+,d0		;rot31
	muls	(a3)+,d1		;rot32
	muls	(a3)+,d2		;rot33
	add.l	d0,d1
	add.l	d1,d2
	add.l	d2,d2
	swap	d2			;d2=new z coord

	add.w	a6,d2			;add z pos
	move.w	d2,(a1)+

	dbf	d7,.rotloop

	move.l	(sp)+,a6
	move.l	(sp)+,a5

	rts
	ENDC


*****************************************************************************
* Performs perspective.
* IN:
* OUT:
* TRASHED:	d0-d7/a0
*****************************************************************************

	IFNE	0
Perspective_Vars:
	dc.w	VEC_XOFFSET		;d4
	dc.w	VEC_YOFFSET		;d5
	dc.w	VEC_ZOFFSET		;d6

Perspective:
	;Get screen offsets
	movem.w	Perspective_Vars(pc),d4-d6	;movem extends to .l

	lea	RotXYZpts(pc),a0	;rot pts 3d
	move.w	(a0)+,d7		;num pts-1
.persloop1:
	movem.w	(a0),d0-d2		;get x,y,z pts (movem extends to .l)

	asl.l	#8,d0			;*256
	asl.l	#8,d1			;*256

	add.w	d6,d2			;pers+z
	beq.s	.store			;trap div by zero
	divs	d2,d0			;new x & y values
	divs	d2,d1
.store:	
	add.w	d4,d0			;add x offset
	neg.w	d1			;flip y axis
	add.w	d5,d1			;add y axis
	
	move.w	d0,(a0)+		;store x
	move.w	d1,(a0)+		;store y
	addq.l	#2,a0			;skip z

	dbf	d7,.persloop1		;next pt

	rts
	ENDC


*****************************************************************************
* Performs rotate and perspective in the same routine for speed.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d7/a0-a4
*****************************************************************************

RotateAndPerspective:
	move.l	CTRL_OBJECTINFO_PTR(a5),a0	;Current object

	movem.w	VEC_OBJ_POSX(a0),d5-d6/a4	;Position
	move.l	VEC_OBJ_PTS_PTR(a0),a0		;Unrotated pts
	lea	RotXYZpts(pc),a1		;Rotated pts
	lea	VEC_MatrixRotObject(pc),a3	;Matrix
	
	move.w	(a0)+,d7		;number of points-1
	move.w	d7,(a1)+
.loop:
	move.l	a3,a2			;Matrix 
; X
	movem.w	(a0),d0-d2		;x,y,z
	muls	(a2)+,d0		;VEC_MROT11
	muls	(a2)+,d1		;VEC_MROT12
	muls	(a2)+,d2		;VEC_MROT13
	add.l	d1,d0
	add.l	d2,d0
	
	add.l	d0,d0			;*2 and swap = divide by 32768
	swap	d0
	
	add.w	d5,d0			;add x pos

; Y
	move.w	(a0),d3			;x
	movem.w	2(a0),d1-d2		;y,z
	muls	(a2)+,d3		;VEC_MROT21
	muls	(a2)+,d1		;VEC_MROT22
	muls	(a2)+,d2		;VEC_MROT23
	add.l	d3,d1
	add.l	d2,d1

	add.l	d1,d1
	swap	d1			;/32768
	
	add.w	d6,d1			;add y pos

; Z 
	move.w	(a0)+,d3		;x
	move.w	(a0)+,d4		;y
	move.w	(a0)+,d2		;z

	muls	(a2)+,d3		;VEC_MROT31
	muls	(a2)+,d4		;VEC_MROT32
	muls	(a2),d2			;VEC_MROT33
	add.l	d3,d2
	add.l	d4,d2
	
	add.l	d2,d2
	swap	d2			;/32768
	
	add.w	a4,d2			;add z pos

;Perspective
	;d0,d1,d2 = x,y,z

	ext.l	d0
	ext.l	d1
	asl.l	#8,d0			;*256 - this should match the WORLDDATA_ZOFFSET value so that
	asl.l	#8,d1			;object coords equate roughly to screen coords
	addi.w	#VEC_ZOFFSET,d2		;Add Z offset
	beq.s	.store
	divs	d2,d0	
	divs	d2,d1
.store:
	addi.w	#VEC_XOFFSET,d0		;add x offset
	neg.w	d1			;invert Y
	addi.w	#VEC_YOFFSET,d1		;add y offset

	move.w	d0,(a1)+		;store x
	move.w	d1,(a1)+		;store y
	addq.l	#2,a1			;skip z

	dbf	d7,.loop
	rts



*****************************************************************************
* Calculates the bounding rectange of the rotated points. Points are expected to
* be in screen coordinates just prior to drawing.
* This uses the overall shape of an object so is used for clearing or filling
* the whole screen.
*
* IN:		
* OUT:
* TRASHED:	d0-d7/a0-a2
*****************************************************************************

Calc_BOB_Fill_Bounding:
	lea	RotXYZpts(pc),a0	;rot pts 3d

	move.w	(a0)+,d7		;num pts-1

	move.w	(a0)+,d0		;first x
	move.w	(a0)+,d1		;first y
	addq.l	#2,a0			;skip z to next x,y,z

	; First coords, use these as the initial clip/fill values
	move.w	d0,d3			;min x
	move.w	d1,d4			;min y
	move.w	d0,d5			;max x
	move.w	d1,d6			;max y
	bra.s	.skipinit
.loop
	move.w	(a0)+,d0		;x
	move.w	(a0)+,d1		;y
	addq.l	#2,a0			;skip to next x,y,z
.skipinit	
	cmp.w	d0,d3
	ble.s	.minx_nochange
	move.w	d0,d3			;new min x
.minx_nochange:
	cmp.w	d1,d4
	ble.s	.miny_nochange
	move.w	d1,d4			;new min y
.miny_nochange:
	cmp.w	d0,d5
	bge.s	.maxx_nochange
	move.w	d0,d5			;new max x
.maxx_nochange:
	cmp.w	d1,d6
	bge.s	.maxy_nochange
	move.w	d1,d6			;new max y
.maxy_nochange:

	dbf	d7,.loop		;next pt
	;d3=minx, d4=miny, d5=maxx, d6=maxy

	move.l	BOB_FrameLookup_Ptr(pc),a0
	lea	LineDraw3_Vars(pc),a1



	IFNE VEC_CLIPPING

	;Check if any points on the screen (object might be completely off screen)
	;also check that minx >= CLIPMINX, maxx <= CLIPMAXX etc
	move.w	LINEDRAW3_CLIP_MINX(a1),d0
	move.w	LINEDRAW3_CLIP_MINY(a1),d1

	cmp.w	d0,d5				;maxx < clipminx
	blt	.exitnofill			;off scr
	cmp.w	d1,d6				;maxy < clipminy
	blt	.exitnofill			;off scr

	cmp.w	d0,d3				;minx > clipminx
	bge.s	.minxok
	move.w	d0,d3				;clip minx
.minxok:
	cmp.w	d1,d4				;miny > clipminy
	bge.s	.minyok
	move.w	d1,d4				;reset miny
.minyok:

	move.w	LINEDRAW3_CLIP_MAXX(a1),d0
	move.w	LINEDRAW3_CLIP_MAXY(a1),d1

	cmp.w	d0,d3				;minx > clipmaxx
	bgt	.exitnofill			;off scr
	cmp.w	d1,d4				;miny < clipmaxy
	bgt	.exitnofill			;off scr

	cmp.w	d0,d5				;maxx < clipmaxx
	ble.s	.maxxok
	move.w	d0,d5				;clip maxx
.maxxok:
	cmp.w	d1,d6				;maxy < clipmaxy
	ble.s	.maxyok
	move.w	d1,d6				;reset maxy
.maxyok:

	ENDC				; VEC_CLIPPING

	; Store the new bounding box values in required regs
.skiptests:
	move.w	d3,d0			;minx
	move.w	d4,d1			;miny
	move.w	d5,d2			;maxx
	move.w	d6,d3			;maxy

	;ASSERT: Our line draw routine does not draw the BOTTOM scanline so that
	;blitter fills work correctly. So we have to sub 1 from our maxy. This may
	;cause miny>maxy so have to handle that below in height calcs.
	subq.w	#1,d3


; ***
; Now work out the blitter values for use in full screen fill/clear
; ***
	; minx = 3, maxy = 15
	; minx&-16 = 0, maxx&-16 = 0, width = 1 word
	moveq	#-16,d4
	and.w	d4,d0			;leftx on word boundary
	and.w	d4,d2			;rightx on word boundary
	move.w	d2,d5			;don't trash max x 
	sub.w	d0,d5			;width in pixels (word aligned)
	lsr.w	#4,d5			;width in words
	addq.w	#1,d5			;width+1, 0 to 304 is 20 words not 19 as calculated
	move.w	d5,BOB_FRAME_FILL_WORDWIDTH(a0)	;save for clear routine

	move.w	d1,BOB_FRAME_FILL_MINY(a0)
	move.w	d3,d4			;don't trash max y
	sub.w	d1,d4			;height (miny may be >= maxy see notes above)
	bmi.s	.exitnofill

.heightok:
	addq.w	#1,d4			;height+1,  255-0 = 255, but height should be 256
	move.w	d4,BOB_FRAME_FILL_HEIGHT(a0)	;save for clear routine

	;moveq	#BPL_BUF_NUMPLANES,d0
	;mulu.w	d0,d4			
	;height * num bpl for interleaved
	move.w	d4,d0
	add.w	d4,d4
	add.w	d0,d4

	lsl.w	#6,d4			;*64 = height portion of bltsize
	add.w	d5,d4			;Add word width, completed bltsize in d4
	move.w	d4,BOB_FRAME_FILL_BLTSIZE(a0)	;save for clear routine

	;d2 = max x in pixels (on word boundary)
	;d3 = max y
	;d4 = bltsize
	;d5 = fill width in words

	add.w	d5,d5			;d5=width in bytes
	moveq	#VEC_BOB_BYTEWIDTH,d1	;screen width in bytes
	sub.w	d5,d1			;modulo
	move.w	d1,BOB_FRAME_FILL_BYTEMODULO(a0)	;save for clear routine

	; our Blitter source has to be the last word on the last bitplane (desending mode)
	; For line 100
	; 100 * (ScreenByteWidth * NumBitplanes)
	; + (ScreenByteWidth * (NumBitplanes-1))
	; + rightx in bytes

	move.l	BOB_FRAME_FILL_BPLPTR(a0),a2
	add.w	d3,d3			;access y mult table in words
	add.w	LINEDRAW3_MULTTABLE(a1,d3.w),a2	;add to address

	lsr.w	#3,d2			;rightx in bytes
	lea 	VEC_BOB_BYTEWIDTH*(VEC_BOB_NUMPLANES-1)(a2,d2.w),a2	;starting position is last word in block (descending mode)
	move.l	a2,BOB_FRAME_FILL_BOB_PTR(a0)	;save for clear routine

	; Work out values to be used when copying from BOB image to screen
	; In ascending mode unlike everything else above
	move.w	BOB_FRAME_FILL_MINY(a0),d0
	mulu	#BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES,d0
	move.l	d0,BOB_FRAME_COPY_SCR_OFFSET(a0)

	move.w	BOB_FRAME_FILL_MINY(a0),d0
	mulu	#VEC_BOB_BYTEWIDTH*VEC_BOB_NUMPLANES,d0
	add.l	BOB_FRAME_FILL_BPLPTR(a0),d0
	move.l	d0,BOB_FRAME_COPY_BOB_PTR(a0)

	move.w	BOB_FRAME_FILL_HEIGHT(a0),d0
	mulu	#VEC_BOB_NUMPLANES*64,d0
	addq.w	#VEC_BOB_WORDWIDTH+1,d0
	move.w	d0,BOB_FRAME_COPY_BLTSIZE(a0)
.exit:
	rts

.exitnofill:
	; No valid fill possible. Reset fill/clear buffers
	move.l	BOB_FrameLookup_Ptr(pc),a0
	clr.l	BOB_FRAME_FILL_BOB_PTR(a0)
	rts


*****************************************************************************
* Checks if we are clipping. A debug function so that we can optimize object
* sizes.
* Coords be in screen coordinates just prior to drawing.
*
* IN:		
* OUT:
* TRASHED:	d0-d7/a0-a2
*****************************************************************************

	IFNE VEC_CLIPCHECK

ClipCheck:
	clr.w	.VEC_ClipCheck_Flag

	lea	RotXYZpts,a0		;rot pts 3d

	move.w	(a0)+,d7		;num pts-1

	movem.w	(a0),d0-d1		; First x and y
	addq.l	#6,a0			;skip to next x,y,z

	; First coords, use these as the initial clip/fill values
	move.w	d0,d3			;min x
	move.w	d1,d4			;min y
	move.w	d0,d5			;max x
	move.w	d1,d6			;max y
	bra.s	.skipinit
.loop
	movem.w	(a0),d0-d1		;get x and y
	addq.l	#6,a0			;skip to next x,y,z
.skipinit	
	cmp.w	d0,d3
	ble.s	.minx_nochange
	move.w	d0,d3			;new min x
.minx_nochange:
	cmp.w	d1,d4
	ble.s	.miny_nochange
	move.w	d1,d4			;new min y
.miny_nochange:
	cmp.w	d0,d5
	bge.s	.maxx_nochange
	move.w	d0,d5			;new max x
.maxx_nochange:
	cmp.w	d1,d6
	bge.s	.maxy_nochange
	move.w	d1,d6			;new max y
.maxy_nochange:

	dbf	d7,.loop		;next pt
	;d3=minx, d4=miny, d5=maxx, d6=maxy

	;Check if any points on the screen (object might be completely off screen)
	;also check that minx >= CLIPMINX, maxx <= CLIPMAXX etc
	move.w	#VEC_LINEDRAW3_CLIPMINX,d0
	move.w	#VEC_LINEDRAW3_CLIPMINY,d1

	cmp.w	d0,d5				;maxx < clipminx
	blt	.crash			;off scr
	cmp.w	d1,d6				;maxy < clipminy
	blt	.crash			;off scr

	cmp.w	d0,d3				;minx > clipminx
	bge.s	.minxok
	move.w	d0,d3				;clip minx
	move.w	#1,.VEC_ClipCheck_Flag		;flag clip
.minxok:
	cmp.w	d1,d4				;miny > clipminy
	bge.s	.minyok
	move.w	d1,d4				;reset miny
	move.w	#1,.VEC_ClipCheck_Flag	;flag clip
.minyok:

	move.w	#VEC_LINEDRAW3_CLIPMAXX,d0
	move.w	#VEC_LINEDRAW3_CLIPMAXY,d1

	cmp.w	d0,d3				;minx > clipmaxx
	bgt	.crash			;off scr
	cmp.w	d1,d4				;miny < clipmaxy
	bgt	.crash			;off scr

	cmp.w	d0,d5				;maxx < clipmaxx
	ble.s	.maxxok
	move.w	d0,d5				;clip maxx
	move.w	#1,.VEC_ClipCheck_Flag	;flag clip
.maxxok:
	cmp.w	d1,d6				;maxy < clipmaxy
	ble.s	.maxyok
	move.w	d1,d6				;reset maxy
	move.w	#1,.VEC_ClipCheck_Flag	;flag clip
.maxyok:

	tst.w	.VEC_ClipCheck_Flag
	bne.s	.crash
.exit:
	rts

.crash:
	bra.s	.crash
	rts

.VEC_ClipCheck_Flag:
	dc.w 	0

	ENDC	;CLIPCHECK

*****************************************************************************
* Calculates the what faces are visible.
*
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d7/a0-a4
*****************************************************************************

;hidden line equation
;			(x2-x1)(y3-y2)-(y2-y1)(x3-x2)			    

Calc_Visible_Faces:
	move.l	CTRL_OBJECTINFO_PTR(a5),a4	;Current object
	move.l	VEC_OBJ_FACELIST_PTR(a4),a4	;Facelist for this object

	move.w	(a4)+,d6		;num faces-1
	bmi.s	.exit			;no faces?

	lea	RotXYZpts+2(pc),a2	;skip numpts
	lea	Visible_Face_Buffer(pc),a1
	lea	2(a1),a0		;skip number of faces

	moveq	#0,d7			;num face to be drawn
.hloop:	
	move.l	(a4)+,a3		;a3=adr of face data
	
	movem.w	VEC_FACE_VERTICES(a3),d0-d2	;indexes for x1,y1,x2,y2,x3,y3

	move.w	(a2,d1.w),d3		;x2
	sub.w	(a2,d0.w),d3		;(x2-x1)
	move.w	2(a2,d2.w),d4		;y3
	sub.w	2(a2,d1.w),d4		;(y3-y2)
	muls	d4,d3			;(x2-x1)(y3-y2)
	
	move.w	2(a2,d1.w),d4		;y2		
	sub.w	2(a2,d0.w),d4		;(y2-y1)
	move.w	(a2,d2.w),d5		;x3
	sub.w	(a2,d1.w),d5		;(x3-x2)
	muls	d5,d4			;(y2-y1)(x3-x2)
	
	sub.l	d4,d3
	ble.b	.backface		;back face

.frontface:
	moveq	#0,d0			;front face flag = 0
	bra.s	.store
.backface:
	; If backface check the backface paper col, if negative then cull it
	; otherwise add it to the draw list so it can be drawn with alt colors
	; Note: leave color00, can do interesting things when used in complex vectors
	move.b	VEC_FACE_PAPER_BACKFACE(a3),d0
	bmi.s	.cull
	moveq	#1,d0			;backface flag = 1
.store:
	move.l	a3,(a0)+			;store face adr in visble faces buffer
	move.w	d0,VEC_FACE_FLAG_BACKFACE(a3)	;store frontface flag
	addq.w	#1,d7				;inc num of faces
.cull:
	dbf	d6,.hloop	

	move.w	d7,(a1)			;store num faces at start of Visble_Face_Buffer

.exit:
	rts


*****************************************************************************
* Draw a simple object.
*
* IN:		a6
* OUT:		
* TRASHED:	d0-d7/a0-a4
*****************************************************************************

DrawObject_Vars:
	dc.l	LineDraw3_Vars		;Line draw variables (premult, color, octants)
	dc.l	Visible_Face_Buffer	;a4
	dc.l	RotXYZpts+2		;a5 miss numpts

DrawObject:
	move.l	a5,-(sp)

	movem.l	DrawObject_Vars(pc),a1/a4/a5

	move.w	(a4)+,d7		;num faces
	beq.s	.exit			;no faces to draw
	subq.w	#1,d7			;correct counter for dbf
	;move.l	#BPL_BOBS_Log,a2		;adr of screen
	move.l	BOB_Frame_Screen_Ptr(pc),a2

	;Setup line draw blit registers that don't change
	moveq	#-1,d0
	WAITBLIT_NASTY_A6
	move.l	d0,bltafwm(a6)		;mask
	move.w	#VEC_BOB_BYTEWIDTH*VEC_BOB_NUMPLANES,bltcmod(a6)	;modulo interleaved
	move.l	#$ffff8000,bltbdat(a6)
	; --------

.faceloop:	
	swap	d7			;save face counter

	move.l	(a4)+,a3		;a3=adr of face data
	move.w	VEC_FACE_FLAG_BACKFACE(a3),d0		;VISIBLE_FACE_BACKFACE flag (0 = normal, 1 = backface)
	add.w	d0,d0			;offset to VEC_FACE_PAPER or VEC_FACE_PAPER_BACKFACE

	move.w	(a3)+,d7		;num lines to draw
	;a3 = VEC_FACE_PAPER
	moveq	#0,d5			;clear d5
	move.b	(a3,d0.w),d5		;paper.b , ink.b (of front or back face)
	move.w  d5,LINEDRAW3_COLOR(a1)	;save color for linedraw routine
				
	lea	(VEC_FACE_VERTICES-VEC_FACE_PAPER)(a3),a3	;Skip to Connections
.lineloop:
	move.l	a2,a0			;restore screen ptr
	move.w	(a3)+,d1		;index1
	move.w	(a3),d3			;index2
	movem.w	(a5,d1.w),d0-d1		;x1,y1
	movem.w	(a5,d3.w),d2-d3		;x2,y2
	bsr.s	LineDraw3_nBpl_DrawFilled	;I:d0-d3/a0/a1/a6, T:d0-d4/a0

	dbf	d7,.lineloop		;more lines

	swap	d7			;restore face counter
	dbf	d7,.faceloop		;next face

.exit	
	move.l	(sp)+,a5
	rts				;byebye


*****************************************************************************
* LineDraw3 For filled vectors. Best when only drawing a line once.
* Single bitplanes / inconvex etc. 
* Coords are not saved as assumes that the next line will be different anyway.
*****************************************************************************

*****************************************************************************
* Calcs and draws a line in one go. Multiple bitplanes.
* Routine ensures that y2>y1 and draws from top of screen to bottom.
* Two ways of removing a corner (for blitter fill):
* - Point bltdpth to a blank word. This causes the FIRST pixel (top) to be
* not written to scree. In this case first scanline is never touched.
* - Reduce dy by one, which seems a kludge but seems to work. In this case
* the last scanline (bottom) is not drawn.
*
* IN:		a6, _custom)
*		a0, screen address
*		a1, linedraw vars
*			linedraw premult table must match the modulo
*		d0-d3, x,y,x2,y2 of the line to draw
* OUT:		
* TRASHED:	d0-d4/a0
*****************************************************************************

LineDraw3_nBpl_DrawFilled:

	;The linedraw routine draws from top of screen to bottom. We want
	;y2 > y1 for the logic used.
	sub.w 	d1,d3			;dy
	beq	.NoDrawLine		;skip when y1=y2
	bpl.b	.line1			;ensure y2>y1
	exg	d0,d2				
	neg.w	d3			
	sub.w	d3,d1			
.line1:
	subq	#1,d3			;dy-1 for blit corner fix
					;replaced with bltdpth scratch space

	moveq	#15,d4			;for AND, also clear top of d4
	and.w	d0,d4			;x & 15
	ror.w	#4,d4			;get shift value in top of word
	ori.w	#$a4a,d4		;inverted line, d4=bltcon0
	swap	d4			;swap to top, bottom is clear (for move.b later)

	sub.w	d0,d2			;dx
	bpl.b	.line2
	neg.w	d2			;make dx postive
	addq	#2,d4
.line2:
	;lsr.w	#3,d0
	;mulu.w	#BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES,d1
	;add.w	d0,d1
	;lea	(a0,d1.w),a0
	lsr.w	#3,d0			;x to byte offset
	add.w	d1,d1			;table access offset in words
	add.w	LINEDRAW3_MULTTABLE(a1,d1.w),d0	;mult lookup and add to byte offset
	lea	(a0,d0.w),a0		;a0 is final screen address

	cmp.w	d2,d3
	bmi.b	.line3
	addq	#1,d4					
	exg	d2,d3
.line3:
	move.w	d3,d1			;dy
	add.w	d1,d1			;2*dy; bltbmod
	swap	d1			;bltbmod in top word
	move.w	d3,d1
	sub.w	d2,d1			;dy-dx
	add.w	d1,d1			;2*(dy-dx) ; bltamod	
					;d1.l = bltbmod/bltamod
	add.w	d3,d3
	sub.w	d2,d3			;(2*dy)-dx ; bltaptl
	addx.w	d4,d4
	move.b	LINEDRAW3_OCTANTS(a1,d4.w),d4	;d4.l now bltcon0/1

	addq.w	#1,d2			;dx+1
	lsl.w	#6,d2			;move to bits 15-6
	addq.w	#2,d2			;bltsize

	move.w  LINEDRAW3_COLOR(a1),d0	;get required color
.colorloop
	btst    #0,d0
	beq.s   .nextbpl

	;Blit nasty seems to hurt in all tested cases...
	WAITBLIT_A6
	;WAITBLIT_NASTY_A6

	; NOTE: These three lines can be moved out if drawing loads of lines at once
	;move.l	#-1,bltafwm(a6)		;mask
	;move.w	#BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES,bltcmod(a6)	;modulo, DMOD NOT required
	;move.l	#$ffff8000,bltbdat(a6)
	; --------
		
	move.l	d4,bltcon0(a6)		;bltcon0/bltcon1
	move.l	d1,bltbmod(a6) 		;bltbmod/bltamod
	move.l  a0,bltcpth(a6) 
	move.w 	d3,bltaptl(a6)
	move.l  a0,bltdpth(a6) 
	;move.l	#FW_LineDraw_Scratch,bltdpth(a6) ;First pixel is written here (offscreen)
	move.w	d2,bltsize(a6)
.nextbpl:
	;lea     VEC_BOB_BYTEWIDTH(a0),a0	;next bitplane
	addq.l	#VEC_BOB_BYTEWIDTH,a0		;next bitplane
	lsr.w   #1,d0				;check next bit 
	bne.s   .colorloop			;any colors left?
.NoDrawLine:	
	rts

;Linedraw variable structure
	RSRESET
LINEDRAW3_OCTANTS:	rs.b 8			;8 Octant byte values
LINEDRAW3_COLOR:	rs.w 1			;Color of lines
LINEDRAW3_CLIP_MINX	rs.w 1
LINEDRAW3_CLIP_MINY	rs.w 1
LINEDRAW3_CLIP_MAXX	rs.w 1
LINEDRAW3_CLIP_MAXY	rs.w 1
LINEDRAW3_MULTTABLE:	rs.w BPL_BUF_HEIGHT	;Y offsets premultiplied

	EVEN
LineDraw3_Vars:
.octantbase = 3				;1=normal,3=1px per line (fill)
.octanttab:
	dc.b	$10+.octantbase		;LINEDRAW3_OCTANTS
	dc.b	$50+.octantbase
	dc.b	$00+.octantbase
	dc.b	$40+.octantbase
	dc.b	$14+.octantbase
	dc.b	$54+.octantbase
	dc.b	$08+.octantbase
	dc.b	$48+.octantbase

	dc.w    0       		; LINEDRAW3_COLOR

	dc.w	LINEDRAW3_CLIP_MINX
	dc.w	LINEDRAW3_CLIP_MINY
	dc.w	LINEDRAW3_CLIP_MAXX
	dc.w	LINEDRAW3_CLIP_MAXY

; Bob height mult by bob bytewidth and number of bitplanes (interleaved)
Mult_BOB_Height_ByteWidth_NumPlanes:
	ds.w	VEC_BOB_HEIGHT


*****************************************************************************

BOB_Frame_Num:		dc.w	0		;The frame number to draw vector on
BOB_Frame_Screen_Ptr:	dc.l	0		;The address of the draw screen
BOB_FrameLookup_Ptr:	dc.l	0		;Address of current frame lookup

	RSRESET
BOB_FRAME_FILL_BPLPTR:		rs.l	1	;bobs address
BOB_FRAME_FILL_MINY:		rs.w	1
BOB_FRAME_FILL_HEIGHT:		rs.w	1
BOB_FRAME_FILL_WORDWIDTH:	rs.w	1
BOB_FRAME_FILL_BYTEMODULO:	rs.w	1
BOB_FRAME_FILL_BOB_PTR:		rs.l	1	;if 0 then do not use
BOB_FRAME_FILL_BLTSIZE:		rs.w	1
BOB_FRAME_COPY_BLTSIZE:		rs.w	1
BOB_FRAME_COPY_SCR_OFFSET:	rs.l	1	;Offset into screen (ascending mode)
BOB_FRAME_COPY_BOB_PTR:		rs.l	1	;Offset into BOB (ascending mode)

BOB_FRAME_SIZEOF:		rs.w	0

; Lookup table of screen addresses for each bob
BOB_FrameLookup:
a set 0
	REPT	BOB_NUMFRAMES
		dc.l	BPL_BOBS_Log+(a*VEC_BOB_TOTALSIZE)
		;dc.l	BPL_BOBS_Log
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.w	0		;bltsize fill
		dc.w	0		;bltsize copy
		dc.l	0
		dc.l	0
a set a+1
	ENDR

*****************************************************************************

BOB_SINE_NUMENTRIES = 512	; Must be power of 2
BOB_SINE_OFFSET_MASK = ((BOB_SINE_NUMENTRIES*2)-2)	; Byte offset access into the table, forced to be even 

;BOB is 32px wide and we need extra word for shifting so max X value
;for 320 wide screen uses byte 34,35,36,37,38,39. Max X for byte 34 is 287 (rsh 4, *2)
;287 will give a shift of 15 so 32px bob will end up right border.
;0-287 range
;Half because we adding two sines together = 143
;0-143 = 0-318

BOB_Sine_X:
	INCLUDE "sine_0_143_512_words.i"

; Main area is 192px, but we want to go 16 px into top and bottom so
; assume 224px
;BOB is 32px high so 223-32 = 191
;we have 2 sines that can be added together so need to half again = 95
;Top min is 16 
;8-103
BOB_Sine_Y_PreMult:
	INCLUDE "sine_8_103_512_words.i"

BOBSnake_Sine1_X_Offset:
	dc.w	0
BOBSnake_Sine2_X_Offset:
	dc.w	0
BOBSnake_Sine1_X_Speed
	dc.w	2
BOBSnake_Sine1_X_Step
	dc.w	244
BOBSnake_Sine2_X_Speed
	dc.w	2
BOBSnake_Sine2_X_Step
	dc.w	126

BOBSnake_Sine1_Y_Offset:
	dc.w	0
BOBSnake_Sine2_Y_Offset:
	dc.w	0
BOBSnake_Sine1_Y_Speed
	dc.w	2
BOBSnake_Sine1_Y_Step
	dc.w	100
BOBSnake_Sine2_Y_Speed
	dc.w	2
BOBSnake_Sine2_Y_Step
	dc.w	28

; list contains x,y values
; X in pixels
; Y in Y*Screen_ByteWidth (for speed)
BOBSnake_DrawList_X:
	REPT	BOB_NUMBOBS
	dc.w	0,0
	ENDR
BOBSnake_DrawList_Y:
	REPT	BOB_NUMBOBS
	dc.w	0,0
	ENDR

*****************************************************************************

;Obj_Glenz24_Info:
;	dc.w	0			; initialised (happens on first load)
;	dc.w	0,0,0			; pos, x,y,z
;	dc.w	0,0,0			; current rotation, x,y,z
;	dc.w	1,2,3			; Rotation step, x,y,z
;	dc.w	0			; Complex 1/0
;	dc.w	1			; Num frames max
;	dc.l	Obj_Glenz24_PtsBuffer	; Pts ptr (in use/buffer)
;	dc.l	Obj_Glenz24_Pts		; Initial points ptr
;	dc.l	Obj_Glenz24_Facelist	; Facelist ptr
;	dc.w	0,0,0			; Morph active flag, counter, speed

	RSRESET
VEC_OBJ_INITIALIZED:	rs.w	1
VEC_OBJ_POSX:		rs.w	1
VEC_OBJ_POSY:		rs.w	1
VEC_OBJ_POSZ:		rs.w	1
VEC_OBJ_THETA_X:	rs.w	1
VEC_OBJ_THETA_Y:	rs.w	1
VEC_OBJ_THETA_Z:	rs.w	1
VEC_OBJ_THETA_DX:	rs.w	1
VEC_OBJ_THETA_DY:	rs.w	1
VEC_OBJ_THETA_DZ:	rs.w	1
VEC_OBJ_COMPLEX:	rs.w	1
VEC_OBJ_NUMFRAMES:	rs.w	1
VEC_OBJ_PTS_PTR:	rs.l	1
VEC_OBJ_PTSINITIAL_PTR:	rs.l	1
VEC_OBJ_FACELIST_PTR:	rs.l	1
VEC_OBJ_MORPH_ACTIVE:	rs.w	1
VEC_OBJ_MORPH_COUNTER	rs.w	1
VEC_OBJ_MORPH_SPEED:	rs.w	1
VEC_OBJ_SIZEOF:		rs.w	0


*****************************************************************************

VEC_FACE_NUMLINES = 0
VEC_FACE_PAPER = 2
VEC_FACE_INK = 3
VEC_FACE_PAPER_BACKFACE = 4
VEC_FACE_INK_BACKFACE = 5
VEC_FACE_FLAG_BACKFACE = 6
VEC_FACE_MINX = 8
VEC_FACE_MINY = 10
VEC_FACE_MAXX = 12
VEC_FACE_MAXY = 14
VEC_FACE_AVG_Z = 16
VEC_FACE_VERTICES = 18

;connect pt 2 to pt 2, *6 is the byte offset in the Rotated pts struct.
VEC_CON		MACRO
		dc.w	\1*6,\2*6
		ENDM

VEC_VERTEX1	MACRO
		dc.w	\1*6
		ENDM

VEC_VERTEX2	MACRO
		dc.w	\1*6,\2*6
		ENDM

VEC_VERTEX3	MACRO
		dc.w	\1*6,\2*6,\3*6
		ENDM

VEC_VERTEX4	MACRO
		dc.w	\1*6,\2*6,\3*6,\4*6
		ENDM

VEC_VERTEX5	MACRO
		dc.w	\1*6,\2*6,\3*6,\4*6,\5*6
		ENDM

VEC_FACE	MACRO			;define face pts,col
		dc.w	\1		;number of lines
		dc.b	\2,\3		;paper, ink (visible)
		dc.b	\4,\5		;paper, ink (backface), paper=-1 to just cull
		dc.w	0		;0/1 - is currently a backface after hidden line calc
		dc.w	0,0,0,0		;minx,y,maxx,y
		dc.w	0		;current average z for this face
		ENDM

; Note: Fastest colors are 1,2,4, then 3,5,6, then 7
; Also features stipple colours, that is, when you
; define a face 2 colours are entered first the
; PAPER & then the INK!
; n.b. if a stipple with col00 is needed then
; make sure that the PAPER is set to 0 and not
; the INK as it is slightly faster this way!

*****************************************************************************

*** Objects and Demo Sequencing ***

	; The actual demo sequences
	INCLUDE "ControllerScript.i"


*****************************************************************************

; Screen height mult by screen bytewidth and number of bitplanes (interleaved)
Mult_SCR_Height_ByteWidth_NumPlanes:
	ds.w	BPL_BUF_HEIGHT

*****************************************************************************

VEC_MAX_PTS	= 64
VEC_MAX_FACES	= 48

RotXYZpts:
	ds.w	VEC_MAX_PTS*3			;rotated 64 pts 3d

; Visible face buffer is a list of pointer to face data structures.
; Room for 64 faces
Visible_Face_Buffer:
	ds.w	1		; num faces
	ds.l	VEC_MAX_FACES	; face ptr.l


*****************************************************************************
*****************************************************************************
*****************************************************************************

	SECTION	IntroFramework_ChipData_Copper,DATA_C	;Chip Data Section for gfx/music

*****************************************************************************

; Copper horizontal blanking notes from Photon/Scoopex
; As established, positions $e7...$03 are not usable. If you're writing a simple 
; copperlist with no need for tight timing, positions $df and $07 are conventionally 
;used for the positions on either side of the horizontal blanking, and for 
; compatibility across chipsets use increments of 4 from these, resulting in 
;positions $db, $0b, and so on.

; This value is used for loading the CL with colours and stuff, we might need
; to do it more than once, for example fading colours in a double-buffered CL
; we need to write the values 2 twice so that after finishing both CLs have been
; Updated. 
; Set to 1 for single CL, 2 for double-buffered, etc
CL_NUM_BUFFERS = 2

*****************************************************************************

; The main CL bobs and reflection. Double-buffered

P0_CL_Phys:
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	CMOVE 	bplcon0,$3200		;3bpl/8 cols
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0000
	
	CMOVE	dmacon,DMAF_SPRITE	;Disable sprites

	;CWAIT	25-1,$7		;Time for altering Copperlist

P0_CL_Cols_Refl_Top:
	CMOVE	tmpcolor00,$001
	CMOVE	color01,$fff
	CMOVE	color02,$f00
	CMOVE	color03,$0f0
	CMOVE	color04,$00f
	CMOVE	color05,$f0f
	CMOVE	color06,$ff0
	CMOVE	color07,$0ff

	CWAIT	DIW_V,$7
P0_CL_Bpl_Refl_Top:
	CMOVE	bpl1pth,$0
	CMOVE	bpl1ptl,$0
	CMOVE	bpl2pth,$0
	CMOVE	bpl2ptl,$0
	CMOVE	bpl3pth,$0
	CMOVE	bpl3ptl,$0
	CMOVE 	bpl1mod,BPL_BPLMOD_REPTPREVLINE	;interleaved,mirror
	CMOVE 	bpl2mod,BPL_BPLMOD_REPTPREVLINE

	;Wait until after reflection area to change to main colors
	;The last few colors will be too late, but it's fine
	;The bplmod are set last as only need to be set by end of line
	CWAIT	DIW_V_MAIN,$7
P0_CL_Bpl:			;Bitplane pointers
	CMOVE	bpl1pth,$0
	CMOVE	bpl1ptl,$0
	CMOVE	bpl2pth,$0
	CMOVE	bpl2ptl,$0
	CMOVE	bpl3pth,$0
	CMOVE	bpl3ptl,$0
P0_CL_Cols:
	CMOVE	tmpcolor00,$000
	CMOVE	color01,$fff
	CMOVE	color02,$f00
	CMOVE	color03,$0f0
	CMOVE	color04,$00f
	CMOVE	color05,$f0f
	CMOVE	color06,$ff0
	CMOVE	color07,$0ff

	CMOVE 	bpl1mod,BPL_BPLMOD	;interleaved mode
	CMOVE 	bpl2mod,BPL_BPLMOD


	IF DIW_V_REFL_BOT>255
		CWAIT	255,$e1			;Into PAL area
	ENDIF

	; For perfect reflection (interleaved):
	; Modulo is added at DDFStop so need to set before then.
	; Before DDFStart at last line of display set modulo to repeat that line
	; -BPL_BUF_BYTEWIDTH
	; Before DDFStart on first line of reflection
	; (-(BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES))-BPL_BUF_BYTEWIDTH

a set DIW_V_REFL_BOT-1		;Have to set modulo at start of last line
	CWAIT	a&$ff,$7		;modulo only added at DDF stop
	CMOVE 	bpl1mod,BPL_BPLMOD_REPTLINE
	CMOVE 	bpl2mod,BPL_BPLMOD_REPTLINE

a set DIW_V_REFL_BOT			;Then change colors at first reflection line
	CWAIT	a&$ff,$7
	CMOVE 	bpl1mod,BPL_BPLMOD_REPTPREVLINE
	CMOVE 	bpl2mod,BPL_BPLMOD_REPTPREVLINE

P0_CL_Cols_Refl_Bot:
	CMOVE	tmpcolor00,$001
	CMOVE	color01,$fff
	CMOVE	color02,$f00
	CMOVE	color03,$0f0
	CMOVE	color04,$00f
	CMOVE	color05,$f0f
	CMOVE	color06,$ff0
	CMOVE	color07,$0ff

	; Trigger copper interrupt
	IFNE FW_IRQ_TYPE_COPPER
		IF DIW_V_REFL_BOT<=255&&SCANLINE_EOF>255
			CWAIT	255,$e1
		ENDIF
		CWAIT	SCANLINE_EOF&$ff,$7
		CMOVE	intreq,INTF_SETCLR|INTF_COPER
	ENDC

	COPPEREND
P0_CL_End:

P0_CL_BPL_REFL_TOP_OFFSET = (P0_CL_Bpl_Refl_Top-P0_CL_Phys)
P0_CL_BPL_OFFSET = (P0_CL_Bpl-P0_CL_Phys)
P0_CL_COL_OFFSET = (P0_CL_Cols-P0_CL_Phys)
P0_CL_COL_REFL_TOP_OFFSET = (P0_CL_Cols_Refl_Top-P0_CL_Phys)
P0_CL_COL_REFL_BOT_OFFSET = (P0_CL_Cols_Refl_Bot-P0_CL_Phys)
P0_CL_SIZE = P0_CL_End-P0_CL_Phys


*****************************************************************************
*****************************************************************************
*****************************************************************************

; ---------
; Map Chip to shared buffer
; ---------

CUR_CHIP_BUF set FW_Chip_Buffer_1

BPL_Phys		=	CUR_CHIP_BUF	;3bpl 320x256 = 30KB
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_Log1		=	CUR_CHIP_BUF	;3bpl = 30KB
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_BOBS_Log		=	CUR_CHIP_BUF	;BOB_NUMFRAMES*VEC_BOB_TOTALSIZE = 98304, 98KB
CUR_CHIP_BUF set CUR_CHIP_BUF+(BOB_NUMFRAMES*VEC_BOB_TOTALSIZE)

P0_CL_Log1		=	CUR_CHIP_BUF	;256B
CUR_CHIP_BUF set CUR_CHIP_BUF+P0_CL_SIZE

;Total: ~160KB

*****************************************************************************
