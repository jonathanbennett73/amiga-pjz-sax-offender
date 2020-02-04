*****************************************************************************

; Name			: ExplodeVector.s
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com)
; Description		: Explode vector
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

	INCLUDE "../IntroConfig.i"
	INCLUDE	"../Framework/CustomExtra.i"
	INCLUDE "../Framework/CustomMacros.i"
	INCLUDE "../Framework/IntroLibrary.i"

; Additional external symbols 
	xref	FW_CheckUserQuitSignal_A6
	xref	FW_ClearBuffer_BlitCPU_A6
	xref	FW_ClearBuffer_CPU
	xref	FW_CopyBuffer_CPU
	xref	FW_InitCopperBplPtrs
	xref	FW_InitCopperColsFromPalette
	xref	FW_LineDraw_Scratch
	xref	FW_Quit_Flag
	xref	FW_SetBaseCopperAndDma_A6
	xref	FW_SetBaseCopperAndLev3Irq_A6
	xref	FW_SetBaseLev3Irq
	xref	FW_SetCopperAndLev3Irq_A6
	xref	FW_SetLev3Irq
	xref	FW_WaitFrame
	xref	FW_WaitRaster_A6
	xref 	FW_VBlankProxy

	xref	LIB_RGB12_Interpolate_Fast
	xref	LIB_RGB12_Interpolate_Fast_Palette
	xref	LIB_SIN_Q15_1024W_Table
	xref	LIB_COS_Q15_1024W_Table

	xref	FW_Chip_Buffer_1
	xref	FW_Chip_Buffer_1_End
	xref	BPL_Zoom_Pattern


*****************************************************************************

	SECTION	ExplodeVector_PublicCode,CODE	;Code section in Public memory

*****************************************************************************

; Photon:
; Best practice is to start with DDF 38,d0 and DIW 2c81,2cc1 and modify these 
; symmetrically and always in whole 16px steps for compatibility.
; Note DDF start of less than or equal to 30 and you start to lose sprites.

*** Display Windows ***

;P0 - 320px width but We have 16 pixels extra DDF at the left for the horinzontal scroll
P0_DIW_V		=	$2c		;Hardware Vstart ($2c normal, $24 overscan)
P0_DIW_H		=	$81		;Hardware Hstart ($81 normal, $71 overscan)
P0_DIW_WIDTH		=	320		;Pixels		 (multiple of 16, 320 normal, 352 overscan)
P0_DIW_HEIGHT		=	256		;Lines		 (256 normal PAL, 272 overscan)

P0_DDF_H		=	$71		;Hardware Hstart ($81 normal, $71 overscan)
P0_DDF_WIDTH		=	336		;Pixels		 (320 normal pal, 352 overscan)
P0_DDF_BYTEWIDTH	=	P0_DDF_WIDTH/8	;Bytes
P0_DDF_WORDWIDTH	=	P0_DDF_BYTEWIDTH/2	;Words

;P1 - Use the full 352px width of the buffer for slicker transition out. Also smaller
;in height by 64 lines (32 top, 32 bottom)
P1_DIW_V		=	$2c+P1_BPL_YOFFSET_TOP		;Hardware Vstart ($2c normal, $24 overscan)
P1_DIW_H		=	$71		;Hardware Hstart ($81 normal, $71 overscan)
P1_DIW_WIDTH		=	352		;Pixels		 (multiple of 16, 320 normal, 352 overscan)
P1_DIW_HEIGHT		=	256-(P1_BPL_YOFFSET_TOP+P1_BPL_YOFFSET_BOT);Lines		 (256 normal PAL, 272 overscan)

P1_DDF_H		=	$71		;Hardware Hstart ($81 normal, $71 overscan)
P1_DDF_WIDTH		=	352		;Pixels		 (320 normal pal, 352 overscan)
P1_DDF_BYTEWIDTH	=	P1_DDF_WIDTH/8	;Bytes
P1_DDF_WORDWIDTH	=	P1_DDF_BYTEWIDTH/2	;Words

*****************************************************************************

*** Screen Buffers  ***

;How far in from the BUF area is the draw area
BPL_XOFFSET		=	16	
BPL_XOFFSET_BYTES	=	BPL_XOFFSET/8

;In P1 we shrink the screen 32 pixels top and bottom for fade out effect
P1_BPL_YOFFSET_TOP	=	32
P1_BPL_YOFFSET_BOT	=	32

;The screen area front and back buffer
BPL_BUF_WIDTH		=	352
BPL_BUF_BYTEWIDTH	=	BPL_BUF_WIDTH/8
BPL_BUF_WORDWIDTH	=	BPL_BUF_BYTEWIDTH/2
BPL_BUF_HEIGHT		=	256
BPL_BUF_NUMPLANES	=	2
BPL_BUF_NUMCOLS 	= 	(1<<BPL_BUF_NUMPLANES)
BPL_BUF_SIZE		=	BPL_BUF_BYTEWIDTH*BPL_BUF_HEIGHT
BPL_BUF_TOTALSIZE	=	BPL_BUF_SIZE*BPL_BUF_NUMPLANES

;draw area
BPL_WIDTH		=	320
BPL_BYTEWIDTH		=	BPL_WIDTH/8
BPL_WORDWIDTH		=	BPL_BYTEWIDTH/2
BPL_HEIGHT		=	BPL_BUF_HEIGHT
BPL_NUMPLANES		=	BPL_BUF_NUMPLANES
BPL_NUMCOLS 		= 	(1<<BPL_NUMPLANES)
BPL_SIZE		=	BPL_BYTEWIDTH*BPL_HEIGHT
BPL_TOTALSIZE		=	BPL_SIZE*BPL_NUMPLANES


;BPL1 is normal interleaved, BPL2 is repeat previous line (zoom bitmap)
P0_SCR_BPL1MOD		=	(BPL_BUF_BYTEWIDTH-P0_DDF_BYTEWIDTH)+(BPL_BUF_BYTEWIDTH*(BPL_BUF_NUMPLANES-1))
P0_SCR_BPL2MOD		=	-P0_DDF_BYTEWIDTH

P1_SCR_BPLCON0		=	$2200
P1_SCR_BPL1MOD		=	(BPL_BUF_BYTEWIDTH-P1_DDF_BYTEWIDTH)+(BPL_BUF_BYTEWIDTH*(BPL_BUF_NUMPLANES-1))
P1_SCR_BPL2MOD		=	P1_SCR_BPL1MOD

SCANLINE_EOF		=	(P0_DIW_V+P0_DIW_HEIGHT)-1	; Safe to starting clearing after this scanline


*****************************************************************************

; We use one "dark" color to indicate the border color in the last phase for nice transition.
; Must be even though because we copy in longword batches. That is why PAL_NUMCOLS_DARK=2
PAL_NUMCOLS_MAIN	= 8		; number of main colour entries in our palettes
PAL_NUMCOLS_DARK	= 2		; number of dark/refl cols
PAL_NUMCOLS_ALL		= (PAL_NUMCOLS_MAIN+PAL_NUMCOLS_DARK)

*****************************************************************************

VEC_XOFFSET		=	(BPL_WIDTH/2)+BPL_XOFFSET	;16 pixels to the left is not shown
VEC_YOFFSET		=	BPL_HEIGHT/2
VEC_ZOFFSET		=	256

P0_CLIPMINX		=	BPL_XOFFSET
P0_CLIPMAXX		=	(BPL_WIDTH-1)+BPL_XOFFSET
P0_CLIPMINY		=	0
P0_CLIPMAXY		=	BPL_HEIGHT-1

P1_CLIPMINX		=	0
P1_CLIPMAXX		=	(BPL_WIDTH-1)+BPL_XOFFSET+BPL_XOFFSET
P1_CLIPMINY		=	(0+P1_BPL_YOFFSET_TOP)-1
P1_CLIPMAXY		=	((BPL_HEIGHT-1)-P1_BPL_YOFFSET_BOT)
; Our line draw reduces height to avoid blitter fill issues, so it never draws
; the topmost pixel of a line. Therefore we need to extend the P1_CLIPMMINY here by -1
; line so that we get it to fit perfectly with the copper color border.
; The buffer is P1_BPL_YOFFSET lines larger so this is ok.

VEC_CLIPPING		=	1	; Slighty quicker with no clipping
VEC_CLIPCHECK		=	0	; Will write 1 into VEC_ClipCheck_Flag if clipping occured
VEC_DRAW_BOUNDING	=	0	; Draw bounding box for debugging
VEC_LIGHTSOURCE		=	1	; Basic light source

*****************************************************************************

;Zoom pattern is 352 width so have to alter offsets to center in our 334x256 window
ZOOM_PATTERN_X_BYTEOFFSET = 0	;We have 16 pixels extra on left so matches the 352 wide pattern (16+320+16)
ZOOM_PATTERN_Y_BYTEOFFSET = 6	;352 vs 256 high = 6 extra bytes to work for a 256 high screen

ZOOM_BPLCON0 = $3200	;2bpl screen, 1bpl pattern


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

	xdef	VECEXP_Start
VECEXP_Start:
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

	;Change to transition out CL for colors/border to match next effect
	jsr	FW_SetBaseLev3Irq
	move.l 	#TOUT_CL_Phys,cop1lch(a6)
	jsr	FW_WaitFrame		;Take effect
	jsr	FW_WaitFrame		;Take effect

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
	xdef	VECEXP_PreCalc_IntroStart
	ENDC
	
SubPartPreCalc_IntroStart:	
VECEXP_PreCalc_IntroStart:
	tst.w	Controller_Info+CTRL_PRECALC_INTROSTART_DONE
	bne.s	.exit			;already done

	movem.l	d2-d7/a2-a6,-(sp)	;save

	;Start

	; Screem height premult
	bsr	Calc_PreMult_ScreenHeight	;T:d0-d3/a0

	; Calculate x and y expansion details
	bsr	Calc_Zoom_Bitmaps

	; Convert sine table to BPLCON shifts
	bsr	Calc_HorizScroll_BPLCON

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
	move.w	d0,CTRL_PALETTE_ACTIVE(a5)
	move.w	d0,CTRL_ROT_CHANGE_ACTIVE(a5)
	move.w	d0,CTRL_MOVE_ACTIVE(a5)
	move.w	d0,CTRL_MOVE2_ACTIVE(a5)
	move.w	#1,CTRL_FRAMES_MIN(a5)

	; Reset fill/clear buffers
	lea	SCR_Fill_Phys_Details(pc),a0
	clr.l	SCR_FILL_SRCPTR(a0)
	move.l	a0,SCR_Fill_Phys_Details_Ptr

	lea	SCR_Fill_Log_Details(pc),a0
	clr.l	SCR_FILL_SRCPTR(a0)
	move.l	a0,SCR_Fill_Log_Details_Ptr

	; Reset clipping size
	lea	LineDraw3_Vars(pc),a0
	move.w	#P0_CLIPMINX,LINEDRAW3_CLIP_MINX(a0)
	move.w	#P0_CLIPMINY,LINEDRAW3_CLIP_MINY(a0)
	move.w	#P0_CLIPMAXX,LINEDRAW3_CLIP_MAXX(a0)
	move.w	#P0_CLIPMAXY,LINEDRAW3_CLIP_MAXY(a0)

	; Set default zoom
	moveq	#0,d0
	moveq	#0,d1
	bsr	Controller_FX_Zoom	;I:d0-d1/a5, T:d0-d1

	; Load default palette
	lea	PAL_Default(pc),a0
	moveq	#0,d0
	bsr	Controller_FX_Palette	;I:d0/a0/a5, T:d0/a0-a1
	
	; Preload first object
	lea	Obj_ExplodeCube6_Info(pc),a0
	bsr	Controller_FX_Load	;I:a0/a5, T:d0-d3/a0-a2

	;Ensure valid points in buffers prior to first loop by doing a 
	;rotate/perspective/hiddenline. This is because we usually
	;do this at the END of the frame during blitter fill. Would crash on
	;first run if this not here.
	bsr	Calc_Rotation_Matrix_For_Object	;I:a5
	bsr 	RotateAndPerspective		;I:a5
	bsr	Calc_Visible_Faces		;I:a5
	bsr	Zoom_X_Axis			;I:a5, T:d0-d5/a0

	; Clear all screen buffers
	bsr	Clear_ScreenBuffers_A6		;I:a6, T:-

	;load current palette (default) into work CL and swap so that it's immediately live
	bsr	P0_CL_InitPtrs			;setup bpl ptrs and copperlists ptrs
	bsr	P0_Write_Palette_To_Copper	;load palette into work copper (including reflection)
	bsr	ScreenBufferSwap		;swap pts and make work copper live

	;Initialise our irq. Leave the copper showing previous frame and let the 
	;irq swap it. This is to minmise any blank frame. The previous routine is
	;expected to be used a different memory range.
	;move.l	CL_Phys_Ptr(pc),a0
	;lea	P0_Lev3Irq(pc),a1
	;jsr	FW_SetCopperAndLev3Irq_A6
	lea	P0_Lev3Irq(pc),a0
	jsr	FW_SetLev3Irq

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

P0_Lev3Irq:				;Blank template VERTB/COP interrupt
	TIMERON
	movem.l	d0-d7/a0-a6,-(sp)

	;All routines must preserve a5/a6
	lea	_custom,a6
	lea	Controller_Info(pc),a5

	bsr	Clear_WorkBuffer_Bounded_Blitter_A6	;I:a6, T:d0-d2/a0

	bsr	Controller_ReadCommands		;I:a5
	bsr	Controller_Perform		;I:a5

	bsr	Calc_Rotation_Matrix_For_Object	;I:a5, T:d0-d7/a0-a2
	bsr 	RotateAndPerspective		;I:a5, T:d0-d7/a0-a4
	bsr	Calc_Visible_Faces		;I:a5, T:d0-d7/a0-a4
	bsr	Zoom_X_Axis			;I:a5, T:d0-d5/a0

	; Work out the minmum size of fill and clr using post-pespective pts
	; Note must be before and in same frame (screen address) as DrawObject/Fill
	bsr	Calc_Screen_Clr_Bounding	;T:d0-d7/a0-a2

	;If the bounding fill box indicates nothing on screen then skip!
	move.l	SCR_Fill_Log_Details_Ptr(pc),a0
	tst.l	SCR_FILL_SRCPTR(a0)
	beq	.finishdrawing			;nothing to draw on screen

	bsr	DrawObject			;I:a6, T:d0-d7/a0-a4
	bsr	Fill_Screen_Bounded		;I:a6, T:d0-d3/a0

.finishdrawing:
	;Changing phase?
	;P1_Init will setup new CL and write CL and colors to new list
	;ScreenBufferSwap will make the correct CL live
	;We can skip all the lightsource/horizscroll as well as new CL
	;doesn't use it.
	cmp.w	#1,CTRL_PHASE(a5)
	bne.s	.nophasechange
	bsr	P1_Init				;I:a5/a6, T:d0-d1/a0-a1	
	bsr	P1_Write_Palette_To_Copper		;I:a5, T:a0-a2
	bra.s	.swapbuffer

.nophasechange:
	bsr	Do_Copper_Zoom			;I:a5, T:d0-d6/a0-a2
 	bsr	Do_Horiz_Scroll			;I:a5, T:d0-d4/a0-a1
	bsr	Do_LightSource_Palette		;T:d0-d7/a2/a3/a4
	bsr	P0_Write_Palette_To_Copper	;I:a5, T:a0-a2

.swapbuffer:
	;Swap buffer ptrs and setup next frame CL ptr
	bsr	ScreenBufferSwap		;I:a6, T:d0-d1/a0

	jsr	FW_VBlankProxy			;TRASHES ALL, Update framecounter, play music

	;Reset interrupt
	IFNE FW_IRQ_TYPE_COPPER
		moveq	#INTF_COPER,d0
	ELSE
		moveq	#INTF_VERTB,d0
	ENDC
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)			;A4000 compat

	movem.l	(sp)+,d0-d7/a0-a6
	WAITBLIT_NASTY				;Blitter nasty on while we finish off any blits. 
	TIMEROFF
	rte


*****************************************************************************
* Inits ptrs in the copper list.
* IN:		
* OUT:		
* TRASHED:	d0-d2/a0-a1
*****************************************************************************

P0_CL_InitPtrs:
	; Copper list buffers - copy screen list into 2nd buffer for doublebuffering
	lea	P0_CL_Phys,a0
	lea	P0_CL_Log1,a1
	move.w	#(P0_CL_SIZE/2),d0	;size in words
	jsr	FW_CopyBuffer_CPU	;I:d0/a0-a1, T:d0-d7/a0-a2

	;Screen copper BPL pointers
	lea	P0_CL_Phys,a0
	move.l	a0,CL_Phys_Ptr
	lea	P0_CL_BPL_OFFSET(a0),a0	;copper bpl pointer block
	moveq	#BPL_BUF_NUMPLANES,d0
	move.l	#BPL_Phys,d1	;in d1 for InitCopperBplPtrs
	move.l	d1,BPL_Phys_Ptr
	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	;Work copper BPL pointers
	lea	P0_CL_Log1,a0
	move.l	a0,CL_Log1_Ptr
	lea	P0_CL_BPL_OFFSET(a0),a0	;copper bpl pointer block
	moveq	#BPL_BUF_NUMPLANES,d0
	move.l	#BPL_Log1,d1	;in d1 for InitCopperBplPtrs
	move.l	d1,BPL_Log1_Ptr
	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	rts


*****************************************************************************
* Start the effect. Any precalc/slow ops should be done in the precalc code.
* Will also call PreCalc routes if not already been done (for safety)
* IN:		a6, _custom
*		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

P1_Init:
	movem.l	d2-d7/a2-a4,-(sp)

	;Reset anything that needs resetting
	move.w	#1,CTRL_PHASE(a5)

	;Init ptrs
	bsr	P1_CL_InitPtrs			;setup bpl ptrs and copperlists ptrs

	;Will need to reload palette into copper
	move.w	#CL_NUM_BUFFERS,CTRL_PALETTE_LOAD_FLAG(a5)

	;Our clip window gets smaller
	lea	LineDraw3_Vars(pc),a0
	move.w	#P1_CLIPMINX,LINEDRAW3_CLIP_MINX(a0)
	move.w	#P1_CLIPMINY,LINEDRAW3_CLIP_MINY(a0)
	move.w	#P1_CLIPMAXX,LINEDRAW3_CLIP_MAXX(a0)
	move.w	#P1_CLIPMAXY,LINEDRAW3_CLIP_MAXY(a0)

	;Initialise our lev 3 irq. ScreenBufferSwap will make new CL live
	lea	P1_Lev3Irq(pc),a0
	jsr	FW_SetLev3Irq

	movem.l	(sp)+,d2-d7/a2-a4
	rts

*****************************************************************************

P1_Lev3Irq:				;Blank template VERTB/COP interrupt
	TIMERON
	movem.l	d0-d7/a0-a6,-(sp)

	;All routines must preserve a5/a6
	lea	_custom,a6
	lea	Controller_Info(pc),a5

	bsr	Clear_WorkBuffer_Bounded_Blitter_A6	;I:a6, T:d0-d2/a0

	bsr	Controller_ReadCommands		;I:a5
	bsr	Controller_Perform		;I:a5

	bsr	Calc_Rotation_Matrix_For_Object	;I:a5, T:d0-d7/a0-a2
	bsr 	RotateAndPerspective		;I:a5, T:d0-d7/a0-a4
	bsr	Calc_Visible_Faces		;I:a5, T:d0-d7/a0-a4

	; Work out the minmum size of fill and clr using post-pespective pts
	; Note must be before and in same frame (screen address) as DrawObject/Fill
	bsr	Calc_Screen_Clr_Bounding	;T:d0-d7/a0-a2

	;If the bounding fill box indicates nothing on screen then skip!
	move.l	SCR_Fill_Log_Details_Ptr(pc),a0
	tst.l	SCR_FILL_SRCPTR(a0)
	beq	.finishdrawing			;nothing to draw on screen

	bsr	DrawObject			;I:a6, T:d0-d7/a0-a4
	bsr	Fill_Screen_Bounded		;I:a6, T:d0-d2/a0

.finishdrawing:
	bsr	Do_LightSource_Palette		;T:d0-d7/a2/a3/a4
	bsr	P1_Write_Palette_To_Copper	;I:a5, T:a0-a2

	bsr	ScreenBufferSwap		;I:a6, T:d0-d1/a0

	jsr	FW_VBlankProxy			;TRASHES ALL, Update framecounter, play music

	;Reset interrupt
	IFNE FW_IRQ_TYPE_COPPER
		moveq	#INTF_COPER,d0
	ELSE
		moveq	#INTF_VERTB,d0
	ENDC
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)			;A4000 compat

	movem.l	(sp)+,d0-d7/a0-a6
	WAITBLIT_NASTY				;Blitter nasty on while we finish off any blits.
	TIMEROFF
	rte


*****************************************************************************
* Inits ptrs in the copper list.
* IN:		
* OUT:		
* TRASHED:	d0-d2/a0-a1
*****************************************************************************

P1_CL_InitPtrs:
	; Copper list buffers - copy screen list into 2nd buffer for doublebuffering
	lea	P1_CL_Phys,a0
	lea	P1_CL_Log1,a1
	move.l	a0,CL_Phys_Ptr
	move.l	a1,CL_Log1_Ptr

	move.w	#(P1_CL_SIZE/2),d0	;size in words
	jsr	FW_CopyBuffer_CPU	;I:d0/a0-a1, T:d0-d7/a0-a2

	;Screen copper BPL pointers, use the current screen BPL, but add 32 lines
	move.l	BPL_Phys_Ptr(pc),d1	;in d1 for InitCopperBplPtrs
	add.l	#P1_BPL_YOFFSET_TOP*BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES,d1

	move.l	CL_Phys_Ptr(pc),a0
	lea	P1_CL_BPL_OFFSET(a0),a0	;copper bpl pointer block
	moveq	#BPL_BUF_NUMPLANES,d0
	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	;Work copper BPL pointers, use the current work BPL
	move.l	BPL_Log1_Ptr(pc),d1	;in d1 for InitCopperBplPtrs
	add.l	#P1_BPL_YOFFSET_TOP*BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES,d1

	move.l	CL_Log1_Ptr(pc),a0
	lea	P1_CL_BPL_OFFSET(a0),a0	;copper bpl pointer block
	moveq	#BPL_BUF_NUMPLANES,d0
	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

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
	; Swap fill details
	lea	SCR_Fill_Phys_Details_Ptr(pc),a0
	movem.l	(a0),d0-d1
	move.l	d1,(a0)+
	move.l	d0,(a0)

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


*****************************************************************************
* Modifies the horiz scroll sine table into bplcon1 shift values.
* Use for one-time setup
* IN:		
* OUT:		
* TRASHED:	d0-d2/a0
*****************************************************************************

Calc_HorizScroll_BPLCON:

	lea	HorizScroll_Table(pc),a0

	; *2 to avoid bounds checking
	move.w	#(HORIZSCROLL_TABLE_NUMWORDS*2)-1,d2
.loop
	move.w	(a0),d0		;get 0-15 value
	;move.w	d0,d1
	;lsl.w	#4,d1
	;or.w	d0,d1		;change 000f to 00ff
	;move.w	d1,(a0)+

	;pf2 only
	lsl.w	#4,d0
	move.w	d0,(a0)+

	dbf	d2,.loop

	rts	


*****************************************************************************
* Calculates the structures for x and y expansion.
* Use for one-time setup
* IN:		
* OUT:		
* TRASHED:	-
*****************************************************************************

Calc_Zoom_Bitmaps:

	; Clear first line (zoom 0, default line)
	lea	BPL_Zoom_Pattern,a1
.clearline:
	REPT	ZOOM_PATTERN_WORDWIDTH
	clr.w	(a1)+
	ENDR

	move.w	#ZOOM_VALUES_NUM-2,d7	;-1 for 0, -1 for dbf
	move.w	#ZOOM_VALUES_START_Z,d6	;zoom level


.line	
	;blackout the line (our new pts will be the cleared pts)
	move.l	a1,a2		;save
	moveq	#-1,d0		;all bits set
	REPT	ZOOM_PATTERN_WORDWIDTH
	move.w	d0,(a1)+
	ENDR
	move.l	a2,a1

	moveq	#0,d0		;x, clear top word
.xloop:
	move.l	d0,d2
	asl.l	#8,d2
	divs	d6,d2		;new x (coord)

	move.w	d2,d4		
	neg.w	d4		;mirror of x :)

	add.w	#ZOOM_XOFFSET,d2
	bmi.s	.nextline	;off the screen, this line is done

	move.w	d2,d3			;save x
	asr.w	#3,d2			;x to byte offset
	not.w	d3			;convert to bset value
	bclr.b	d3,(a1,d2.w)		;clear point (show vector)

	add.w	#ZOOM_XOFFSET,d4
	move.w	d4,d3			;save mirror x
	asr.w	#3,d4			;x to byte offset
	not.w	d3			;convert to bset value
	bclr.b	d3,(a1,d4.w)		;clear point (show vector)

	subq.l	#1,d0			;next x
	cmp.w	#-ZOOM_XOFFSET,d0
	bgt.s	.xloop
.nextline:

	lea	ZOOM_PATTERN_BYTEWIDTH(a1),a1	;next line
	subq.w	#ZOOM_VALUES_INC,d6	;closer
	dbf	d7,.line

	rts


*****************************************************************************
* Clears entire front and back buffer. Done once at startup to ensure clean env.
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

	RSRESET
SCR_FILL_MINX:		rs.w	1
SCR_FILL_MINY:		rs.w	1
SCR_FILL_MAXX:		rs.w	1
SCR_FILL_MAXY:		rs.w	1
SCR_FILL_HEIGHT:	rs.w	1
SCR_FILL_WORDWIDTH:	rs.w	1
SCR_FILL_BYTEMODULO:	rs.w	1
SCR_FILL_SRCPTR:	rs.l	1	;if 0 then do not use
SCR_FILL_BLTSIZE:	rs.w	1
SCR_FILL_SIZE:		rs.w	0

SCR_Fill_Phys_Details:
	dc.w	0,0,0,0	;minx,miny,maxx,maxy
	dc.w	0	;height in lines to clear
	dc.w	0	;width in words to clear
	dc.w	0	;modulo for next line
	dc.l	0	;screen ptr for bottom right src (descending) INIT AS 0
	dc.w	0	;bltsize

SCR_Fill_Log_Details:
	dc.w	0,0,0,0	;minx,miny,maxx,maxy
	dc.w	0	;height in lines to clear
	dc.w	0	;width in words to clear
	dc.w	0	;modulo for next line
	dc.l	0	;screen ptr for bottom right src (descending) INIT AS 0
	dc.w	0	;bltsize

; Details of the overall full screenscreen fills and clears - we keep a bounding
; box of the screen area used to optimize fills and clears
SCR_Fill_Phys_Details_Ptr:	dc.l 	0
SCR_Fill_Log_Details_Ptr:	dc.l 	0


*****************************************************************************
* Fills a bounded area of the screen.
* IN:		a6, _custom
* OUT:		
* TRASHED:	d0-d3/a0
*****************************************************************************

Fill_Screen_Bounded:
	; We only fill the box that we've drawn in.
	move.l	SCR_Fill_Log_Details_Ptr(pc),a0

	; Get start address of bottom right corner (fills are in descending mode so match with clear)
	; If the value is 0 then no valid fill was done, so skip the clear as well - happens at
	; start of routine
	move.l	SCR_FILL_SRCPTR(a0),d0
	beq.s	.exit

	move.w	SCR_FILL_BLTSIZE(a0),d2	;completed bltsize in d4
	move.w	SCR_FILL_BYTEMODULO(a0),d1

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

Clear_WorkBuffer_Bounded_Blitter_A6:
	
	; We only fill the box that we've drawn in. 
	move.l	SCR_Fill_Log_Details_Ptr(pc),a0

	; Get start address of bottom right corner (fills are in descending mode so match with clear)
	; If the value is 0 then no valid fill was done, so skip the clear as well - happens at
	; start of routine
	move.l	SCR_FILL_SRCPTR(a0),d0
	beq.s	.exit

	move.w	SCR_FILL_BLTSIZE(a0),d2	;completed bltsize in d4
	move.w	SCR_FILL_BYTEMODULO(a0),d1

	WAITBLIT_A6
	move.l	#$01000002,bltcon0(a6)	;desending to match fill routine calculations
	move.w	d1,bltdmod(a6)
	move.l	d0,bltdpth(a6)
	move.w	d2,bltsize(a6)
.exit:
	rts

*****************************************************************************

	RSRESET
CTRL_PAUSE_COUNTER:		rs.w 1		;Pause counter, 0=running
CTRL_SCRIPT_PTR:		rs.l 1		;Script Ptr
CTRL_FINISHED:			rs.w 1		;1 if quitting
CTRL_PRECALC_INTROSTART_DONE	rs.w 1		;1 if intro start precalc done
CTRL_PHASE:			rs.w 1		;Current phase
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
CTRL_HORIZSCROLL_ACTIVE:	rs.w 1
CTRL_HORIZSCROLL_SIN1_OFFSET:	rs.w 1
CTRL_HORIZSCROLL_SIN1_SPEED:	rs.w 1
CTRL_HORIZSCROLL_SIN1_STEP:	rs.w 1
CTRL_ZOOM:			rs.w 1		;current zoom level
CTRL_ZOOM_ACTIVE:		rs.w 1		;move active, final x,y,z,speed
CTRL_ZOOM_LOAD_FLAG		rs.w 1		;set to >1 to force zoom load
CTRL_ZOOM_SPEED:		rs.w 1
CTRL_ZOOM_REQ:			rs.w 1		;Final zoom level
CTRL_MOVE2_ACTIVE:		rs.w 1		;move active, final x,y,z,speed
CTRL_MOVE2_SPEED:		rs.w 1
CTRL_MOVE2_X:			rs.w 1
CTRL_MOVE2_Y:			rs.w 1
CTRL_MOVE2_Z:			rs.w 1
CTRL_MOVE2_XVEL:		rs.w 1
CTRL_MOVE2_YVEL:		rs.w 1
CTRL_MOVE2_ZVEL:		rs.w 1
CTRL_LIGHTSOURCE_ACTIVE		rs.w 1
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
	beq	.fx_pallete

	cmpi.w	#FX_SCRIPTJMP_FLAG,d0
	beq.s	.fx_scriptjmp

	cmp.w	#FX_NEXT_PHASE_FLAG,d0
	beq	.fx_next_phase

	cmpi.w	#FX_PAUSE_FLAG,d0
	beq.s	.fx_pause

	cmpi.w	#FX_LOAD_FLAG,d0
	beq	.fx_load

	cmpi.w	#FX_MORPH_FLAG,d0
	beq	.fx_morph

	cmp.w	#FX_CLONE_ROTATION_FLAG,d0
	beq	.fx_clone_rotation

	cmp.w	#FX_CHANGE_ROT_DELTA_FLAG,d0
	beq	.fx_change_rot_delta

	cmp.w	#FX_CHANGE_ROT_FLAG,d0
	beq	.fx_change_rot

	cmp.w	#FX_HORIZSINE_FLAG,d0
	beq	.fx_horizsine

	cmp.w	#FX_MOVE_FLAG,d0
	beq	.fx_move

	cmp.w	#FX_MOVE2_FLAG,d0
	beq	.fx_move2

	cmp.w	#FX_ZOOM_FLAG,d0
	beq	.fx_zoom

	cmp.w	#FX_LIGHTSOURCE_FLAG,d0
	beq	.fx_lightsource

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
	rts					;exit when starting pause

.fx_scriptjmp:
	move.l	(a4)+,a4		;New script
	move.l	a4,CTRL_SCRIPT_PTR(a5)
	bra	.loop

.fx_pallete:
	move.w	(a4)+,d0		;Speed
	move.l	(a4)+,a0		;New pallete
	bsr	Controller_FX_Palette	
	bra	.loop

.fx_next_phase:
	addq.w	#1,CTRL_PHASE(a5)
	move.l	a4,CTRL_SCRIPT_PTR(a5)
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

.fx_horizsine:
	movem.w	(a4)+,d0-d1		;speed,step
	bsr	Controller_FX_HorizSine
	bra	.loop

.fx_move:
	movem.w	(a4)+,d0-d3		;speed,x,y,z
	bsr	Controller_FX_Move
	bra	.loop

.fx_move2:
	movem.w	(a4)+,d0-d3		;speed,x,y,z
	bsr	Controller_FX_Move2
	bra	.loop

.fx_zoom:
	movem.w	(a4)+,d0-d1		;speed,w
	bsr	Controller_FX_Zoom
	bra	.loop

.fx_lightsource:
	move.w	(a4)+,CTRL_LIGHTSOURCE_ACTIVE(a5)	;boolean
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
	beq.s	.move2
	bsr	Controller_FX_Move_Perform	;I:a5, T:d0-d6/a0
.move2:
	tst.w	CTRL_MOVE2_ACTIVE(a5)
	beq.s	.zoom
	bsr	Controller_FX_Move2_Perform	;I:a5, T:d0-d6/a0
.zoom:
	tst.w	CTRL_ZOOM_ACTIVE(a5)
	beq.s	.exit
	bsr	Controller_FX_Zoom_Perform

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

	clr.w	CTRL_MOVE2_ACTIVE(a5)	;disable change move2

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
* Moves. Will do over time is speed >0
* IN:		a5, Controller_Info
* 		d0-d3, speed,new x,y,z
* OUT:		
* TRASHED:	d0-d4/a0
*****************************************************************************

Controller_FX_Move2:
	move.l	CTRL_OBJECTINFO_PTR(a5),a0	;Current object

	moveq	#0,d4
	move.w	d4,CTRL_MOVE_ACTIVE(a5)		;disable change move1

	tst.w	d0
	bne.s	.slow
	move.w	d4,CTRL_MOVE2_ACTIVE(a5)	;disable change
	movem.w	d1-d3,VEC_OBJ_POSX(a0)		;change pos

	move.w	d4,CTRL_MOVE2_XVEL(a5)		;reset velocities - hard stop
	move.w	d4,CTRL_MOVE2_YVEL(a5)
	move.w	d4,CTRL_MOVE2_ZVEL(a5)
	
	rts

.slow:
	move.w	#1,CTRL_MOVE2_ACTIVE(a5)	;enable change
	movem.w	d0-d3,CTRL_MOVE2_SPEED(a5)	;store speed,x,y,z

	;clr.w	CTRL_MOVE2_XVEL(a1)		;reset velocities - no!
	;clr.w	CTRL_MOVE2_YVEL(a1)
	;clr.w	CTRL_MOVE2_ZVEL(a1)

	rts


*****************************************************************************
* Performs move change
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d6/a0
*****************************************************************************

Controller_FX_Move2_Perform:
	move.l	CTRL_OBJECTINFO_PTR(a5),a0	;Current object

	moveq	#0,d6			;clear flag

	move.w	CTRL_MOVE2_X(a5),d1	;final value
	move.w	VEC_OBJ_POSX(a0),d0	;current value
	move.w	CTRL_MOVE2_SPEED(a5),d2	;speed
	move.w	CTRL_MOVE2_XVEL(a5),d3	;current vel
	bsr.s	Move2_Coord_Towards
	move.w	d0,VEC_OBJ_POSX(a0)	;save value
	move.w	d3,CTRL_MOVE2_XVEL(a5)
	cmp.w	d0,d1
	sne.b	d6			;set to $ff if ne
.y:
	move.w	CTRL_MOVE2_Y(a5),d1	;final value
	move.w	VEC_OBJ_POSY(a0),d0	;current value
	move.w	CTRL_MOVE2_SPEED(a5),d2	;speed
	move.w	CTRL_MOVE2_YVEL(a5),d3	;current vel
	bsr.s	Move2_Coord_Towards
	move.w	d0,VEC_OBJ_POSY(a0)	;save value
	move.w	d3,CTRL_MOVE2_YVEL(a5)
	cmp.w	d0,d1
	sne.b	d6
.z:
	move.w	CTRL_MOVE2_Z(a5),d1	;final value
	move.w	VEC_OBJ_POSZ(a0),d0	;current value
	move.w	CTRL_MOVE2_SPEED(a5),d2	;speed
	move.w	CTRL_MOVE2_ZVEL(a5),d3	;current vel
	bsr.s	Move2_Coord_Towards
	move.w	d0,VEC_OBJ_POSZ(a0)	;save value
	move.w	d3,CTRL_MOVE2_ZVEL(a5)
	cmp.w	d0,d1
	sne.b	d6			

	tst.b	d6
	bne.s	.exit			;test flag, if not zero not finished
	clr.w	CTRL_MOVE2_ACTIVE(a5)	;finished
.exit:
	rts


*****************************************************************************
* Moves a coordinate towards a value at given speed.
* IN:		d0, current value
*		d1, target value
*		d2, max speed
*		d3, velocity
* OUT:		d0, new value
*		d3, new velocity
* TRASHED:	
*****************************************************************************

Move2_Coord_Towards:
VEC_MOVE2_ACCEL = 2	;value to slow down the acceleration per frame

	cmp.w	d1,d0
	bgt.s	.greater
	blt.s	.less

	;already there
	moveq	#0,d3			;new velocity is 0
	rts
.less:					
	;velocity should be +ve, if not then assume need to increase +ve vel
	;i.e. reverse direction slowly
	tst.w	d3
	bmi.s	.less_accel

	;distance to come to stop = (speed*speed)/(2*maxDecel)
	;maxAccel is essentially 1 per frame so 2*1 = 2 = asr.w #1
	move.w	d3,d4			;current velocity
	muls	d4,d4			;speed*speed
	asr.w	#1,d4			;d4=distance to come to stop 

	move.w	d1,d5			;save target
	sub.w	d0,d5			;distance
	asl.w	#VEC_MOVE2_ACCEL,d5	;scale up to match velocity/speeds
	cmp.w	d4,d5			;Do we need to start decelerating?
	bge.s	.less_accel		;carry on accelerating
.less_decel:	
	cmp.w	#(1<<VEC_MOVE2_ACCEL),d3	;keep vel above minimum for this scaling
	ble.s	.less_do_move		;don't change any more
	subq.w	#1,d3			;decelerate more
	bra.s	.less_do_move
.less_accel:				;still accelerating
	cmp.w	d2,d3
	beq.s	.less_do_move		;max vel?
	bgt.s	.less_accel_rev
	addq.w	#1,d3			;increase velocity
	bra.s	.less_do_move
.less_accel_rev:
	subq.w	#1,d3			;decrease velocity
.less_do_move:
	move.w	d3,d2			;save
	asr.w	#VEC_MOVE2_ACCEL,d2	;scale velocity down
	add.w	d2,d0			;add the velocity
	;cmp.w	d1,d0			;at target?
	;bge.s	.set_to_final		;finished or overshot?
	rts				;d0=new pt, d3=new vel

; -----

.greater:
	;velocity should be -ve, if not then assume need to increase -ve vel
	;i.e. reverse direction slowly
	neg.w	d2			;maxspeed is -ve
	tst.w	d3
	bpl.s	.greater_accel

	;distance to come to stop = (speed*speed)/(2*maxDecel)
	;maxAccel is essentially 1 per frame so 2*1 = 2 = asr.w #1
	move.w	d3,d4			;current velocity
	muls	d4,d4			;speed*speed (will be postive)
	asr.w	#1,d4			;d4=distance to come to stop 

	move.w	d0,d5			;save cur
	sub.w	d1,d5			;distance, 
	asl.w	#VEC_MOVE2_ACCEL,d5	;scale up to match velocity/speeds
	cmp.w	d4,d5			;Do we need to start decelerating?
	bge.s	.greater_accel		;carry on accelerating
.greater_decel:	
	cmp.w	#-(1<<VEC_MOVE2_ACCEL),d3	;keep vel above minimum for this scaling
	bge.s	.greater_do_move		;don't change any more
	addq.w	#1,d3			;decelerate more
	bra.s	.greater_do_move
.greater_accel:				;still accelerating
	cmp.w	d2,d3
	beq.s	.greater_do_move	;max vel?
	blt.s	.greater_accel_rev
	subq.w	#1,d3			;increase velocity
	bra.s	.greater_do_move
.greater_accel_rev:
	addq.w	#1,d3			;decrease velocity
.greater_do_move:
	move.w	d3,d2			;save
	neg	d2			;asr on negative numbers always gives at least -1
	asr.w	#VEC_MOVE2_ACCEL,d2	;scale velocity down
	sub.w	d2,d0			;sub the velocity (vel positive here)
	;cmp.w	d1,d0			;at target?
	;ble.s	.set_to_final		;finished or overshot?
	rts				;d0=new pt, d3=new vel

.set_to_final:
	move.w	d1,d0			;overshot, set value to final
	moveq	#0,d3			;new velocity is 0
	rts

*****************************************************************************
* Zooms. Will do over time is speed >0
* IN:		a5, Controller_Info
* 		d0-d1, speed,new w
* OUT:		
* TRASHED:	d0-d1
*****************************************************************************

Controller_FX_Zoom:	
	cmpi.w	#ZOOM_MAX,d1
	bls.s	.zoomok
	move.w	#ZOOM_MAX,d1
.zoomok:
	tst.w	d0			;Speed=0?
	bne.s	.slow
	move.w	d1,CTRL_ZOOM(a5)	;change zoom immediately
	move.w	d0,CTRL_ZOOM_ACTIVE(a5)	;0, disable
	move.w	#CL_NUM_BUFFERS,CTRL_ZOOM_LOAD_FLAG(a5)	
.slow:
	move.w	#1,CTRL_ZOOM_ACTIVE(a5)	;enable change
	move.w	d0,CTRL_ZOOM_SPEED(a5)	;store params in controller info
	move.w	d1,CTRL_ZOOM_REQ(a5)	;store params in controller info

	rts


*****************************************************************************
* Performs move change
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d6
*****************************************************************************

Controller_FX_Zoom_Perform:
	;Request copper load this twice for double buffer cl issues
	move.w	#CL_NUM_BUFFERS,CTRL_ZOOM_LOAD_FLAG(a5)

	move.w	CTRL_ZOOM_SPEED(a5),d2	;speed
	move.w	CTRL_ZOOM_REQ(a5),d1	;final value
	move.w	CTRL_ZOOM(a5),d0	;current value
	bsr	Move_Coord_Towards

	cmpi.w	#ZOOM_MAX,d0
	bls.s	.zoomok
	move.w	#ZOOM_MAX,d0
.zoomok:
	move.w	d0,CTRL_ZOOM(a5)	;save value
	cmp.w	d0,d1			;finished?
	bne.s	.exit
	clr.w	CTRL_ZOOM_ACTIVE(a5)	;finished (double buffered)
.exit:
	rts



*****************************************************************************
* Changes the horizontal scroll effect
* IN:		a5, Controller_Info
* 		d0-d3, speed,step,speed,step
* OUT:		
* TRASHED:	
*****************************************************************************

Controller_FX_HorizSine:

	;2 because when we deactivate we use this as a count for coping
	;with doublebuffered copper
	move.w	#CL_NUM_BUFFERS,CTRL_HORIZSCROLL_ACTIVE(a5)
	movem.w	d0-d1,CTRL_HORIZSCROLL_SIN1_SPEED(a5)

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

; This is the actual palette poked into the copperlist. PAL_Current is used as a source
; and any lightsourcing may modify the values and generate this final version. If no
; lightsourcing then PAL_Current is just copied directly into here.
PAL_Palette_Copper:	dcb.w	PAL_NUMCOLS_MAIN,0		;main colours
			dcb.w	PAL_NUMCOLS_DARK,0		;reflection colours

; A darkened version of the palette for lightsourcing
PAL_Palette_Dark:	dcb.w	PAL_NUMCOLS_MAIN,0		;main colours
			dcb.w	PAL_NUMCOLS_DARK,0		;reflection colours


*****************************************************************************
* Makes the current palette lighter or darker depending on lightsource.
* If lightsource not active then it just copies the palette.
* Note: This function may be complicated by weird bitplane configs for 
* exploding vectors so the function tries to map "normal" vector palettes
* to the right configuration for the display which may involved reordering
* and/or duplicating colors as needed.
* IN:		
* OUT:
* TRASHED:	d0-d7/a2/a3/a4
*****************************************************************************

Do_LightSource_Palette:

	;lightsource?
	tst.w	CTRL_LIGHTSOURCE_ACTIVE(a5)
	beq.s	.copy

	;Create darkened version of palette
	;In, d0=numcols, d1=step, a0-a2 palettes
	;Out, d1=step
	;trashes d0/d2-d7,a0-a2

	lea	PAL_Palette_Dark(pc),a1
	REPT	PAL_NUMCOLS_MAIN
	clr.w	(a1)+
	ENDR

	lea	PAL_Current(pc),a0
	lea	PAL_AllBlack(pc),a1
	lea	PAL_Palette_Dark(pc),a2
	moveq	#PAL_NUMCOLS_MAIN,d0
	moveq	#7,d1
	jsr	LIB_RGB12_Interpolate_Fast_Palette	



	lea	PAL_Current(pc),a0
	lea	PAL_Palette_Dark(pc),a1
	lea	PAL_Palette_Copper(pc),a2
	move.w	(a0),(a2)		;color00 is untouched
	move.w	PAL_DARK(a0),PAL_DARK(a2)	;border color

	lea	Visible_Face_Buffer(pc),a3
	move.w	(a3)+,d7		;num faces
	beq.s	.exit			;No faces
	subq.w	#1,d7			;dbf and color00

	moveq	#0,d6			;clear top of d6
.facel:
	move.l	(a3)+,a4		;a3=adr of face data

	;get color
	move.b	VEC_FACE_PAPER(a4),d6	;(top is clear)
	add.w	d6,d6			;access pal in words
	move.w	(a0,d6.w),d0		;source colors
	move.w	(a1,d6.w),d1		;darkened colors

	move.l	VEC_FACE_LIGHTVAL(a4),d3	;lightval
	;lsr.l	#3,d3
	;lsr.l	#8,d3
	divu	#950,d3
	moveq	#15,d2
	cmp.w	d2,d3
	ble.s	.rangeok
	move.w	d2,d3			;set to 15
.rangeok:				;d3=0-15
	sub.w	d3,d2			;reverse as making darker
	beq.s	.store
	jsr	LIB_RGB12_Interpolate_Fast	;in d0,d1,d2
	bra	.store
.store:
	move.w	d0,(a2,d6.w)		;store new color
	dbf	d7,.facel
.exit:
	rts

.copy:
	lea	PAL_Current(pc),a0
	lea	PAL_Palette_Copper(pc),a1
	REPT	PAL_NUMCOLS_ALL
	move.w	(a0)+,(a1)+
	ENDR

	rts	


*****************************************************************************
* Loads the current colors into the current copperlist
* Note: This function may be complicated by weird bitplane configs for 
* exploding vectors so the function tries to map "normal" vector palettes
* to the right configuration for the display which may involved reordering
* and/or duplicating colors as needed.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	a0-a2
*****************************************************************************

P0_Write_Palette_To_Copper:

	;If lightsource we have to load copper each frame
	tst.w	CTRL_LIGHTSOURCE_ACTIVE(a5)
	bne.s	.write

	;Load flag may be > 1 if using double buffered CL or changing between CLs
	tst.w	CTRL_PALETTE_LOAD_FLAG(a5)
	beq	.exit
	subq.w	#1,CTRL_PALETTE_LOAD_FLAG(a5)

.write:
	lea	PAL_Palette_Copper(pc),a0
	move.l	CL_Log1_Ptr(pc),a1

	; For exploding cube we use bpl 1,3,5 for the main colors so the 
	;vec palette to hardware color mapping is:
	;3 = 4 5= 16
	;0 	= 0
	;1	= 1
	;2	= 4
	;3	= 5
	;4	= 16
	;5	= 17
	;6	= 20
	;7	= 21
	;In addition the background color to be used on any color where a bit
	;in bpl2,4 is set ()
	;2,3,6,7,8,9,10,11,12,13,14,15,18,19,22,23,24,25,26,27,28,29,30,31

	;save background
	move.w	(a0),d0

	;Normal colors
	lea	P0_CL_COL_OFFSET+2(a1),a1	
	REPT	PAL_NUMCOLS_MAIN
	move.w	(a0)+,(a1)
	addq.l	#4,a1			;next color
	ENDR
	
	;Background duplicates for exploding
	REPT	(32-PAL_NUMCOLS_MAIN)
	move.w	d0,(a1)
	addq.l	#4,a1			;next color
	ENDR

	;Reflection colors
	;lea	VEC_CL_COL_REFL_OFFSET+2(a1),a2	
	;REPT	PAL_NUMCOLS_MAIN
	;move.w	(a0)+,(a2)
	;addq.l	#4,a2			;next color
	;ENDR
.exit:
	rts


*****************************************************************************
* Loads the current colors into the current copperlist
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	a0-a2
*****************************************************************************

P1_Write_Palette_To_Copper:
	;Load flag may be > 1 if using double buffered CL or changing between CLs
	tst.w	CTRL_PALETTE_LOAD_FLAG(a5)
	beq	.exit
	subq.w	#1,CTRL_PALETTE_LOAD_FLAG(a5)

	lea	PAL_Palette_Copper(pc),a0
	move.l	CL_Log1_Ptr(pc),a1
	move.l	a1,a2			;save

	;Normal colors
	lea	P1_CL_COL_OFFSET+2(a1),a1	
	REPT	PAL_NUMCOLS_MAIN
	move.w	(a0)+,(a1)
	addq.l	#4,a1			;next color
	ENDR

	;Border colors
	move.w	(a0),d0
	move.w	d0,P1_CL_COL_TOP_OFFSET+2(a2)
	move.w	d0,P1_CL_COL_BOT_OFFSET+2(a2)
.exit:
	rts


*****************************************************************************
* Does the sine based horizontal scroll effect
* IN:		a6, _custom
*		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d4/a0-a1
*****************************************************************************

; Format of the horizontal scroll entries in copper:
;CWAIT x,x (4 bytes)
;CMOVE bplcon1,x (4 bytes) we want byte 6 for the value

Do_Horiz_Scroll:
	; Active?
	tst.w	CTRL_HORIZSCROLL_ACTIVE(a5)
	beq.s	.exit

	; Active, but do we need to shut it off? (speed and step = 0)
	movem.w	CTRL_HORIZSCROLL_SIN1_SPEED(a5),d0/d1
	add.w	d0,d1
	bne.s	.active

; Shut it off
	;Get work address of the horiz scroll part of the copper list
	move.l	CL_Log1_Ptr(pc),a1
	lea	P0_CL_PATTERN_OFFSET+CL_PATTERN_BPLCON1+2(a1),a1
	move.w	#BPL_BUF_HEIGHT-1,d2
	moveq	#CL_PATTERN_SIZEOF,d1
	moveq	#0,d0			;bplcon shift = 0, and flag = 0
.offloop:
	move.w	d0,(a1)
	add.l	d1,a1			;next bplcon1 entry
	dbf	d2,.offloop

	;deactivate (need to do twice because of doublebuffered copper)
	subq.w	#1,CTRL_HORIZSCROLL_ACTIVE(a5)
	rts


.active:
	;Get the speed (movement per frame) and adjust based on the object's num frames so that
	;effect runs at same speed for fast and slow vector objects
	move.w	CTRL_HORIZSCROLL_SIN1_SPEED(a5),d3
	move.w	CTRL_FRAMES_MIN(a5),d0
	subq.w	#2,d0
	bmi.s	.speedok
	move.w	d3,d4
.speedloop:
	add.w	d4,d3			;add original speed to ourself
	dbf	d0,.speedloop

.speedok:

	move.w	CTRL_HORIZSCROLL_SIN1_OFFSET(a5),d4	;offset in words
	add.w	d3,d3				;speed to word offset
	add.w	d3,d4				;add speed to offset (movement per frame)
	and.w	#HORIZSCROLL_TABLE_OFFSET_MASK,d4	;keep in range
	move.w	d4,CTRL_HORIZSCROLL_SIN1_OFFSET(a5)	;save for next frame
	;move.w	CTRL_HORIZSCROLL_SIN1_STEP(a0),d3	;step, movement per line
	;add.w	d3,d3				;step to word offset
	
	;Get work address of the horiz scroll part of the copper list
	;for the work copper list
	;format is
	;CWAIT x,x (4 bytes)
	;CMOVE bplcon1,x (4 bytes) we want byte 6 for the value
	move.l	CL_Log1_Ptr(pc),a1
	lea	P0_CL_PATTERN_OFFSET+CL_PATTERN_BPLCON1+2(a1),a1

	;Sine table has been modified into bplcon shift values like 00ff
	lea	HorizScroll_Table(pc),a0
	;move.w	#HORIZSCROLL_TABLE_OFFSET_MASK,d2
	moveq	#CL_PATTERN_SIZEOF,d0

	; We have 256*2 entries in table so we don't need to bounds check
	; each loop
	move.w	#BPL_BUF_HEIGHT-1,d2
	add.w	d4,a0
.loop:
	;move.w	(a0,d4.w),(a1)
	move.w	(a0)+,(a1)
	add.l	d0,a1			;next bplcon1 entry
	;add.w	d3,d4			;increase by step value
	;and.w	d2,d4			;ensure in range
	dbf	d2,.loop

.exit:
	rts


*****************************************************************************
* Loads the current colors into the current copperlist
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d6/a0-a2
*****************************************************************************

Do_Copper_Zoom:
	;Load flag may be > 1 if using double buffered CL or changing between CLs
	tst.w	CTRL_ZOOM_LOAD_FLAG(a5)
	beq.s	.exit
	subq.w	#1,CTRL_ZOOM_LOAD_FLAG(a5)

	;if no live zoom then reset everything (required because of double buffer delay)
	move.w	CTRL_ZOOM(a5),d6	;0 is nozoom
	bne.s	.zooming

	move.l	CL_Log1_Ptr(pc),a0
	lea	P0_CL_PATTERN_OFFSET+CL_PATTERN_BPLCON0+2(a0),a1

	; Clear vertical bplcon0 values (default is vector visible as normal)
	move.w	#BPL_BUF_HEIGHT-1,d1
	move.w	#ZOOM_BPLCON0,d0		;3bpl (2vector, 1pattern)
	moveq	#CL_PATTERN_SIZEOF,d2
.clearloop:
	move.w	d0,(a1)
	add.l	d2,a1
	dbf	d1,.clearloop	

	;Set horizontal pattern to no zoom - 1st line of pattern
	;Our zoom bitmap is 352 wide, so we add 2 for each 16 pixels 
	;difference to cur scr or ZOOM_PATTERN_X_BYTEOFFSET
	move.l	CL_Log1_Ptr(pc),a0
	lea	P0_CL_ZOOM_PATTERN_OFFSET(a0),a0
	moveq	#2,d0			;2bpl (same bpl repeated)
	move.l	#BPL_Zoom_Pattern+ZOOM_PATTERN_X_BYTEOFFSET,d1	;in d1 for InitCopperBplPtrs
	moveq 	#0,d2			;no modulo, repeat
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	;Set vertial scaling to 0, i.e. just showing the normal screen
	move.l	CL_Log1_Ptr(pc),a0
	lea	P0_CL_BPL_OFFSET(a0),a0
	moveq	#BPL_BUF_NUMPLANES,d0
	move.l	BPL_Log1_Ptr(pc),d1	;in d1 for InitCopperBplPtrs
	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo
.exit:
	rts


.zooming
	;d6=zoom level
	cmpi.w	#ZOOM_MAX,d6
	ble.s	.maxok
	move.w	#ZOOM_MAX,d6
.maxok:
	cmpi.w	#0,d6
	bge.s	.minok
	moveq	#0,d6
.minok

	;horizontal zoom, just change the pattern
	;Our zoom bitmap is 352 wide, so we add 2 for each 16 pixels 
	;difference to cur scr or ZOOM_PATTERN_X_BYTEOFFSET	
	move.l	CL_Log1_Ptr(pc),a0
	lea	P0_CL_ZOOM_PATTERN_OFFSET(a0),a0
	moveq	#2,d0			;2bpl (same bpl repeated)
	move.w	d6,d1
	mulu	#ZOOM_PATTERN_BYTEWIDTH,d1	;Find Y line in pattern
	add.l	#BPL_Zoom_Pattern+ZOOM_PATTERN_X_BYTEOFFSET,d1	;select line and offset
	moveq 	#0,d2			;no modulo
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	;vertial zoom, turn bitplanes on/off to stretch
	;Our zoom bitmap is 352 wide, so we add 2 for each 16 pixels 
	;difference to cur scr or ZOOM_PATTERN_X_BYTEOFFSET	
	moveq	#0,d4			;blank line count
	move.w	d6,d1
	mulu	#ZOOM_PATTERN_BYTEWIDTH,d1
	lea	BPL_Zoom_Pattern+ZOOM_PATTERN_Y_BYTEOFFSET,a0
	add.l	d1,a0			;a0 is the line

	move.l	CL_Log1_Ptr(pc),a1
	lea	P0_CL_PATTERN_OFFSET+CL_PATTERN_BPLCON0+2(a1),a1
	move.w	#$0200,d2		;blank
	move.w	#ZOOM_BPLCON0,d3	;visible

	move.w	#(BPL_BUF_HEIGHT/16)-1,d6	;number of 16bit words in the height
	move.w	#CL_PATTERN_SIZEOF,a2
.yloop2:
	move.w	(a0)+,d0		;next word
	moveq	#16-1,d5		;16bits
.xloop2:
	add.w	d0,d0			;move bit into carry
	bcc.s	.showbpl
	move.w	d2,d1			;blank
	addq.w	#1,d4			;increase count of blanked lines
	bra.s	.nextbit
.showbpl:
	move.w	d3,d1			;visible
.nextbit:
	move.w	d1,(a1)
	add.l	a2,a1			;next copper entry
	dbf	d5,.xloop2		;next bit
	dbf	d6,.yloop2

	;d4 = total number of blank lines for entire screen, divide by two to get y offset
	;note:interleaved
	lsr.w	#1,d4
	mulu	#BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES,d4	;pointer to first line to show

	;Increase the y offset by this many lines
	move.l	CL_Log1_Ptr(pc),a0
	lea	P0_CL_BPL_OFFSET(a0),a0
	moveq	#BPL_BUF_NUMPLANES,d0
	move.l	BPL_Log1_Ptr(pc),d1		;in d1 for InitCopperBplPtrs
	add.l	d4,d1			
	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

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


*****************************************************************************
* Performs perspective.
* IN:
* OUT:
* TRASHED:	d0-d7/a0
*****************************************************************************

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
* Apply additional perspective zoom. Must be done on perspective points.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d5/a0
*****************************************************************************

Zoom_X_Axis:
	move.w	CTRL_ZOOM(a5),d3
	beq.s	.exit

	move.w	#ZOOM_VALUES_START_Z,d4	;Start level used in zoom bitmap
	sub.w	d3,d4			;subtract zoom level
	bne.s	.notzero
	moveq	#1,d4			;trap div by 0
.notzero:
	move.l	#VEC_XOFFSET,d2		;ensure top of d2 ok
	lea	RotXYZpts(pc),a0	;rot pts 3d
	move.w	(a0)+,d5		;num pts-1
.persloop1:
	movem.w	(a0),d0-d1		;get x,y (movem sign extends to .l)
	
	sub.l	d2,d0			;back into non-screen coords

	asl.l	#8,d0			;* 256
	divs	d4,d0			;expand x
	add.w	d2,d0			;into screen coords

	move.w	d0,(a0)+		;store x
	addq.l	#4,a0			;skip y,z

	dbf	d5,.persloop1		;next pt
.exit:
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

Calc_Screen_Clr_Bounding:
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

	move.l	SCR_Fill_Log_Details_Ptr(pc),a0
	lea	LineDraw3_Vars(pc),a1



	IFNE VEC_CLIPPING

	;Check if any points on the screen (object might be completely off screen)
	;also check that minx >= CLIPMINX, maxx <= CLIPMAXX etc
	move.w	LINEDRAW3_CLIP_MINX(a1),d0
	move.w	LINEDRAW3_CLIP_MINY(a1),d1

	cmp.w	d0,d5				;maxx < clipminx
	blt.s	.exitnofill			;off scr
	cmp.w	d1,d6				;maxy < clipminy
	blt.s	.exitnofill			;off scr

	cmp.w	d0,d3				;minx > clipminx
	bge.s	.minxok
	move.w	d0,d3				;clip minx
	IFNE VEC_CLIPCHECK
		move.w	#1,VEC_ClipCheck_Flag	;flag clip
	ENDC
.minxok:
	cmp.w	d1,d4				;miny > clipminy
	bge.s	.minyok
	move.w	d1,d4				;reset miny
	IFNE VEC_CLIPCHECK
		move.w	#1,VEC_ClipCheck_Flag	;flag clip
	ENDC
.minyok:

	move.w	LINEDRAW3_CLIP_MAXX(a1),d0
	move.w	LINEDRAW3_CLIP_MAXY(a1),d1

	cmp.w	d0,d3				;minx > clipmaxx
	bgt.s	.exitnofill			;off scr
	cmp.w	d1,d4				;miny < clipmaxy
	bgt.s	.exitnofill			;off scr

	cmp.w	d0,d5				;maxx < clipmaxx
	ble.s	.maxxok
	move.w	d0,d5				;clip maxx
	IFNE VEC_CLIPCHECK
		move.w	#1,VEC_ClipCheck_Flag	;flag clip
	ENDC
.maxxok:
	cmp.w	d1,d6				;maxy < clipmaxy
	ble.s	.maxyok
	move.w	d1,d6				;reset maxy
	IFNE VEC_CLIPCHECK
		move.w	#1,VEC_ClipCheck_Flag	;flag clip
	ENDC
.maxyok:
	ENDC				; VEC_CLIPPING

	; Store the new bounding box values in required regs

	move.w	d3,d0
	move.w	d4,d1
	move.w	d5,d2
	move.w	d6,d3
	;movem.w	d0-d3,SCR_FILL_MINX(a0)	;save min/maxy details
	bra.s	Calc_Screen_Clr_Bounding_BlitVars
	;rts

.exitnofill:
	; No valid fill possible. Reset fill/clear buffers
	clr.l	SCR_FILL_SRCPTR(a0)
	rts

	IFNE VEC_CLIPCHECK
VEC_ClipCheck_Flag:
	dc.w 	0
	ENDC


*****************************************************************************
* Calculates the bounding rectange of the rotated points. Points are expected to
* be in screen coordinates just prior to drawing.
* This uses the overall shape of an object so is used for clearing or filling
* the whole screen.
*
* IN:		a6, custom
*		a0, SCR_Fill_Log_Details_Ptr 
*		a1, LineDraw_Vars (for premult table)
*		d0-d3,minx,y,maxx,y
* OUT:
* TRASHED:	d0-d7/a0-a2
*****************************************************************************

Calc_Screen_Clr_Bounding_BlitVars:

	;movem.w	SCR_FILL_MINX(a0),d0-d3

	;ASSERT: Our line draw routine does not draw the TOP scanline so that
	;blitter fills work correctly. So we have to add 1 to our miny. This may
	;cause miny>maxy so have to handle that below in height calcs.
	addq.w	#1,d1

	IFNE	VEC_DRAW_BOUNDING
		; Draw bounding box for debugging
		movem.l	d0-d7/a0-a6,-(sp)
		WAITBLIT_NASTY_A6
		move.l	#-1,bltafwm(a6)		;mask
		move.w	#BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES,bltcmod(a6)	;modulo interleaved
		move.l	#-$8000,bltbdat(a6)
		; --------
		move.l	BPL_Log1_Ptr,a0
		lea	LineDraw3_Vars(pc),a1
		move.w  #1,LINEDRAW3_COLOR(a1)	;save color for linedraw routine
		bsr	LineDraw3_nBpl_ClipAndDrawFilled
		movem.l	(sp)+,d0-d7/a0-a6
	ENDC


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
	;move.w	d5,SCR_FILL_WORDWIDTH(a0)	;save for clear routine

	move.w	d3,d4			;don't trash max y
	sub.w	d1,d4			;height (miny may be >= maxy see notes above)
	bpl.s	.heightok
	clr.l	SCR_FILL_SRCPTR(a0)	;no fill
	rts

.heightok:
	addq.w	#1,d4			;height+1,  255-0 = 255, but height should be 256

	;move.w	d4,SCR_FILL_HEIGHT(a0)	;save for clear routine
	
	;moveq	#BPL_BUF_NUMPLANES,d0
	;mulu.w	d0,d4			
	;height * num bpl for interleaved
	IFEQ	2-BPL_BUF_NUMPLANES
		add.w	d4,d4
	ENDC
	IFEQ	3-BPL_BUF_NUMPLANES
		move.w	d4,d0
		add.w	d4,d4
		add.w	d0,d4
	ENDC
	IFEQ	4-BPL_BUF_NUMPLANES
		add.w	d4,d4
		add.w	d4,d4
	ENDC
	IFEQ	5-BPL_BUF_NUMPLANES
		move.w	d4,d0
		add.w	d4,d4
		add.w	d4,d4
		add.w	d0,d4
	ENDC

	lsl.w	#6,d4			;*64 = height portion of bltsize
	add.w	d5,d4			;Add word width, completed bltsize in d4
	move.w	d4,SCR_FILL_BLTSIZE(a0)	;save for clear routine

	;d2 = max x in pixels (on word boundary)
	;d3 = max y
	;d4 = bltsize
	;d5 = fill width in words

	add.w	d5,d5			;d5=width in bytes
	moveq	#BPL_BUF_BYTEWIDTH,d1	;screen width in bytes
	sub.w	d5,d1			;modulo
	move.w	d1,SCR_FILL_BYTEMODULO(a0)	;save for clear routine

	; our Blitter source has to be the last word on the last bitplane (desending mode)
	; For line 100
	; 100 * (ScreenByteWidth * NumBitplanes)
	; + (ScreenByteWidth * (NumBitplanes-1))
	; + rightx in bytes

	move.l	BPL_Log1_Ptr(pc),a2
	add.w	d3,d3			;access y mult table in words
	add.w	LINEDRAW3_MULTTABLE(a1,d3.w),a2	;add to address

	lsr.w	#3,d2			;rightx in bytes
	lea 	BPL_BUF_BYTEWIDTH*(BPL_BUF_NUMPLANES-1)(a2,d2.w),a2	;starting position is last word in block (descending mode)
	move.l	a2,SCR_FILL_SRCPTR(a0)	;save for clear routine
	rts


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
	
	sub.l	d4,d3			;d3=light val
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
	neg.l	d3			;change -ve ligttval to +ve
.store:
	move.l	a3,(a0)+			;store face adr in visble faces buffer
	move.w	d0,VEC_FACE_FLAG_BACKFACE(a3)	;store frontface flag
	move.l	d3,VEC_FACE_LIGHTVAL(a3)	;save value for lightsourcing
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
	move.l	BPL_Log1_Ptr(pc),a2	;adr of screen

	;Setup line draw blit registers that don't change
	moveq	#-1,d0
	WAITBLIT_NASTY_A6
	move.l	d0,bltafwm(a6)		;mask
	move.w	#BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES,bltcmod(a6)	;modulo interleaved
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
	bsr.s	LineDraw3_nBpl_ClipAndDrawFilled	; trashes d0-d6/a0

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
* Original clipping coded by Prophet/Goldfire (Thomas Szirtes), 
* Bug fixed by Antiriad/Goldfire (Jonathan Bennett)
* Note: the left side isn't fill clipped. So use 0, or ensure you align
* The left side to the region being filled (word aligned minx essentially)
* This may draw a new vertical line at maxx to ensure filling works.
*
* IN:		a6, _custom
*		a0, screen address
*		a1, linedraw vars
*		d0-d3, x,y,x2,y2 of the line to draw
* OUT:		
* TRASHED:	d0-d6/a0 (linedraw on its own is d0-d4/a0)
*****************************************************************************

LineDraw3_nBpl_ClipAndDrawFilled:
	IFNE VEC_CLIPPING

	cmp	d1,d3
	bgt.s	.ClipTop		;and always draw top to Bottom, y2>y1
	beq	.NoDrawLine
	exg	d1,d3
	exg	d0,d2
.ClipTop
	move.w	LINEDRAW3_CLIP_MINY(a1),d4
	cmp	d4,d1
	bgt.b	.ClipBottom
	cmp	d4,d3
	bgt.s	.miss1			
	bra	.NoDrawLine		;Clip it all

.miss1	Sub	d1,d4			;Miny-Y1
	Move	d2,d5
	Sub	d0,d5			;x2-x1		
	Move	d3,d6
	Sub	d1,d6			;y2-y1
	Muls	d5,d4			;(x2-x1)*(miny-y1)
	Divs	d6,d4			;/(y2-y1)
	Add	d4,d0			;add to d0
	move.w	LINEDRAW3_CLIP_MINY(a1),d1	;Clip d1

.ClipBottom
	move.w	LINEDRAW3_CLIP_MAXY(a1),d4
	Cmp	d3,d4
	Bgt.s	.ClipRight
	Cmp	d1,d4
	Bgt.s	.miss2			
	bra	.NoDrawLine		;clip it all

.miss2	Sub	d3,d4			;MaxY-y2
	Move	d2,d5
	Sub	d0,d5			;x2-x1
	Move	d3,d6
	Sub	d1,d6			;y2-y1
	Muls	d5,d4			;(x2-x1)*(maxy-y2)
	Divs	d6,d4			;/(y2-y1)
	Add	d4,d2			;add 
	move.w	LINEDRAW3_CLIP_MAXY(a1),d3
.ClipRight
; X1<X2 
	move.w	LINEDRAW3_CLIP_MAXX(a1),d4		;d4=Maxx
	Cmp	d0,d2			;If X2<X1
	Beq.s	.CheckRStr		;Stop Division by 0
	Blt.s	.XCSwap			;Then do reverse Calc
	Cmp	d2,d4			;If x2<Maxx
	Bgt	.ClipLeft		;Don't Clip
	Cmp	d0,d4			;if x1>Maxx
	Ble.s	.ClipRightWhole		;Clipwholeline
	Sub	d2,d4			;(Maxx-x2)
	Move	d2,d5			;Copy X2
	Sub	d0,d5			;d5 = delta x
	Move	d3,d6			;Copy y2
	Sub	d1,d6			;d6 = delta y
	Muls	d6,d4			;deltay*maxx-x2
	Divs	d5,d4			;and divide by delta x
	Move	d3,d5			;copy y2
	Add	d4,d5			;add d4 to y2
	
	Movem	d0-d1/d5,-(sp)		;Drawing two lines, save
	move.l	a0,-(sp)
	Move	d5,d1
	Cmp	d1,d3
	Beq.s	.skip1
	move.w	LINEDRAW3_CLIP_MAXX(a1),d2
	Move	d2,d0
	bsr	.DrawLine		;LINE DRAW CALL. Trashes d0-d4/a0
.skip1	
	move.l	(sp)+,a0
	Movem	(sp)+,d0-d1/d3		;Note d5 restored into d3 intentionally

	move.w	LINEDRAW3_CLIP_MAXX(a1),d2
	Bra.b	.ClipLeft
.CheckRStr
	Cmp	d0,d4			;if x1>Maxx
	Blt.s	.ClipRightWhole		;Clipwholeline
	Bra.s	.ClipLeft
.XCSwap
; X2<X1 
	Cmp	d0,d4			;if x1<d4
	Bgt.s	.ClipLeft		;don't clip
	Cmp	d2,d4			;if x2>Maxx
	Blt.s	.ClipRightWhole		;Clipwholeline
	Sub	d0,d4			;(Maxx-x1)
	Move	d2,d5			;Copy X2
	Sub	d0,d5			;d5 = delta x
	Move	d3,d6			;Copy y1
	Sub	d1,d6			;d6 = delta y
	Muls	d6,d4			;deltay*maxx-x1
	Divs	d5,d4			;and divide by delta x
	Move	d1,d5			;copy y2
	Add	d4,d5			;add d4 to y1

	Movem	d2-d3,-(sp)		;Have to draw two lines
	move.l	a0,-(sp)
	Move	d5,d3
	Cmp	d1,d3
	Beq.s	.skip2
	move.w	LINEDRAW3_CLIP_MAXX(a1),d0
	move	d0,d2
	bsr.s	.DrawLine		;LINE DRAW CALL. Trashes d0-d4/a0
.skip2	
	move.l	(sp)+,a0
	Movem	(sp)+,d2-d3

	move.w	d5,d1
	move.w	LINEDRAW3_CLIP_MAXX(a1),d0
	Bra.s	.ClipLeft
.ClipRightWhole	
	move.w	LINEDRAW3_CLIP_MAXX(a1),d2
	Move	d2,d0
	bra.b	.DrawLine
	
.ClipLeft
; X1>X2 Left Boundary Clip
	move.w	LINEDRAW3_CLIP_MINX(a1),d4		;d4=Minx
	Cmp	d0,d2			;If X2<X1
	Beq.b	.CheckLStr		;Stop Division by 0
	Bgt.b	.XmCSwap		;Then do reverse Calc
	Cmp	d2,d4			;If x2>minx
	Blt.b	.DrawLine		;Don't Clip
	Cmp	d0,d4			;if x1<minx
	;Bgt.b	.ClipLeftWhole		;Clipwholeline
	Bgt	.NoDrawLine		;Clipwholeline
	Sub	d2,d4			;(Minx-x2)
	Move	d2,d5			;Copy X2
	Sub	d0,d5			;d5 = delta x
	Move	d3,d6			;Copy y2
	Sub	d1,d6			;d6 = delta y
	Muls	d6,d4			;deltay*maxx-x1
	Divs	d5,d4			;and divide by delta x
	Add	d4,d3			;add d4 to y1
	move.w	LINEDRAW3_CLIP_MINX(a1),d2	;New x2 = Minx
	Bra.b	.DrawLine
.CheckLStr
	Cmp	d0,d4			;if x1>Minx
	;Bgt.s	.ClipWholeLeft		;Clipwholeline
	Bgt	.NoDrawLine		;Clipwholeline
	Bra.s	.DrawLine
;.ClipLeftWhole
	; Not implmented on left for speed
;	bra.s	.NoDrawLine
.XmCSwap
; X2>X1 Left Boundary Clip
	Cmp	d0,d4			;if x1<d4
	Blt.s	.DrawLine		;don't clip
	Cmp	d2,d4			;if x2>Minx
	;Bgt.s	.ClipWholeLeft		;Clipwholeline
	Bgt	.NoDrawLine		;Clipwholeline
	Sub	d0,d4			;(Minx-x1)
	Move	d2,d5			;Copy X2
	Sub	d0,d5			;d5 = delta x
	Move	d3,d6			;Copy y2
	Sub	d1,d6			;d6 = delta y
	Muls	d6,d4			;deltay*minx-x1
	Divs	d5,d4			;and divide by delta x
	Add	d4,d1			;add d4 to y1
	move.w	LINEDRAW3_CLIP_MINX(a1),d0	;New x1 = Minx

;.DrawLine	
;	bra	VEC_LineDraw3_DrawFilled	* CALL LINE DRAW
;.NoDrawLine
;	rts
	; ****
	; Fall through to line draw
	; ****

	ENDC	;VEC_CLIPPING


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

.DrawLine:
;LineDraw3_nBpl_DrawFilled:

	;The linedraw routine draws from top of screen to bottom. We want
	;y2 > y1 for the logic used.
	sub.w 	d1,d3			;dy
	beq	.NoDrawLine		;skip when y1=y2
	bpl.b	.line1			;ensure y2>y1
	exg	d0,d2				
	neg.w	d3			
	sub.w	d3,d1			
.line1:
	;subq	#1,d3			;dy-1 for blit corner fix
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
	;move.l  a0,bltdpth(a6) 
	move.l	#FW_LineDraw_Scratch,bltdpth(a6) ;First pixel is written here (offscreen)
	move.w	d2,bltsize(a6)
.nextbpl:
	lea     BPL_BUF_BYTEWIDTH(a0),a0	;next bitplane
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

	dc.w	P0_CLIPMINX	; LINEDRAW3_CLIP_MINX
	dc.w	P0_CLIPMINY	; LINEDRAW3_CLIP_MINY
	dc.w	P0_CLIPMAXX	; LINEDRAW3_CLIP_MAXX
	dc.w	P0_CLIPMAXY	; LINEDRAW3_CLIP_MAXY

Mult_SCR_Height_ByteWidth_NumPlanes:
	ds.w	BPL_BUF_HEIGHT


*****************************************************************************

;bplcon1 scroll values
HORIZSCROLL_TABLE_NUMWORDS = 256
;HORIZSCROLL_TABLE_OFFSET_MASK = HORIZSCROLL_TABLE_NUMWORDS-1
HORIZSCROLL_TABLE_OFFSET_MASK = ((HORIZSCROLL_TABLE_NUMWORDS*2)-2)	; Byte offset access into the table, forced to be even 


;Inclue table twice, 
;Table must have BPL_BUF_HEIGHT*2 to avoid bounds checking
HorizScroll_Table:
	INCLUDE "Sine_0_6_256_words.i"	
	INCLUDE "Sine_0_6_256_words.i"


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
VEC_FACE_LIGHTVAL = 18
VEC_FACE_VERTICES = 22

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
		dc.l	0		;lightval
		ENDM


*****************************************************************************

*** Objects and Demo Sequencing ***

	; The actual demo sequences
	INCLUDE "ControllerScript.i"


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

MEMORYFETCHMODE		=	0		;0 (OCS),1 or 3 

	IFEQ	MEMORYFETCHMODE
DDF_INCREMENT	=	1
	ELSE
DDF_INCREMENT	=	(MEMORYFETCHMODE+1)&$fffe
	ENDC	

*****************************************************************************

DIW_VSTART set (P0_DIW_V&$ff)<<8
DIW_VSTOP set ((P0_DIW_V+P0_DIW_HEIGHT)&$ff)<<8
DIW_HSTART set P0_DIW_H&$ff
DIW_HSTOP set (DIW_HSTART+P0_DIW_WIDTH)&$ff
DIW_START set DIW_VSTART!DIW_HSTART
DIW_STOP set DIW_VSTOP!DIW_HSTOP
DDF_START set (P0_DDF_H/2)-8
DDF_STOP set DDF_START+((P0_DDF_WORDWIDTH-DDF_INCREMENT)*8)

	EVEN
P0_CL_Phys:
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	CMOVE 	bplcon0,$0200		;3bpl/8 cols
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0000
	CMOVE 	bpl1mod,P0_SCR_BPL1MOD	;interleaved mode
	CMOVE 	bpl2mod,P0_SCR_BPL2MOD	;repeat line

P0_CL_Bpl:				;Bitplane pointers
	CMOVE	bpl1pth,$0		;1+3+5 used for drawing
	CMOVE	bpl1ptl,$0
	CMOVE	bpl3pth,$0
	CMOVE	bpl3ptl,$0
	CMOVE	bpl5pth,$0
	CMOVE	bpl5ptl,$0
P0_CL_Bpl_Pattern:	
	CMOVE	bpl2pth,$0
	CMOVE	bpl2ptl,$0
	CMOVE	bpl4pth,$0
	CMOVE	bpl4ptl,$0

P0_CL_Cols:
	CMOVE	tmpcolor00,$000
	CMOVE	color01,$000		;1 See P0_Write_Palette_To_Copper for explanation of colors
	CMOVE	color04,$000		;2
	CMOVE	color05,$000		;3
	CMOVE	color16,$000		;4
	CMOVE	color17,$000		;5
	CMOVE	color20,$000		;6
	CMOVE	color21,$000		;7

	CMOVE	color02,$000		;24 remaining colors
	CMOVE	color03,$000
	CMOVE	color06,$000
	CMOVE	color07,$000
	CMOVE	color08,$000
	CMOVE	color09,$000
	CMOVE	color10,$000
	CMOVE	color11,$000
	CMOVE	color12,$000
	CMOVE	color13,$000
	CMOVE	color14,$000
	CMOVE	color15,$000
	CMOVE	color18,$000
	CMOVE	color19,$000
	CMOVE	color22,$000
	CMOVE	color23,$000
	CMOVE	color24,$000
	CMOVE	color25,$000
	CMOVE	color26,$000
	CMOVE	color27,$000
	CMOVE	color28,$000
	CMOVE	color29,$000
	CMOVE	color30,$000
	CMOVE	color31,$000

P0_CL_Zoom_Pattern:
a set P0_DIW_V
	REPT BPL_BUF_HEIGHT
	CWAIT	(a&$ff),$7		;0,2
	CMOVE 	bplcon0,$0200		;4,6
	CMOVE	bplcon1,$0000		;8,10 - Scroll value
	CWAIT	(a&$ff),$df		;12,14
a set a+1
	ENDR


	; Trigger copper interrupt - immediately after above CL
	IFNE FW_IRQ_TYPE_COPPER
		;IF FW_IRQ_COPPER_SCANLINE>255	;already done above
		;	CWAIT	255,$e1
		;ENDIF
		;CWAIT	(FW_IRQ_COPPER_SCANLINE&$ff),$7
		CMOVE	intreq,INTF_SETCLR|INTF_COPER
	ENDC


	COPPEREND
P0_CL_End:

P0_CL_BPL_OFFSET = (P0_CL_Bpl-P0_CL_Phys)
P0_CL_ZOOM_PATTERN_OFFSET = (P0_CL_Bpl_Pattern-P0_CL_Phys)
P0_CL_PATTERN_OFFSET = (P0_CL_Zoom_Pattern-P0_CL_Phys)
P0_CL_COL_OFFSET = (P0_CL_Cols-P0_CL_Phys)
P0_CL_SIZE = P0_CL_End-P0_CL_Phys

	RSRESET
CL_PATTERN_WAIT1:	rs.w	2
CL_PATTERN_BPLCON0:	rs.w	2
CL_PATTERN_BPLCON1:	rs.w	2
CL_PATTERN_WAIT2:	rs.w	2
CL_PATTERN_SIZEOF:	rs.w	0


*****************************************************************************

DIW_VSTART set (P1_DIW_V&$ff)<<8
DIW_VSTOP set ((P1_DIW_V+P1_DIW_HEIGHT)&$ff)<<8
DIW_HSTART set P1_DIW_H&$ff
DIW_HSTOP set (DIW_HSTART+P1_DIW_WIDTH)&$ff
DIW_START set DIW_VSTART!DIW_HSTART
DIW_STOP set DIW_VSTOP!DIW_HSTOP
DDF_START set (P1_DDF_H/2)-8
DDF_STOP set DDF_START+((P1_DDF_WORDWIDTH-DDF_INCREMENT)*8)

	EVEN
P1_CL_Phys:
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
P1_CL_BPLCON0:
	CMOVE 	bplcon0,P1_SCR_BPLCON0
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0000
	CMOVE 	bpl1mod,P1_SCR_BPL1MOD
	CMOVE 	bpl2mod,P1_SCR_BPL2MOD

P1_CL_Bpl:				;Bitplane pointers
	CMOVE	bpl1pth,$0		
	CMOVE	bpl1ptl,$0
	CMOVE	bpl2pth,$0
	CMOVE	bpl2ptl,$0
	CMOVE	bpl3pth,$0
	CMOVE	bpl3ptl,$0

	;CWAIT	P0_DIW_V,$7
P1_CL_ColTopBorder:
	CMOVE	tmpcolor00,$001
;	CMOVE	color01,$001		;1 See P0_Write_Palette_To_Copper for explanation of colors
;	CMOVE	color02,$001		;2
;	CMOVE	color03,$001		;3

	CWAIT	P1_DIW_V,$7
P1_CL_Cols:
	CMOVE	tmpcolor00,$000
	CMOVE	color01,$f00		;1 See P0_Write_Palette_To_Copper for explanation of colors
	CMOVE	color02,$0f0		;2
	CMOVE	color03,$00f		;3
	CMOVE	color04,$f00		;4
	CMOVE	color05,$0f0		;5
	CMOVE	color06,$00f		;6
	CMOVE	color07,$f00		;7

	CWAIT	255,$df

	CWAIT	(P1_DIW_V+P1_DIW_HEIGHT),$7
P1_CL_ColBotBorder:
	CMOVE	tmpcolor00,$001
;	CMOVE	color01,$001		;1 See P0_Write_Palette_To_Copper for explanation of colors
;	CMOVE	color02,$001		;2
;	CMOVE	color03,$001		;3

	; Trigger copper interrupt - immediately after above CL
	IFNE FW_IRQ_TYPE_COPPER
		;IF SCANLINE_EOF>255	;already done above
		;	CWAIT	255,$e1
		;ENDIF
		CWAIT	(SCANLINE_EOF&$ff),$7
		CMOVE	intreq,INTF_SETCLR|INTF_COPER
	ENDC


	COPPEREND
P1_CL_End:

P1_CL_BPLCON0_OFFSET = (P1_CL_BPLCON0-P1_CL_Phys)
P1_CL_BPL_OFFSET = (P1_CL_Bpl-P1_CL_Phys)
P1_CL_COL_OFFSET = (P1_CL_Cols-P1_CL_Phys)
P1_CL_COL_TOP_OFFSET = (P1_CL_ColTopBorder-P1_CL_Phys)
P1_CL_COL_BOT_OFFSET = (P1_CL_ColBotBorder-P1_CL_Phys)
P1_CL_SIZE = P1_CL_End-P1_CL_Phys


*****************************************************************************

DIW_VSTART set (P1_DIW_V&$ff)<<8
DIW_VSTOP set ((P1_DIW_V+P1_DIW_HEIGHT)&$ff)<<8
DIW_HSTART set P1_DIW_H&$ff
DIW_HSTOP set (DIW_HSTART+P1_DIW_WIDTH)&$ff
DIW_START set DIW_VSTART!DIW_HSTART
DIW_STOP set DIW_VSTOP!DIW_HSTOP
DDF_START set (P1_DDF_H/2)-8
DDF_STOP set DDF_START+((P1_DDF_WORDWIDTH-DDF_INCREMENT)*8)

	EVEN
TOUT_CL_Phys:
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	CMOVE 	bplcon0,$0200		;All off
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0000

	;Top border
	;CWAIT	P0_DIW_V,$7
	CMOVE	tmpcolor00,PAL_B_TOUT_BDR

	;Main
	CWAIT	P1_DIW_V,$7
	CMOVE	tmpcolor00,PAL_B_TOUT_FACE

	;Bottom border
	CWAIT	255,$df
	CWAIT	(P1_DIW_V+P1_DIW_HEIGHT),$7
	CMOVE	tmpcolor00,PAL_B_TOUT_BDR

	; Trigger copper interrupt
	IFNE FW_IRQ_TYPE_COPPER
		;IFGT SCANLINE_EOF-255
		;	CWAIT	255,$df
		;ENDIF
		CWAIT	(SCANLINE_EOF&$ff),$7
		CMOVE	intreq,INTF_SETCLR|INTF_COPER
	ENDC

	COPPEREND
TOUT_CL_End:


*****************************************************************************
*****************************************************************************
*****************************************************************************

; ---------
; Map chip
; ---------
;Map from end to avoid clashing with previous effect
CUR_CHIP_BUF set FW_Chip_Buffer_1_End

CUR_CHIP_BUF set (CUR_CHIP_BUF-BPL_BUF_TOTALSIZE)
BPL_Phys		=	CUR_CHIP_BUF	;3bpl 320x256 = 30720

CUR_CHIP_BUF set (CUR_CHIP_BUF-BPL_BUF_TOTALSIZE)
BPL_Log1		=	CUR_CHIP_BUF	;3bpl = 30720

CUR_CHIP_BUF set (CUR_CHIP_BUF-P0_CL_SIZE)
P0_CL_Log1		=	CUR_CHIP_BUF	;512

CUR_CHIP_BUF set (CUR_CHIP_BUF-P1_CL_SIZE)
P1_CL_Log1		=	CUR_CHIP_BUF	;512

*****************************************************************************
