*****************************************************************************

; Name			: Template.s
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com)
; Description		: Template routine.
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

	INCLUDE "IntroConfig.i"
	INCLUDE	"../Framework/customextra.i"
	INCLUDE "../Framework/CustomMacros.i"
	INCLUDE "../Framework/IntroLibrary.i"

; Additional external symbols 
	xref	FW_CheckUserQuitSignal_A6
	xref	FW_ClearBuffer_BlitCPU_A6
	xref	FW_ClearBuffer_CPU
	xref	FW_ClrCopperSprPtrs
	xref	FW_CopyBuffer_CPU
	xref	FW_CPUClearBuffer
	xref	FW_InitCopperBplPtrs
	xref	FW_InitCopperColsFromPalette
	xref	FW_InitCopperSprPtrs
	xref	FW_Quit_Flag
	xref	FW_SafeDisableSpriteDma_A6	
	xref	FW_SetBaseCopperAndLev3Irq_A6
	xref	FW_SetBaseLev3Irq
	xref	FW_SetCopperAndLev3Irq_A6
	xref	FW_SetLev3Irq
	xref	FW_WaitFrame
	xref	FW_WaitRaster_A6
	xref	FW_WaitRasterExact_A6
	xref 	FW_VBlankProxy

	xref	LIB_RGB12_Interpolate_Fast
	xref	LIB_RGB12_Interpolate_Fast_Palette
	xref	LIB_NRV2S_Depack

	xref	FW_Chip_Buffer_1
	xref	FW_Chip_Buffer_1_End


*****************************************************************************

;Phases
;P0 - Simple copperlist covering screen to transition in/out.
;P1 - Main routine
;P2 - (With P0 CL) fading out, transition out


*****************************************************************************

	SECTION	Miami_PublicCode,CODE	;Code section in Public memory

*****************************************************************************

*** Changeable Parameters For Display ***

*** Display Window ***
DIW_V			=	$2c	;Hardware Vstart ($2c normal, $24 overscan)
DIW_H			=	$71	;Hardware Hstart ($81 normal, $71 overscan)
DIW_WIDTH		=	352	;Pixels		 (multiple of 16, 320 normal, 352 overscan)
DIW_HEIGHT		=	256	;Lines		 (256 normal PAL, 272 overscan)

*** Data Fetch ***
MEMORYFETCHMODE		=	0		;0 (OCS),1 or 3 
DDF_H			=	$71		;Hardware Hstart ($81 normal, $71 overscan)
DDF_WIDTH		=	352		;Pixels		 (320 normal pal, 352 overscan)
DDF_BYTEWIDTH		=	DDF_WIDTH/8	;Bytes
DDF_WORDWIDTH		=	DDF_BYTEWIDTH/2	;Words


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

;PF1: SCR_PLM_ palm tree background
;PF1: SCR_SIN_ sine scroller main bpl
;PF2: SCR_BD1_ building first layer
;PF2: SCR_BD2_ building 2nd layer

;The palm trees, 3 screens 352 pixels wide we just scroll through it with no double buffer
SCR_BUF_WIDTH		=	1056
SCR_BUF_BYTEWIDTH	=	SCR_BUF_WIDTH/8
SCR_BUF_WORDWIDTH	=	SCR_BUF_BYTEWIDTH/2
SCR_BUF_HEIGHT		=	256			
SCR_BUF_NUMPLANES	=	1			
SCR_BUF_NUMCOLS 	= 	(1<<SCR_BUF_NUMPLANES)
SCR_BUF_SIZE		=	SCR_BUF_BYTEWIDTH*SCR_BUF_HEIGHT
SCR_BUF_TOTALSIZE	=	SCR_BUF_SIZE*SCR_BUF_NUMPLANES

;The sine scroller buffer, same 3 screen width as palm trees for the buffer to keep the
;same bpl1mod value.
SCR_SIN_BUF_WIDTH	=	1056
SCR_SIN_BUF_BYTEWIDTH	=	SCR_SIN_BUF_WIDTH/8
SCR_SIN_BUF_WORDWIDTH	=	SCR_SIN_BUF_BYTEWIDTH/2
SCR_SIN_BUF_HEIGHT	=	256
SCR_SIN_BUF_NUMPLANES	=	1
SCR_SIN_BUF_NUMCOLS 	= 	(1<<SCR_SIN_BUF_NUMPLANES)
SCR_SIN_BUF_SIZE	=	SCR_SIN_BUF_BYTEWIDTH*SCR_SIN_BUF_HEIGHT
SCR_SIN_BUF_TOTALSIZE	=	SCR_SIN_BUF_SIZE*SCR_SIN_BUF_NUMPLANES

;But we only draw on one screen. We double buffer between screen 1 and 2 means we
;only wasting 1 screen. So screen buffer addresses are buf and buf+44 (pseudo interleaved)
SCR_SIN_WIDTH		=	352
SCR_SIN_BYTEWIDTH	=	SCR_SIN_WIDTH/8
SCR_SIN_WORDWIDTH	=	SCR_SIN_BYTEWIDTH/2
SCR_SIN_HEIGHT		=	216-5; 208-3 ;256-16
SCR_SIN_YOFFSET		=	(SCR_SIN_BUF_HEIGHT-SCR_SIN_HEIGHT)-8
SCR_SIN_YOFFSET_BYTES	=	SCR_SIN_YOFFSET*SCR_SIN_BUF_BYTEWIDTH
SCR_SIN_NUMPLANES	=	1
SCR_SIN_NUMCOLS 	= 	(1<<SCR_SIN_NUMPLANES)
SCR_SIN_SIZE		=	SCR_SIN_BYTEWIDTH*SCR_SIN_HEIGHT
SCR_SIN_TOTALSIZE	=	SCR_SIN_SIZE*SCR_SIN_NUMPLANES

;Building buffer has 32 bits (1long) extra on the left and right for easy drawing
SCR_BLD_BUF_WIDTH	=	352+32+32
SCR_BLD_BUF_BYTEWIDTH	=	SCR_BLD_BUF_WIDTH/8
SCR_BLD_BUF_WORDWIDTH	=	SCR_BLD_BUF_BYTEWIDTH/2
SCR_BLD_BUF_HEIGHT	=	1
SCR_BLD_BUF_NUMPLANES	=	1
SCR_BLD_BUF_NUMCOLS 	= 	(1<<SCR_BLD_BUF_NUMPLANES)
SCR_BLD_BUF_SIZE	=	SCR_BLD_BUF_BYTEWIDTH*SCR_BLD_BUF_HEIGHT
SCR_BLD_BUF_TOTALSIZE	=	SCR_BLD_BUF_SIZE*SCR_BLD_BUF_NUMPLANES
BUILDING_MAX_X 		= 	SCR_BLD_BUF_WIDTH-32

SCR_BPL1MOD		=	SCR_BUF_BYTEWIDTH-DDF_BYTEWIDTH
SCR_BPL2MOD		=	(SCR_BLD_BUF_BYTEWIDTH-DDF_BYTEWIDTH)-SCR_BLD_BUF_BYTEWIDTH	;PF2 is going to repeat previous line


; The actual font details
SIN_Font_BPL_Height		=	10	; How big the font is in the bitmap (overrun for vfill precalc)
SIN_Font_Height			=	8	; Real height with no extra pixels, this is what we draw
SIN_Font_PixelWidth		=	64	; Pixel width, may be smaller. Used for spacing.
SIN_Font_HeightMultiplier	=	12	; final height will be (SIN_Font_Height-1) * SIN_Font_HeightMultiplier

; Bit weird because of kingcon conversion, but basically our "bitmap" is
; 16 pixels wide 8lines high, 1 bpl and 59 letters. Each letter is stored directly after
;each other
SIN_Font_Bitmap_Width		=	16
SIN_Font_Bitmap_ByteWidth	=	SIN_Font_Bitmap_Width/8
;SIN_Font_NumLetters		=	59	;Font1
SIN_Font_NumLetters		=	63	;Font3b

; The font matrix picks out a word aligned block in the font sheet. Usually the fonts aren't
; perfectly aligned so we need to adjust the Y position (lines) to match exactly
; Also may need to offset the word boundary. For example 16x16 font uses a 32x32 matrix but
; the font is stored in the bottom right of each cell. So YOffsetPixels=16, and XOffsetBytes=2
SIN_Font_Bitmap_YOffsetPixels	=	0	
SIN_Font_Bitmap_XOffsetBytes	=	0

; The buffer that the scroller uses. After scrolling this is plotted onto the back buffer
; in a sine wave. It can be made a touch wider than the screen for smooth scrolling. New letters will be plotted
; At the right edge of this buffer minus SIN_Scroller_ExtraByteWidth
; NOTE: Min size is Screen Width. Add the font width in multiples of 16. We will add 64 for our
; large font (we are converting 8px wide font to 64 px wide)
SIN_Scroller_ExtraWidth		=	64
SIN_Scroller_ExtraByteWidth	= 	SIN_Scroller_ExtraWidth/8
SIN_Scroller_ExtraWordWidth	= 	SIN_Scroller_ExtraByteWidth/2

SIN_Scroller_Width		=	SCR_SIN_WIDTH+SIN_Scroller_ExtraWidth
SIN_Scroller_ByteWidth		=	SIN_Scroller_Width/8		;Bytes
SIN_Scroller_WordWidth		=	SIN_Scroller_ByteWidth/2	;Words
SIN_Scroller_Height		=	SIN_Font_Height
SIN_Scroller_NumPlanes		=	1
SIN_Scroller_Size		=	SIN_Scroller_ByteWidth*SIN_Scroller_Height
SIN_Scroller_TotalSize		=	SIN_Scroller_Size*SIN_Scroller_NumPlanes


SCANLINE_EOF	=	(DIW_V+DIW_HEIGHT)-1	; Safe to starting clearing after this scanline

COL_TRANS_IN	= $134	;Starting background color (from previous routine)
COL_BUILDING	= $000
COL_TRANS_OUT	= $134	;Ending background color


*****************************************************************************
* Start the effect (usually used for setting up the copper)
* This is called each time the effect is started
* IN:		a0, (script ptr)
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

	IFEQ _INTROWRAPPER
	xdef	SubPartStart
SubPartStart:	
	ENDC

	xdef	MIA_Start
MIA_Start:
	movem.l	d2-d7/a2-a6,-(sp)	;save
	;lea	MIA_ControllerScript,a0

	lea	_custom,a6
	lea	Controller_Info,a5

	;Save script pointer
	;move.l	a0,CTRL_SCRIPT_PTR(a5)

	; Init phase 0
	bsr.s	P0_Init			;I:a5/a6, T:d0-d1/a0-a1

	; Init phase 1
	bsr.s	P1_Init			;I:a5/a6, T:d0-d1/a0-a1

	; Continue with main loop
	bsr	P0_MainLoop		;T:None

	;Safely disable sprite DMA (Now done in copper)
	;jsr	FW_SafeDisableSpriteDma_A6	;I:a6, T:d0

	;Restore base Irq for music and the basic transition CL
	;move.l	#P0_CL_Phys,cop1lch(a6)
	;jsr	FW_SetBaseLev3Irq
	;Last phase does this but still wait for 2 frames to take effect
	jsr	FW_WaitFrame
	jsr	FW_WaitFrame

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
	xdef	MIA_PreCalc_IntroStart
	ENDC
	
SubPartPreCalc_IntroStart:
MIA_PreCalc_IntroStart:
	tst.w	Controller_Info+CTRL_PRECALC_INTROSTART_DONE
	bne.s	.exit			;already done

	movem.l	d2-d7/a2-a6,-(sp)	;save

	;Start

	; Convert font bitmap to xor'ed version for vertical fill
	bsr	SIN_FontToVFill

	; Multiply sine values by screen widths
	bsr	SIN_PreMult_SineForScreen	;T:d0-d1/a0-a1

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
	;movem.l	d2-d7/a2-a4,-(sp)

	;Get transition screen (solid color) up so we can depack and precalc
	clr.w	CTRL_PALETTE_STEP(a5)
	clr.w	CTRL_PALETTE_COUNTER(a5)

	;Start the copper and irq, effectively hides the screen while we precalc
	bsr	P0_CL_InitPtrs
	lea	P0_CL_Phys,a0
	lea	P0_Lev3Irq(pc),a1
	jsr	FW_SetCopperAndLev3Irq_A6	;I:a0-a1/a6, T:a0

	;movem.l	(sp)+,d2-d7/a2-a4
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

	; Run intro startup precalc if not already done (only runs once)
	bsr.s	MIA_PreCalc_IntroStart	;T:None

	clr.w	CTRL_FINISHED(a5)
	clr.w	CTRL_PALM_OFFSET(a5)
	clr.w	CTRL_PAUSE_COUNTER(a5)

	; Reset sines to default and initialise other sine variables
	bsr	SIN_Init_Sine_Params	;I:a5, T:None

	; Create copper palettes
	bsr	MIA_Copper_Palettes_Create	;T:d0-d1/a0-a1

	;Depack palm trees while we show transition
	IFEQ RasterTest
	lea	MIA_Scr_Palm_Source(pc),a0
	lea	MIA_Scr_Palm,a1
	jsr	LIB_NRV2S_Depack	;I:a0/a1, T:d0-d1/a0-a1
	ELSE
	lea	MIA_Scr_Palm,a0
	move.w	#(SCR_BUF_TOTALSIZE/2),d0
	jsr	FW_ClearBuffer_CPU	;I:d0/a0, T:d0-d7/a0-a4
	ENDC
	
	;Depack sprites
	lea	Sprite_Frames_Source,a0
	lea	Sprite_Frames,a1
	jsr	LIB_NRV2S_Depack	;I:a0/a1, T:d0-d1/a0-a1

	;Precalc sprite control tables
	;bsr	Precalc_Sprite_Control_Table

	; Clear all screen buffers and setup ptrs
	bsr	MIA_Clear_ScreenBuffers_A6	;I:a6
	bsr	P1_CL_InitPtrs			;T:d0-d3/a0-a3

	;Signal that P1 IRQ/CL is ready to go
	move.w	#1,CTRL_P1_READY(a5)


	movem.l	(sp)+,d2-d7/a2-a4
	rts


*****************************************************************************
* Runs the effect.
* IN		a6, _custom
*		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d1/a0-a1
*
*****************************************************************************

P0_MainLoop:
	movem.l	d2-d7/a2-a4,-(sp)

.Mainloop1:
	tst.w	CTRL_FINISHED(a5)	;Check if script ended itself
	bne	.exit

	jsr	FW_CheckUserQuitSignal_A6	;I:a6, O:d0, T:d0
	beq.s	.Mainloop1

	;If user initiated section/intro exit then load default CL/Irq
	;So that sprites/screen is blanked and next section will not have weird fx
	jsr	FW_SetBaseCopperAndLev3Irq_A6
	jsr	FW_WaitFrame
.exit
	movem.l	(sp)+,d2-d7/a2-a4
	rts


*****************************************************************************

P0_Lev3Irq:				
	movem.l	d0-d7/a0-a6,-(sp)

	lea	_custom,a6
	lea	Controller_Info,a5
	
	;Color 00 to black
	move.w	CTRL_PALETTE_STEP(a5),d2
	cmpi.w	#15,d2
	bge.s	.p1

	lea	P0_CL_Cols,a0
	move.w	#COL_TRANS_IN,d0	;starting colour
	moveq	#$000,d1		;dest col
	addq.w	#1,d2
	move.w	d2,CTRL_PALETTE_STEP(a5)
	jsr	LIB_RGB12_Interpolate_Fast	;I:d0-d2, O:d0, T:d0-d4
	move.w	d0,2(a0)		;Write colour00
	bra.s	.noupdate
.p1:	
	;Before exiting must make sure next routine is fully ready
	tst.w	CTRL_P1_READY(a5)
	beq.s	.noupdate
	move.w	#1,CTRL_BLINDS_IN_ACTIVE(a5)
	lea	P1_Lev3Irq(pc),a0
	jsr	FW_SetLev3Irq		;Switch to main routine

	; Enable sprite dma (now done in copper)
	;move.w	#DMAF_SETCLR|DMAF_SPRITE,dmacon(a6)

.noupdate
	jsr	FW_VBlankProxy		;T:d0-d7/a0-a4
	
	;Reset interrupt
	IFNE FW_IRQ_TYPE_COPPER
		moveq	#INTF_COPER,d0
	ELSE
		moveq	#INTF_VERTB,d0
	ENDC
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)			;A4000 compat	

	movem.l	(sp)+,d0-d7/a0-a6
	rte


*****************************************************************************

P1_Lev3Irq:	
	TIMERON
	movem.l	d0-d7/a0-a6,-(sp)

	lea	_custom,a6
	lea	Controller_Info,a5

	;Use full blitter clear as Pretracker uses up 99% of CPU here
	bsr	MIA_Sine_Clear		;I:a6, Clear sine scroller screen

	jsr	FW_VBlankProxy		;T:d0-d7/a0-a4

	;Blit scroller buffer to screen
	bsr	SIN_CopyScrollToSine	;I:a5/a6, T:d0-d7/a0-a4

	;Print new letters and scroll the offscreen buffer
	bsr	SIN_PrintAndScroll	;I:a5/a6, T:d0-d4/a0-a1

	bsr	MIA_Sine_VerticalFill	;I:a6, T:d0/a0-a1
	bsr	MIA_Palm_ScrollBuffer	;I:a5, T:d0-d2/a0, Update palm layer (CPU)
	bsr	MIA_Building_Layer_Draw	;I:a5, T:d0-d2/a0-a3
	
	TIMERON2 $0ff
	bsr	MIA_Draw_Sprites	;I:a5
	TIMERON

	bsr	MIA_Copper_Sublist_Change	;T:d0-d2/a0, Poke the sublist into work copper

	;If the routine is finishing, change to blank screen CL for next routine
	;do not run ScreenBufferSwap as this will change the copper back
	tst.w	CTRL_P2_READY(a5)
	beq.s	.doswap
	clr.w	CTRL_PALETTE_STEP(a5)
	lea	P0_CL_Phys,a0
	lea	P2_Lev3Irq(pc),a1
	jsr	FW_SetCopperAndLev3Irq_A6
	bra.s	.skipswap

	; Buffer swap has to be done before end of frame (in time for changing CL ptr)
.doswap:
	bsr	ScreenBufferSwap	;I:a6, T:d0-d1/a0, swap buffers and CopperLists
.skipswap
	;Reset interrupt
	IFNE FW_IRQ_TYPE_COPPER
		moveq	#INTF_COPER,d0
	ELSE
		moveq	#INTF_VERTB,d0
	ENDC
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)			;A4000 compat

	movem.l	(sp)+,d0-d7/a0-a6

	;Blitter nasty on while we finish off any blits.
	WAITBLIT_NASTY
	TIMEROFF
	rte

*****************************************************************************

P2_Lev3Irq:				
	movem.l	d0-d7/a0-a6,-(sp)

	lea	_custom,a6
	lea	Controller_Info,a5
	
	;subq.w	#1,CTRL_PALETTE_COUNTER(a5)
	;bpl.s	.noupdate
	;move.w	#4,CTRL_PALETTE_COUNTER(a5)

	;Color 00 from black to transout
	move.w	CTRL_PALETTE_STEP(a5),d2
	cmpi.w	#15,d2
	bge.s	.p3

	lea	P0_CL_Cols,a0
	move.w	#$000,d0		;starting colour
	move.w	#COL_TRANS_OUT,d1	;dest col
	addq.w	#1,d2
	move.w	d2,CTRL_PALETTE_STEP(a5)
	jsr	LIB_RGB12_Interpolate_Fast	;I:d0-d2, O:d0, T:d0-d4
	move.w	d0,2(a0)		;Write colour00
	bra.s	.noupdate
.p3:	
	;Leave CL in place and put default irq
	jsr	FW_SetBaseLev3Irq
	move.w	#1,CTRL_FINISHED(a5)	;Signal quit

.noupdate
	jsr	FW_VBlankProxy		;T:d0-d7/a0-a4

	;Reset interrupt
	IFNE FW_IRQ_TYPE_COPPER
		moveq	#INTF_COPER,d0
	ELSE
		moveq	#INTF_VERTB,d0
	ENDC
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)			;A4000 compat

	movem.l	(sp)+,d0-d7/a0-a6
	rte


*****************************************************************************
* Setup copper list pointers.
* IN:		
* OUT:		
* TRASHED:	d0-d3/a0-a3
*
*****************************************************************************

P0_CL_InitPtrs:
	lea	P0_CL_Bpl,a0			;copper bpl pointer block
	moveq	#1,d0				;1 bpl (don't need to pass d2)
	move.l	#P0_BPL_Transition_AllBlack+4,d1	;Screen address, skip first long
	jsr	FW_InitCopperBplPtrs		;I:d0-d2/a0 T:d0/d1

	rts

*****************************************************************************
* Setup copper list pointers.
* IN:		
* OUT:		
* TRASHED:	d0-d3/a0-a3
*
*****************************************************************************

P1_CL_InitPtrs:

	;Setup items the same in front/back copper lists
	; Palm layer, assume starts at zero offset
	lea	P1_CL_Phys,a0
	lea	P1_CL_BPL_PALM_OFFSET(a0),a0	;copper bpl pointer block
	moveq	#SCR_BUF_NUMPLANES,d0
	move.l	#MIA_Scr_Palm,d1		;in d1 for InitCopperBplPtrs
	moveq 	#SCR_BPL1MOD,d2
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	; Setup sublist
	lea	P1_CL_Scr_CopJmp,a0
	move.l	#MIA_CL_Scr_Sublist_1,d1
	move.w	d1,6(a0)		;cop2lcl
	swap	d1
	move.w	d1,2(a0)		;cop2lch

	; Point sprites at dummy sprite in base copper list
	lea	P1_CL_Scr_Sprites,a0
	moveq	#8,d0
	jsr	FW_ClrCopperSprPtrs

	; Copper list buffers - copy screen list into 2nd buffer for doublebuffering
	lea	P1_CL_Phys,a2		;source
	lea	P1_CL_Log1,a3		;dest
	move.l	a2,CL_Phys_Ptr	;store CL ptrs
	move.l	a3,CL_Log1_Ptr

	move.l	a2,a0
	move.l	a3,a1
	move.w	#(P1_CL_SIZE/2)-1,d0	;size in words
.copy:
	move.w	(a0)+,(a1)+
	dbf	d0,.copy

	moveq	#0,d2			;clear top of d2

	; Sine scroller layer is double buffered so write the pointers into the two CLs
	; We use screen1 for Screen and screen2 (+SCR_SIN_BYTEWIDTH) for Work, pseudo interleaved (see screen definitions)
	; We add the top of the buffer into CL but then we add a Y offset so that all other
	; functions drawn in the centre of the screen
	lea	P1_CL_BPL_SINE_OFFSET(a2),a0	;copper bpl pointer block
	moveq	#SCR_SIN_BUF_NUMPLANES,d0
	move.l	#MIA_Scr_Sine,d1	;in d1 for InitCopperBplPtrs
	move.l	d1,d3
	move.w 	#SCR_SIN_BUF_BYTEWIDTH,d2
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1.l=scr buffer, d2.l=modulo, trashed=d0/d1
	add.l	#SCR_SIN_YOFFSET_BYTES,d3	;Move to middle of buffer
	move.l	d3,BPL_SIN_Phys_Ptr

	lea	P1_CL_BPL_SINE_OFFSET(a3),a0	;copper bpl pointer block
	moveq	#SCR_SIN_BUF_NUMPLANES,d0
	move.l	#MIA_Scr_Sine+SCR_SIN_BYTEWIDTH,d1	;in d1 for InitCopperBplPtrs
	move.l	d1,d3
	move.w	#SCR_SIN_BUF_BYTEWIDTH,d2
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1.l=scr buffer, d2.l=modulo, trashed=d0/d1
	add.l	#SCR_SIN_YOFFSET_BYTES,d3	;Move to middle of buffer
	move.l	d3,BPL_SIN_Log1_Ptr

	; Building layer, there is an extra long on the left/right for overdraw so
	;+4 for screen address
	lea	P1_CL_BPL_BUILDING_OFFSET(a2),a0	;copper bpl pointer block
	moveq	#SCR_BLD_BUF_NUMPLANES,d0
	move.l	#MIA_Scr_BLD_Scr+4,d1	;in d1 for InitCopperBplPtrs
	move.w 	#SCR_BLD_BUF_BYTEWIDTH,d2
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1.l=scr buffer, d2.l=modulo, trashed=d0/d1
	move.l	#MIA_Scr_BLD_Scr,BPL_BLD_Phys_Ptr

	lea	P1_CL_BPL_BUILDING_OFFSET(a3),a0	;copper bpl pointer block
	moveq	#SCR_BLD_BUF_NUMPLANES,d0
	move.l	#MIA_Scr_BLD_Work+4,d1	;in d1 for InitCopperBplPtrs
	move.w 	#SCR_BLD_BUF_BYTEWIDTH,d2
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1.l=scr buffer, d2.l=modulo, trashed=d0/d1
	move.l	#MIA_Scr_BLD_Work,BPL_BLD_Log1_Ptr

	rts


*****************************************************************************

;Note: These 6 ptrs must stay in this order as are accessed as a group
BPL_SIN_Phys_Ptr:	dc.l	0	; Sine buffer pointers
BPL_SIN_Log1_Ptr:	dc.l	0
BPL_BLD_Phys_Ptr:	dc.l	0	; Building buffer pointers
BPL_BLD_Log1_Ptr:	dc.l	0
CL_Phys_Ptr:		dc.l	0	; Copper list pointers
CL_Log1_Ptr:		dc.l	0

; 8 sublists to cycle between
MIA_CL_Sublists:
	dc.l	MIA_CL_Scr_Sublist_1
	dc.l	MIA_CL_Scr_Sublist_2
	dc.l	MIA_CL_Scr_Sublist_3
	dc.l	MIA_CL_Scr_Sublist_4
	dc.l	MIA_CL_Scr_Sublist_5
	dc.l	MIA_CL_Scr_Sublist_6
	dc.l	MIA_CL_Scr_Sublist_7
	dc.l	MIA_CL_Scr_Sublist_8

*****************************************************************************
* Swaps the copperlist, screen and clr/fill pointers and activates the CL
* for the next frame. 
* NOTE: Call BEFORE vblank so new copper takes effect next frame.
* IN:		a6, _custom
* OUT:		
* TRASHED:	d0-d1/a0
*****************************************************************************

ScreenBufferSwap:

	; Note: Relies on these 3 groups of ptrs  being adjacent
	; Swap sine buffer details
	lea	BPL_SIN_Phys_Ptr(pc),a0
	movem.l	(a0),d0-d1
	move.l	d1,(a0)+
	move.l	d0,(a0)+

	; Swap building buffer details
	;lea	BPL_BLD_Phys_Ptr(pc),a0
	movem.l	(a0),d0-d1
	move.l	d1,(a0)+
	move.l	d0,(a0)+

	; Swap copper buffer details
	;lea	CL_Phys_Ptr(pc),a0
	movem.l	(a0),d0-d1
	move.l	d1,(a0)+
	move.l	d0,(a0)+
	
	move.l 	d1,cop1lch(a6)		; Active NEXT frame

	rts

*****************************************************************************
* Pokes a sublist into the current work copperlist.
* IN:		
* OUT:		
* TRASHED:	d0-d2/a0
*****************************************************************************

MIA_Copper_Sublist_Change:

	bsr	MIA_PRNG	;O:d2, T:d0/d2, get random in d2
	andi.w	#8-1,d2		;0-3 range

	;get sublist pointer (*4 for long word selection)
	lea	MIA_CL_Sublists(pc),a0
	add.w	d2,d2
	add.w	d2,d2
	move.l	(a0,d2.w),d0

.write:
	;poke pointer (d0)
	move.l	CL_Log1_Ptr(pc),a0
	lea	P1_CL_COPJMP_OFFSET+2(a0),a0	;copper bpl pointer block
	move.w	d0,4(a0)	;cop2lcl
	swap	d0
	move.w	d0,(a0)		;cop2lch

	rts


*****************************************************************************
* Updates the pointers to the palm bitmap by 16 pixels to scroll.
* IN:		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d2/a0
*****************************************************************************

MIA_Palm_ScrollBuffer:

	;Update the palm layer to scroll left by incrementing pointer by 16 pixels (2 bytes)
	;When it starts at the 3rd screen (copy of screen 1) then loop back to screen 1
	;This isn't double buffered at all.
	move.w	CTRL_PALM_OFFSET(a5),d0
	ext.l	d0			;clear top
	addq.w	#2,d0			;16pixels
	cmpi.w	#SCR_BPL1MOD,d0		;reached wrap point?
	blt.s	.offsetok
	moveq	#0,d0
.offsetok:
	move.w	d0,CTRL_PALM_OFFSET(a5)
	move.l	#MIA_Scr_Palm,d1
	add.l	d0,d1

	move.l	CL_Log1_Ptr(pc),a0
	lea	P1_CL_BPL_PALM_OFFSET+2(a0),a0	;copper bpl pointer block
	move.w	d1,4(a0)	;ptl
	swap	d1
	move.w	d1,(a0)		;pth

	rts


*****************************************************************************
* Clears entire screen buffers. Done once at startup to ensure clean
* environment.
* IN:		a6, _custom
* OUT:		
* TRASHED:	
*****************************************************************************

MIA_Clear_ScreenBuffers_A6:

	; Clear building buffers
	WAITBLIT_A6
	move.l	#$01000000,bltcon0(a6)
	move.w	#0,bltdmod(a6)
	move.l	#MIA_Scr_BLD_Scr,bltdpth(a6)
	move.w	#((SCR_BLD_BUF_HEIGHT*SCR_BLD_BUF_NUMPLANES)*64)+SCR_BLD_BUF_WORDWIDTH,bltsize(a6)
	WAITBLIT_A6
	move.l	#MIA_Scr_BLD_Work,bltdpth(a6)
	move.w	#((SCR_BLD_BUF_HEIGHT*SCR_BLD_BUF_NUMPLANES)*64)+SCR_BLD_BUF_WORDWIDTH,bltsize(a6)

	; Clear palm trees area
	;WAITBLIT_A6
	;move.l	#$01000000,bltcon0(a6)
	;move.w	#0,bltdmod(a6)
	;move.l	#MIA_Scr_Palm,bltdpth(a6)
	;move.w	#((SCR_BUF_HEIGHT*SCR_BUF_NUMPLANES)*64)+SCR_BUF_WORDWIDTH,bltsize(a6)

	; Clear off screen scroller area
	WAITBLIT_A6
	move.l	#MIA_Scr_Sine_Scroller,bltdpth(a6)
	move.w	#((SIN_Scroller_Height*SIN_Scroller_NumPlanes)*64)+SIN_Scroller_WordWidth,bltsize(a6)

	; Clear sine scroller screen area
	; Max height = 1024, max wordwidth = 64 so have to be creative
	WAITBLIT_A6
	;move.l	#$01000000,bltcon0(a6)
	;move.w	#0,bltdmod(a6)
	move.l	#MIA_Scr_Sine,bltdpth(a6)
	move.w	#((SCR_SIN_BUF_HEIGHT*3*SCR_SIN_BUF_NUMPLANES)*64)+SCR_SIN_WORDWIDTH,bltsize(a6)
	
	rts


*****************************************************************************
* Clears the entire work buffer screen. Every frame.
* Clears with the blitter only.
* IN:		a6, _custom/$dff000
* OUT:		
* TRASHED:	None
*****************************************************************************

;Clear_BlitHeight = 124; 147
;Clear_CPUHeight = (SCR_SIN_HEIGHT-Clear_BlitHeight)
;Clear_CPUHeight_StartOffset = (((SCR_SIN_HEIGHT-1)*SCR_SIN_BUF_BYTEWIDTH)+SCR_SIN_BYTEWIDTH)
MIA_Sine_Clear:
	move.l	BPL_SIN_Log1_Ptr(pc),a0

	WAITBLIT_NASTY_A6
	move.l	#$01000000,bltcon0(a6)
	move.l	a0,bltdpth(a6)
	move.w	#SCR_SIN_BUF_BYTEWIDTH-SCR_SIN_BYTEWIDTH,bltdmod(a6)
	move.w	#((SCR_SIN_HEIGHT)*64)+SCR_SIN_WORDWIDTH,bltsize(a6)
	;move.w	#((Clear_BlitHeight)*64)+SCR_SIN_WORDWIDTH,bltsize(a6)
	; Max height = 1024, max wordwidth = 64
	rts

*****************************************************************************
* Copies the source copper sublist into 4 lists. Then creates 4 different
* variations.
* IN:		
* OUT:
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

MIA_Copper_Palettes_Create:
	movem.l	d2-d7/a2-a6,-(sp)

	;Copy Sublists
	lea	MIA_CL_Scr_Sublist_1,a3	;source

	move.l	a3,a0
	lea	MIA_CL_Scr_Sublist_2,a1	;dest
	move.w	#(MIA_CL_SUBLIST_SIZE/2),d0	;size in words
	jsr	FW_CopyBuffer_CPU	;I:d0/a0-a1, T:d0-d7/a0-a2

	move.l	a3,a0
	lea	MIA_CL_Scr_Sublist_3,a1	;dest
	move.w	#(MIA_CL_SUBLIST_SIZE/2),d0	;size in words
	jsr	FW_CopyBuffer_CPU	;I:d0/a0-a1, T:d0-d7/a0-a2

	move.l	a3,a0
	lea	MIA_CL_Scr_Sublist_4,a1	;dest
	move.w	#(MIA_CL_SUBLIST_SIZE/2),d0	;size in words
	jsr	FW_CopyBuffer_CPU	;I:d0/a0-a1, T:d0-d7/a0-a2

	move.l	a3,a0			;source
	lea	MIA_CL_Scr_Sublist_5,a1	;dest
	move.w	#(MIA_CL_SUBLIST_SIZE/2),d0	;size in words
	jsr	FW_CopyBuffer_CPU	;I:d0/a0-a1, T:d0-d7/a0-a2	

	move.l	a3,a0
	lea	MIA_CL_Scr_Sublist_6,a1	;dest
	move.w	#(MIA_CL_SUBLIST_SIZE/2),d0	;size in words
	jsr	FW_CopyBuffer_CPU	;I:d0/a0-a1, T:d0-d7/a0-a2

	move.l	a3,a0
	lea	MIA_CL_Scr_Sublist_7,a1	;dest
	move.w	#(MIA_CL_SUBLIST_SIZE/2),d0	;size in words
	jsr	FW_CopyBuffer_CPU	;I:d0/a0-a1, T:d0-d7/a0-a2

	move.l	a3,a0
	lea	MIA_CL_Scr_Sublist_8,a1	;dest
	move.w	#(MIA_CL_SUBLIST_SIZE/2),d0	;size in words
	jsr	FW_CopyBuffer_CPU	;I:d0/a0-a1, T:d0-d7/a0-a2


	;Create 4 palettes from our original backgroun with some darker versions
	lea	MIA_Palette_BG_1(pc),a0
	lea	PAL_AllBlack256(pc),a1
	lea	MIA_Palette_BG_2(pc),a2
	move.w	#MIA_PAL_BG_NUMCOLS,d0			;Num cols
	move.w	#0,d1			;Step
	jsr	LIB_RGB12_Interpolate_Fast_Palette

	lea	MIA_Palette_BG_1(pc),a0
	lea	PAL_AllBlack256(pc),a1
	lea	MIA_Palette_BG_3(pc),a2
	move.w	#MIA_PAL_BG_NUMCOLS,d0			;Num cols
	move.w	#1,d1			;Step
	jsr	LIB_RGB12_Interpolate_Fast_Palette

	lea	MIA_Palette_BG_1(pc),a0
	lea	PAL_AllBlack256(pc),a1
	lea	MIA_Palette_BG_4(pc),a2
	move.w	#MIA_PAL_BG_NUMCOLS,d0			;Num cols
	move.w	#2,d1			;Step
	jsr	LIB_RGB12_Interpolate_Fast_Palette

	;Create 4 palettes of background merged with white for text
	lea	MIA_Palette_BG_1(pc),a0
	lea	PAL_AllWhite256(pc),a1
	lea	MIA_Palette_BG_Text_1(pc),a2
	move.w	#MIA_PAL_BG_NUMCOLS,d0			;Num cols
	move.w	#10,d1			;Step
	jsr	LIB_RGB12_Interpolate_Fast_Palette

	lea	MIA_Palette_BG_2(pc),a0
	lea	PAL_AllWhite256(pc),a1
	lea	MIA_Palette_BG_Text_2(pc),a2
	move.w	#MIA_PAL_BG_NUMCOLS,d0			;Num cols
	move.w	#10,d1			;Step
	jsr	LIB_RGB12_Interpolate_Fast_Palette

	lea	MIA_Palette_BG_3(pc),a0
	lea	PAL_AllWhite256(pc),a1
	lea	MIA_Palette_BG_Text_3(pc),a2
	move.w	#MIA_PAL_BG_NUMCOLS,d0			;Num cols
	move.w	#10,d1			;Step
	jsr	LIB_RGB12_Interpolate_Fast_Palette

	lea	MIA_Palette_BG_4(pc),a0
	lea	PAL_AllWhite256(pc),a1
	lea	MIA_Palette_BG_Text_4(pc),a2
	move.w	#MIA_PAL_BG_NUMCOLS,d0			;Num cols
	move.w	#10,d1			;Step
	jsr	LIB_RGB12_Interpolate_Fast_Palette



	; Create 16 copper lists with the various random palettes
	lea	MIA_CL_Scr_Sublist_1,a0	
	bsr	MIA_Copper_Palette_Init	;I:a0, T:d0-d3/a1-a5
	lea	MIA_CL_Scr_Sublist_2,a0
	bsr	MIA_Copper_Palette_Init
	lea	MIA_CL_Scr_Sublist_3,a0
	bsr	MIA_Copper_Palette_Init
	lea	MIA_CL_Scr_Sublist_4,a0
	bsr	MIA_Copper_Palette_Init
	lea	MIA_CL_Scr_Sublist_5,a0
	bsr	MIA_Copper_Palette_Init
	lea	MIA_CL_Scr_Sublist_6,a0
	bsr	MIA_Copper_Palette_Init
	lea	MIA_CL_Scr_Sublist_7,a0
	bsr	MIA_Copper_Palette_Init
	lea	MIA_CL_Scr_Sublist_8,a0
	bsr	MIA_Copper_Palette_Init
	
	movem.l	(sp)+,d2-d7/a2-a6
	rts


*****************************************************************************
* Creates a randomised dark/light copper sublist
* variations.
* IN:		a0, copper sublist
* OUT:
* TRASHED:	d0-d3/a1-a5
*****************************************************************************

MIA_Copper_Palette_Init:

	lea	MIA_Palette_BG_List(pc),a1
	lea	MIA_Palette_BG_Text_List(pc),a2

	;1st half of list
	lea	MIA_CL_SUBLIST_COL1_OFFSET(a0),a5
	moveq	#0,d4		;Start line (0-255, in words)
	move.w	#(MIA_PAL_BG_NUMCOLS-DIW_V)-1,d3
	bsr	MIA_Copper_Palette_Init_Part

	;2nd half of list
	lea	MIA_CL_SUBLIST_COL2_OFFSET(a0),a5
	move.w	#((DIW_V+MIA_PAL_BG_NUMCOLS)-MIA_PAL_BG_NUMCOLS)-1,d3
	bsr	MIA_Copper_Palette_Init_Part

	rts

;d0.w=startline
;a5=cl ptr
;d3.w=count-1
;d4=palette entry (words)
MIA_Copper_Palette_Init_Part:

.loop:	
	bsr	MIA_PRNG	;get random number in d2, trashes d0-d2
	andi.w	#4-1,d2		;0-3 source palettes

	add.w	d2,d2
	add.w	d2,d2		;access table in longs

	;Background
	move.l	(a1,d2.w),a3	;Palette ptr
	move.w	(a3,d4.w),MIA_CL_SUBLIST_LINE_BKG+2(a5)	;Write the palette color into cl

	;Text
	move.l	(a2,d2.w),a3	;Palette ptr
	move.w	(a3,d4.w),MIA_CL_SUBLIST_LINE_TEXT+2(a5)	;Write the palette color into cl

	addq.w	#2,d4		;Next palette entry (next word in palette)
	lea	MIA_CL_SUBLIST_LINE_SIZE(a5),a5	;Next line
	dbf	d3,.loop

	rts

*****************************************************************************
* Random number.
* IN:	
* OUT:		d2.w
* TRASHED:	d0-d2
*****************************************************************************

MIA_PRNG:    
	movem.l   MIA_PRNG_State(pc),d0-d1

	move.l	d0,d2
	lsl.l	#2,d0
	eor.l	d2,d0           ; T = A^(A<<2)

	move.l	d1,d2
	lsr.l	#3,d2
	eor.l	d1,d2           ; B^(B>>3)

	eor.l	d0,d2           ; B^(B>>3)^T
	lsr.l	#7,d0
	eor.l	d0,d2           ; B^(B>>3)^T^(T>>7)

	movem.l   d1-d2,MIA_PRNG_State
	rts                        ; return random number in D2

MIA_PRNG_State:
	;ds.l      2                ; initialize once to non zero
	dc.l $9876fedc
	dc.l $abcd1234


*****************************************************************************
* Premultiplies the sine values by the sine screen width so we can skip some mults.
* IN:	
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

SIN_PreMult_SineForScreen:
	lea	SIN_Sine_Source(pc),a0	
	lea	SIN_Sine(pc),a1

	move.w #SIN_Sine_NumEntries-1,d1
.loop:
	move.w	(a0)+,d0
	muls 	#SCR_SIN_BUF_BYTEWIDTH,d0	; Values are positive and negative
	move.w 	d0,(a1)+			; truncate (assume no overflow and store)
	dbf	d1,.loop

	;Now copy the same data multiple times to remove need for bounds checks
	moveq	#SIN_Sine_NumEntries_Rept-1-1,d1	;-1 as already done 1 block
	lea	SIN_Sine(pc),a1
.l2
	lea	SIN_Sine_NumEntries*2(a1),a1	;skip to next destination
	move.w	d1,-(sp)
	move.l	a1,-(sp)

	lea	SIN_Sine(pc),a0			;source
	move.w	#SIN_Sine_NumEntries,d0		;number of WORDS to copy
	jsr	FW_CopyBuffer_CPU		;I:d0/a0/a1, T:d0-d7/a0-a2

	move.l	(sp)+,a1
	move.w	(sp)+,d1
	dbf	d1,.l2

	rts


*****************************************************************************
* Converts a font BPL data into something that can be used for blitter vertical filling.
* This routine assumes 1 word wide font.
* IN:		
* OUT:		
* TRASHED:	d0-d5/a0
*****************************************************************************

SIN_FontToVFill:
	lea	FON_BPL_Font8px(pc),a0	
	moveq	#SIN_Font_Bitmap_ByteWidth,d0
	moveq	#SIN_Font_NumLetters-1,d2
.nextletter	
	moveq	#0,d1
	moveq	#SIN_Font_BPL_Height-1,d5
	moveq	#0,d4
.nextline	
	move.w	(a0,d1.w),d3
	eor.w	d3,d4
	move.w	d4,(a0,d1.w)
	move.w	d3,d4
	add.w	d0,d1
	dbf	d5,.nextline

	lea	SIN_Font_BPL_Height*SIN_Font_Bitmap_ByteWidth(a0),a0
	dbf	d2,.nextletter
	
	rts


*****************************************************************************
* Resets sine values to defaults.
* IN:		a5, Controller_Info	
* OUT:		
* TRASHED:	d0
*****************************************************************************

SIN_Init_Sine_Params:
	moveq	#0,d0
	move.w	d0,CTRL_SIN_LASTLETTERFRAMECOUNT(a5)
	move.w	d0,CTRL_SIN_STOPSCROLL(a5)
	bsr.s	SIN_Reset_Sine_Params
	
	rts

SIN_Reset_Sine_Params:
	moveq	#0,d0
	move.w	d0,CTRL_SIN_SINE1_OFFSET(a5)
	move.w	d0,CTRL_SIN_SINE2_OFFSET(a5)
	move.w	#-4,CTRL_SIN_SINE1_SPEED(a5)	;motion per frame
	move.w	#3,CTRL_SIN_SINE1_STEP(a5)	;motion per pixel
	move.w	d0,CTRL_SIN_SINE2_SPEED(a5)
	move.w	d0,CTRL_SIN_SINE2_STEP(a5)

	move.w	#3,CTRL_SIN_SPEED(a5)

	rts


*****************************************************************************
* Prints letters and scrolls the hidden scroller buffer.
* IN:		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d4/a0-a1
*****************************************************************************

SIN_PrintAndScroll:

	; Are we pausing?
	cmpi.w #0,CTRL_SIN_STOPSCROLL(a5)
	beq.s .ReadInput
	subq.w #1,CTRL_SIN_STOPSCROLL(a5)
	rts

.ReadInput:
	;Check if need to draw a new letter. If not, scroll the entire
	; buffer and rts
	moveq	#$f,d0
	and.w	CTRL_SIN_SPEED(a5),d0		;keep in range of single blitter shift
	add.w	d0,CTRL_SIN_LASTLETTERFRAMECOUNT(a5)	;Keep track of how much scrolling since last letter written

	cmpi.w	#(SIN_Font_PixelWidth),CTRL_SIN_LASTLETTERFRAMECOUNT(a5)
	blt	SIN_Scroll_Blit			;I:a5/a6, T:d0-d3/a0-a1

	clr.w	CTRL_SIN_LASTLETTERFRAMECOUNT(a5)
	move.l	CTRL_SIN_TEXT_PTR(a5),a0
.read_next
	moveq	#0,d0			;clear top of d0 for .b access
	move.b	(a0)+,d0
	bne.s	.CharOK			;Finished?
	move.w	#1,CTRL_FINISHED(a5)	;End of message
	rts

.CharOK:
	cmpi.b	#SIN_FX_PAUSE,d0
	beq.s	.pause_command
	cmpi.b	#SIN_FX_SPEED,d0
	beq.s	.speed
	cmpi.b	#SIN_FX_SINE_SPEEDSTEP,d0
	beq.s	.sine_speedstep
	cmpi.b	#SIN_FX_SINE_RESET,d0
	beq.s	.sine_reset
	cmpi.b	#SIN_FX_BUILDINGS_OFF,d0
	beq.s	.buildings_off

	; Must be a normal letter
.DrawLetter
	move.l	a0,CTRL_SIN_TEXT_PTR(a5)	;Save pointer

	; Scroll smaller section of screen so we can draw new letter with CPU :)
	move.w	d0,d4
	bsr 	SIN_Scroll_Blit_NoLetter	;I:a5/a6, T:d0-d3/a0-a1
	move.w	d4,d0
	bsr	MIA_Sine_PrintLetter_CPU	;T:d0-d4/d6-d7/a0-a2
	rts

.speed:	
	move.b	(a0)+,d0
	move.w	d0,CTRL_SIN_SPEED(a5)
	bra.s	.read_next

.sine_speedstep:	
	move.b	(a0)+,d0
	ext.w	d0
	move.b	(a0)+,d1
	ext.w	d1
	move.b	(a0)+,d2
	ext.w	d2
	move.b	(a0)+,d3
	ext.w	d3
	movem.w	d0-d3,CTRL_SIN_SINE1_SPEED(a5)
	bra.s	.read_next

.sine_reset:	
	bsr	SIN_Reset_Sine_Params		;I:a5, T:d0
	bra	.read_next

.buildings_off:
	move.w	#1,CTRL_BUILDINGS_STOP_FLAG(a5)
	bra	.read_next

.pause_command	
	move.b	(a0)+,d0
	
	;mulu.w	#10,d0
	lsl.w	#3,d0	;*8 is close enough

	move.w	d0,CTRL_SIN_STOPSCROLL(a5)
	move.l	a0,CTRL_SIN_TEXT_PTR(a5)
	rts


*****************************************************************************
* Scrolls the hidden buffer at the correct speed
* IN:		a5, Controller_Info
*		a6, _custom
* OUT:		
* TRASHED:	d0-d3/a0-a1
*****************************************************************************

SIN_Scroll_Blit:
	; destination is the last word of the scrollbuffer (as we are in descending mode for left scrolling)
	; Add our by our padding height (so we drawing the letter in the center of the buffer)
	lea	MIA_Scr_Sine_Scroller+((SIN_Font_Height*SIN_Scroller_ByteWidth)-2),a0	; Assumes 1bpl

	; Shift speed (max $f)
	move.l	#$09f00002,d2			;bltcon0

	move.w	CTRL_SIN_SPEED(a5),d0		;Shift
	move.w	d0,d3				;save shift speed
	ror.l	#4,d0				;move it to the last nibble
	or.l	d0,d2				;Or it with bltcon0

;To prevent data that is scrolled out of the left reappearing on the right we have to mask the LAST word 
;By the same amount that we are shifting. Last mask is applied BEFORE the shift occurs so only zeros
;get shifted back into the right
;shift 1, mask $7fff
;shift 2, mask $3fff
	moveq	#-1,d0				; $ffffffff for FWM/LWM
	lsr.w	d3,d0				; add x number of zeroed bits to LOW word (LWM)

	lea	bltapth(a6),a1
	moveq	#0,d1

	WAITBLIT_NASTY_A6
	move.l	d2,bltcon0(a6)			;4 pixels,use A & D,LF=copy
	move.l	d0,bltafwm(a6)			;FWM, LWM
	move.l	d1,bltamod(a6)			;No modulo in A and D
	move.l	a0,(a1)+			;bltapth
	move.l	a0,(a1)+			;bltdpth
	move.w	#SIN_Font_Height*64+(SIN_Scroller_WordWidth),(a1)	;bltsize

	rts


; Instead of scrolling entire buffer, scroll just visible - we can then update letter
; at the right edge with the CPU at the same time
SIN_Scroll_Blit_NoLetter:
	; destination is the last word of the scrollbuffer (as we are in descending mode for left scrolling)
	; Add our by our padding height (so we drawing the letter in the center of the buffer)
	lea	(MIA_Scr_Sine_Scroller+((SIN_Font_Height*SIN_Scroller_ByteWidth)-2))-SIN_Scroller_ExtraByteWidth,a0	; Assumes 1bpl

	; Shift speed (max $f)
	move.l	#$09f00002,d2			;bltcon0, copy descending mode

	move.w	CTRL_SIN_SPEED(a5),d0		;Shift
	move.w	d0,d3				;save shift speed
	ror.l	#4,d0				;move it to the last nibble
	or.l	d0,d2				;Or it with bltcon0

;To prevent data that is scrolled out of the left reappearing on the right we have to mask the LAST word 
;By the same amount that we are shifting. Last mask is applied BEFORE the shift occurs so only zeros
;get shifted back into the right
;shift 1, mask $7fff
;shift 2, mask $3fff
	moveq	#-1,d0				; $ffffffff for FWM/LWM
	lsr.w	d3,d0				; add x number of zeroed bits to LOW word (LWM)

	lea	bltapth(a6),a1
	moveq	#SIN_Scroller_ExtraByteWidth,d1	;modulo

	WAITBLIT_NASTY_A6
	move.l	d2,bltcon0(a6)			;
	move.l	d0,bltafwm(a6)			;FWM, LWM
	move.w	d1,bltamod(a6)			;
	move.w	d1,bltdmod(a6)			;
	move.l	a0,(a1)+			;bltapth
	move.l	a0,(a1)+			;bltdpth
	move.w	#SIN_Font_Height*64+(SIN_Scroller_WordWidth-SIN_Scroller_ExtraWordWidth),(a1)	;bltsize

	rts

*****************************************************************************
* Blits a given letter to the scroll buffer
* IN:		d0.w - ASCII value of letter to print
* OUT:		
* TRASHED:	d0-d6/a0-a1
*****************************************************************************

MIA_Sine_PrintLetter_CPU:
	ext.w	d0			;clear top
	
	;bob number (top d0 still clear) is letter valid? $ff = not valid
	;assumption is will never have >127 bobs in a font so can test for $ff by bmi
	lea	FON_FAR_Font8px(pc),a1
	move.b	(a1,d0.w),d0
	bmi.s	.exit

	lea	FON_BPL_Font8px(pc),a0		;root bob bitplane data
	lea	FON_BOB_Font8px(pc),a1
	mulu	#LIB_BOBTABLE_SIZEOF,d0		;find bob offset for this bob in FON_BOB_Font8px
	lea	(a1,d0.w),a1			;BOB entry
	add.l	LIB_BOBTABLE_OFFSET(a1),a0	;offset to bob bpl

	; Destination is scroller buffer extreme right edge, minus font width
	lea	MIA_Scr_Sine_Scroller+(SIN_Scroller_ByteWidth-SIN_Scroller_ExtraByteWidth),a1	
	
	; CPU BLIT
	; We are taking 8 pixels wide and change to 64 pixels
	moveq	#SIN_Scroller_ByteWidth-8,d5	;we are auto advancing dest by 2 longs per loop
	moveq	#SIN_Font_Height-1,d6	;8 lines high

	moveq	#$f,d4
 	moveq   #$f<<2,d3
.loop:
	moveq	#0,d1		;ensure bottom word is clear at start
	move.b	(a0),d1		;Get the byte of the source font
	move.w	d1,d2		;save

	lsr.w	#2,d2		;top nibble, access in longs
        and.w   d3,d2
	move.l	.lut(pc,d2.w),(a1)+	;save long
	;move.l	#$ffffffff,(a1)+

	and.w	d4,d1		;d1 is bottom nibble of orginal
	add.w	d1,d1
	add.w	d1,d1		;access in longs
	move.l	.lut(pc,d1.w),(a1)+	;save long
	;move.l	#$ffffffff,(a1)+

	addq.l	#2,a0		;next line src
	add.w	d5,a1		;next line dest
	dbf	d6,.loop
.exit:
	rts

.lut:
	dc.l	$00000000	;0
	dc.l	$000000ff	;1
	dc.l	$0000ff00	;2
	dc.l	$0000ffff	;3
	dc.l	$00ff0000	;4
	dc.l	$00ff00ff	;5
	dc.l	$00ffff00	;6
	dc.l	$00ffffff	;7
	dc.l	$ff000000	;8
	dc.l	$ff0000ff	;9
	dc.l	$ff00ff00	;10
	dc.l	$ff00ffff	;11
	dc.l	$ffff0000	;12
	dc.l	$ffff00ff	;13
	dc.l	$ffffff00	;14
	dc.l	$ffffffff	;15


*****************************************************************************
* Copies the scroller from the scroll buffer and plots in a sine wave.
* Tried various blitter nasty tweaks. But running with blitter nasty off is
* a few cycles quicker in this case becuase of the CPU code run outside each
* blit. Also tried precalculating all the sines up front each frame but not
* faster.
* IN:		a6, _custom
*		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d7/a0-a4
*****************************************************************************

;a0 - Sine points ptr (premult by screen width in bytes)
;a1 - temp for writing blit registers quickly
;a2 - bltafwm(a6) speed up blitter loading
;a3 - Screen buffer ptr
;a4 - scroller buffer (source)
;a5 - Screen buffer (Destination)
;a6 - custom
;d0 - bltsize
;d1 - blit loop count / #6 for blitwait (swap every 16 pixels)
;d2 - ROT mask
;d3 - Sine1 step (offset into sine table)
;d4 - Sine2 step (offset into sine table)
;d5 - Sine offset mask
;d6 - Sine1 (offsets into sine table)
;d7 - Sine2 (offsets into sine table)

; Do OR blit
BlitSine	MACRO
	move.l	a3,a5				;scr adr
	add.w 	(a0,d6.w),a5		        ;add sine 1 (premult)
	add.w	(a0,d7.w),a5		        ;add sine 2 (premult)
	add.w	d3,d6				;Increase sine1 offsets 
	add.w	d4,d7				;Increase sine2 offsets 

	lea	bltbpth(a6),a1			;for quick blitter loading

	;TIMERON	$f00
.bwt\@	btst.b	d1,dmaconr(a6)			;Blitwait
	bne.s	.bwt\@
	;TIMERON	$111

	; write blt registers as fast as possible
	move.w	#\1,(a2)			;bltafwm($44)
	move.l	a5,(a1)+			;bltbpth - $4c
	move.l	a4,(a1)+			;bltapth - $50
	move.l	a5,(a1)+			;bltdpth - $54
	move.w	d0,(a1)				;bltsize - $58

	ENDM

SIN_CopyScrollToSine:
	lea	bltafwm(a6),a2

	;Get sine speed and steps
	lea	SIN_Sine(pc),a0

	move.w	#SIN_Sine_Offset_Mask,d5	;Used to keep the offset in range

	; Sine 1
	move.w	CTRL_SIN_SINE1_OFFSET(a5),d6	;Offset in words
	move.w	CTRL_SIN_SINE1_SPEED(a5),d3	;Get speed (movement per frame)
	add.w	d3,d3				;Offset in words
	add.w	d3,d6
	and.w	d5,d6				;Ensure in range
	move.w	d6,CTRL_SIN_SINE1_OFFSET(a5)	;Save for next frame
	move.w	CTRL_SIN_SINE1_STEP(a5),d3	;Get step (movement per pixel)
	add.w	d3,d3				;Offset in words

	; Sine 2
	move.w	CTRL_SIN_SINE2_OFFSET(a5),d7	;Offset in words
	move.w	CTRL_SIN_SINE2_SPEED(a5),d4	;Get speed (movement per frame)
	add.w	d4,d4				;Offset in words
	add.w	d4,d7
	and.w	d5,d7				;Ensure in range
	move.w	d7,CTRL_SIN_SINE2_OFFSET(a5)	;Save for next frame
	move.w	CTRL_SIN_SINE2_STEP(a5),d4	;Get step (movement per pixel)
	add.w	d4,d4				;Offset in words

	; Our starting source buffer and screen address. Add the padding.
	; Padding will copied every 16th blit
	lea 	MIA_Scr_Sine_Scroller,a4	;scroller buffer
	move.l	BPL_SIN_Log1_Ptr(pc),a3	;scr adr
	move.w 	#(SIN_Font_Height)*64+1,d0	;bltsize
	moveq	#6,d1				;Speed up blitwait test

	moveq	#SCR_SIN_WORDWIDTH-1,d2		;number of loops to cover screen in 16pixel blocks
	lea	bltbmod(a6),a1			;Speed up blitter loading, after wait
	WAITBLIT_NASTY_A6
	;D=A or B
	;move.l	#$0dfc0000,bltcon0(a6)		; bltcon0+1	
	move.l	#((BLT_SRC_ABD+(BLT_A|BLT_B))<<16)|0000,bltcon0(a6)
	move.w	#$ffff,bltalwm(a6)		; Doesn't change
	move.w	#(SCR_SIN_BUF_BYTEWIDTH*SIN_Font_HeightMultiplier)-2,(a1)+	; Screen buffer mod, bltbmod($62)
	move.w	#SIN_Scroller_ByteWidth-2,(a1)+	; Scroller buffer mod, bltamod($64)
	move.w	#(SCR_SIN_BUF_BYTEWIDTH*SIN_Font_HeightMultiplier)-2,(a1)	; Screen buffer mod, bltdmod($66)
.blitloop:
	;swap	d1				;Change counter to blitwait #6

	; 16 blits on same word with different masks
	BlitSine $8000
	BlitSine $4000
	BlitSine $2000
	BlitSine $1000
	BlitSine $0800
	BlitSine $0400
	BlitSine $0200
	BlitSine $0100
	BlitSine $0080
	BlitSine $0040
	BlitSine $0020
	BlitSine $0010
	BlitSine $0008
	BlitSine $0004
	BlitSine $0002
	BlitSine $0001

	addq.l 	#2,a3				;add to dest address
	addq.l 	#2,a4				;add to source address
	
	;Only do masking/bounds check every 16 steps. For this to work we have more sine entries 
	;Totally removed bounds checking, needed 6 copies of the sine table :)
	;and.w	d5,d6				;and mask to 0-2047 (1024 sine entries, in words)
	;and.w	d5,d7				;and mask to 0-2047 (1024 sine entries, in words)

	dbf	d2,.blitloop

	lea	Controller_Info(pc),a5	;restore a5 as we trashed it

	rts	


*****************************************************************************
* Does a blitter vertical fill on the screen.
* IN:		A6(_custom)
* OUT:		
* TRASHED:	d0-d1/a0-a3
*****************************************************************************

MIA_Sine_VerticalFill:
	;rts
; A XOR B
; A B C  D  ABD=$d, LF = $3C
; 0 0 x  0
; 0 0 x  0
; 0 1 x  1
; 0 1 x  1
; 1 0 x  1
; 1 0 x  1
; 1 1 x  0
; 1 1 x  0

	move.l	BPL_SIN_Log1_Ptr(pc),a1	;B PTR is first line
	lea	SCR_SIN_BUF_BYTEWIDTH(a1),a0	;A and D PTR is the next line
	lea	bltbpth(a6),a2
	lea	bltbmod(a6),a3
	moveq	#-1,d0
	moveq	#SCR_SIN_BUF_BYTEWIDTH-SCR_SIN_BYTEWIDTH,d1

	WAITBLIT_NASTY_A6
	;move.l	#$0d3c0000,bltcon0(a6)	;bltcon0/1
	move.l	#((BLT_SRC_ABD+(BLT_A^BLT_B))<<16)|0000,bltcon0(a6)	;bltcon0/1
	move.l	d0,bltafwm(a6)
	move.w	d1,(a3)+		;bltbmod
	move.w	d1,(a3)+		;bltamod
	move.w	d1,(a3)			;bltdmod
	move.l	a1,(a2)+		;bltbpth
	move.l	a0,(a2)+		;bltapth
	move.l	a0,(a2)+		;bltdpth
	move.w	#(SCR_SIN_HEIGHT-1)*64+(SCR_SIN_WORDWIDTH*SCR_SIN_NUMPLANES),(a2) ;bltsize
	;WAITBLIT
	;move.l	BPL_SIN_Log1_Ptr(pc),a1
	;lea	(SCR_SIN_HEIGHT-1)*SCR_SIN_BUF_BYTEWIDTH(a1),a1
	;move.l	#$ffffffff,(a1)

	rts


*****************************************************************************
* Draws the buildings.
* IN:		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d2/a0-a3
*****************************************************************************

MIA_Building_Layer_Draw:
	; Are we drawing transition blinds? 
	tst.w	CTRL_BLINDS_IN_ACTIVE(a5)
	bne.s	MIA_Building_Layer_Draw_BlindsIn
	
	tst.w	CTRL_BLINDS_OUT_ACTIVE(a5)
	bne	MIA_Building_Layer_Draw_BlindsOut

	bra	MIA_Building_Layer_Draw_Normal


*****************************************************************************
* Draws opening blinds.
* IN:		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d2/a0-a3
*****************************************************************************

MIA_Building_Layer_Draw_BlindsIn:
	move.l	CL_Log1_Ptr(pc),a0
	move.w	#COL_TRANS_IN,P1_CL_COL_BLD_OFFSET+2(a0)

	;Time to update?
	lea	.count(pc),a0
	moveq	#0,d3			;incrment
	tst.w	(a0)
	bne.s	.justdraw
	moveq	#1,d3			;inc + 1
	move.w	#3,(a0)		;reset count
.justdraw
	subq.w	#1,(a0)

	lea	Blinds_In_Anim_1_BPL(pc),a0
	move.l	BPL_BLD_Log1_Ptr(pc),a1
	lea	.Blinds_Frame(pc),a2
	addq.l	#4,a1			;skip first long

	moveq	#(SCR_BLD_BUF_WORDWIDTH/2)-2-1,d0	;skip left/right longs
	moveq	#0,d2			;flag
.l1:	
	move.w	(a2),d1			;get anim frame
	bpl.s	.checkmax		;<0 = frame 0
	moveq	#0,d1			;draw frame 0
.checkmax
	cmp.w	#BLINDS_IN_ANIM_FRAMES,d1 ;max frame done?
	blt.s	.frameok
	move.w	#BLINDS_IN_ANIM_FRAMES-1,d1	;draw last frame
	bra.s	.draw
.frameok:
	moveq	#1,d2			;flag we stil updating
.draw:
	add.w	d1,d1
	add.w	d1,d1			;frame to long offset
	move.l	(a0,d1.w),(a1)		;draw to screen
	add.w	d3,(a2)			;update frame with current increment
.next:
	addq.l	#2,a2			;Next blind data word
	addq.l	#4,a1			;Next long on screen
	dbf	d0,.l1

	;Has everything finish
	tst.w	d2
	bne.s	.exit
	clr.w	CTRL_BLINDS_IN_ACTIVE(a5)
.exit:
	rts

.count:	dc.w	0

.Blinds_Frame:
	;This should be (SCR_BLD_BUF_WORDWIDTH/2)-2 as no anim needed on left/right long
	;11 longs for 352 screen
	dc.w	-40,-32,-24,-16,-8,0,-8,-16,-24,-32,-40

Blinds_In_Anim_1_BPL:
	dc.l	%11111111111111111111111111111111
	dc.l	%01111111111111111111111111111110
	dc.l	%00111111111111111111111111111100
	dc.l	%00011111111111111111111111111000
	dc.l	%00001111111111111111111111110000
	dc.l	%00000111111111111111111111100000
	dc.l	%00000011111111111111111111000000
	dc.l	%00000001111111111111111110000000
	dc.l	%00000000111111111111111100000000
	dc.l	%00000000011111111111111000000000
	dc.l	%00000000001111111111110000000000
	dc.l	%00000000000111111111100000000000
	dc.l	%00000000000011111111000000000000
	dc.l	%00000000000001111110000000000000
	dc.l	%00000000000000111100000000000000
	dc.l	%00000000000000011000000000000000
	dc.l	%00000000000000000000000000000000
Blinds_In_Anim_1_BPL_End:

BLINDS_IN_ANIM_FRAMES = (Blinds_In_Anim_1_BPL_End-Blinds_In_Anim_1_BPL)/4


*****************************************************************************
* Draws closing blinds.
* IN:		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d2/a0-a3
*****************************************************************************

MIA_Building_Layer_Draw_BlindsOut:
	move.l	CL_Log1_Ptr(pc),a0
	move.w	#COL_TRANS_IN,P1_CL_COL_BLD_OFFSET+2(a0)	

	;Time to update?
	lea	.count(pc),a0
	moveq	#0,d3			;incrment
	tst.w	(a0)
	bne.s	.justdraw
	moveq	#1,d3			;inc + 1
	move.w	#3,(a0)		;reset count
.justdraw
	subq.w	#1,(a0)

	lea	Blinds_Out_Anim_1_BPL(pc),a0
	move.l	BPL_BLD_Log1_Ptr(pc),a1
	lea	.Blinds_Frame(pc),a2
	addq.l	#4,a1			;skip first long

	moveq	#(SCR_BLD_BUF_WORDWIDTH/2)-2-1,d0	;skip left/right longs
	moveq	#0,d2			;flag
.l1:	
	move.w	(a2),d1			;get anim frame
	bpl.s	.checkmax		;<0 = frame 0
	moveq	#0,d1			;draw frame 0
.checkmax
	cmp.w	#BLINDS_OUT_ANIM_FRAMES,d1 ;max frame done?
	blt.s	.frameok
	move.w	#BLINDS_OUT_ANIM_FRAMES-1,d1	;draw last frame
	bra.s	.draw
.frameok:
	moveq	#1,d2			;flag we stil updating
.draw:
	add.w	d1,d1
	add.w	d1,d1			;frame to long offset
	move.l	(a0,d1.w),(a1)		;draw to screen
	add.w	d3,(a2)			;update frame with current increment
.next:
	addq.l	#2,a2			;Next blind data word
	addq.l	#4,a1			;Next long on screen
	dbf	d0,.l1

	;Has everything finish
	tst.w	d2
	bne.s	.exit
	move.w	#1,CTRL_P2_READY(a5)	;Ask for P3
.exit:
	rts

.count:	dc.w	0

.Blinds_Frame:
	;This should be (SCR_BLD_BUF_WORDWIDTH/2)-2 as no anim needed on left/right long
	;11 longs for 352 screen
	dc.w	-40,-36,-32,-28,-24,-20,-16,-12,-8,-4,0

Blinds_Out_Anim_1_BPL:
	dc.l	%00000000000000000000000000000000
	dc.l	%00000000000000011000000000000000
	dc.l	%00000000000000111100000000000000
	dc.l	%00000000000001111110000000000000
	dc.l	%00000000000011111111000000000000
	dc.l	%00000000000111111111100000000000
	dc.l	%00000000001111111111110000000000
	dc.l	%00000000011111111111111000000000
	dc.l	%00000000111111111111111100000000
	dc.l	%00000001111111111111111110000000
	dc.l	%00000011111111111111111111000000
	dc.l	%00000111111111111111111111100000
	dc.l	%00001111111111111111111111110000
	dc.l	%00011111111111111111111111111000
	dc.l	%00111111111111111111111111111100
	dc.l	%01111111111111111111111111111110
	dc.l	%11111111111111111111111111111111

Blinds_Out_Anim_1_BPL_End:

BLINDS_OUT_ANIM_FRAMES = (Blinds_Out_Anim_1_BPL_End-Blinds_Out_Anim_1_BPL)/4


*****************************************************************************
* Draws the buildings.
* IN:		a5, MIA_Contrller_Info
* OUT:		
* TRASHED:	d0-d2/a0-a3
*****************************************************************************

MIA_Building_Layer_Draw_Normal:
	move.l	CL_Log1_Ptr(pc),a0
	move.w	#COL_BUILDING,P1_CL_COL_BLD_OFFSET+2(a0)

	;Clear buffer, don't need to clear the two longs on left/right
	move.l	BPL_BLD_Log1_Ptr(pc),a1
	lea	4(a1),a0		;skip first long
	moveq	#0,d0
	REPT	(SCR_BLD_BUF_WORDWIDTH/2)-2	;2 longs less
	move.l	d0,(a0)+
	ENDR


	moveq	#0,d3			;flag if any buildings visible
	lea 	MIA_Building1_Positions(pc),a0
	moveq	#MIA_BUILDING1_POSITIONS_NUM-1,d2

.buildingloop:
	move.w	(a0),d0			;pos

	;if greater than BUILDING_MAX_X and we stopping just skip it
	tst.w	CTRL_BUILDINGS_STOP_FLAG(a5)
	beq.s	.notstopping
	cmpi.w	#BUILDING_MAX_X,d0
	bgt.s	.skipdraw
.notstopping:
	move.w	2(a0),d1		;speed
	sub.w	d1,d0			;subtract
	bpl.s	.xok
	add.w	#BUILDING_MAX_X*2,d0	;wrap
.xok:
	move.w	d0,(a0)
	
	; Is drawable?
	cmpi.w	#BUILDING_MAX_X,d0
	bgt.s	.skipdraw
	moveq	#1,d3			;something will be drawn

	move.w	d0,d1
	andi.w	#16-1,d0		;shift value
	lsr.w	#4,d1			;byte offset
	add.w	d1,d1			;even byte offset
	lea	(a1,d1.w),a3		;a3 is screen address

	add.w	d1,d1			;long offset (for BPL lookup)
	lea	MIA_Building32_BPL(pc),a2
	lea	(a2,d1.w),a2

	moveq	#0,d1			;clear top of d1 for swaps
	move.w	(a2)+,d1
	swap	d1			;move data to top of long for shifting
	lsr.l	d0,d1			;do the shift
	or.w	d1,2(a3)		;shift overspill into next word
	swap	d1
	or.w	d1,(a3)+		;shifted data, and advance 1 word

	moveq	#0,d1			;clear top of d1 for swaps
	move.w	(a2)+,d1
	swap	d1			;move data to top of long for shifting
	lsr.l	d0,d1			;do the shift
	or.w	d1,2(a3)		;shift overspill into next word
	swap	d1
	or.w	d1,(a3)			;shifted data

.skipdraw:
	addq.l	#4,a0			;next building
	dbf	d2,.buildingloop

	;If we are stopping then check if anything was drawn
	tst.w	CTRL_BUILDINGS_STOP_FLAG(a5)
	beq.s	.exit
	tst.w	d3
	bne.s	.exit
	move.w	#1,CTRL_BLINDS_OUT_ACTIVE(a5)	;Start closing blinds
	
.exit:
	rts

MIA_Building1_Positions:
;	dc.w	BUILDING_MAX_X,3			;position, speed
;	dc.w	BUILDING_MAX_X*2,4
;	dc.w	BUILDING_MAX_X,7			;position, speed
;	dc.w	BUILDING_MAX_X*2,7			;position, speed

	dc.w	(BUILDING_MAX_X*2)+32,2
	dc.w	(BUILDING_MAX_X*2)+32,2

	dc.w	(BUILDING_MAX_X*2)+32,3
	dc.w	(BUILDING_MAX_X*2)+32+20,3

	dc.w	(BUILDING_MAX_X*2)+32,4

	;dc.w	(BUILDING_MAX_X*2)+32,5
	;dc.w	(BUILDING_MAX_X*2)+32+20,5

	dc.w	(BUILDING_MAX_X*2)+32,6
	dc.w	(BUILDING_MAX_X*2)+32,7
	dc.w	(BUILDING_MAX_X*2)+32,8

MIA_Building1_Positions_End:
MIA_BUILDING1_POSITIONS_NUM = (MIA_Building1_Positions_End-MIA_Building1_Positions)/4


;352+32+32 = 24 longs
MIA_Building32_BPL:
	INCBIN "AssetsConverted\Miami_Buildings_32x24x1.BPL"


*****************************************************************************
* Draws the bird sprites.
* IN:		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d5/a0-a2
*****************************************************************************

SPRITE_XPOS_MAX = SCR_SIN_WIDTH+96	;Don't immediately wrap around
SPRITE_YPOS = 16

MIA_Draw_Sprites:
	;Get frame number and update if needed
	;Initial framecount is zero so will set valid ptrs on first run
	move.w	CTRL_SPRITE_BIRD_FRAME(a5),d1
	subq.w	#1,CTRL_SPRITE_BIRD_COUNTER(a5)
	bpl.s	.getptr
	move.w	#4,CTRL_SPRITE_BIRD_COUNTER(a5)
	addq.w	#1,d1			;next frame
	andi.w	#SPRITE_FRAME_MASK,d1	;keep in range
	move.w	d1,CTRL_SPRITE_BIRD_FRAME(a5)
.getptr
	;get the offsets of the two sprites
	lea	Sprites_Frames_Y_Pos(pc),a2
	add.w	d1,d1			;access in words
	move.w	(a2,d1.w),d5		;y pos to plot at

	lea	Sprite_Frames,a2
	add.w	d1,d1			;skip to offset of 2 sprites (2 frames, 1 words per sprite)
	move.w	(a2,d1.w),d0		;sprite 0 offset
	move.w	2(a2,d1.w),d1		;sprite 1 offset
	lea	(a2,d0.w),a1		;sprite 0 ptr
	lea	(a2,d1.w),a2		;sprite 1 ptr

	;Update sprite ptrs
	move.l	CL_Log1_Ptr(pc),a0
	lea	P1_CL_SPRITES_OFFSET+2(a0),a0	;copper spr0 ptr
	move.l	a1,d1
	move.w	d1,4(a0)		;ptl
	swap	d1
	move.w	d1,(a0)			;pth
	addq.l	#8,a0	
	move.l	a2,d1
	move.w	d1,4(a0)		;ptl
	swap	d1
	move.w	d1,(a0)			;pth

	;Update x pos
	move.w	CTRL_SPRITE_BIRD_XPOS(a5),d0
	addq.w	#1,d0
	cmpi.w	#SPRITE_XPOS_MAX,d0
	blt.s	.xok
	moveq	#-32,d0			;Just off left of screen
.xok
	move.w	d0,CTRL_SPRITE_BIRD_XPOS(a5)
	move.w	d0,d4			;save
	cmp.w	#SCR_SIN_WIDTH,d0	;If sprite off screen, blank the ctrl words
	blt.s	.spr0xok			

	moveq	#0,d0
	move.l	d0,(a1)			;blank both ctrl words
	bra.s	.spr1
.spr0xok
	;sprite 0 control words
	move.w	d5,d1			;y position
	moveq	#SPRITE_HEIGHT,d2	;height
	GET_SPRITE_CTRL_WORDS		;I:d0-d2, O:d1-d2, T:d0-d3
	move.w	d1,(a1)+		;update spr0 control words
	move.w	d2,(a1)

.spr1:
	move.w	d4,d0			;x position	
	add.w	#16,d0			;x+16
	cmp.w	#SCR_SIN_WIDTH,d0	;If sprite off screen, blank the ctrl words
	blt.s	.spr1xok		

	moveq	#0,d0
	move.l	d0,(a2)			;blank both ctrl words
	bra.s	.exit
.spr1xok
	;sprite 1 control words
	move.w	d5,d1			;y position
	moveq	#SPRITE_HEIGHT,d2	;height
	GET_SPRITE_CTRL_WORDS		;I:d0-d2, O:d1-d2, T:d0-d3
	move.w	d1,(a2)+		;update spr0 control words
	move.w	d2,(a2)
.exit:
	rts


*****************************************************************************
* Gets the 2 control words for a sprite given x,y,height (given in DIW display coords)
* X 0 to 447
* Y 0 to 262
*
* VSTART,VSTOP,HSTART are 9 bit values.
*
* Word1 SPRxPOS
*	Bits 15-8 contain the low 8 bits of VSTART
*	Bits 7-0 contain the high 8 bits of HSTART
*
* Word2 SPRxCTL
*	Bits 15-8	The low eight bits of VSTOP
*	Bit 7		(Used in attachment)
*	Bits 6-3	Unused (make zero)
*	Bit 2		The VSTART high bit
*	Bit 1		The VSTOP high bit
*	Bit 0		The HSTART low bit
*
* IN:		d0-d2, x,y,height  (x,y in DIW/display values)
* OUT:		d1,d2, control words
* TRASHED:	d0/d3
*****************************************************************************
	IFNE 0
Get_Sprite_Control_Words:
	add.w	#DIW_H-1,d0   		;Sprites are X-1, mistake in HKRM
	add.w	#DIW_V,d1 

	add.w	d1,d2     		;d2 is vstop
	moveq	#0,d3         

	lsl.w	#8,d1			;vstart low 8 bits to top of word
	addx.b	d3,d3     		;left shift and vstart high bit to d3

	lsl.w	#8,d2			;vstop low 8 bits to top of word
	addx.b	d3,d3     		;left shift and vstop high bit to d3 

	lsr.w	#1,d0			;shift out hstart low bit
	addx.b	d3,d3     		;left shift and h start low bit to d3

	move.b	d0,d1			;make first control word
	move.b	d3,d2			;second control word

	rts
	ENDC


*****************************************************************************
* Gets the 2 control words for a sprite given x,y,height (given in DIW display coords)
* X 0 to 447
* Y 0 to 262
*
* VSTART,VSTOP,HSTART are 9 bit values.
*
* Word1 SPRxPOS
*	Bits 15-8 contain the low 8 bits of VSTART
*	Bits 7-0 contain the high 8 bits of HSTART
*
* Word2 SPRxCTL
*	Bits 15-8	The low eight bits of VSTOP
*	Bit 7		(Used in attachment)
*	Bits 6-3	Unused (make zero)
*	Bit 2		The VSTART high bit
*	Bit 1		The VSTOP high bit
*	Bit 0		The HSTART low bit
*
* IN:		d0-d1, x,y in screen pixels, height is assumed to be 32
* OUT:		d0.l, 2 control words
* TRASHED:	d0-d1/a0
*****************************************************************************
	IFNE 0
Get_Sprite_Control_Words2:
	add.w	#32,d0   
	add.w	#SPRITE_HEIGHT,d1 

	add.w	d0,d0
	add.w	d0,d0		;access in longs
	lea	Sprite_Lookup_X(pc),a0
	move.l	(a0,d0.w),d0

	add.w	d1,d1
	add.w	d1,d1		;access in longs
	lea	Sprite_Lookup_Y(pc),a0
	or.l	(a0,d1.w),d0

	rts

Precalc_Sprite_Control_Table:

	;X
	moveq	#(DIW_H-1)-32,d0		;Sprites are X-1, then -32 
	move.w	#(DIW_WIDTH+64)-1,d1		;Allow for overrun of 32 pixels at both sides
	lea	Sprite_Lookup_X(pc),a0
.xloop:
	move.w	d0,d2
	moveq	#0,d3
	lsr.w	#1,d2			;shift out hstart low bit
	addx.w	d3,d3			;left shift and hstart low bit to d3

	move.w	d2,(a0)+		;1st word
	move.w	d3,(a0)+		;2nd word

	addq.w	#1,d0
	dbf	d1,.xloop

	;Y
	moveq	#DIW_V-SPRITE_HEIGHT,d0
	moveq	#SPRITE_HEIGHT,d1
	move.w	#(DIW_HEIGHT+SPRITE_HEIGHT)-1,d2	;Allow overrun at top	
	lea	Sprite_Lookup_Y(pc),a0
.yloop:
	move.w	d0,d3			;vstart
	move.w	d0,d4		
	add.w	d1,d4			;vstop
	moveq	#0,d5

	lsl.w	#8,d3			;vstart low 8 bits to top of word
	addx.w	d5,d5     		;left shift and vstart high bit to d5

	lsl.w	#8,d4			;vstop low 8 bits to top of word
	addx.w	d5,d5     		;left shift and vstop high bit to d5
	lsl.w	#1,d5			;shift to correct positions	
	or.w	d5,d4			;merge

	move.w	d3,(a0)+		;1st word
	move.w	d4,(a0)+		;2nd word

	addq.w	#1,d0
	dbf	d2,.yloop

	rts

Sprite_Lookup_X:
	ds.l	(DIW_WIDTH+64)	;Allow 32 pixels either side

Sprite_Lookup_Y:
	ds.l	(DIW_HEIGHT+SPRITE_HEIGHT)	;Allow 32 pixels at top
	ENDC


*****************************************************************************

SIN_FX_PAUSE		=	1
SIN_FX_SPEED		=	2	;0-255
SIN_FX_SINE_SPEEDSTEP	=	3	;-128 to 127
SIN_FX_SINE_RESET	=	4
SIN_FX_BUILDINGS_OFF	=	5

	EVEN
SIN_Text1:
	dc.b	SIN_FX_SINE_RESET
	dc.b	SIN_FX_SPEED,5
	dc.b	SIN_FX_SINE_SPEEDSTEP,-8,1,-4,1		; modulated
	dc.b	"    WELCOME TO OUR LITTLE INTRO...   SAX OFFENDER      "

	dc.b	SIN_FX_SINE_RESET
	dc.b	SIN_FX_SPEED,5
	dc.b	SIN_FX_SINE_SPEEDSTEP,-7,2,14,2		; Elastic
	dc.b	"GRAPHICS BY  OPTIC     "
	dc.b	"MUSIC BY  TECON     "
	dc.b	"CODE BY  ANTIRIAD      "

	dc.b	SIN_FX_SINE_SPEEDSTEP,-8,1,-4,1		; modulated
	dc.b	SIN_FX_BUILDINGS_OFF
	dc.b	"trarararararararara "
	dc.b	"trarararararara "
	dc.b	"trarararararara "

	; Most severe sine for clr screen testing
	;dc.b	SIN_FX_SINE_RESET
	;dc.b	SIN_FX_SPEED,5
	;dc.b	SIN_FX_SINE_SPEEDSTEP,-9,8,-4,16
	;dc.b	"---===OOO===---                                          "

	dc.b	0			;End
	EVEN


*****************************************************************************

; For speed the Y range (taking account of padding) is not checked and the two sine waves used must
; be created so that the maximum top/bottom Y values do not cause the sine scroller blit
; to exceed the screen area.
; This routine:
;Max font height after filling = 8 * (SIN_Font_HeightMultiplier-1)
;8*11 = 88. Screen height 216 - 88 = 128.  0-64 (added together)
;8*11 = 88. Screen height 224 - 88 = 136.  0-68 (added together)
;Screen height 208. Max font height is 8*14=112. Last Y is 208-112 = 0-96

SIN_Sine_NumEntries = 1024		;Must be power of 2
SIN_Sine_NumEntries_Rept = 6		;Repeat the sine entries to reduce bounds checking
SIN_Sine_Offset_Mask = ((SIN_Sine_NumEntries*2)-2)	; Byte offset access into the table, forced to be even 

; Sine values
SIN_Sine_Source:
	INCLUDE "sine_0-64_1024_words.i"
SIN_Sine:
	ds.w	SIN_Sine_NumEntries*SIN_Sine_NumEntries_Rept

*****************************************************************************

MIA_PAL_BG_NUMCOLS	= 256		; number of main colour entries in our palettes
MIA_PAL_BG_SIZEOF	= MIA_PAL_BG_NUMCOLS*2

;grdmstr_data:6,256
;hsv:50,50,50
;0:#64017c
;60:#b80362
;120:#fd3a02
;160:#ff8000
;220:#fadb05
;255:#fadb05

MIA_Palette_BG_1:
; colors count: 256

	dc.w		$608,$608,$608,$708,$708,$708,$708,$708
	dc.w		$708,$708,$707,$707,$707,$707,$707,$807
	dc.w		$807,$807,$807,$807,$807,$807,$807,$807
	dc.w		$807,$807,$907,$907,$907,$907,$907,$907
	dc.w		$907,$907,$907,$907,$907,$907,$a07,$a07
	dc.w		$a07,$a07,$a07,$a07,$a07,$a07,$a07,$a06
	dc.w		$a06,$b06,$b06,$b06,$b06,$b06,$b06,$b06
	dc.w		$b06,$b06,$b06,$b06,$c06,$c06,$c06,$c06
	dc.w		$c06,$c06,$c16,$c15,$c15,$c15,$c15,$c15
	dc.w		$c15,$c15,$d15,$d15,$d15,$d14,$d14,$d14
	dc.w		$d14,$d14,$d14,$d24,$d24,$d24,$d24,$d23
	dc.w		$e23,$e23,$e23,$e23,$e23,$e23,$e23,$e23
	dc.w		$e23,$e22,$e22,$e22,$e22,$e32,$f32,$f32
	dc.w		$f32,$f32,$f32,$f31,$f31,$f31,$f31,$f31
	dc.w		$f31,$f31,$f31,$f31,$f31,$f30,$f40,$f40
	dc.w		$f40,$f40,$f40,$f40,$f40,$f40,$f40,$f40
	dc.w		$f50,$f50,$f50,$f50,$f50,$f50,$f50,$f50
	dc.w		$f50,$f50,$f60,$f60,$f60,$f60,$f60,$f60
	dc.w		$f60,$f60,$f60,$f70,$f70,$f70,$f70,$f70
	dc.w		$f70,$f70,$f70,$f70,$f80,$f80,$f80,$f80
	dc.w		$f80,$f80,$f80,$f80,$f80,$f80,$f90,$f90
	dc.w		$f90,$f90,$f90,$f90,$f90,$f90,$f90,$f90
	dc.w		$fa0,$fa0,$fa0,$fa0,$fa0,$fa0,$fa0,$fa0
	dc.w		$fa0,$fa0,$fa0,$fb0,$fb0,$fb0,$fb0,$fb0
	dc.w		$fb0,$fb0,$fb0,$fb0,$fb0,$fc0,$fc0,$fc0
	dc.w		$fc0,$fc0,$fc0,$fc0,$fc0,$fc0,$fc0,$fc0
	dc.w		$fd0,$fd0,$fd0,$fd0,$fd0,$fd0,$fd0,$fd0
	dc.w		$fd0,$fd0,$fd0,$fe0,$fe0,$fe0,$fe0,$fe0
	dc.w		$fe0,$fe0,$fe0,$fe0,$fe0,$fe0,$fe0,$fe0
	dc.w		$fe0,$fe0,$fe0,$fe0,$fe0,$fe0,$fe0,$fe0
	dc.w		$fe0,$fe0,$fe0,$fe0,$fe0,$fe0,$fe0,$fe0
	dc.w		$fe0,$fe0,$fe0,$fe0,$fe0,$fe0,$fe0,$fe0

MIA_Palette_BG_2:	ds.w	MIA_PAL_BG_NUMCOLS
MIA_Palette_BG_3:	ds.w	MIA_PAL_BG_NUMCOLS
MIA_Palette_BG_4:	ds.w	MIA_PAL_BG_NUMCOLS

;Text versions (the background merged with white for text)
MIA_Palette_BG_Text_1:	ds.w	MIA_PAL_BG_NUMCOLS
MIA_Palette_BG_Text_2:	ds.w	MIA_PAL_BG_NUMCOLS
MIA_Palette_BG_Text_3:	ds.w	MIA_PAL_BG_NUMCOLS
MIA_Palette_BG_Text_4:	ds.w	MIA_PAL_BG_NUMCOLS

; All black and white palettes used for fades/lightsources
PAL_AllBlack256:	dcb.w	MIA_PAL_BG_NUMCOLS,0
PAL_AllWhite256:	dcb.w	MIA_PAL_BG_NUMCOLS,$fff

MIA_Palette_BG_List:
	dc.l	MIA_Palette_BG_1
	dc.l	MIA_Palette_BG_2
	dc.l	MIA_Palette_BG_3
	dc.l	MIA_Palette_BG_4

MIA_Palette_BG_Text_List:
	dc.l	MIA_Palette_BG_Text_1
	dc.l	MIA_Palette_BG_Text_2
	dc.l	MIA_Palette_BG_Text_3
	dc.l	MIA_Palette_BG_Text_4


*****************************************************************************

; Source bitmap for palm trees. We copy it to chip ram.
	EVEN
MIA_Scr_Palm_Source:
	INCBIN "AssetsConverted/Miami_Palm_1056x256x1.BPL.nrv2s"
	EVEN

*****************************************************************************

; Font BOB structure and font ascii lookup
; .BPL Can be in public memory as we are drawing it with the CPU
FON_BOB_Font8px:
	INCLUDE "AssetsConverted/Miami_Font_8x8x1.BOB_dcw.i"
FON_FAR_Font8px:
	INCLUDE "AssetsConverted/Miami_Font_8x8x1.FAR_dcb.i"
FON_BPL_Font8px:
	INCBIN "AssetsConverted/Miami_Font_8x8x1.BPL"

*****************************************************************************

; Sprite data, to be depacked to chip
Sprite_Frames_Source:
	INCBIN "AssetsConverted\Miami_BirdAnim_32x32_8Frames.SPR.nrv2s"

; Each of the frames is plotted at a different height :)
BIRD_BASE_YPOS = 2
Sprites_Frames_Y_Pos:
	dc.w	BIRD_BASE_YPOS+1	;0
	dc.w	BIRD_BASE_YPOS+2	;1
	dc.w	BIRD_BASE_YPOS+4	;2
	dc.w	BIRD_BASE_YPOS+3	;3
	dc.w	BIRD_BASE_YPOS+2	;4
	dc.w	BIRD_BASE_YPOS+1	;5
	dc.w	BIRD_BASE_YPOS+0	;6
	dc.w	BIRD_BASE_YPOS+0	;7

; 2 sprites of 32 high
; frame 0: spr0 offset, spr1 offset
; ...
; frame 7: spr0 offset, spr1 offset
; frame0, sprite0 control words and data
; frame0, sprite1 control words and data
; ...
; frame7, sprite0 control words and data
; frame7, sprite1 control words and data

;Sprite_Frames:
;	INCBIN "AssetsConverted\Miami_BirdAnim_32x32_8Frames.SPR"
SPRITE_TOTALSIZE = 4*1024	;About 4k uncompressed

SPRITE_NUM_FRAMES = 8
SPRITE_FRAME_MASK = SPRITE_NUM_FRAMES-1
SPRITE_FRAME_SPEED = 4
SPRITE_HEIGHT = 32


*****************************************************************************

	RSRESET
CTRL_PRECALC_INTROSTART_DONE:	rs.w 1		;1 if intro precalc done
CTRL_P1_READY:			rs.w 1		;P1 IRQ/CL is ready
CTRL_P2_READY:			rs.w 1		;1 if requesting transition out
CTRL_FINISHED:			rs.w 1		;1 if quitting
CTRL_SCRIPT_PTR:		rs.l 1		;0 - Script Ptr
CTRL_PAUSE_COUNTER:		rs.w 1		;4 - Pause counter, 0=running
CTRL_PALETTE_COUNTER:		rs.w 1		;12 - Palette counter, speed
CTRL_PALETTE_STEP:		rs.w 1
CTRL_PALM_OFFSET		rs.w 1		;Scroll offset of palm layer
CTRL_SIN_TEXT_PTR		rs.l 1		;Ptr to sine text
CTRL_SIN_SPEED			rs.w 1		;Speed
CTRL_SIN_SINE1_OFFSET		rs.w 1		;CTRL_SIN_SINE1_OFFSET
CTRL_SIN_SINE2_OFFSET		rs.w 1		;Sine2 offset
CTRL_SIN_SINE1_SPEED		rs.w 1		
CTRL_SIN_SINE1_STEP		rs.w 1
CTRL_SIN_SINE2_SPEED		rs.w 1
CTRL_SIN_SINE2_STEP		rs.w 1		;CTRL_SIN_SINE2_STEP
CTRL_SIN_LASTLETTERFRAMECOUNT	rs.w 1		;CTRL_SIN_LASTLETTERFRAMECOUNT
CTRL_SIN_STOPSCROLL		rs.w 1		;CTRL_SIN_STOPSCROLL
CTRL_BLINDS_IN_ACTIVE:		rs.w 1
CTRL_BLINDS_OUT_ACTIVE:		rs.w 1
CTRL_BUILDINGS_STOP_FLAG	rs.w 1		;Set to 1 to make buildings stop
CTRL_SPRITE_BIRD_FRAME		rs.w 1		;Frame 
CTRL_SPRITE_BIRD_COUNTER	rs.w 1
CTRL_SPRITE_BIRD_XPOS		rs.w 1		

CTRL_SIZE:			rs.w 0

	EVEN
Controller_Info:
	dc.w	0			;CTRL_PRECALC_INTROSTART_DONE
	dc.w	0			;CTRL_P1_READY
	dc.w	0			;CTRL_P2_READY
	dc.w	0			;CTRL_FINISHED
	dc.l	0			;CTRL_SCRIPT_PTR
	dc.w	0			;CTRL_PAUSE_COUNTER
	dc.w	0			;CTRL_PALETTE_COUNTER
	dc.w	0			;CTRL_PALETTE_STEP
	dc.w	0			;CTRL_PALM_OFFSET
	dc.l	SIN_Text1		;CTRL_SIN_TEXT_PTR
	dc.w	1			;CTRL_SIN_SPEED
	dc.w	0			;CTRL_SIN_SINE1_OFFSET
	dc.w	0			;CTRL_SIN_SINE2_OFFSET
	dc.w	0
	dc.w	0
	dc.w	0
	dc.w	0			;CTRL_SIN_SINE2_STEP
	dc.w	0			;CTRL_SIN_LASTLETTERFRAMECOUNT
	dc.w	0			;CTRL_SIN_STOPSCROLL
	dc.w	0			;CTRL_BLINDS_IN_ACTIVE
	dc.w	0			;CTRL_BLINDS_OUT_ACTIVE
	dc.w	0			;CTRL_BUILDINGS_STOP_FLAG
	dc.w	0			;CTRL_SPRITE_BIRD_FRAME
	dc.w	0			;CTRL_SPRITE_BIRD_COUNTER
	dc.w	0			;CTRL_SPRITE_BIRD_XPOS

	EVEN

*****************************************************************************
*****************************************************************************
*****************************************************************************

	SECTION	IntroFramework_ChipData_Copper,DATA_C	;Chip Data Section for gfx/music

*****************************************************************************

*** THE COPPERLISTS ***

; Copper horizontal blanking notes from Photon/Scoopex
; As established, positions $e7...$03 are not usable. If you're writing a simple 
; copperlist with no need for tight timing, positions $df and $07 are conventionally 
;used for the positions on either side of the horizontal blanking, and for 
; compatibility across chipsets use increments of 4 from these, resulting in 
;positions $db, $0b, and so on.

*****************************************************************************

; Transition in/out copper. Just a blank color 01 screen. Sprites are disabled
P0_CL_Phys:
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	CMOVE 	bplcon0,$1200		;1 bpl
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0000
	CMOVE 	bpl1mod,SCR_BPL2MOD
	CMOVE 	bpl2mod,SCR_BPL2MOD	;Repeat a line like buildings CL

	CMOVE	dmacon,DMAF_SPRITE	;Disable sprite DMA

	CWAIT	DIW_V-1,$7
P0_CL_Bpl:
	CMOVE	bpl1pth,$0		;Building 1
	CMOVE	bpl1ptl,$0

	CWAIT	DIW_V,$7
P0_CL_Cols:
	CMOVE	color00,COL_TRANS_IN
	CMOVE	color01,COL_TRANS_IN


	CWAIT	255,$e1
	CWAIT	(DIW_V+DIW_HEIGHT)&$ff,$7
	CMOVE	tmpcolor00,$000		;Black out screen for wrap around

	IFNE FW_IRQ_TYPE_COPPER
	;	IFGT SCANLINE_EOF-255
	;		CWAIT	255,$e1
	;	ENDC
	;	CWAIT	(SCANLINE_EOF&$ff),$7
		CMOVE	intreq,INTF_SETCLR|INTF_COPER
	ENDC

	COPPEREND

P0_BPL_Transition_AllBlack:
	dcb.b	SCR_BLD_BUF_BYTEWIDTH,$ff	;Solid bpl1, 1 line
	EVEN

*****************************************************************************

P1_CL_Phys:
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	CMOVE 	bplcon0,$3600		;Dual Playfield, 2 bpl
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0041		;PF2 priority, sprite0 on top of pf1
	CMOVE 	bpl1mod,SCR_BPL1MOD
	CMOVE 	bpl2mod,SCR_BPL2MOD

	CMOVE	dmacon,DMAF_SETCLR|DMAF_SPRITE	;enable sprites

	CMOVE	tmpcolor00,$000
	CMOVE	color01,$000		;PF1,Palm silloutte layer
	CMOVE	color02,$aaa		;Sine +palm (step 10 equivalent)
	CMOVE	color03,$fff		;Sine
	;CMOVE	color08,$000		;PF2.Transparent, dont need
	CMOVE	color17,$000		;Sprite 0&1 color01
P1_CL_Col_Building:
	CMOVE	color09,$fff		;Building

	;Copperlist is double buffers so can write pointers early
P1_CL_Scr_Bpl_Palm:
	CMOVE	bpl1pth,$0		;Palm trees
	CMOVE	bpl1ptl,$0

P1_CL_Scr_Bpl_Building:
	CMOVE	bpl2pth,$0		;Building 1
	CMOVE	bpl2ptl,$0

P1_CL_Scr_Bpl_Sine:
	CMOVE	bpl3pth,$0		;Sine scroller
	CMOVE	bpl3ptl,$0

P1_CL_Scr_Sprites:
	CMOVE	spr0pth,$0
	CMOVE	spr0ptl,$0
	CMOVE	spr1pth,$0
	CMOVE	spr1ptl,$0
	CMOVE	spr2pth,$0
	CMOVE	spr2ptl,$0
	CMOVE	spr3pth,$0
	CMOVE	spr3ptl,$0
	CMOVE	spr4pth,$0
	CMOVE	spr4ptl,$0
	CMOVE	spr5pth,$0
	CMOVE	spr5ptl,$0
	CMOVE	spr6pth,$0
	CMOVE	spr6ptl,$0
       	CMOVE	spr7pth,$0
	CMOVE	spr7ptl,$0

	; Jump to secondary list for colours and irq
P1_CL_Scr_CopJmp:
	CMOVE	cop2lch,$0
	CMOVE	cop2lcl,$0
	CMOVE	copjmp2,$0
	COPPEREND

P1_CL_End:

P1_CL_BPL_PALM_OFFSET = (P1_CL_Scr_Bpl_Palm-P1_CL_Phys)
P1_CL_BPL_SINE_OFFSET = (P1_CL_Scr_Bpl_Sine-P1_CL_Phys)
P1_CL_SPRITES_OFFSET = (P1_CL_Scr_Sprites-P1_CL_Phys)
P1_CL_COL_BLD_OFFSET = (P1_CL_Col_Building-P1_CL_Phys)
P1_CL_BPL_BUILDING_OFFSET = (P1_CL_Scr_Bpl_Building-P1_CL_Phys)
P1_CL_COPJMP_OFFSET = (P1_CL_Scr_CopJmp-P1_CL_Phys)
P1_CL_SIZE = P1_CL_End-P1_CL_Phys


*****************************************************************************

; This is the bottom of the main copper list that just handles 256 lines of 
; colours
MIA_CL_Scr_Sublist_1:
MIA_CL_Scr_Cols1:

a set DIW_V
		REPT	256-DIW_V
		CWAIT	(a)&$ff,$7
		CMOVE	color01,$000	;Palm tree background
		CMOVE	color03,$fff	;The scroller over pure background
a set a+1
		ENDR

	;Handle pal swapover
	CWAIT	255,$df

MIA_CL_Scr_Cols2:
a set 0
		REPT	(DIW_V+256)-256
		CWAIT	(a)&$ff,$7
		CMOVE	color01,$000	;Palm tree background
		CMOVE	color03,$fff	;The scroller over pure background
a set a+1
		ENDR

	; Trigger copper interrupt
	IFNE FW_IRQ_TYPE_COPPER
		CMOVE	intreq,INTF_SETCLR|INTF_COPER
	ENDC

	;CWAIT	255,$e1
	CWAIT	(DIW_V+DIW_HEIGHT)&$ff,$7
	CMOVE	tmpcolor00,$000		;Black out screen for wrap around

	COPPEREND
MIA_CL_Scr_Sublist_End:

MIA_CL_SUBLIST_COL1_OFFSET = (MIA_CL_Scr_Cols1-MIA_CL_Scr_Sublist_1)
MIA_CL_SUBLIST_COL2_OFFSET = (MIA_CL_Scr_Cols2-MIA_CL_Scr_Sublist_1)
MIA_CL_SUBLIST_SIZE = MIA_CL_Scr_Sublist_End-MIA_CL_Scr_Sublist_1

	RSRESET
MIA_CL_SUBLIST_LINE_WAIT:	rs.w	2
MIA_CL_SUBLIST_LINE_BKG:	rs.w	2
MIA_CL_SUBLIST_LINE_TEXT:	rs.w	2
MIA_CL_SUBLIST_LINE_SIZE:	rs.w	0

;Size is ~4words * 256 = 2KB

;The 4 sublists (shared mem)
;MIA_CL_Scr_Sublist_1:	ds.b	MIA_CL_SUBLIST_SIZE
;MIA_CL_Scr_Sublist_2:	ds.b	MIA_CL_SUBLIST_SIZE
;MIA_CL_Scr_Sublist_3:	ds.b	MIA_CL_SUBLIST_SIZE
;MIA_CL_Scr_Sublist_4:	ds.b	MIA_CL_SUBLIST_SIZE


*****************************************************************************
*****************************************************************************
*****************************************************************************

; Map screens to shared chipmem buffers
CUR_CHIP_BUF set FW_Chip_Buffer_1

; Palm trees 3 screens wide - 33KB
MIA_Scr_Palm		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+SCR_BUF_TOTALSIZE

; Sine scroller screen buffer 3 screens wide 
; Screen1 for screen, screen2 for double buffer- 33KB * 2 = 66KB
MIA_Scr_Sine		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+SCR_SIN_BUF_TOTALSIZE

; Buildings - 48B
MIA_Scr_BLD_Scr		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+SCR_BLD_BUF_TOTALSIZE
MIA_Scr_BLD_Work	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+SCR_BLD_BUF_TOTALSIZE

; Sine scroller scroll buffer (where we write the text and scroll) - 416B
MIA_Scr_Sine_Scroller	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+SIN_Scroller_TotalSize

;Sprites - 4KB
Sprite_Frames		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+SPRITE_TOTALSIZE

; Copper lists - 2KB per list
P1_CL_Log1		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+P1_CL_SIZE

;Color lists
MIA_CL_Scr_Sublist_2	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+MIA_CL_SUBLIST_SIZE
MIA_CL_Scr_Sublist_3	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+MIA_CL_SUBLIST_SIZE
MIA_CL_Scr_Sublist_4	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+MIA_CL_SUBLIST_SIZE
MIA_CL_Scr_Sublist_5	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+MIA_CL_SUBLIST_SIZE
MIA_CL_Scr_Sublist_6	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+MIA_CL_SUBLIST_SIZE
MIA_CL_Scr_Sublist_7	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+MIA_CL_SUBLIST_SIZE
MIA_CL_Scr_Sublist_8	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+MIA_CL_SUBLIST_SIZE
;Sublists are ~2KB per list
;8 * 2KB = 16KB

;Total: ~110KB

*****************************************************************************

