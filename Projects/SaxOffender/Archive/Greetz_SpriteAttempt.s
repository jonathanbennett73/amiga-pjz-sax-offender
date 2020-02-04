*****************************************************************************

; Name			: Template.s
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com)
; Description		: Template routine.
; Date last edited	: 09/12/2019

; CPU Required		: MC68000 or better
; ChipSet Required	: OCS or better
				
*****************************************************************************

RasterTest set 0	;color00 timing bar, 0=off, 1=overall, 2=show blitwaits

	IFEQ	RasterTest
tmpcolor00 set color00	;use tmpcolor00 in place of color00 everywhere so timing is easier
	ELSE
tmpcolor00 set color01
	ENDC

*****************************************************************************

	INCLUDE "hardware/custom.i"
	INCLUDE "hardware/dmabits.i"	
	INCLUDE "hardware/intbits.i"	

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
	xref	FW_GetFrame
	xref	FW_InitCopperBplPtrs
	xref	FW_InitCopperColsFromPalette
	xref	FW_InitCopperSprPtrs	
	xref	FW_IsFrameOver
	xref	FW_SetBaseCopperAndDma_A6
	xref	FW_SetBaseCopperAndLev3Irq_A6
	xref	FW_SetBaseLev3Irq
	xref	FW_SetCopperAndDma_A6
	xref	FW_SetCopperAndLev3Irq_A6
	xref	FW_SetLev3Irq
	xref	FW_WaitFrame
	xref	FW_WaitRaster_A6
	xref 	FW_VBlankProxy

	xref	LIB_RGB12_Interpolate_Fast
	xref	LIB_RGB12_Interpolate_Fast_Palette
	xref	LIB_NRV2S_Depack

	xref	FW_Chip_Buffer_1
	xref	FW_Chip_Buffer_1_End
	xref	FW_Public_Buffer_1
	xref	FW_Public_Buffer_1_End
	xref	BPL_Zoom_Pattern


*****************************************************************************

	SECTION	Greetz_PublicCode,CODE	;Code section in Public memory

*****************************************************************************

; Photon:
; Best practice is to start with DDF 38,d0 and DIW 2c81,2cc1 and modify these 
; symmetrically and always in whole 16px steps for compatibility.
; Note DDF start of less than 30 and you start to lose sprites.

*** Display Window ***
P0_DIW_V		=	$24		;Hardware Vstart ($2c normal, $24 overscan)
P0_DIW_H		=	$79		;Hardware Hstart ($81 normal, $71 overscan)
P0_DIW_WIDTH		=	334		;Pixels		 (multiple of 16, 320 normal, 352 overscan)
P0_DIW_HEIGHT		=	272		;Lines		 (256 normal PAL, 272 overscan)

P0_DDF_H		=	$79		;Hardware Hstart ($81 normal, $71 overscan)
P0_DDF_WIDTH		=	336		;Pixels		 (320 normal pal, 352 overscan)
P0_DDF_BYTEWIDTH	=	P0_DDF_WIDTH/8
P0_DDF_WORDWIDTH	=	P0_DDF_BYTEWIDTH/2

*****************************************************************************

*** Screen Definitions ***

;The visible screen area and back buffer
BPL_BUF_WIDTH		=	352
BPL_BUF_BYTEWIDTH	=	BPL_BUF_WIDTH/8		;Bytes
BPL_BUF_WORDWIDTH	=	BPL_BUF_BYTEWIDTH/2	;Words
BPL_BUF_HEIGHT		=	272			;Rule of thirds
BPL_BUF_NUMPLANES	=	1			;3bpl/8 cols
BPL_BUF_NUMCOLS 	= 	(1<<BPL_BUF_NUMPLANES)
BPL_BUF_SIZE		=	BPL_BUF_BYTEWIDTH*BPL_BUF_HEIGHT
BPL_BUF_TOTALSIZE	=	BPL_BUF_SIZE*BPL_BUF_NUMPLANES


; For perfect reflection (interleaved):
; Modulo is added at DDFStop so need to set before then.
; Before DDFStart at last line of display set modulo to repeat that line
; -P0_DDF_BYTEWIDTH
; Then, before DDFStart on first line of reflection
; (-(BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES))-P0_DDF_BYTEWIDTH
BPL_BPLMOD		=	(BPL_BUF_BYTEWIDTH-P0_DDF_BYTEWIDTH)+(BPL_BUF_BYTEWIDTH*(BPL_BUF_NUMPLANES-1))	;interleaved mode
BPL_BPLMOD_REPTLINE	=	-P0_DDF_BYTEWIDTH
BPL_BPLMOD_REPTPREVLINE	=	(-(BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES))-P0_DDF_BYTEWIDTH
BPL_BPLMOD_REPT2LINE	=	BPL_BPLMOD_REPTPREVLINE-(BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES)

SCANLINE_EOF		=	P0_DIW_V+P0_DIW_HEIGHT	; Safe to starting clearing after this scanline

*****************************************************************************

; This is the scratch buffer used for drawing each line of greetz before it's
; copied into the screen
BPL_BUF_FONT_SCRATCH_WIDTH	=	112
BPL_BUF_FONT_SCRATCH_BYTEWIDTH	=	BPL_BUF_FONT_SCRATCH_WIDTH/8
BPL_BUF_FONT_SCRATCH_WORDWIDTH	=	BPL_BUF_FONT_SCRATCH_BYTEWIDTH/2
BPL_BUF_FONT_SCRATCH_HEIGHT	=	16
BPL_BUF_FONT_SCRATCH_NUMPLANES	=	1
BPL_BUF_FONT_SCRATCH_SIZE	=	BPL_BUF_FONT_SCRATCH_BYTEWIDTH*BPL_BUF_FONT_SCRATCH_HEIGHT
BPL_BUF_FONT_SCRATCH_TOTALSIZE	=	BPL_BUF_FONT_SCRATCH_SIZE*BPL_BUF_FONT_SCRATCH_NUMPLANES


; This is the actual font
; Have to declare font sizes at top to avoid assembler issues.
; Set MONO to 0 (proportional fonts) to work out on the fly. Much slower.
FONT1_MONO = 1
FONT1_NUMPLANES = 1
FONT1_HEIGHT = 10
FONT1_WIDTH = 8
FONT1_BYTEWIDTH = FONT1_WIDTH/8
FONT1_WORDWIDTH = 1	; Have to be at least 1
FONT1_BLTSIZE = ((FONT1_HEIGHT*FONT1_NUMPLANES*64)+FONT1_WORDWIDTH)


*****************************************************************************

PAL_NUMCOLS_MAIN	= BPL_BUF_NUMCOLS		; number of main colour entries in our palettes
PAL_NUMCOLS_DARK	= 0				; number of dark/refl cols
PAL_NUMCOLS_ALL		= (PAL_NUMCOLS_MAIN+PAL_NUMCOLS_DARK)


*****************************************************************************

;Zoom pattern is 352 width so have to alter offsets to center in our 334x256 window
ZOOM_PATTERN_X_BYTEOFFSET = 0	;352 vs 352 = 0 extra bytes
ZOOM_PATTERN_Y_BYTEOFFSET = 4	;352 vs 272 high = 4 extra bytes to work for a 272 high screen


*****************************************************************************
* Start the effect (usually used for setting up the copper)
* This is called each time the effect is started
* IN:		a0, script ptr
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

	IFEQ _INTROWRAPPER
	xdef	SubPartStart
SubPartStart:	
	ENDC

	xdef	GTZ_Start
GTZ_Start:
	movem.l	d2-d7/a2-a6,-(sp)	;save
	lea	ControllerScript,a0

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
	;jsr	FW_SetBaseLev3Irq
	jsr	FW_SetBaseCopperAndLev3Irq_A6
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
	xdef	GTZ_PreCalc_IntroStart
	ENDC
	
SubPartPreCalc_IntroStart:	
GTZ_PreCalc_IntroStart:
	tst.w	Controller_Info+CTRL_PRECALC_INTROSTART_DONE
	bne.s	.exit			;already done

	movem.l	d2-d7/a2-a6,-(sp)	;save
	lea	Controller_Info(pc),a5
	
	IFEQ _INTROWRAPPER
	;Done by ExplodeVector.s in full demo
	bsr	Calc_Zoom_Bitmaps
	ENDC

	;Completed!
	move.w	#1,CTRL_PRECALC_INTROSTART_DONE(a5)

	movem.l	(sp)+,d2-d7/a2-a6	;restore
.exit:
	rts


*****************************************************************************
* To be called prior to a new phase for doing non permanent precalc.
* i.e. the buffers used may be only initialised just prior to effect running.
* This function should be reentrant if supports calling in batches and 
* only setting CTRL_P0_PRECALC_DONE when complete.
* CPU ONLY - No Blitter
* IN:		
* OUT:		d0, 1=precalc completed, 0=need to run again
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

	;Only export one symbol depending if in demo or standalone mode
	IFEQ _INTROWRAPPER
	xdef	SubPart_P0_PreCalc
	ELSE
	xdef	GTZ_P0_PreCalc
	ENDC

SubPart_P0_PreCalc:	
GTZ_P0_PreCalc:
	movem.l	d2-d7/a2-a6,-(sp)	;save (not d0)
	lea	Controller_Info(pc),a5
	tst.w	CTRL_P0_PRECALC_DONE(a5)
	bne.s	.exit			;already done

	;Change color to show where this starts
	;move.w	#$fff,color00+_custom


	;Completed!
	move.w	#1,CTRL_P0_PRECALC_DONE(a5)
.exit:
	move.w	CTRL_P0_PRECALC_DONE(a5),d0
	movem.l	(sp)+,d2-d7/a2-a6	;restore (not d0)
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

	; Run phase startup precalc if not already done - may be in batches
.precalc_effectstart:
	tst.w	CTRL_P0_PRECALC_DONE(a5)
	bne.s	.precalc_effectstart_done
	bsr.s	SubPart_P0_PreCalc	;O:d0, T:d0-d1/a0-a1
	bra.s	.precalc_effectstart
.precalc_effectstart_done:

	;Reset anything that needs resetting
	moveq	#0,d0
	move.w	d0,CTRL_PHASE(a5)	
	move.w	d0,CTRL_FINISHED(a5)
	move.w	d0,CTRL_FRAME_COUNT(a5)
	move.w	d0,CTRL_PAUSE_COUNTER(a5)
	move.w	d0,CTRL_PALETTE_ACTIVE(a5)

	move.w	#1,CTRL_BKG_FRAME_SPEED(a5)
	move.l	#BPL_Pic_0,CTRL_BKG_FRAME_PTR(a5)	;Ensure 1st frame ptr valid

	move.w	d0,CTRL_SINE_V_1(a5)
	move.w	#2,CTRL_SINE_V_1_SPEED(a5)
	move.w	#128,CTRL_SINE_V_2(a5)
	move.w	#4,CTRL_SINE_V_2_SPEED(a5)
	
	move.w	d0,CTRL_SINE_H_1(a5)
	move.w	#2,CTRL_SINE_H_1_SPEED(a5)
	move.w	#2,CTRL_SINE_H_1_STEP(a5)
	move.w	d0,CTRL_SINE_H_2(a5)
	move.w	#-4,CTRL_SINE_H_2_SPEED(a5)
	move.w	#-2,CTRL_SINE_H_2_STEP(a5)

	; Clear all screen buffers (previous routine must have blanked screen/colors)
	bsr	Clear_ScreenBuffers_CPU

	; Load default palette
	lea	PAL_AllBlack(pc),a0
	moveq	#0,d0
	bsr	Controller_FX_Palette	;I:d0/a0/a5, T:d0/a0-a1

	;lea	PAL_Pic_Source(pc),a0
	;moveq	#4,d0
	;bsr	Controller_FX_Palette	;I:d0/a0/a5, T:d0/a0-a1

	; Multiply sine values by screen widths
	bsr	Calc_PreMult_ScreenHeight

	;Depack pictures
	lea	BPL_Pic_Source_0,a0
	lea	BPL_Pic_0,a1
	jsr	LIB_NRV2S_Depack		;I:a0-a1, T:d0-d1/a0-a1	
	lea	BPL_Pic_Source_1,a0
	lea	BPL_Pic_1,a1
	jsr	LIB_NRV2S_Depack		;I:a0-a1, T:d0-d1/a0-a1	
	lea	BPL_Pic_Source_2,a0
	lea	BPL_Pic_2,a1
	jsr	LIB_NRV2S_Depack		;I:a0-a1, T:d0-d1/a0-a1	
	lea	BPL_Pic_Source_3,a0
	lea	BPL_Pic_3,a1
	jsr	LIB_NRV2S_Depack		;I:a0-a1, T:d0-d1/a0-a1	
	lea	BPL_Pic_Source_4,a0
	lea	BPL_Pic_4,a1
	jsr	LIB_NRV2S_Depack		;I:a0-a1, T:d0-d1/a0-a1	
	lea	BPL_Pic_Source_5,a0
	lea	BPL_Pic_5,a1
	jsr	LIB_NRV2S_Depack		;I:a0-a1, T:d0-d1/a0-a1	
	lea	BPL_Pic_Source_6,a0
	lea	BPL_Pic_6,a1
	jsr	LIB_NRV2S_Depack		;I:a0-a1, T:d0-d1/a0-a1	

	; Set default zoom
	;moveq	#0,d0
	move.w	#ZOOM_MAX,d0
	moveq	#0,d1
	bsr	Controller_FX_Zoom	;I:a5,d0,d1

	;Setup phys/logical bpl and copperlist ptrs and load palette
	bsr	P0_CL_InitPtrs		;I:a6, T:d0-d2/a0-a1
	bsr	Write_Palette_To_Copper	;I:a5, T:d0/a0-a2

	;Swap buffers and load copper for next frame
	;bsr	P0_ScreenBufferSwap	;d0-d1/a0

	;Initialise our new irq and ensure phys copper loaded (should have had palette loaded above)
	move.l	CL_Phys_Ptr(pc),a0
	lea	P0_Lev3Irq(pc),a1
	jsr	FW_SetCopperAndLev3Irq_A6	;I:a0/a1/a6, T:a0

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
	jsr	FW_WaitFrame
	jsr	FW_WaitFrame	

.exit
	movem.l	(sp)+,d2-d7/a2-a4
	rts


*****************************************************************************

P0_Lev3Irq:
	TIMERON
	movem.l	d0-d7/a0-a6,-(sp)

	lea	_custom,a6
	lea	Controller_Info(pc),a5

	;Use full blitter clear as Pretracker uses up 99% of CPU here
	;bsr	Clear_WorkBuffer_BlitterCPU_A6	;entire screen
	;bsr	Clear_WorkBuffer_Blitter_A6		;entire screen

	;WAITBLIT_NASTY_A6

	bsr	P0_Write_Copper_Sprites

	jsr	FW_VBlankProxy		;T:d0-d7/a0-a4

	; Read new script lines and perform
	addq.w	#1,CTRL_FRAME_COUNT(a5)	;Update local frame count
	bsr	Controller_ReadCommands	;Read new commands
	bsr	Controller_Perform	;Do any ongoing time-based effects and update angles

	;Update background image
	bsr	Update_Background_Frame

	;Do vertical zoom (updates CL BPL ptrs)
	bsr	Do_Copper_V_Zoom

	;Do horizontal wave
	;bsr	Do_Copper_H_Zoom

	;Load current palette into CL if needed
	bsr	Write_Palette_To_Copper	;I:a5, T:d0/a0-a2

	;Check reflection/last line visibility, draw 3 lines
	;move.l	BPL_Log1_Ptr(pc),a1
	;lea	(BPL_BUF_HEIGHT-3)*BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES(a1),a1
	;move.l	#$a000a000,(a1)
	;move.l	#$0a000a00,1*(BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES)(a1)
	;move.l	#$00a000a0,2*(BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES)(a1)


	bsr	Clear_Font_Scratch
	;bsr	Draw_Text
* IN:		a6, custom
*		a0, screen
*		a1, y premult
*		d0.w, x
*		d1.w, y
*		d2.b, ascii value
	;move.l	CTRL_BKG_FRAME_PTR(a5),a0
	lea	BPL_Font_Scratch,a0
	move.w	#0,d0
	move.w	#0,d1
	move.w	#65,d2
	bsr	Font1_Plot_Letter_Blit	

	;move.l	CTRL_BKG_FRAME_PTR(a5),a0
	lea	BPL_Font_Scratch,a0
	move.w	#8,d0
	move.w	#0,d1
	move.w	#66,d2
	bsr	Font1_Plot_Letter_Blit	

	bsr	Copy_Font_Scratch_To_Sprite

	;Swap buffers and load copper for next frame
	bsr	P0_ScreenBufferSwap	;d0-d1/a0

	;Reset interrupt
	IFNE FW_IRQ_TYPE_COPPER
		moveq	#INTF_COPER,d0
	ELSE
		moveq	#INTF_VERTB,d0
	ENDC
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)		;A4000 compat

	movem.l	(sp)+,d0-d7/a0-a6
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

	; Point sprites at dummy sprite in base copper list (really only sprite7 needs)
	lea	P0_CL_Scr_Sprites,a0
	moveq	#8,d0
	jsr	FW_ClrCopperSprPtrs


	; Copper list buffers - copy screen list into 2nd buffer for doublebuffering
	lea	P0_CL_Phys,a0		;source
	lea	P0_CL_Log1,a1		;dest
	move.w	#(P0_CL_SIZE/2),d0	;size in words
	jsr	FW_CopyBuffer_CPU	;I:d0/a0-a1, T:d0-d7/a0-a2

	;
	;Front buffer copper BPL pointers
	;

	lea	P0_CL_Phys,a0
	move.l	a0,CL_Phys_Ptr
	lea	P0_CL_BPL_OFFSET(a0),a0	;copper bpl pointer block
	moveq	#BPL_BUF_NUMPLANES,d0
	move.l	#BPL_Phys,d1		;in d1 for InitCopperBplPtrs
	move.l	d1,BPL_Phys_Ptr
	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	;
	;Back buffer copper BPL pointers
	;

	lea	P0_CL_Log1,a0
	move.l	a0,CL_Log1_Ptr
	lea	P0_CL_BPL_OFFSET(a0),a0	;copper bpl pointer block
	moveq	#BPL_BUF_NUMPLANES,d0
	;move.l	#BPL_Log1,d1		;in d1 for InitCopperBplPtrs
	move.l	#BPL_Phys,d1		;in d1 for InitCopperBplPtrs
	move.l	d1,BPL_Log1_Ptr
	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo




	rts


*****************************************************************************
* Swaps the frame buffers, copperlists, and activates the CL next frame.
* NOTE: Call before vblank so new copper takes effect next frame.
* IN:		a6, _custom
* OUT:		
* TRASHED:	d0-d1/a0
*****************************************************************************

P0_ScreenBufferSwap:

	; Swap screen buffer details - different ways left here
	; as a reminder which is fastest

	; Double buffer
;	lea	BPL_Phys_Ptr(pc),a0	;8 (2/0)
;	move.l	(a0),d0			;12 (3/0)
;	move.l 	4(a0),d1		;16 (4/0)
;	exg	d0,d1			;6 (1/0)
;	move.l	d0,(a0)+		;12 (1/2)
;	move.l	d1,(a0)			;12 (1/2)
					;66 (12/4)

;	lea	BPL_Phys_Ptr(pc),a0	;8 (2/0)
;	movem.l	(a0),d0-d1		;12+8n (3+2n/0) = 28 (7/0)
;	exg	d0,d1			;6 (1/0)
;	movem.l	d0-d1,(a0)		;8+8n (2/2n) = 24 (2/4)
					;66 (12/4)
	
;	lea	BPL_Phys_Ptr(pc),a0	;8 (2/0)
;	movem.l	(a0),d0-d1		;12+8n (3+2n/0) = 28 (7/0)
;	move.l	d1,(a0)+		;12 (1/2)
;	move.l	d0,(a0)			;12 (1/2)
					;60 (11/4)
	;Triple buffer
;	lea	BPL_Phys_Ptr(pc),a0	;8 (2/0)
;	movem.l	(a0),d0-d2		;12+8n (3+2n/0) = 28 (7/0)
;	exg	d0,d1			;6 (1/0)
;	exg	d1,d2			;6 (1/0)
;	movem.l	d0-d2,(a0)		;8+8n (2/2n) = 32 (2/6)
					;80 (13/6)

;	lea	BPL_Phys_Ptr(pc),a0	;8 (2/0)
;	movem.l	(a0),d0-d2		;12+8n (3+2n/0) = 28 (7/0)
;	movem.l	d1-d2,(a0)		;8+8n (2/2n) = 24 (2/4)
;	move.l 	d0,8(a0)		;16 (2/2)
					;76 (13/6)

	; Swap double buffer screens then copper - assumes copper ptrs follow screen!
	;BPL_Phys_Ptr:		dc.l	0	;Screen buffers - physical
	;BPL_Log1_Ptr:		dc.l	0	;Logical1
	;CL_Phys_Ptr:		dc.l	0	;Copper ptr - physical
	;CL_Log1_Ptr:		dc.l	0	;Logical1	
	lea	BPL_Phys_Ptr(pc),a0
	movem.l	(a0),d0-d1
	move.l	d1,(a0)+
	move.l	d0,(a0)+
	;lea	CL_Phys_Ptr(pc),a0
	movem.l	(a0),d0-d1
	move.l	d1,(a0)+
	move.l	d0,(a0)			;d1 is phys

	; and activate next frame - d1 is physical
	move.l 	d1,cop1lch(a6)		; Active NEXT frame

	rts


*****************************************************************************
* Swaps the frame buffers, copperlists, and activates the CL next frame.
* NOTE: Call before vblank so new copper takes effect next frame.
* IN:		a6, _custom
* OUT:		
* TRASHED:	d0-d1/a0
*****************************************************************************

MY_GET_SPRITE_CTRL_WORDS	MACRO		
	add.w	#P0_DIW_H-1,d0   		;Sprites are X-1, mistake in HKRM
	add.w	#P0_DIW_V,d1 

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
	ENDM

P0_Write_Copper_Sprites:
	; And init sprites
	lea	SPR_Phys_0,a0
	move.l	CL_Log1_Ptr,a1
	lea	P0_CL_SPR_OFFSET+2(a1),a1

	moveq	#7-1,d4
	moveq	#100,d5
.sprloop
	move.l	a0,d0
	move.w	d0,4(a1)
	swap 	d0
	move.w	d0,(a1)

	;Updare control words
	move.w	d5,d0			;X
	move.w	#44,d1			;Y
	move.w	#SPR_HEIGHT,d2		;HEIGHT
	MY_GET_SPRITE_CTRL_WORDS	;I:d0-d2, O:d1-d2, T:d0-d3
	move.w	d1,(a0)			;update spr0 control words
	move.w	d2,2(a0)	

	lea	SPR_LIST_LINE_SIZEOF(a0),a0	;next sprite list ptr
	lea	8(a1),a1			;next sprite ptr in CL
	add.w	#16,d5
	dbf	d4,.sprloop
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
	lea	Mult_SCR_Height_ByteWidth_NumPlanes,a0
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
* Calculates the structures for x and y expansion.
* Use for one-time setup
* IN:		
* OUT:		
* TRASHED:	-
*****************************************************************************
	
	IFEQ _INTROWRAPPER	;Shared precalc in full demo
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
	ENDC	;_INTROWRAPPER


*****************************************************************************
* Clears the entire work buffer screen. Every frame.
* Clears with the blitter and CPU.
* IN:		A6(_custom/$dff000)
* OUT:		
* TRASHED:	d0-d7/a0-a5
*****************************************************************************

Clear_WorkBuffer_BlitterCPU_A6:

; Tune this so that the very first WAITBLIT call AFTER this
; routine should take no time at all.
	move.l	a5,-(sp)
	move.l  BPL_Log1_Ptr(pc),a0		;memory to clear
	move.w	#BPL_BUF_TOTALSIZE/2,d0		;size in words
	move.w	#35150,d1
	jsr	FW_ClearBuffer_BlitCPU_A6	;I:a6, T:d0-d7/a0-a4
	move.l	(sp)+,a5
	rts


*****************************************************************************
* Clears the entire work buffer screen. Every frame.
* Clears with the blitter only.
* IN:		A6(_custom/$dff000)
* OUT:		
* TRASHED:	d0-d7/a0-a5
*****************************************************************************

Clear_WorkBuffer_Blitter_A6:

	move.l  BPL_Log1_Ptr(pc),a0
	WAITBLIT_NASTY_A6
	move.l	#$01000000,bltcon0(a6)
	move.l	a0,bltdpth(a6)
	move.w	#0,bltdmod(a6)
	;move.w	#((BPL_BUF_HEIGHT*BPL_BUF_NUMPLANES)*64)+BPL_BUF_WORDWIDTH,bltsize(a6)
	; Max height = 1024, made wordwidth = 64

	rts


*****************************************************************************
* Clears all the buffers we will use. Done once at startup to ensure clean
* environment. USes CPU for precalc routine.
* IN:		
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

Clear_ScreenBuffers_CPU:
	movem.l	d2-d7/a2-a6,-(sp)

	lea	BPL_Phys,a0
	move.w	#(BPL_BUF_TOTALSIZE/2),d0
	jsr	FW_ClearBuffer_CPU	;I:d0/a0, T:All

	lea	BPL_Log1,a0
	move.w	#(BPL_BUF_TOTALSIZE/2),d0
	jsr	FW_ClearBuffer_CPU	;I:d0/a0, T:All

	movem.l	(sp)+,d2-d7/a2-a6
	rts


*****************************************************************************
* Updates the background frame.
* IN:		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d1/a0
*****************************************************************************

Update_Background_Frame:

	;Update the frame to show and keep in range
	move.w	CTRL_BKG_FRAME(a5),d0

	;check counter
	subq.w	#1,CTRL_BKG_FRAME_COUNTER(a5)
	bgt.s	.sameframe			;1 or greater we skip

	;Reset counter for next time
	move.w	CTRL_BKG_FRAME_SPEED(a5),CTRL_BKG_FRAME_COUNTER(a5)

	addq.w	#1,d0
	cmp.w	#BPL_PIC_NUMFRAMES,d0
	blt.s	.frameok
	moveq	#0,d0
.frameok:
	move.w	d0,CTRL_BKG_FRAME(a5)
.sameframe:

	;Multiply frame number by 4 (longwords) and lookup the matching frame ptr
	add.w	d0,d0
	add.w	d0,d0
	lea	BPL_Pic_Frames(pc),a0
	move.l	(a0,d0.w),d0			;d0 ios the active frame
	move.l	d0,CTRL_BKG_FRAME_PTR(a5)

	;This is written into copper in Do_Copper_V_Zoom routine
;	move.l	CL_Log1_Ptr(pc),a0
;	lea	P0_CL_BPL_OFFSET+2(a0),a0	;copper bpl pointer block
;	move.w	d0,4(a0)	;ptl
;	swap	d0
;	move.w	d0,(a0)		;pth
.exit:
	rts

*****************************************************************************
* Runs the controller script.
* Note the commands are read until a FX_PAUSE command is reached. So beware
* of hogging the CPU with too many commands at once.
* IN:		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d7/a0-a4
*****************************************************************************

Controller_ReadCommands:

	;First check if any types of pause/wait are happening
	move.w	CTRL_PAUSE_COUNTER(a5),d0
	bne.s	.pausing
	
	move.w	CTRL_ISFRAMEOVER_COUNTER(a5),d0
	bne.s	.isframeover

	move.w	CTRL_ISMASTERFRAMEOVER_COUNTER(a5),d0
	bne.s	.ismasterframeover

	tst.w	CTRL_FINISHED(a5)
	bne.s	.exit

	; Get current script pointer
.readcmd:
	move.l	CTRL_SCRIPT_PTR(a5),a4
.loop:
	move.w	(a4)+,d0		;End = 0

	; subroutines need to preserve a4-a6
	cmpi.w	#FX_PALETTE_FLAG,d0
	beq.s	.fx_pallete

	cmpi.w	#FX_SCRIPTJMP_FLAG,d0
	beq.s	.fx_scriptjmp

	cmpi.w	#FX_ISMASTERFRAMEOVER_FLAG,d0
	beq.s	.fx_ismasterframeover

	cmpi.w	#FX_ISFRAMEOVER_FLAG,d0
	beq.s	.fx_isframeover

	cmpi.w	#FX_GET_MASTERFRAME_FLAG,d0
	beq.s	.fx_get_masterframe

	cmp.w	#FX_NEXT_PHASE_FLAG,d0
	beq	.fx_next_phase

	cmpi.w	#FX_PAUSE_FLAG,d0
	beq.s	.fx_pause

	;assume end of script, don't save ptr
	move.w	#1,CTRL_FINISHED(a5)	;exit
.exit:
	rts

.pausing:
	subq.w	#1,CTRL_PAUSE_COUNTER(a5)
	beq.s	.readcmd
	rts

.isframeover:
	cmp.w	CTRL_FRAME_COUNT(a5),d0
	bhi.s	.isframeover_exit
	clr.w	CTRL_ISFRAMEOVER_COUNTER(a5)	;finished
.isframeover_exit
	rts

.ismasterframeover:
	jsr	FW_IsFrameOver		;I:d0, O:d0 
	beq.s	.ismasterframeover_exit
	clr.w	CTRL_ISMASTERFRAMEOVER_COUNTER(a5)	;finished
.ismasterframeover_exit
	rts

.fx_pause:
	move.w	(a4)+,CTRL_PAUSE_COUNTER(a5)
	move.l	a4,CTRL_SCRIPT_PTR(a5)
	rts				;exit when starting pause

.fx_pallete:
	move.w	(a4)+,d0		;Speed
	move.l	(a4)+,a0		;New pallete
	bsr.s	Controller_FX_Palette	;I:d0/a0/a5, T:d0/a0-a1
	bra.s	.loop

.fx_ismasterframeover:
	move.w	(a4)+,d0		;frame to wait for
	move.w	d0,CTRL_ISMASTERFRAMEOVER_COUNTER(a5)
	move.l	a4,CTRL_SCRIPT_PTR(a5)
	rts

.fx_isframeover:
	move.w	(a4)+,d0		;frame to wait for
	move.w	d0,CTRL_ISFRAMEOVER_COUNTER(a5)
	move.l	a4,CTRL_SCRIPT_PTR(a5)
	rts

.fx_scriptjmp:
	move.l	(a4)+,a4		;New script
	move.l	a4,CTRL_SCRIPT_PTR(a5)
	bra	.loop

.fx_get_masterframe:
	jsr	FW_GetFrame		;Get master frame count
	tst.w	d0			;Set breakpoint here
	move.w	CTRL_FRAME_COUNT(a5),d0
	rts

.fx_next_phase:
	addq.w	#1,CTRL_PHASE(a5)
	move.l	a4,CTRL_SCRIPT_PTR(a5)
	rts


*****************************************************************************
* Performs any time-based controller routines.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d7/a2-a5
*****************************************************************************

Controller_Perform:

	tst.w	CTRL_PALETTE_ACTIVE(a5)
	beq.s	.exit
	bsr	Controller_FX_Palette_Perform
.exit:
	rts


*****************************************************************************
* Sets up the pallet change process.
* IN:		a5, vec controller info
*		a0, new pallete
*		d0, speed
* OUT:		
* TRASHED:	d0/a0-a1
*****************************************************************************

Controller_FX_Palette:
	; If speed is 0 just instastransform
	tst.w	d0
	bne.s	.palette

	move.w	d0,CTRL_PALETTE_ACTIVE(a5)	;disable change, d0 is zero here
	move.w	#CL_NUM_BUFFERS,CTRL_PALETTE_LOAD_FLAG(a5)	;request copper loads immediately twice for double buffer CL issues

	lea	PAL_Current(pc),a1
	REPT PAL_NUMCOLS_ALL/2			;Number of longs
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

	lea	PAL_Current(pc),a0			;current active colors
	lea	PAL_Current_Src(pc),a1		;store original active colors
	
	REPT PAL_NUMCOLS_ALL/2			;Number of longs
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
	jsr	LIB_RGB12_Interpolate_Fast_Palette	;I:d0-d1/a0-a2, T:d0/d2-d7/a0-a2

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
* Zooms. Will do over time is speed >0
* IN:		a5, vec controller info
* 		d0-d1, speed,new w
* OUT:		
* TRASHED:	d0-d1
*****************************************************************************

Controller_FX_Zoom:
	;min/max zoom level is not limited here. It is limited in the copper routine

	tst.w	d0
	bne.s	.slow
	;d0=0
	move.w	d1,CTRL_ZOOM(a5)		;change zoom immediately
	move.w	d0,CTRL_ZOOM_VEL(a5)		;reset velocities - hard stop
	move.w	d0,CTRL_ZOOM_ACTIVE(a5)		;disable change over time
	move.w	#CL_NUM_BUFFERS,CTRL_ZOOM_LOAD_FLAG(a5)
	rts
.slow:
	move.w	#1,CTRL_ZOOM_ACTIVE(a5)
	move.w	d0,CTRL_ZOOM_SPEED(a5)	
	move.w	d1,CTRL_ZOOM_REQ(a5)	
	;clr.w	CTRL_ZOOM_VEL(a5)		;no

	rts


*****************************************************************************
* Loads the current colors into the current copperlist if changed.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0/a0-a1
*****************************************************************************

Write_Palette_To_Copper:
	;Load flag may be > 1 if using double buffered CL or changing between CLs
	tst.w	CTRL_PALETTE_LOAD_FLAG(a5)
	beq	.exit
	subq.w	#1,CTRL_PALETTE_LOAD_FLAG(a5)

	lea	PAL_Current(pc),a0
	move.l	CL_Log1_Ptr(pc),a1

	;Normal colors
	lea	P0_CL_COL_OFFSET+2(a1),a1	

;	moveq	#(1<<BPL_BUF_NUMPLANES)-1,d0
;.loop:
	REPT	PAL_NUMCOLS_MAIN
	move.w	(a0)+,(a1)
	addq.l	#4,a1			;next color
	ENDR
;	dbf	d0,.loop
.exit:	
	rts


*****************************************************************************
* Loads the current zoom into the current copperlist if changed.
* IN:		a5, VEC_Controller_Info
* OUT:
* TRASHED:	d0-d6/a0-a2 
*****************************************************************************

Do_Copper_V_Zoom:

	;Update zoom value
	lea	Sine_V,a0
	move.w	#SINE_V_OFFSET_MASK,d1
	
	move.w	CTRL_SINE_V_1(a5),d0
	add.w	CTRL_SINE_V_1_SPEED(a5),d0
	and.w	d1,d0
	move.w	d0,CTRL_SINE_V_1(a5)	;value for next frame
	move.w	(a0,d0.w),d6

	move.w	CTRL_SINE_V_2(a5),d0
	add.w	CTRL_SINE_V_2_SPEED(a5),d0
	and.w	d1,d0
	move.w	d0,CTRL_SINE_V_2(a5)	;value for next frame
	add.w	(a0,d0.w),d6

	lsr.w	#1,d6			;2 sines added, half for sum

	tst.w	d6
	bne.s	.zooming

	move.l	CL_Log1_Ptr(pc),a0
	lea	P0_CL_ZOOM_PATTERN_OFFSET+CL_PATTERN_BPL1MOD+2(a0),a1

	; Clear vertical bplcon0 values (default is vector visible as normal)
	move.w	#BPL_BUF_HEIGHT-1,d1
	move.w	#BPL_BPLMOD,d0		;Normal bplmod
	moveq	#CL_PATTERN_SIZEOF,d2
.clearloop:
	move.w	d0,(a1)
	add.l	d2,a1
	dbf	d1,.clearloop

	;Set vertical scaling to 0
	move.l	CL_Log1_Ptr(pc),a0
	lea	P0_CL_BPL_OFFSET(a0),a0
	moveq	#BPL_BUF_NUMPLANES,d0
	move.l	CTRL_BKG_FRAME_PTR(a5),d1	;current frame of animation
	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo
.exit:
	rts


.zooming
	;vertial zoom, turn bitplanes on/off to stretch
	;Our zoom bitmap is 352 wide, so we add 2 for each 16 pixels 
	;difference to cur scr or ZOOM_PATTERN_X_BYTEOFFSET	
	;Bit is 0 = show line/point normally (more common, make fastest path)
	;Bit is 1 = repeat previous line
	moveq	#0,d4			;blank line count
	move.w	d6,d1
	mulu	#ZOOM_PATTERN_BYTEWIDTH,d1
	lea	BPL_Zoom_Pattern+ZOOM_PATTERN_Y_BYTEOFFSET,a0
	add.l	d1,a0			;a0 is the line

	move.l	CL_Log1_Ptr(pc),a1
	lea	P0_CL_ZOOM_PATTERN_OFFSET+CL_PATTERN_BPL1MOD+2(a1),a1
	move.w	#BPL_BPLMOD_REPTLINE,d2	;show previous
	move.w	#BPL_BPLMOD,d3		;show next

	move.w	#(BPL_BUF_HEIGHT/16)-1,d6	;number of 16bit words in the height
	move.w	#CL_PATTERN_SIZEOF,a2
.wloop:
	move.w	(a0)+,d0		;next word
	moveq	#16-1,d5		;16bits
.bloop:
	add.w	d0,d0			;move bit into carry
	bcc.s	.showline
	move.w	d2,d1			;show previous line again
	addq.w	#1,d4			;increase count of stretched lines
	bra.s	.nextbit
.showline:
	move.w	d3,d1			;show next line
.nextbit:
	move.w	d1,(a1)
	add.l	a2,a1			;next copper entry
	dbf	d5,.bloop		;next bit
	dbf	d6,.wloop

	;d4 = total number of blank lines for entire screen, divide by two to get y offset
	;note:interleaved
	lsr.w	#1,d4
	mulu	#BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES,d4	;pointer to first line to show

	;Increase the y offset by d4 lines to center
	move.l	CL_Log1_Ptr(pc),a0
	lea	P0_CL_BPL_OFFSET+2(a0),a0	;copper bpl pointer block
	move.l	CTRL_BKG_FRAME_PTR(a5),d1	;current frame of animation
	add.l	d4,d1			
	move.w	d1,4(a0)	;ptl
	swap	d1
	move.w	d1,(a0)		;pth

;	move.l	CL_Log1_Ptr(pc),a0
;	lea	P0_CL_BPL_OFFSET+8(a0),a0
;	moveq	#BPL_BUF_NUMPLANES,d0
;	move.l	CTRL_BKG_FRAME_PTR(a5),d1	;current frame of animation
;	moveq 	#BPL_BUF_BYTEWIDTH,d2	;interleaved
;	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	rts



*****************************************************************************
* Loads the current zoom into the current copperlist if changed.
* IN:		a5, VEC_Controller_Info
* OUT:
* TRASHED:	d0-d6/a0-a2 
*****************************************************************************

Do_Copper_H_Zoom:
	IFNE 0
	;Update zoom value
	lea	Sine_H,a0
	move.w	#SINE_H_OFFSET_MASK,d0
	
	move.w	CTRL_SINE_H_1(a5),d1
	add.w	CTRL_SINE_H_1_SPEED(a5),d1
	and.w	d0,d1
	move.w	d1,CTRL_SINE_H_1(a5)	;value for next frame

	move.w	CTRL_SINE_H_2(a5),d2
	add.w	CTRL_SINE_H_2_SPEED(a5),d2
	and.w	d1,d2
	move.w	d2,CTRL_SINE_H_2(a5)	;value for next frame


	move.l	CL_Log1_Ptr(pc),a1
	lea	P0_CL_ZOOM_PATTERN_OFFSET+CL_PATTERN_BPLCON1+2(a1),a1
	move.w	#CL_PATTERN_SIZEOF,a2
	move.w	#BPL_BUF_HEIGHT-1,d3
.loop	
	move.w	(a0,d1.w),d6
	add.w	(a0,d2.w),d6
	lsr.w	#1,d6

	add.w	CTRL_SINE_H_1_STEP(a5),d1
	add.w	CTRL_SINE_H_2_STEP(a5),d2
	and.w	d0,d1
	and.w	d0,d2



	move.w	d6,(a1)
	add.l	a2,a1			;next line

	dbf	d3,.loop

	ENDC
	rts


****************************************************************************
* Draws a letter from font1 at given x,y using blitter copy.
* Note assumes BOB BPL is interleaved.
* This routine only plots at any x position. Slower than Aligned version.
* IN:		a6, custom
*		a0, screen
*		a1, y premult
*		d0.w, x
*		d1.w, y
*		d2.b, ascii value
* OUT:		
* TRASHED:	d0-d3/a0/a2/a3
*****************************************************************************

Font1_Plot_Letter_Blit:

	lea	FAR_Font8px,a2
	ext.w	d2			;clear top
	move.b	(a2,d2.w),d2		;bob number (top d2 still clear)
	bmi.s	.exit			;is letter valid? $ff = not valid
					;assumption is will never have >127 bobs
					;in a font so can test for $ff by bmi

	lea	BOB_Font8px,a2
	mulu	#LIB_BOBTABLE_SIZEOF,d2	;find bob entry for this letter
	lea	(a2,d2.w),a2

	lea	BPL_Font8px,a3	;root bob bitplane data
	add.l	LIB_BOBTABLE_OFFSET(a2),a3	;offset to bob bpl

	;If premult table available, use it
	;add.w	d1,d1			;y value, access table in words
	;add.w	(a1,d1.w),a0		;add y value to screen adr
	mulu	#BPL_BUF_FONT_SCRATCH_BYTEWIDTH*BPL_BUF_NUMPLANES,d1
	add.l	d1,a0

	ext.l	d0			;ensure top word clear
	ror.l	#4,d0			;hiword of d0 contains shift in highest nibble
	add.w	d0,d0			;loword d0 contains byte offset into screen 
	add.w	d0,a0			;add byte offset to y address
	swap	d0			;d0 word now contains shift value
	or.w	#BLT_SRC_ACD+(BLT_A|BLT_C),d0	;$bfa,D=A|C
	swap	d0			;d0=bltcon0 and bltcon1
	clr.w	d0			;bltcon1=0

	; Need to have an extra word for shifting, so artificially increased
	; bob size by 1 word (and -2 for modulos)
	IFEQ FONT1_MONO		;proportional, have to work out widths
		move.w	LIB_BOBTABLE_HEIGHT(a2),d1	
		mulu	#FONT1_NUMPLANES*64,d1
		move.w	LIB_BOBTABLE_WIDTHINWORDS(a2),d3
		add.w	d3,d1		;bltsize

		add.w	d3,d3		;bytewidth
		moveq	#BPL_BUF_FONT_SCRATCH_BYTEWIDTH,d2	;ensure top of d2 is 0
		sub.w	d3,d2		;modulo
	ELSE
		move.w	#FONT1_BLTSIZE+1,d1		;same for all bobs (mono font)
		moveq	#BPL_BUF_FONT_SCRATCH_BYTEWIDTH-(FONT1_BYTEWIDTH+2),d2	;modulo (top of d2 is 0)
	ENDC

	WAITBLIT_NASTY_A6
	move.l	d0,bltcon0(a6)
	move.l	#$ffff0000,bltafwm(a6)	;mask last word as it part of next letter!
	move.w	d2,bltcmod(a6)
	move.w	#-2,bltamod(a6)	; interleaved (mod 0) but have extra word for shifting
	move.w	d2,bltdmod(a6)
	move.l	a0,bltcpth(a6)	;Screen
	move.l	a3,bltapth(a6)	;BOB data
	move.l	a0,bltdpth(a6)	;Screen
	move.w	d1,bltsize(a6)
.exit:
	rts


****************************************************************************
* Clears the font scratch area. 
* IN:		a6, custom
* OUT:		
* TRASHED:	None
*****************************************************************************

Clear_Font_Scratch:
	WAITBLIT_NASTY_A6
	move.l	#$01000000,bltcon0(a6)
	move.l	#BPL_Font_Scratch,bltdpth(a6)
	move.w	#0,bltdmod(a6)
	move.w	#((BPL_BUF_FONT_SCRATCH_HEIGHT*BPL_BUF_FONT_SCRATCH_NUMPLANES)*64)+BPL_BUF_FONT_SCRATCH_WORDWIDTH,bltsize(a6)
	rts


****************************************************************************
* Copies the font scratch area into the offscreen bottom sprite.
* IN:		a6, custom
* OUT:		
* TRASHED:	None
*****************************************************************************

Copy_Font_Scratch_To_Sprite:

	lea	BPL_Font_Scratch,a2
	lea	SPR_Phys_0+4,a1		;Skip to bpl data

	;Sprite 0
	lea	(a2),a0
	WAITBLIT_NASTY_A6
	move.l	#$09f00000,bltcon0(a6)
	move.l	#-1,bltafwm(a6)
	move.l	a0,bltapth(a6)
	move.l	a1,bltdpth(a6)
	move.w	#BPL_BUF_FONT_SCRATCH_BYTEWIDTH-2,bltamod(a6)
	move.w	#2,bltdmod(a6)		;Only 1 bpl at the mo
	move.w	#((FONT1_HEIGHT*FONT1_NUMPLANES)*64)+1,bltsize(a6)	;1 word blits per line

	;6 more sprites (7 is unused)
	REPT	6
	lea	(a2),a0
	lea	SPR_LIST_LINE_SIZEOF(a1),a1
	WAITBLIT_NASTY_A6
	move.l	a0,bltapth(a6)
	move.l	a1,bltdpth(a6)
	move.w	#((FONT1_HEIGHT*FONT1_NUMPLANES)*64)+1,bltsize(a6)	;1 word blits per line
	ENDR
	
	rts


*****************************************************************************

	RSRESET
CTRL_SCRIPT_PTR:			rs.l 1		;Script Ptr
CTRL_FINISHED:				rs.w 1		;1 if quitting
CTRL_PRECALC_INTROSTART_DONE:		rs.w 1		;1 if intro precalc done
CTRL_PHASE:				rs.w 1		;Current phase
CTRL_FRAME_COUNT:			rs.w 1		;Local (effect) frame counter
CTRL_PAUSE_COUNTER:			rs.w 1		;Pause counter, 0=running
CTRL_ISFRAMEOVER_COUNTER:		rs.w 1		;Waiting for frame, 0=inactive
CTRL_ISMASTERFRAMEOVER_COUNTER:		rs.w 1		;Waiting for frame, 0=inactive
CTRL_P0_PRECALC_DONE:			rs.w 1		;1 if effect precalc done
CTRL_PALETTE_LOAD_FLAG			rs.w 1		;set to >1 to force palette load
CTRL_PALETTE_ACTIVE:			rs.w 1		;Palette change active
CTRL_PALETTE_PTR:			rs.l 1		;src Palette ptr (16 words of colors)
CTRL_PALETTE_COUNTER:			rs.w 1		;Palette counter, speed
CTRL_PALETTE_SPEED:			rs.w 1		;How often to update, higher is slower, 0 = instant
CTRL_PALETTE_STEP:			rs.w 1		;Current step to interpolate between current color and final 0-15
CTRL_BKG_FRAME				rs.w 1		;Current background frame
CTRL_BKG_FRAME_PTR			rs.l 1		;Address of the frame BPL
CTRL_BKG_FRAME_SPEED			rs.w 1		;Delay between frames
CTRL_BKG_FRAME_COUNTER			rs.w 1		;Frame delay counter
CTRL_ZOOM:				rs.w 1		;current zoom level
CTRL_ZOOM_ACTIVE:			rs.w 1		;zoom active
CTRL_ZOOM_LOAD_FLAG			rs.w 1		;set to >1 to force zoom load
CTRL_ZOOM_SPEED:			rs.w 1
CTRL_ZOOM_REQ:				rs.w 1		;Final zoom level
CTRL_ZOOM_VEL:				rs.w 1
CTRL_SINE_V_1:				rs.w 1
CTRL_SINE_V_1_SPEED:			rs.w 1
CTRL_SINE_V_2:				rs.w 1
CTRL_SINE_V_2_SPEED:			rs.w 1	
CTRL_SINE_H_1:				rs.w 1
CTRL_SINE_H_1_SPEED:			rs.w 1	
CTRL_SINE_H_1_STEP:			rs.w 1	
CTRL_SINE_H_2:				rs.w 1
CTRL_SINE_H_2_SPEED:			rs.w 1	
CTRL_SINE_H_2_STEP:			rs.w 1	
CTRL_SIZE:				rs.w 0

	EVEN
Controller_Info:
	dcb.b	CTRL_SIZE,0			;Init all to zero by default
	EVEN

*****************************************************************************

; Screen buffer and copper ptrs, must stay in this order as accessed as a group
BPL_Phys_Ptr:		dc.l	0	;Screen buffers - physical
BPL_Log1_Ptr:		dc.l	0	;Logical1
CL_Phys_Ptr:		dc.l	0	;Copper ptr - physical
CL_Log1_Ptr:		dc.l	0	;Logical1


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

BPL_PIC_NUMFRAMES = 7		;Number of frames annoyingly not power of 2

BPL_Pic_Frames:
	dc.l	BPL_Pic_0
	dc.l	BPL_Pic_1
	dc.l	BPL_Pic_2
	dc.l	BPL_Pic_3
	dc.l	BPL_Pic_4
	dc.l	BPL_Pic_5
	dc.l	BPL_Pic_6

	EVEN
BPL_Pic_Source_0:
	INCBIN "AssetsConverted/Warp_352x272x1_0.BPL.nrv2s"
	EVEN
BPL_Pic_Source_1:
	INCBIN "AssetsConverted/Warp_352x272x1_1.BPL.nrv2s"
	EVEN
BPL_Pic_Source_2:
	INCBIN "AssetsConverted/Warp_352x272x1_2.BPL.nrv2s"
	EVEN
BPL_Pic_Source_3:
	INCBIN "AssetsConverted/Warp_352x272x1_3.BPL.nrv2s"
	EVEN
BPL_Pic_Source_4:
	INCBIN "AssetsConverted/Warp_352x272x1_4.BPL.nrv2s"
	EVEN
BPL_Pic_Source_5:
	INCBIN "AssetsConverted/Warp_352x272x1_5.BPL.nrv2s"
	EVEN
BPL_Pic_Source_6:
	INCBIN "AssetsConverted/Warp_352x272x1_6.BPL.nrv2s"
	EVEN

PAL_Pic_Source:
	dc.w	$000,$fff
	;INCLUDE "AssetsConverted/PlayItAgainSam_320x256x5_inter.PAL_dcw.i"


*****************************************************************************

SINE_V_NUMENTRIES = 256	; Must be power of 2
SINE_V_OFFSET_MASK = ((SINE_V_NUMENTRIES*2)-2)	; Word offset access into the table
; SineX is now actually the word offset so don't need to multiply by two
; but must make sure it's even

Sine_V:
	INCLUDE "sine_0_255_256_words.i"

SINE_H_NUMENTRIES = 256	; Must be power of 2
SINE_H_OFFSET_MASK = ((SINE_H_NUMENTRIES*2)-2)	; Word offset access into the table

Sine_H:
	INCLUDE "sine_0_15_256_words.i"


*****************************************************************************

; Font BOB structure and font ascii lookup, BOB and FAR in public
BOB_Font8px:
	INCLUDE "AssetsConverted/Miami_Font_8x8x1.BOB_dcw.i"
FAR_Font8px:
	INCLUDE "AssetsConverted/Miami_Font_8x8x1.FAR_dcb.i"


*****************************************************************************

FX_END_FLAG			= 	0
FX_PAUSE_FLAG			=	1
FX_GET_MASTERFRAME_FLAG		=	2
FX_ISMASTERFRAMEOVER_FLAG	=	3
FX_ISFRAMEOVER_FLAG		=	4
FX_SCRIPTJMP_FLAG		=	5
FX_PALETTE_FLAG			=	6
FX_NEXT_PHASE_FLAG		=	7

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

FX_ISMASTERFRAMEOVER	MACRO
		dc.w	FX_ISMASTERFRAMEOVER_FLAG
		dc.w	\1		;frames wait for (global timing)
		ENDM

FX_ISFRAMEOVER	MACRO
		dc.w	FX_ISFRAMEOVER_FLAG
		dc.w	\1		;frames wait for (local timing)
		ENDM

FX_GET_MASTERFRAME	MACRO
		dc.w	FX_GET_MASTERFRAME_FLAG
		ENDM

FX_NEXT_PHASE	MACRO
		dc.w	FX_NEXT_PHASE_FLAG
		ENDM

*****************************************************************************

ControllerScript:
	;First pause needs to allow for depack time
	;FX_PAUSE	10

	FX_PALETTE	1,PAL_Pic_Source

	FX_PAUSE	1000

	FX_END


*****************************************************************************
*****************************************************************************
*****************************************************************************

	SECTION	IntroFramework_ChipData_Copper,DATA_C	;Chip Data Section for gfx/music

*****************************************************************************

; This value is used for loading the CL with colours and stuff, we might need
; to do it more than once, for example fading colours in a double-buffered CL
; we need to write the values 2 twice so that after finishing both CLs have been
; Updated. 
; Set to 1 for single CL, 2 for double-buffered, etc
CL_NUM_BUFFERS = 2

; Copper horizontal blanking notes from Photon/Scoopex
; As established, positions $e7...$03 are not usable. If you're writing a simple 
; copperlist with no need for tight timing, positions $df and $07 are conventionally 
;used for the positions on either side of the horizontal blanking, and for 
; compatibility across chipsets use increments of 4 from these, resulting in 
;positions $db, $0b, and so on.

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

P0_CL_Phys:
	;IFNE FW_IRQ_TYPE_COPPER
	;	CMOVE	intreq,INTF_SETCLR|INTF_COPER	;Trigger IRQ, nothing visible
	;ENDC

	;CMOVE	fmode,MEMORYFETCHMODE	;Chip Ram fetch mode (0=OCS)
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	CMOVE 	bplcon0,$1200
	CMOVE 	bplcon1,$0088
	CMOVE	bplcon2,$0038		;Sprites infront of PF2
	CMOVE 	bpl1mod,BPL_BPLMOD
	CMOVE 	bpl2mod,BPL_BPLMOD

	CMOVE	dmacon,DMAF_SETCLR|DMAF_SPRITE	;enable sprites

	;CWAIT	25-1,$7			;Time for altering Copperlist in vblank
	;CWAIT	P0_DIW_V-1,$7

P0_CL_Scr_Sprites:			        ;Sprite pointers
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

P0_CL_Bpl:				;Bitplane pointers
	CMOVE	bpl1pth,$0
	CMOVE	bpl1ptl,$0
;	CMOVE	bpl2pth,$0
;	CMOVE	bpl2ptl,$0
;	CMOVE	bpl3pth,$0
;	CMOVE	bpl3ptl,$0
;	CMOVE	bpl4pth,$0
;	CMOVE	bpl4ptl,$0
;	CMOVE	bpl5pth,$0
;	CMOVE	bpl5ptl,$0

P0_CL_Cols:
	CMOVE	tmpcolor00,$000
a set color01
	REPT	PAL_NUMCOLS_MAIN-1	
	CMOVE	a,$000
a set a+2
	ENDR

	CMOVE	color02,$000
	CMOVE	color03,$f00

;	CMOVE	color16,$000		;spr0/1
	CMOVE	color17,$600
	CMOVE	color18,$060
	CMOVE	color19,$006

;	CMOVE	color20,$000		;spr2/3
	CMOVE	color21,$600
	CMOVE	color22,$060
	CMOVE	color23,$006

;	CMOVE	color24,$000		;spr4/5
	CMOVE	color25,$600
	CMOVE	color26,$060
	CMOVE	color27,$006

;	CMOVE	color28,$000		;spr6/7
	CMOVE	color29,$600
	CMOVE	color30,$060
	CMOVE	color31,$006

P0_CL_Zoom_Pattern:
a set P0_DIW_V
	REPT BPL_BUF_HEIGHT
	CWAIT	(a&$ff),$7		;0,2
	CMOVE 	bpl1mod,BPL_BPLMOD	;4,6
;	CMOVE	bplcon1,$0000
	CWAIT	(a&$ff),$df		;8,10
a set a+1
	ENDR

	; Trigger copper interrupt
	IFNE FW_IRQ_TYPE_COPPER
		;IF SCANLINE_EOF>255
		;	CWAIT	255,$e1
		;ENDIF
		;CWAIT	(SCANLINE_EOF-1)&$ff,$df
		CMOVE	intreq,INTF_SETCLR|INTF_COPER
	ENDC

	COPPEREND
P0_CL_End:

P0_CL_BPL_OFFSET = (P0_CL_Bpl-P0_CL_Phys)
P0_CL_ZOOM_PATTERN_OFFSET = (P0_CL_Zoom_Pattern-P0_CL_Phys)
P0_CL_SPR_OFFSET = (P0_CL_Scr_Sprites-P0_CL_Phys)
P0_CL_COL_OFFSET = (P0_CL_Cols-P0_CL_Phys)
P0_CL_SIZE = P0_CL_End-P0_CL_Phys

	RSRESET
CL_PATTERN_WAIT1:	rs.w	2
CL_PATTERN_BPL1MOD:	rs.w	2
;CL_PATTERN_BPLCON1:	rs.w	2
CL_PATTERN_WAIT2:	rs.w	2
CL_PATTERN_SIZEOF:	rs.w	0


*****************************************************************************

;Sprite list
SPR_HEIGHT		= FONT1_HEIGHT		;Actual sprite height
SPR_V_SPACING		= 16			;Spaced every 16 lines
SPR_NUM_LINES 		= BPL_BUF_HEIGHT/SPR_V_SPACING

SPR_Phys_0:
	dc.w	0,0
	dcb.l	SPR_HEIGHT,$fff00000
SPR_Phys_0_End:

SPR_Phys_1:
	dc.w	0,0
	dcb.l	SPR_HEIGHT,$fff00000
SPR_Phys_2:
	dc.w	0,0
	dcb.l	SPR_HEIGHT,$fff00000
SPR_Phys_3:
	dc.w	0,0
	dcb.l	SPR_HEIGHT,$fff00000
SPR_Phys_4:
	dc.w	0,0
	dcb.l	SPR_HEIGHT,$fff00000
SPR_Phys_5:
	dc.w	0,0
	dcb.l	SPR_HEIGHT,$fff00000
SPR_Phys_6:
	dc.w	0,0
	dcb.l	SPR_HEIGHT,$fff00000
SPR_Phys_7:
	dc.w	0,0
	dcb.l	SPR_HEIGHT,$fff00000

	dc.w	0,0		;End of list

;Size of a single "line" of a sprite list
SPR_LIST_LINE_SIZEOF = SPR_Phys_0_End-SPR_Phys_0


*****************************************************************************

; Font BPL in chip
BPL_Font8px:
	INCBIN "AssetsConverted/Miami_Font_8x8x1.BPL"

; Scratch area for writing font
BPL_Font_Scratch:
	dcb.b	BPL_BUF_FONT_SCRATCH_TOTALSIZE,0

*****************************************************************************
*****************************************************************************

; Map screens to shared chipmem buffers
CUR_CHIP_BUF set FW_Chip_Buffer_1

BPL_Phys		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_Log1		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_Pic_0		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_Pic_1		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_Pic_2		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_Pic_3		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_Pic_4		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_Pic_5		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_Pic_6		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

P0_CL_Log1		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+P0_CL_SIZE

*****************************************************************************

; Map shared public buffers
CUR_PUBLIC_BUF set FW_Public_Buffer_1

Mult_SCR_Height_ByteWidth_NumPlanes = CUR_PUBLIC_BUF
CUR_PUBLIC_BUF set CUR_PUBLIC_BUF+(BPL_BUF_HEIGHT*2)	;height * dc.w

*****************************************************************************

