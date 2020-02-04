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


*****************************************************************************

	SECTION	EndCredits_PublicCode,CODE	;Code section in Public memory

*****************************************************************************

; Photon:
; Best practice is to start with DDF 38,d0 and DIW 2c81,2cc1 and modify these 
; symmetrically and always in whole 16px steps for compatibility.
; Note DDF start of less than 30 and you start to lose sprites.

*** Display Window ***
P0_DIW_V		=	$2c		;Hardware Vstart ($2c normal, $24 overscan)
P0_DIW_H		=	$81		;Hardware Hstart ($81 normal, $71 overscan)
P0_DIW_WIDTH		=	320		;Pixels		 (multiple of 16, 320 normal, 352 overscan)
P0_DIW_HEIGHT		=	256		;Lines		 (256 normal PAL, 272 overscan)

P0_DDF_H		=	$81		;Hardware Hstart ($81 normal, $71 overscan)
P0_DDF_WIDTH		=	320		;Pixels		 (320 normal pal, 352 overscan)
P0_DDF_BYTEWIDTH	=	P0_DDF_WIDTH/8
P0_DDF_WORDWIDTH	=	P0_DDF_BYTEWIDTH/2

*****************************************************************************

; This is the actual font
; Have to declare font sizes at top to avoid assembler issues.
; Set MONO to 0 (proportional fonts) to work out on the fly. Much slower.
FONT1_MONO = 0
FONT1_SPACE_WIDTH = 4
FONT1_NUMPLANES = 2
FONT1_HEIGHT = 16
FONT1_WIDTH = 8		;Actual max width of font in pixels (visible pixels)

;These values are for mono-width blits. Propoptional sizes are worked out
;on-the-fly. But the the max width is used for working out the largest X plot
;Note, even if a font is 1-15px wide, that would be a minimum blit of 1 word
;(we also add on a word for shifting when drawing, not shown here)
;All these widths must be 16px/word aligned
FONT1_BLT_WIDTH = ((FONT1_WIDTH+16)/16)*16	;Round up to nearest 16px
FONT1_BLT_BYTEWIDTH = FONT1_BLT_WIDTH/8
FONT1_BLT_WORDWIDTH = FONT1_BLT_BYTEWIDTH/2
FONT1_BLTSIZE = ((FONT1_HEIGHT*FONT1_NUMPLANES*64)+FONT1_BLT_WORDWIDTH)

; Max x is width minus size of blt, minus 16px (extra word for shifting)
FONT1_MIN_X = 8
FONT1_MAX_X = (BPL_BUF_FON_WIDTH-1)-16
FONT1_X_SPACING = 0


*****************************************************************************

*** Screen Definitions ***

;The pictures
BPL_BUF_PIC_3BPL_WIDTH		=	320
BPL_BUF_PIC_3BPL_BYTEWIDTH	=	BPL_BUF_PIC_3BPL_WIDTH/8
BPL_BUF_PIC_3BPL_WORDWIDTH	=	BPL_BUF_PIC_3BPL_BYTEWIDTH/2
BPL_BUF_PIC_3BPL_HEIGHT		=	256
BPL_BUF_PIC_3BPL_NUMPLANES	=	3
BPL_BUF_PIC_3BPL_NUMCOLS 	= 	(1<<BPL_BUF_PIC_3BPL_NUMPLANES)
BPL_BUF_PIC_3BPL_SIZE		=	BPL_BUF_PIC_3BPL_BYTEWIDTH*BPL_BUF_PIC_3BPL_HEIGHT
BPL_BUF_PIC_3BPL_TOTALSIZE	=	BPL_BUF_PIC_3BPL_SIZE*BPL_BUF_PIC_3BPL_NUMPLANES

BPL_BUF_PIC_5BPL_WIDTH		=	320
BPL_BUF_PIC_5BPL_BYTEWIDTH	=	BPL_BUF_PIC_5BPL_WIDTH/8
BPL_BUF_PIC_5BPL_WORDWIDTH	=	BPL_BUF_PIC_5BPL_BYTEWIDTH/2
BPL_BUF_PIC_5BPL_HEIGHT		=	256
BPL_BUF_PIC_5BPL_NUMPLANES	=	5
BPL_BUF_PIC_5BPL_NUMCOLS 	= 	(1<<BPL_BUF_PIC_5BPL_NUMPLANES)
BPL_BUF_PIC_5BPL_SIZE		=	BPL_BUF_PIC_5BPL_BYTEWIDTH*BPL_BUF_PIC_5BPL_HEIGHT
BPL_BUF_PIC_5BPL_TOTALSIZE	=	BPL_BUF_PIC_5BPL_SIZE*BPL_BUF_PIC_5BPL_NUMPLANES


;Vertical spacing of lines
VSCROLL_BUF_HEIGHT	=	FONT1_HEIGHT+(FONT1_HEIGHT/3)

;The scrolling font screen (2 screens high plus 3xTXT AREA)
BPL_BUF_FON_WIDTH	=	BPL_BUF_PIC_3BPL_WIDTH
BPL_BUF_FON_BYTEWIDTH	=	BPL_BUF_FON_WIDTH/8
BPL_BUF_FON_WORDWIDTH	=	BPL_BUF_FON_BYTEWIDTH/2
BPL_BUF_FON_HEIGHT	=	(BPL_BUF_PIC_3BPL_HEIGHT*2)+(VSCROLL_BUF_HEIGHT*3)
BPL_BUF_FON_NUMPLANES	=	2
BPL_BUF_FON_NUMCOLS 	= 	(1<<BPL_BUF_FON_NUMPLANES)
BPL_BUF_FON_SIZE	=	BPL_BUF_FON_BYTEWIDTH*BPL_BUF_FON_HEIGHT
BPL_BUF_FON_TOTALSIZE	=	BPL_BUF_FON_SIZE*BPL_BUF_FON_NUMPLANES

VSCROLL_MIN		=	VSCROLL_BUF_HEIGHT
VSCROLL_MAX		=	BPL_BUF_PIC_3BPL_HEIGHT+(VSCROLL_MIN*2)

P0_BPL_BPL1MOD		=	(BPL_BUF_PIC_5BPL_BYTEWIDTH-P0_DDF_BYTEWIDTH)+(BPL_BUF_PIC_5BPL_BYTEWIDTH*(BPL_BUF_PIC_5BPL_NUMPLANES-1))	;interleaved mode
P0_BPL_BPL2MOD		=	P0_BPL_BPL1MOD

P1_BPL_BPL1MOD		=	(BPL_BUF_PIC_3BPL_BYTEWIDTH-P0_DDF_BYTEWIDTH)+(BPL_BUF_PIC_3BPL_BYTEWIDTH*(BPL_BUF_PIC_3BPL_NUMPLANES-1))	;interleaved mode
P1_BPL_BPL2MOD		=	(BPL_BUF_FON_BYTEWIDTH-P0_DDF_BYTEWIDTH)+(BPL_BUF_FON_BYTEWIDTH*(BPL_BUF_FON_NUMPLANES-1))	;interleaved mode

SCANLINE_EOF		=	P0_DIW_V+P0_DIW_HEIGHT	; Safe to starting clearing after this scanline


*****************************************************************************

PAL_NUMCOLS_MAIN	= BPL_BUF_PIC_5BPL_NUMCOLS	; number of main colour entries in our palettes
PAL_NUMCOLS_DARK	= 0				; number of dark/refl cols
PAL_NUMCOLS_ALL		= (PAL_NUMCOLS_MAIN+PAL_NUMCOLS_DARK)


*****************************************************************************

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

	xdef	END_Start
END_Start:
	movem.l	d2-d7/a2-a6,-(sp)	;save
	lea	ControllerScript,a0

	lea	_custom,a6
	lea	Controller_Info,a5

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
	xdef	END_PreCalc_IntroStart
	ENDC
	
SubPartPreCalc_IntroStart:	
END_PreCalc_IntroStart:
	tst.w	Controller_Info+CTRL_PRECALC_INTROSTART_DONE
	bne.s	.exit			;already done

	movem.l	d2-d7/a2-a6,-(sp)	;save
	lea	Controller_Info,a5
	
	;Completed!
	move.w	#1,CTRL_PRECALC_INTROSTART_DONE(a5)

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
	move.w	d0,CTRL_FRAME_COUNT(a5)
	move.w	d0,CTRL_PAUSE_COUNTER(a5)
	move.w	d0,CTRL_PALETTE_ACTIVE(a5)
	move.w	d0,CTRL_SCROLL_DELAY_COUNTER(a5)

	move.l	#Scroller_Text,CTRL_TEXT_PTR(a5)
	move.w	#VSCROLL_MIN,CTRL_VSCROLL(a5)
	move.w	#VSCROLL_BUF_HEIGHT,CTRL_SCROLL_COUNTER(a5)


	; Clear all screen buffers (previous routine must have blanked screen/colors)
	bsr	Clear_ScreenBuffers_CPU

	; Load default palette
	lea	PAL_AllBlack(pc),a0
	moveq	#0,d0
	bsr	Controller_FX_Palette	;I:d0/a0/a5, T:d0/a0-a1

	;Depack pictures
	lea	BPL_Pic_3Bpl_Source(pc),a0
	lea	BPL_Pic_3bpl,a1
	jsr	LIB_NRV2S_Depack		;I:a0-a1, T:d0-d1/a0-a1	
	lea	BPL_Pic_5Bpl_Source(pc),a0
	lea	BPL_Pic_5bpl,a1
	jsr	LIB_NRV2S_Depack		;I:a0-a1, T:d0-d1/a0-a1	

	;Depack font
	lea	BPL_Font8px_Source(pc),a0
	lea	BPL_Font8px,a1
	jsr	LIB_NRV2S_Depack		;I:a0-a1, T:d0-d1/a0-a1	

	;Setup phys/logical bpl and copperlist ptrs and load palette
	bsr	P0_CL_InitPtrs		;I:a6, T:d0-d2/a0-a1
	bsr	P0_Write_Palette_To_Copper	;I:a5, T:d0/a0-a2
	
	;Swap CL buffer so that the physical CL has had the colors loaded
	bsr	ScreenBufferSwap	;d0-d1/a0

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
	lea	Controller_Info,a5

	jsr	FW_VBlankProxy		;T:d0-d7/a0-a4

	; Read new script lines and perform
	addq.w	#1,CTRL_FRAME_COUNT(a5)	;Update local frame count
	bsr	Controller_ReadCommands	;Read new commands
	bsr	Controller_Perform	;Do any ongoing time-based effects and update angles

	;Load current palette into CL if needed
	bsr	P0_Write_Palette_To_Copper	;I:a5, T:d0/a0-a2

	;Swap buffers and load copper for next frame
	bsr	ScreenBufferSwap	;d0-d1/a0

	;Changing phase?
	cmp.w	#1,CTRL_PHASE(a5)
	bne.s	.nophase
	bsr	P1_Init				;I:a5/a6, T:d0-d1/a0-a1	
.nophase:

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

	;Background doesn't change
	lea	P0_CL_Phys,a0
	lea	P0_CL_BPL_OFFSET(a0),a0	;copper bpl pointer block
	moveq	#BPL_BUF_PIC_5BPL_NUMPLANES,d0
	move.l	#BPL_Pic_5bpl,d1		;in d1 for InitCopperBplPtrs
	moveq 	#BPL_BUF_PIC_5BPL_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo



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

	;
	;Back buffer copper BPL pointers
	;

	lea	P0_CL_Log1,a0
	move.l	a0,CL_Log1_Ptr

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
	;bsr	P1_Write_Palette_To_Copper		;I:a5, T:a0-a2

	;Initialise our irq. Leave the copper showing previous frame and let the 
	;irq swap it. This is to minmise any blank frame. The previous routine is
	;expected to be used a different memory range.
	;move.l	CL_Phys_Ptr(pc),a0
	;lea	P0_Lev3Irq(pc),a1
	;jsr	FW_SetCopperAndLev3Irq_A6
	lea	P1_Lev3Irq(pc),a0
	jsr	FW_SetLev3Irq

	movem.l	(sp)+,d2-d7/a2-a4
	rts


*****************************************************************************

P1_Lev3Irq:
	TIMERON
	movem.l	d0-d7/a0-a6,-(sp)

	lea	_custom,a6
	lea	Controller_Info,a5

	jsr	FW_VBlankProxy		;T:d0-d7/a0-a4

	; Read new script lines and perform
	addq.w	#1,CTRL_FRAME_COUNT(a5)	;Update local frame count
	bsr	Controller_ReadCommands	;Read new commands
	bsr	Controller_Perform	;Do any ongoing time-based effects and update angles

	; Scroll the text and write a new line if needed
	bsr	Text_Scroll		;I:a5/a6, T:d0-d2/a0

	;Load current palette into CL if needed
	bsr	P1_Write_Palette_To_Copper	;I:a5, T:d0/a0-a2

	;Swap buffers and load copper for next frame
	bsr	ScreenBufferSwap	;d0-d1/a0

	;Reset interrupt
	IFNE FW_IRQ_TYPE_COPPER
		moveq	#INTF_COPER,d0
	ELSE
		moveq	#INTF_VERTB,d0
	ENDC
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)		;A4000 compat

	movem.l	(sp)+,d0-d7/a0-a6

	;Blitter nasty on while we finish off any blits.
	WAITBLIT_NASTY
	TIMEROFF
	rte


*****************************************************************************
* Inits ptrs in the copper list.
* IN:		
* OUT:		
* TRASHED:	d0-d2/a0-a1
*****************************************************************************

P1_CL_InitPtrs:

	;Setup items the same in front/back copper lists

	;Overlay doesn't change
	lea	P1_CL_Phys,a0
	lea	P1_CL_BPL_PF1_OFFSET(a0),a0	;copper bpl pointer block
	moveq	#BPL_BUF_PIC_3BPL_NUMPLANES,d0
	move.l	#BPL_Pic_3bpl,d1		;in d1 for InitCopperBplPtrs
	moveq 	#BPL_BUF_PIC_3BPL_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo



	; Copper list buffers - copy screen list into 2nd buffer for doublebuffering
	lea	P1_CL_Phys,a0		;source
	lea	P1_CL_Log1,a1		;dest
	move.w	#(P1_CL_SIZE/2),d0	;size in words
	jsr	FW_CopyBuffer_CPU	;I:d0/a0-a1, T:d0-d7/a0-a2

	;
	;Front buffer copper BPL pointers
	;

	lea	P1_CL_Phys,a0
	move.l	a0,CL_Phys_Ptr
	lea	P1_CL_BPL_PF2_OFFSET(a0),a0	;copper bpl pointer block
	moveq	#BPL_BUF_FON_NUMPLANES,d0
	move.l	#BPL_Scroller,d1		;in d1 for InitCopperBplPtrs
	move.l	d1,BPL_Phys_Ptr
	moveq 	#BPL_BUF_FON_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	;
	;Back buffer copper BPL pointers
	;

	lea	P1_CL_Log1,a0
	move.l	a0,CL_Log1_Ptr
	lea	P1_CL_BPL_PF2_OFFSET(a0),a0	;copper bpl pointer block
	moveq	#BPL_BUF_FON_NUMPLANES,d0
	;move.l	#BPL_Log1,d1		;in d1 for InitCopperBplPtrs
	move.l	#BPL_Scroller,d1		;in d1 for InitCopperBplPtrs
	move.l	d1,BPL_Log1_Ptr
	moveq 	#BPL_BUF_FON_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	rts


*****************************************************************************
* Swaps the frame buffers, copperlists, and activates the CL next frame.
* NOTE: Call before vblank so new copper takes effect next frame.
* IN:		a6, _custom
* OUT:		
* TRASHED:	d0-d1/a0
*****************************************************************************

ScreenBufferSwap:


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
* Clears all the buffers we will use. Done once at startup to ensure clean
* environment. USes CPU for precalc routine.
* IN:		
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

Clear_ScreenBuffers_CPU:
	movem.l	d2-d7/a2-a6,-(sp)

	lea	BPL_Scroller,a0
	move.w	#(BPL_BUF_FON_TOTALSIZE/2),d0
	jsr	FW_ClearBuffer_CPU	;I:d0/a0, T:d0-d7/a0-a4

;	lea	BPL_Log1,a0
;	move.w	#(BPL_BUF_PIC_3BPL_TOTALSIZE/2),d0
;	jsr	FW_ClearBuffer_CPU	;I:d0/a0, T:d0-d7/a0-a4

	movem.l	(sp)+,d2-d7/a2-a6
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
* Loads the current colors into the current copperlist if changed.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0/a0-a1
*****************************************************************************

P0_Write_Palette_To_Copper:
	;Load flag may be > 1 if using double buffered CL or changing between CLs
	tst.w	CTRL_PALETTE_LOAD_FLAG(a5)
	beq	.exit
	subq.w	#1,CTRL_PALETTE_LOAD_FLAG(a5)

	lea	PAL_Current(pc),a0
	move.l	CL_Log1_Ptr(pc),a1

	;Normal colors
	lea	P0_CL_COL_OFFSET+2(a1),a1	

	REPT	BPL_BUF_PIC_5BPL_NUMCOLS
	move.w	(a0)+,(a1)
	addq.l	#4,a1			;next color
	ENDR
.exit:	
	rts


*****************************************************************************
* Loads the current colors into the current copperlist if changed.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0/a0-a1
*****************************************************************************

P1_Write_Palette_To_Copper:
	;Load flag may be > 1 if using double buffered CL or changing between CLs
	tst.w	CTRL_PALETTE_LOAD_FLAG(a5)
	beq	.exit
	subq.w	#1,CTRL_PALETTE_LOAD_FLAG(a5)

	lea	PAL_Current(pc),a0
	move.l	CL_Log1_Ptr(pc),a1

	;Normal colors
	lea	P1_CL_COL_PF1_OFFSET+2(a1),a1	

	;We only change the number of colours for the background picture

	REPT	BPL_BUF_PIC_3BPL_NUMCOLS
	move.w	(a0)+,(a1)
	addq.l	#4,a1			;next color
	ENDR
.exit:	
	rts


****************************************************************************
* Prepares a line of text, works out starting x value for centered text
* IN:		a6, custom
*		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d2/a0-a1
*****************************************************************************

Text_Get_Initial_X:
	moveq	#0,d2			;Initial width

	move.l	CTRL_TEXT_PTR(a5),a1
	move.b	(a1)+,d0		;Get first letter of line or null
	beq.s	.exit			;End of scroller
.letterloop:
	cmp.b	#10,d0			;linefeed?
	beq.s	.linefeed
	cmp.b	#32,d0			;Space?
	bne.s	.notspace
	addq.w	#FONT1_SPACE_WIDTH,d2
	bra.s	.nextletter
.notspace:
	;Work out the size of text
	lea	FAR_Font8px(pc),a0
	ext.w	d0
	move.b	(a0,d0.w),d0		;bob number (top d0 still clear)
	bmi.s	.nextletter		;is letter valid? ($ff is invalid, we assume >127 for bmi)

	lea	BOB_Font8px(pc),a0
	mulu	#LIB_BOBTABLE_SIZEOF,d0	;find bob entry for this letter
	lea	(a0,d0.w),a0
	add.w	LIB_BOBTABLE_WIDTH(a0),d2	;Increase X by width

.nextletter:
	move.b	(a1)+,d0
	cmp.b	#10,d0			;linefeed?
	beq.s	.linefeed

	IFNE FONT1_X_SPACING		;Additional spacing
	addq.w	#FONT1_X_SPACING,d2
	ENDC
	bra	.letterloop

.linefeed	

	;We have the width so work out the centered X start
	lsr.w	#1,d2			;W/2
	move.w	#BPL_BUF_FON_WIDTH/2,d1
	sub.w	d2,d1
	move.w	d1,d2
.exit:
	move.w	d2,CTRL_TEXT_X(a5)	;Store starting X
	rts


****************************************************************************
* Draws a line of text to the under-screen position.
* IN:		a6, custom
*		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d3/a0/a2/a3
*****************************************************************************

Text_Draw:
	;Time to draw text (line spacing counter)
	subq.w	#1,CTRL_SCROLL_COUNTER(a5)
	beq.s	.update
	rts
.update:
	bsr	Text_Get_Initial_X

	move.w	#VSCROLL_BUF_HEIGHT,CTRL_SCROLL_COUNTER(a5)	;reset

	;Clear the first draw area which is VSCROLL pos-font height
	move.w	CTRL_VSCROLL(a5),d0
	sub.w	#VSCROLL_BUF_HEIGHT,d0
	mulu	#BPL_BUF_FON_BYTEWIDTH*BPL_BUF_FON_NUMPLANES,d0
	move.l	BPL_Log1_Ptr(pc),a0
	add.l	d0,a0

	WAITBLIT_NASTY_A6
	move.l	#$01000000,bltcon0(a6)
	move.l	a0,bltdpth(a6)
	move.w	#0,bltdmod(a6)
	move.w	#((VSCROLL_BUF_HEIGHT*BPL_BUF_FON_NUMPLANES)*64)+BPL_BUF_FON_WORDWIDTH,bltsize(a6)


	move.l	CTRL_TEXT_PTR(a5),a4
	move.b	(a4)+,d2		;Get first letter of line or null or lf
	bne.s	.notnull		;End of scroller
	lea	Scroller_Text,a4
	bra.s	.copyline		;Copy blank line cleared aboive
.notnull:

	move.w	CTRL_TEXT_X(a5),d4	;Initial X
.letterloop:
	cmp.b	#10,d2			;linefeed?
	beq.s	.copyline
	cmp.b	#32,d2			;Space?
	bne.s	.notspace
	addq.w	#FONT1_SPACE_WIDTH,d4
	bra.s	.nextletter		;Done for this letter
.notspace:

	cmp.w	#FONT1_MAX_X,d4
	bgt.s	.nextletter

.drawletter:
	move.w	d4,d0			;Current x
	moveq	#0,d1			;Y is always 0
	move.l	a0,-(sp)		;Save screen ptr
	bsr	Text_Blit_Letter	;I:a0/a6, O:d0, T:d0-d3/a0/a2/a3
	move.l	(sp)+,a0
	add.w	d0,d4			;next X based on previous letter

.nextletter:
	move.b	(a4)+,d2
	bra.s	.letterloop

.copyline:
	;Also copy the text drawn to BPL_BUF_PIC_3BPL_HEIGHT+VSCROLL_BUF_HEIGHT as well
	lea	(BPL_BUF_PIC_3BPL_HEIGHT+VSCROLL_BUF_HEIGHT)*BPL_BUF_FON_BYTEWIDTH*BPL_BUF_FON_NUMPLANES(a0),a1
	WAITBLIT_NASTY_A6
	move.l	#$09f00000,bltcon0(a6)
	move.l	#-1,bltafwm(a6)	
	move.l	#0,bltamod(a6)		;A/DMOD
	move.l	a0,bltapth(a6)		;BOB data
	move.l	a1,bltdpth(a6)		;Screen
	move.w	#((VSCROLL_BUF_HEIGHT*BPL_BUF_FON_NUMPLANES)*64)+BPL_BUF_FON_WORDWIDTH,bltsize(a6)

	;save pointer
	move.l	a4,CTRL_TEXT_PTR(a5)

.exit:
	rts


****************************************************************************
* Draws a letter from font1 at given x,y using blitter copy.
* Note assumes BOB BPL is interleaved.
* This routine only plots at any x position. Slower than Aligned version.
* IN:		a6, custom
*		a0, screen
*		(a1, y premult)
*		d0.w, x
*		d1.w, y
*		d2.b, ascii value
* OUT:		
* TRASHED:	d0-d3/a0/a2/a3
*****************************************************************************

Text_Blit_Letter:

	lea	FAR_Font8px(pc),a2
	ext.w	d2			;clear top
	move.b	(a2,d2.w),d2		;bob number (top d2 still clear)
	bmi	.badletter		;is letter valid? $ff = not valid, assume >127 bad with bmi

	lea	BOB_Font8px(pc),a2
	mulu	#LIB_BOBTABLE_SIZEOF,d2	;find bob entry for this letter
	lea	(a2,d2.w),a2

	lea	BPL_Font8px,a3		;root bob bitplane data
	add.l	LIB_BOBTABLE_OFFSET(a2),a3	;offset to bob bpl

	;If premult table available, use it
	;add.w	d1,d1			;y value, access table in words
	;add.w	(a1,d1.w),a0		;add y value to screen adr
	;Removed as Y always 0
	;mulu	#BPL_BUF_FON_BYTEWIDTH*BPL_BUF_FON_NUMPLANES,d1
	;add.l	d1,a0

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
		move.w	LIB_BOBTABLE_WIDTHINWORDS(a2),d3
		addq.w	#1,d3		;extra word for shifting
		
		move.w	d3,d1
		add.w	#FONT1_HEIGHT*FONT1_NUMPLANES*64,d1	;bltsize

		add.w	d3,d3		;bytewidth
		moveq	#BPL_BUF_FON_BYTEWIDTH,d2	;ensure top of d2 is 0
		sub.w	d3,d2		;modulo
	ELSE
		move.w	#FONT1_BLTSIZE+1,d1		;same for all bobs (mono font)
		moveq	#BPL_BUF_FON_BYTEWIDTH-(FONT1_BLT_BYTEWIDTH+2),d2	;modulo (top of d2 is 0)
							;+1/+2 extra word for shifting,
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
	move.w	LIB_BOBTABLE_WIDTH(a2),d0	;Actual pixel width we used
	
	IFNE FONT1_X_SPACING		;Additional spacing
	addq.w	#FONT1_X_SPACING,d0
	ENDC

	rts

.badletter:
	moveq	#0,d0		;Moved on 0 px
	rts


****************************************************************************
* Changes the scroll position of the text. And draws new text if needed
* IN:		a6, custom
*		a5, Controller_Info
* OUT:		
* TRASHED:	None
*****************************************************************************

Text_Scroll:
	;Scroll text?
	subq.w	#1,CTRL_SCROLL_DELAY_COUNTER(a5)
	bpl.s	.updatecopper		;Even if skipping, because double buffered
	move.w	#1,CTRL_SCROLL_DELAY_COUNTER(a5)

	move.w	CTRL_VSCROLL(a5),d0

	addq.w	#1,d0
	cmp.w	#VSCROLL_MAX,d0
	blt.s	.vscrollok
	move.w	#VSCROLL_MIN,d0
.vscrollok:
	move.w	d0,CTRL_VSCROLL(a5)
	
	bsr	Text_Draw		;T:d0-d3/a0/a2/a3
	
.updatecopper:
	move.w	CTRL_VSCROLL(a5),d0
	move.l	CL_Log1_Ptr(pc),a0
	lea	P1_CL_BPL_PF2_OFFSET+2(a0),a0	;copper bpl pointer block	

	mulu	#BPL_BUF_FON_BYTEWIDTH*BPL_BUF_FON_NUMPLANES,d0
	add.l	BPL_Log1_Ptr(pc),d0

	moveq	#BPL_BUF_FON_BYTEWIDTH,d1
	moveq	#BPL_BUF_FON_NUMPLANES-1,d2
.l:	
	swap	d0			;Swap high & low words
	move.w	d0,(a0)			;High ptr
	swap	d0			;Swap high & low words
	move.w	d0,4(a0)		;Low ptr
	addq.l	#8,a0			;Next set of ptrs
	add.l	d1,d0			;Next bitplane
	dbf	d2,.l


	rts


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

; Font BOB structure and font ascii lookup, BOB and FAR in public
BOB_Font8px:
	INCLUDE "AssetsConverted/Greetz_Font_8x16x2.BOB_dcw.i"
FAR_Font8px:
	INCLUDE "AssetsConverted/Greetz_Font_8x16x2.FAR_dcb.i"

	EVEN
BPL_Font8px_Source:
	INCBIN "AssetsConverted/Greetz_Font_8x16x2.BPL.nrv2s"
	EVEN

; Need the decompressed font size to reserve room
BPL_FONT8PX_TOTALSIZE = 8*1024

*****************************************************************************

; # - star
; ~ - heart
; % - train1
; ^ - train2
; & - train3
; Keep to around 37 chars 

	EVEN
Scroller_Text:
	dc.b	10,"You have been watching...",10
	dc.b	10
	dc.b	"SAX OFFENDER",10
	dc.b	10
	dc.b	"Released in February 2020",10
	dc.b	10
	dc.b	"Dedicated to Anthony William Bennett",10
	dc.b	"(Antiriad's Dad)",10
	dc.b	10
	dc.b	10
	dc.b	10
	dc.b	10
	dc.b	"CREDITS",10
	dc.b	"Graphics - Optic",10
	dc.b	"Music - Tecon",10
	dc.b	"Code - Antiriad",10


	;OPTIC

	dc.b	10,10,10,"Message from Optic:",10
	dc.b	10
	dc.b	"%&^^^&^^^&^^^",10
	dc.b	"It's been a while since I've written",10
	dc.b	"one of these, so bear with me whilst",10
	dc.b	"I deal with the ringrust.  All being",10
	dc.b	"well, this little production (relative",10
	dc.b	"term - timewise), sees the light of day",10
	dc.b	"at GERP 2020.  The place to release",10
	dc.b	"Amiga stuff these days.",10
	dc.b	10
	dc.b	10 
	dc.b	"It's been a few months of ups and",10
	dc.b	"downs, but by far one of the more",10
	dc.b	"enjoyable productions I've worked on.",10
	dc.b	10
	dc.b	10
	dc.b	"Hats off to my man ~ Antiriad ~ for",10
	dc.b	"stepping up and dealing with a couple",10
	dc.b	"of less than reliable and demanding",10
	dc.b	"Norwegians.  Not sure we would have",10
	dc.b	"gotten the same resounding yes, if",10
	dc.b	" he'd known at the start... #",10
	dc.b	10
	dc.b	10
	dc.b	"One little addendum to the credits,",10
	dc.b	"the font for the PJZ logo was very",10
	dc.b	"much inspired by the stylings of our",10
	dc.b	"long lost (to the scene) brother from",10
	dc.b	"another mother, DVize ~. So whether",10
	dc.b	"you like it or not, Mark, I am",10
	dc.b	"dragging you kicking and screaming",10
	dc.b	"back to the demoscene.",10
	dc.b	10
	dc.b	10
	dc.b	"Anyway, hope you're all enjoying the",10
	dc.b	"party! # Mygg and Corial # told me",10
	dc.b	"beer was on them, for anyone wondering.",10
	dc.b	10
	dc.b	"- Optic out!",10
	dc.b	"%&^^^&^^^&^^^",10

	;TECON

	dc.b	10,10,10,"Message from Tecon:",10
	dc.b	10
	dc.b 	"We are 20 seconds past 2 minutes to",10
	dc.b 	"midnight - doomsday clock tells us.",10
	dc.b	"Now plenty of time to sit back,",10
	dc.b	 "relax, and enjoy the show!",10
	dc.b	"Have some popcorn while the USA makes",10
	dc.b	"another war. Or grab a beer and watch",10
	dc.b	"the swedes fail at sports.",10
	dc.b	10
	dc.b	"Should the tides be turning, it only",10
	dc.b	"means there is still hope..",10
	dc.b	"A sign that you might live to see",10
	dc.b	"another planet.jazz demo.",10
	dc.b	10
	dc.b	10
	dc.b	"~ Thanks ~",10
	dc.b	"Antiriad for coming back in style!",10
	dc.b	"Optic for the wicked song request!",10
	dc.b	"I dubbed this tune Terminal Massage.",10
	dc.b	10
	dc.b	"# Note #",10
	dc.b	"PreTracker replayer has a quirky flaw",10
	dc.b	"playing of a couple of the snares",10
	dc.b	"incorrectly on a500 (a few times they",10
	dc.b	"occur 'weak' or with louder noise)",10
	dc.b	10
	dc.b	10
	dc.b	"Special CPU love and girl power to",10
	dc.b	"Shinobi # Lunix # Olle",10
	dc.b	"Frenzy/Resistance # Gerp farmers",10
	dc.b	"Datastormers # data-yard peeps!",10
	dc.b	"DJ Zyron for his amazing mixes",10
	dc.b	"And all you fabulous sceners doing",10
	dc.b	"your thing on c64/amiga et al",10
	dc.b	10
	dc.b	"%&& Peace out! ",10	
	

	;ANTIRIAD

	dc.b	10,10,10,"Message from Antiriad:",10
	dc.b	10
	dc.b	"Thanks to Optic and Tecon for being so",10
	dc.b	"cool to work with. I've got another",10
	dc.b	"couple of friends that I've yet to meet!",10
	dc.b	"Optic, your picture of my dad is stunning",10
	dc.b	"and brings a tear to my eye. ~"
	dc.b	10
	dc.b	10
	dc.b	10
	dc.b	"Last year Tecon asked if I was",10
	dc.b	"interested in doing a short intro.",10
	dc.b	"I was keen to learn how to code a",10
	dc.b	"couple of certain retro effects and",10
	dc.b	"I had just done a nice dot flag which",10
	dc.b	"was going to be the only effect. But",10
	dc.b	"things quickly got out of hand and",10
	dc.b	"this quick intro took MONTHS!!!",10
	dc.b	10
	dc.b	10
	dc.b	10
	dc.b	"Huge thanks to the following people.",10
	dc.b	"This prod would not be possible without",10
	dc.b	"your tools and scripts!",10
	dc.b	10
	dc.b	10
	dc.b	"ross - NRV2S Packer",10
	dc.b	"Hannibal/Lemon. - KingCon",10
	dc.b	"Pink/Abyss - PreTracker",10
	dc.b	"bifat/TEK - Cranker",10
	dc.b	"Soundy/The Deadliners - Gradient Master",10
	dc.b	"Axis/Oxyron - Planet Rocklobster source",10
	dc.b	"Frank Wille - VASM 68k",10
	dc.b	"prb28 - ASM extension for VSCode",10
	dc.b	"Toni Wilen - WinUAE",10
	dc.b	10
	dc.b	10
	dc.b	"Also a big thanks to all the people at",10
	dc.b	"English Amiga Board who take the time",10
	dc.b	"to answer my daft questions ~",10
	dc.b	10
	dc.b	10
	dc.b	"Source code will be available soon at:",10
	dc.b	"github.com/jonathanbennett73",10
	dc.b	10
	dc.b	10
	dc.b	10
	dc.b	10
	dc.b	10
	dc.b	10
	dc.b	10
	dc.b	10
	dc.b	"~ Amiga OCS Forever ~"
	dc.b	10,10,10,10,10,10,10,10,10,10,10
	dc.b	0
	EVEN


*****************************************************************************

	EVEN
BPL_Pic_3Bpl_Source:
	INCBIN "AssetsConverted/NeonDays_320x256x3_inter.BPL.nrv2s"
	EVEN

PAL_Pic_3Bpl_Source:
	INCLUDE "AssetsConverted/NeonDays_320x256x3_inter.PAL_dcw.i"
	dcb.w	24,$000			;Our palette code expects 32 colors

	EVEN
BPL_Pic_5Bpl_Source:
	INCBIN "AssetsConverted/NeonDays_320x256x5_inter.BPL.nrv2s"
	EVEN

PAL_Pic_5Bpl_Source:
	INCLUDE "AssetsConverted/NeonDays_320x256x5_inter.PAL_dcw.i"

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
CTRL_TEXT_PTR:				rs.l 1
CTRL_TEXT_X:				rs.w 1		;Current X value of next letter to print
CTRL_VSCROLL:				rs.w 1		;Upwards scroll. 0=no scroll.	
CTRL_SCROLL_COUNTER:			rs.w 1
CTRL_SCROLL_DELAY_COUNTER:		rs.w 1		;Only scroll every other frame
CTRL_SIZE:				rs.w 0

	EVEN
Controller_Info:
	dcb.b	CTRL_SIZE,0			;Init all to zero by default
	EVEN


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
	;FX_PAUSE	10

	FX_PALETTE	4,PAL_Pic_5Bpl_Source
	FX_PAUSE	50*5
	;FX_PALETTE	1,PAL_AllWhite
	
	FX_NEXT_PHASE
	FX_PALETTE	3,PAL_Pic_3Bpl_Source


	FX_PAUSE	32767

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

; Straight 32 color bitmap
P0_CL_Phys:
	;Because this routine completes instantly on a4000/040 it can mess up
	;if trying to use a bottom of frame irq.
	IFNE FW_IRQ_TYPE_COPPER
		CMOVE	intreq,INTF_SETCLR|INTF_COPER	;Trigger IRQ, nothing visible
	ENDC

	;CMOVE	fmode,MEMORYFETCHMODE	;Chip Ram fetch mode (0=OCS)
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	CMOVE 	bplcon0,$5200		;5BPL single playfield
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0040		;PF1 on bottom
	CMOVE 	bpl1mod,P0_BPL_BPL1MOD
	CMOVE 	bpl2mod,P0_BPL_BPL2MOD

	;CMOVE	dmacon,DMAF_SETCLR|DMAF_SPRITE	;Enable sprites
	CMOVE	dmacon,DMAF_SPRITE	;Disable sprites

P0_CL_Bpl:				;Overlay
	CMOVE	bpl1pth,$0
	CMOVE	bpl1ptl,$0
	CMOVE	bpl2pth,$0
	CMOVE	bpl2ptl,$0
	CMOVE	bpl3pth,$0
	CMOVE	bpl3ptl,$0
	CMOVE	bpl4pth,$0
	CMOVE	bpl4ptl,$0
	CMOVE	bpl5pth,$0
	CMOVE	bpl5ptl,$0

P0_CL_Cols:
	CMOVE	tmpcolor00,$000
	CMOVE	color01,$f00
	CMOVE	color02,$0f0
	CMOVE	color03,$00f
	CMOVE	color04,$ff0
	CMOVE	color05,$0ff
	CMOVE	color06,$f0f
	CMOVE	color07,$fff
	CMOVE	color08,$000
	CMOVE	color09,$000
	CMOVE	color10,$eff
	CMOVE	color11,$bbd
	CMOVE	color12,$0f0
	CMOVE	color13,$f00
	CMOVE	color14,$0f0
	CMOVE	color15,$f00
	CMOVE	color16,$f00
	CMOVE	color17,$0f0
	CMOVE	color18,$00f
	CMOVE	color19,$ff0
	CMOVE	color20,$0ff
	CMOVE	color21,$f0f
	CMOVE	color22,$fff
	CMOVE	color23,$000
	CMOVE	color24,$000
	CMOVE	color25,$eff
	CMOVE	color26,$bbd
	CMOVE	color27,$0f0
	CMOVE	color28,$f00
	CMOVE	color29,$0f0
	CMOVE	color30,$f00
	CMOVE	color31,$f00

	; Trigger copper interrupt
	;IFNE FW_IRQ_TYPE_COPPER
	;	IF SCANLINE_EOF>255
	;		CWAIT	255,$e1
	;	ENDIF
	;	CWAIT	(SCANLINE_EOF-1)&$ff,$df
	;	CMOVE	intreq,INTF_SETCLR|INTF_COPER
	;ENDC

	COPPEREND
P0_CL_End:

P0_CL_BPL_OFFSET = (P0_CL_Bpl-P0_CL_Phys)
P0_CL_COL_OFFSET = (P0_CL_Cols-P0_CL_Phys)
P0_CL_SIZE = P0_CL_End-P0_CL_Phys


*****************************************************************************

; Scroller + bitmap
P1_CL_Phys:
	;Because this routine completes instantly on a4000/040 it can mess up
	;if trying to use a bottom of frame irq.
	IFNE FW_IRQ_TYPE_COPPER
		CMOVE	intreq,INTF_SETCLR|INTF_COPER	;Trigger IRQ, nothing visible
	ENDC

	;CMOVE	fmode,MEMORYFETCHMODE	;Chip Ram fetch mode (0=OCS)
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	CMOVE 	bplcon0,$5600		;5BPL dual playfield
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0040		;PF1 on bottom
	CMOVE 	bpl1mod,P1_BPL_BPL1MOD
	CMOVE 	bpl2mod,P1_BPL_BPL2MOD

	;CMOVE	dmacon,DMAF_SETCLR|DMAF_SPRITE	;Enable sprites
	CMOVE	dmacon,DMAF_SPRITE	;Disable sprites

P1_CL_Bpl_PF1:				;Overlay
	CMOVE	bpl1pth,$0
	CMOVE	bpl1ptl,$0
	CMOVE	bpl3pth,$0
	CMOVE	bpl3ptl,$0
	CMOVE	bpl5pth,$0
	CMOVE	bpl5ptl,$0

P1_CL_Bpl_PF2:				;Scroller
	CMOVE	bpl2pth,$0
	CMOVE	bpl2ptl,$0
	CMOVE	bpl4pth,$0
	CMOVE	bpl4ptl,$0

P1_CL_Cols_PF1:
	CMOVE	tmpcolor00,$000
	CMOVE	color01,$f00		;PF1 (overlay)
	CMOVE	color02,$0f0
	CMOVE	color03,$00f
	CMOVE	color04,$ff0
	CMOVE	color05,$0ff
	CMOVE	color06,$f0f
	CMOVE	color07,$fff

P1_CL_Cols_PF2:
	CMOVE	color08,$000		;PF2 (font)
	CMOVE	color09,$000
	CMOVE	color10,$eff
	CMOVE	color11,$bbd
;	CMOVE	color12,$0f0
;	CMOVE	color13,$f00
;	CMOVE	color14,$0f0
;	CMOVE	color15,$f00

	; Trigger copper interrupt
	;IFNE FW_IRQ_TYPE_COPPER
	;	IF SCANLINE_EOF>255
	;		CWAIT	255,$e1
	;	ENDIF
	;	CWAIT	(SCANLINE_EOF-1)&$ff,$df
	;	CMOVE	intreq,INTF_SETCLR|INTF_COPER
	;ENDC

	COPPEREND
P1_CL_End:

P1_CL_BPL_PF1_OFFSET = (P1_CL_Bpl_PF1-P1_CL_Phys)
P1_CL_BPL_PF2_OFFSET = (P1_CL_Bpl_PF2-P1_CL_Phys)
P1_CL_COL_PF1_OFFSET = (P1_CL_Cols_PF1-P1_CL_Phys)
P1_CL_COL_PF2_OFFSET = (P1_CL_Cols_PF2-P1_CL_Phys)
P1_CL_SIZE = P1_CL_End-P1_CL_Phys


*****************************************************************************
*****************************************************************************

; Map screens to shared chipmem buffers
CUR_CHIP_BUF set FW_Chip_Buffer_1

BPL_Scroller		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_FON_TOTALSIZE		;50KB

;BPL_Log1		=	CUR_CHIP_BUF
;CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_PIC_3BPL_TOTALSIZE

BPL_Pic_3bpl		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_PIC_3BPL_TOTALSIZE	;30KB

BPL_Pic_5bpl		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_PIC_5BPL_TOTALSIZE	;50KB

BPL_Font8px		=	CUR_CHIP_BUF			;6KB
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_FONT8PX_TOTALSIZE

P0_CL_Log1		=	CUR_CHIP_BUF			;1KB
CUR_CHIP_BUF set CUR_CHIP_BUF+P0_CL_SIZE

P1_CL_Log1		=	CUR_CHIP_BUF			;1KB
CUR_CHIP_BUF set CUR_CHIP_BUF+P1_CL_SIZE

;Total: ~140KB

*****************************************************************************
