
*****************************************************************************

; Name			: IntroSharedData.i
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com)
; Description		: Resources that need to be shared between parts.
;			  generally music files, screen buffers, and fonts.
; Date last edited	: 24/05/2019

; CPU Required		: MC68000 or better
; ChipSet Required	: OCS or better
				
*****************************************************************************

RasterTest set 0	;color00 timing bar, 0=off, 1=overall, 2=show blitwaits

	IFEQ	RasterTest
tmpcolor00 set color00
	ELSE
tmpcolor00 set $1fe	;dummy
	ENDC

*****************************************************************************

	INCLUDE "hardware/custom.i"
	INCLUDE "hardware/intbits.i"	
	INCLUDE	"Framework/customextra.i"
	INCLUDE "Framework/CustomMacros.i"

	INCLUDE "IntroConfig.i"


; Additional External symbols
	xref	FW_ClearBuffer_CPU
	xref	FW_CopyBuffer_CPU
	xref	FW_InitCopperBplPtrs
	xref	FW_SetCopperAndLev3Irq_A6
	xref	FW_SetLev3Irq
	xref	FW_VBlankProxy

	xref	LIB_RGB12_Interpolate_Fast
	xref	LIB_RGB12_Interpolate_Fast_Palette
	xref	LIB_NRV2S_Depack

	xref	FW_Chip_Buffer_1
	xref	FW_Public_Buffer_1

*****************************************************************************
*****************************************************************************
*****************************************************************************

	SECTION	PreCalc_PublicCode,CODE	;Code section in Public memory

*****************************************************************************


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

BAR_HEIGHT 		= 	60
BAR_MAX_Y		= 	(256-1)-BAR_HEIGHT

BAR_NUM_LEVELS		= 	31		;-15,15

BAR_NUM_PAL		= 	2		;number of palettes to cycles
BAR_NUM_PAL_MASK	= 	BAR_NUM_PAL-1	;and mask
BAR_PAL_SIZE_BYTES 	= 	BAR_HEIGHT*2

BPL_BUF_SAX_WIDTH	=	320
BPL_BUF_SAX_BYTEWIDTH	=	BPL_BUF_SAX_WIDTH/8		;Bytes
BPL_BUF_SAX_WORDWIDTH	=	BPL_BUF_SAX_BYTEWIDTH/2	;Words
BPL_BUF_SAX_HEIGHT	=	256			
BPL_BUF_SAX_NUMPLANES	=	5			;5bpl/32 cols
BPL_BUF_SAX_NUMCOLS 	= 	(1<<BPL_BUF_SAX_NUMPLANES)
BPL_BUF_SAX_SIZE	=	BPL_BUF_SAX_BYTEWIDTH*BPL_BUF_SAX_HEIGHT
BPL_BUF_SAX_TOTALSIZE	=	BPL_BUF_SAX_SIZE*BPL_BUF_SAX_NUMPLANES

BPL_BUF_DED_WIDTH	=	320
BPL_BUF_DED_BYTEWIDTH	=	BPL_BUF_DED_WIDTH/8
BPL_BUF_DED_WORDWIDTH	=	BPL_BUF_DED_BYTEWIDTH/2
BPL_BUF_DED_HEIGHT	=	256			
BPL_BUF_DED_NUMPLANES	=	5			;4bpl/16 cols
BPL_BUF_DED_NUMCOLS 	= 	(1<<BPL_BUF_DED_NUMPLANES)
BPL_BUF_DED_SIZE	=	BPL_BUF_DED_BYTEWIDTH*BPL_BUF_DED_HEIGHT
BPL_BUF_DED_TOTALSIZE	=	BPL_BUF_DED_SIZE*BPL_BUF_DED_NUMPLANES


;SCR_BPL1MOD		=	BPL_BUF_SAX_BYTEWIDTH*(BPL_BUF_SAX_NUMPLANES-1)	;interleaved
SCR_BPL1MOD		=	0	;non interleaved for ease of switching 1bpl to 5bpl


*****************************************************************************

	xdef	PRC_Init
PRC_Init:
	movem.l	d2-d7/a2-a6,-(sp)

	lea	_custom,a6
	lea	Controller_Info(pc),a5

	;Depack dedication picture
	lea	BPL_Dedication_Source,a0
	lea	BPL_Dedication,a1
	jsr	LIB_NRV2S_Depack

	; Setup up CL ptrs and initial colours
	bsr	P0_CL_InitPtrs

	lea	PAL_Black32(pc),a0
	lea	PAL_Dedication_Source,a1
	lea	PAL_Current(pc),a2
	moveq	#BPL_BUF_DED_NUMCOLS,d0
	moveq	#0,d1
	jsr	LIB_RGB12_Interpolate_Fast_Palette
	bsr	P0_Write_Palette_To_Copper

	; Activate copper and irq next frame
	lea	P0_CL_Phys,a0
	lea	P0_Lev3Irq(pc),a1
	jsr	FW_SetCopperAndLev3Irq_A6


	; Setup next phase for after dedication

	;Start at max fade (dark bars)
	move.w	#BAR_NUM_LEVELS-1,CTRL_BAR_FADE_VAL(a5)

	; Create bars
	lea	.bardefinition(pc),a3
	lea	.bardefinitionend(pc),a4
.barinitloop:
	move.l	(a3)+,a0		;Bar ptr
	move.w	(a3)+,d0		;col
	move.w	(a3)+,d1		;pos
	move.w	(a3)+,d2		;speed
	bsr	Init_Bar		;I:d0-d2/a0, T:d0-d7/a1-a2
	cmpa.l	a3,a4
	bne.s	.barinitloop

	;depack main pictures 
	lea	BPL_SaxMan_Sil_Source,a0
	lea	BPL_SaxMan_Sil,a1
	jsr	LIB_NRV2S_Depack

	lea	BPL_SaxMan_Source(pc),a0
	lea	BPL_SaxMan,a1
	jsr	LIB_NRV2S_Depack

	movem.l	(sp)+,d2-d7/a2-a6
	
	rts

.bardefinition:	;barptr, color, pos, speed
	dc.l	BUF_Bar1
	dc.w	COLOR_NEON_PINK,0,1

	dc.l	BUF_Bar2
	dc.w	COLOR_NEON_YELLOW,64,1

	dc.l	BUF_Bar3
	dc.w	COLOR_NEON_PURPLE,BAR_MAX_Y-128,-1

	dc.l	BUF_Bar4
	dc.w	COLOR_NEON_GREEN,BAR_MAX_Y-64,-1

	dc.l	BUF_Bar5
	dc.w	COLOR_NEON_RED,128,1

	dc.l	BUF_Bar6
	dc.w	COLOR_NEON_ORANGE,192,1

.bardefinitionend:

*****************************************************************************

;Dedication fade in
P0_Lev3Irq:
	TIMERON	$111
	movem.l	d0-d7/a0-a6,-(sp)

	lea 	_custom,a6
	lea	Controller_Info(pc),a5

	; Update colours from black to actual color
	subq.w	#1,CTRL_PIC_FADE_COUNT(a5)
	bpl.s	.skipfade
	move.w	#2,CTRL_PIC_FADE_COUNT(a5)

	move.w	CTRL_PIC_FADE_VAL(a5),d1
	cmpi.w	#15,d1
	blt.s	.stepok
	bsr	P1_Init			;next phase
	bra.s	.skipfade		;nothing else to do
.stepok:
	addq.w	#1,d1
	move.w	d1,CTRL_PIC_FADE_VAL(a5)

	;If this is the final step (15) then next fade count should be 4 seconds
	cmpi.w	#15,d1
	bne.s	.notfinal
	move.w	#250,CTRL_PIC_FADE_COUNT(a5)
.notfinal:
	lea	PAL_Black32(pc),a0
	lea	PAL_Dedication_Source,a1
	lea	PAL_Current(pc),a2
	moveq	#BPL_BUF_DED_NUMCOLS,d0
	jsr	LIB_RGB12_Interpolate_Fast_Palette

.skipfade:
	bsr	P0_Write_Palette_To_Copper

.exit:
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
* Setup next phase.
* IN:		a5, Controller_Info
*		a6, _custom
* OUT:		
* TRASHED:	d0/a0-a1
*****************************************************************************

P1_Init:
	clr.w	CTRL_PIC_FADE_VAL(a5)

	; Set new lev3 irq, leave CL alone
	lea	P1_Lev3Irq(pc),a0
	jsr	FW_SetLev3Irq			;trashes a1

	
	rts

*****************************************************************************

;Dedication fade out
P1_Lev3Irq:
	TIMERON	$111
	movem.l	d0-d7/a0-a6,-(sp)

	lea 	_custom,a6
	lea	Controller_Info(pc),a5

	; Update colours from black to actual color
	subq.w	#1,CTRL_PIC_FADE_COUNT(a5)
	bpl.s	.skipfade
	move.w	#2,CTRL_PIC_FADE_COUNT(a5)

	;If we at final color then next phase init
	move.w	CTRL_PIC_FADE_VAL(a5),d1
	cmpi.w	#15,d1
	blt.s	.stepok
	bsr	P2_Init			;next phase
	bra.s	.skipfade		;nothing else to do
.stepok:
	addq.w	#1,d1
	move.w	d1,CTRL_PIC_FADE_VAL(a5)

	;If this is the final step (15) then next fade count should be 1 seconds
	cmpi.w	#15,d1
	bne.s	.notfinal
	move.w	#25,CTRL_PIC_FADE_COUNT(a5)	
.notfinal:
	lea	PAL_Dedication_Source,a0
	lea	PAL_Black32(pc),a1
	lea	PAL_Current(pc),a2
	moveq	#BPL_BUF_DED_NUMCOLS,d0
	jsr	LIB_RGB12_Interpolate_Fast_Palette

.skipfade:
	bsr	P0_Write_Palette_To_Copper

.exit:
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
* Setup next phase.
* IN:		a5, Controller_Info
*		a6, _custom
* OUT:		
* TRASHED:	d0/a0-a1
*****************************************************************************

P2_Init:
	bsr	P2_CL_InitPtrs		;setup cl pointers 

	clr.w	CTRL_PAUSE_COUNTER(a5)

	; Set new copper and irq for next phase
	lea	P2_CL_Phys,a0
	lea	P2_Lev3Irq(pc),a1
	jsr	FW_SetCopperAndLev3Irq_A6	;I:a0-a1/a6
	
	rts

*****************************************************************************

; Silouette and neons

P2_Lev3Irq:				;Blank template VERTB/COP interrupt
	TIMERON	$111
	movem.l	d0-d7/a0-a6,-(sp)

	lea 	_custom,a6
	lea	Controller_Info(pc),a5

	;Increase framecount (pause count)
	addq.w	#1,CTRL_PAUSE_COUNTER(a5)

	; Check if fully started, if not increase bar fade val
	tst.w	CTRL_BAR_FULLY_STARTED(a5)
	bne.s	.checkdone

	subq.w	#1,CTRL_BAR_FADE_COUNT(a5)	;Update fade?
	bpl.s	.checkdone
	move.w	#3,CTRL_BAR_FADE_COUNT(a5)
	subq.w	#1,CTRL_BAR_FADE_VAL(a5)
	bne.s	.checkdone
	move.w	#1,CTRL_BAR_FULLY_STARTED(a5)	;Fade is zero, fully started
	
.checkdone:
	; Check if all precal finished
	tst.w	CTRL_PRECALC_DONE(a5)
	beq.s	.dobars

	; Check that we've shown the bars for at least 10 seconds on fast amigas
	cmpi.w	#450,CTRL_PAUSE_COUNTER(a5)
	blt.s	.dobars	

	; Start exiting by fading down the colours
	subq.w	#1,CTRL_BAR_FADE_COUNT(a5)	;Update fade?
	bpl.s	.dobars
	move.w	#3,CTRL_BAR_FADE_COUNT(a5)
	addq.w	#1,CTRL_BAR_FADE_VAL(a5)
	cmpi.w	#BAR_NUM_LEVELS,CTRL_BAR_FADE_VAL(a5)
	blt.s	.dobars
	bsr	P3_Init		;Switch to next phase/copper/irq
	bra.s	.exit

.dobars:
	bsr	Clear_Bars		;I:a6, T:d0-d1/a0
	bsr	Animate_Bars		;I:a5, T:d0-d3/a0
	WAITBLIT_NASTY_A6		;Let bar clear finish

	bsr	Draw_Bars		;T:d0-d7/a0-a2
	bsr	ScreenBufferSwap

.exit:
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
* Setup next phase.
* IN:		a5, Controller_Info
*		a6, _custom
* OUT:		
* TRASHED:	d0/a0-a1
*****************************************************************************

P3_Init:
	bsr	P3_CL_InitPtrs		;setup cl pointers 

	clr.w	CTRL_PIC_FADE_VAL(a5)
	move.w	#25,CTRL_PIC_FADE_COUNT(a5)

	; Set new copper and irq for next phase
	lea	P3_CL_Phys,a0
	lea	P3_Lev3Irq(pc),a1
	jsr	FW_SetCopperAndLev3Irq_A6	;I:a0-a1/a6
	
	rts

*****************************************************************************

; Fade sax guy in
P3_Lev3Irq:
	TIMERON	$111
	movem.l	d0-d7/a0-a6,-(sp)

	lea 	_custom,a6
	lea	Controller_Info(pc),a5

	jsr	FW_VBlankProxy		;T:d0-d7/a0-a4

	; Update colours from black to actual color
	subq.w	#1,CTRL_PIC_FADE_COUNT(a5)
	bpl.s	.skipfade
	move.w	#1,CTRL_PIC_FADE_COUNT(a5)

	move.w	CTRL_PIC_FADE_VAL(a5),d1
	cmpi.w	#15,d1
	blt.s	.stepok
	bsr	P4_Init			;next phase
	bra.s	.exit			;nothing else to do
.stepok:
	addq.w	#1,d1
	move.w	d1,CTRL_PIC_FADE_VAL(a5)

	;If this is the final step (15) then next fade count should be 5 seconds
	cmpi.w	#15,d1
	bne.s	.notfinal
	move.w	#250,CTRL_PIC_FADE_COUNT(a5)
.notfinal:
	lea	PAL_Black32(pc),a0
	lea	PAL_SaxMan_Source,a1
	lea	PAL_Current(pc),a2
	moveq	#BPL_BUF_SAX_NUMCOLS,d0
	jsr	LIB_RGB12_Interpolate_Fast_Palette
.skipfade:

	bsr	P3_Write_Palette_To_Copper

.exit:
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
* Setup next phase.
* IN:		a5, Controller_Info
*		a6, _custom
* OUT:		
* TRASHED:	d0/a0-a1
*****************************************************************************

P4_Init:
	moveq	#0,d0
	move.w	d0,CTRL_PIC_FADE_VAL(a5)
	move.w	d0,CTRL_PIC_FADE_COUNT(a5)

	; Set new lev3 irq, leave CL alone
	lea	P4_Lev3Irq(pc),a0
	jsr	FW_SetLev3Irq			;trashes a1
	
	rts

*****************************************************************************

;Fading out
P4_Lev3Irq:
	TIMERON	$111
	movem.l	d0-d7/a0-a6,-(sp)

	lea 	_custom,a6
	lea	Controller_Info(pc),a5

	jsr	FW_VBlankProxy		;T:d0-d7/a0-a4

	; Update colours from black to actual color
	subq.w	#1,CTRL_PIC_FADE_COUNT(a5)
	bpl.s	.skipfade
	move.w	#1,CTRL_PIC_FADE_COUNT(a5)

	move.w	CTRL_PIC_FADE_VAL(a5),d1
	cmpi.w	#15,d1
	blt.s	.stepok
	move.w	#1,CTRL_FINISHED(a5)
	bra.s	.skipfade
.stepok:
	addq.w	#1,d1
	move.w	d1,CTRL_PIC_FADE_VAL(a5)

	;If this is the final step (15) then next fade count should be 2 frames
	cmpi.w	#15,d1
	bne.s	.notfinal
	move.w	#2,CTRL_PIC_FADE_COUNT(a5)
.notfinal:
	lea	PAL_SaxMan_Source,a0
	lea	PAL_Transition(pc),a1
	lea	PAL_Current(pc),a2
	moveq	#BPL_BUF_SAX_NUMCOLS,d0
	jsr	LIB_RGB12_Interpolate_Fast_Palette
.skipfade:

	bsr	P3_Write_Palette_To_Copper

.exit:
	;Reset interrupt
	IFNE FW_IRQ_TYPE_COPPER
		moveq	#INTF_COPER,d0
	ELSE
		moveq	#INTF_VERTB,d0
	ENDC
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)			;A4000 060 compat

	movem.l	(sp)+,d0-d7/a0-a6
	TIMEROFF
	rte



*****************************************************************************
* Writes current palette into CL.
* IN:		
* OUT:		
* TRASHED:	d0/a0-a1
*****************************************************************************

P0_Write_Palette_To_Copper:
	lea	PAL_Current(pc),a0
	move.l	CL_Log1_Ptr(pc),a1
	lea	P0_CL_COL_OFFSET+2(a1),a1	;Ptr to COLOR value 00
	move.w	#BPL_BUF_DED_NUMCOLS-1,d0
.loop:
	move.w	(a0)+,(a1)
	addq.l	#4,a1			;Skip to next color
	dbf	d0,.loop

	rts


*****************************************************************************
* Writes current palette into CL.
* IN:		
* OUT:		
* TRASHED:	d0/a0-a1
*****************************************************************************

P3_Write_Palette_To_Copper:
	lea	PAL_Current(pc),a0
	move.l	CL_Log1_Ptr(pc),a1
	lea	P3_CL_COL_OFFSET+2(a1),a1	;Ptr to COLOR value 00
	move.w	#BPL_BUF_SAX_NUMCOLS-1,d0
.loop:
	move.w	(a0)+,(a1)
	addq.l	#4,a1			;Skip to next color
	dbf	d0,.loop

	rts


*****************************************************************************
* Sets up the copper lists
* IN:		
* OUT:		
* TRASHED:
*****************************************************************************

P0_CL_InitPtrs:

	lea	P0_CL_Phys,a0		;copperlist root
	move.l	a0,CL_Phys_Ptr		;Update ptrs as some routines use
	move.l	a0,CL_Log1_Ptr

	lea	P0_CL_Bpl,a0		;copper bpl pointer block
	moveq	#BPL_BUF_DED_NUMPLANES,d0
	move.l	#BPL_Dedication,d1	;in d1 for InitCopperBplPtrs
	move.l 	#BPL_BUF_DED_SIZE,d2	;non-interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	rts


*****************************************************************************
* Sets up the copper lists
* IN:		
* OUT:		
* TRASHED:
*****************************************************************************

P2_CL_InitPtrs:

	;Setup items the same in front/back copper lists

	lea	P2_CL_Bpl,a0		;copper bpl pointer block
	moveq	#1,d0
	move.l	#BPL_SaxMan_Sil,d1	;in d1 for InitCopperBplPtrs
	move.l 	#BPL_BUF_SAX_SIZE,d2	;non-interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	; Copper list buffers - copy screen list into 2nd buffer for doublebuffering
	lea	P2_CL_Phys,a0		;source
	lea	P2_CL_Log1,a1		;dest
	move.l	a0,CL_Phys_Ptr		;save
	move.l	a1,CL_Log1_Ptr
	move.w	#(P2_CL_SIZE/2),d0	;size in words
	jsr	FW_CopyBuffer_CPU	;I:d0/a0-a1, T:d0-d7/a0-a2

	rts


*****************************************************************************
* Sets up the copper lists for phase 1
* IN:		
* OUT:		
* TRASHED:
*****************************************************************************

P3_CL_InitPtrs:

	lea	P3_CL_Phys,a0		;copperlist root
	move.l	a0,CL_Phys_Ptr		;Update ptrs as some routines use
	move.l	a0,CL_Log1_Ptr

	lea	P3_CL_Bpl,a0		;copper bpl pointer block
	moveq	#BPL_BUF_SAX_NUMPLANES,d0
	move.l	#BPL_SaxMan,d1	;in d1 for InitCopperBplPtrs
	move.l 	#BPL_BUF_SAX_SIZE,d2	;non-interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo


	rts

*****************************************************************************
* Swaps the copperlist, screen and clr/fill pointers and activates the CL
* for the next frame. 
* NOTE: Call before vblank so new copper takes effect next frame.
* IN:		
* OUT:		
* TRASHED:	d0-d1/a0
*****************************************************************************

ScreenBufferSwap:
	; Swap copper buffer details
	lea	CL_Phys_Ptr(pc),a0
	movem.l	(a0),d0-d1
	move.l	d1,(a0)+
	move.l	d0,(a0)

	; and activate next frame
	move.l 	d1,cop1lch(a6)		; Active NEXT frame

	rts


*****************************************************************************

CL_Phys_Ptr:		dc.l	0	;Copper ptr - physical
CL_Log1_Ptr:		dc.l	0	;Logical1

*****************************************************************************

	xdef	PRC_PreCalc_Done	;For access from IntroWrapper.s
	xdef	PRC_Finished

	RSRESET
CTRL_PAUSE_COUNTER:	rs.w 	1
CTRL_BAR_FULLY_STARTED:	rs.w	1
CTRL_BAR_FADE_VAL:	rs.w	1
CTRL_BAR_FADE_COUNT:	rs.w	1
CTRL_PIC_FADE_VAL:	rs.w	1
CTRL_PIC_FADE_COUNT:	rs.w	1
CTRL_PRECALC_DONE:	rs.w	1
CTRL_FINISHED:		rs.w	1
CTRL_SIZE:		rs.w	0

	EVEN
Controller_Info:
	dc.w	0			;CTRL_PAUSE_COUNTER
	dc.w	0			;CTRL_BAR_FULLY_STARTED
	dc.w	0			;CTRL_BAR_FADE_VAL
	dc.w	0			;CTRL_BAR_FADE_COUNT
	dc.w	0			;CTRL_PIC_FADE_VAL
	dc.w	0			;CTRL_PIC_FADE_COUNT
PRC_PreCalc_Done:			;xdef for outside access
	dc.w	0			;CTRL_PRECALC_DONE
PRC_Finished:				;xdef for outside access
	dc.w	0			;CTRL_FINISHED


*****************************************************************************
* Intialises a bar..
* IN:		a0, bar address
*		d0, colour
*		d1, pos
*		d2, vel
* OUT:		
* TRASHED:	d0-d7/a1-a2
*****************************************************************************

Init_Bar:
	move.w	d0,BAR_COL(a0)
	move.w	d1,BAR_POS(a0)
	move.w	d2,BAR_VEL(a0)
	move.w	#BAR_NUM_LEVELS-1,BAR_LEVEL(a0)	;darkest
	clr.w	BAR_COUNTER(a0)
	bra	Create_Bar_Palette	;I:a0, T:d0-d7/a1-a2
	;rts


*****************************************************************************
* Clears the bar at given position (rather than clearing whole list)
* IN:		a0, bar address
** OUT:		
* TRASHED:	d0-d2/a0
*****************************************************************************

Clear_Bars:
;	move.l	CL_Log1_Ptr(pc),a0
;	lea	P2_CL_BAR_OFFSET+CL_BAR_CMOVE_COL+2(a0),a0	;Ptr to first COLOR value
;	moveq	#CL_BAR_SIZEOF,d1
;	moveq	#0,d2
;	move.w	#BPL_BUF_SAX_HEIGHT-1,d0
;.loop:
;	move.w	d2,(a0)
;	add.w	d1,a0
;	dbf	d0,.loop

	;For blit clear we are just clearing a single word (the color00 value)
	;and then using bltdmod to skip to next line
	move.l	CL_Log1_Ptr(pc),a0
	lea	P2_CL_BAR_OFFSET+CL_BAR_CMOVE_COL+2(a0),a0	;Ptr to first COLOR value

	WAITBLIT_A6
	move.l	#$01000000,bltcon0(a6)
	move.w	#CL_BAR_SIZEOF-2,bltdmod(a6)
	move.l	a0,bltdpth(a6)
	move.w	#(BPL_BUF_SAX_HEIGHT*64)+1,bltsize(a6)	;single word per line clear

	rts


*****************************************************************************
* Draws a bar at given position.
* IN:
* OUT:		
* TRASHED:	d0-d7/a1-a2
*****************************************************************************

Draw_Bars:
	lea	Bar_List(pc),a3

	;First bar doesn't need to do any special color calcs, so just draw it
	move.l	(a3)+,a0		;Get the first bar

	; Get a pointer to the current palette
	move.w	BAR_POS(a0),d0
	move.l	CL_Log1_Ptr(pc),a1
	lea	P2_CL_BAR_OFFSET+CL_BAR_CMOVE_COL+2(a1),a1	;Ptr to 1st COLOR value
	mulu	#CL_BAR_SIZEOF,d0	;Find this position in CL
	add.l	d0,a1		

	lea	BAR_PAL(a0),a2		;source palette
	move.w	BAR_LEVEL(a0),d0	;level
	;cmpi.w	#BAR_NUM_LEVELS,d0	;fully black?
	;beq.s	.exit
	mulu	#BAR_PAL_SIZEOF,d0
	add.l	d0,a2			;Find palette for this level

	moveq	#BAR_HEIGHT-1,d5
.bar1loop:	
	move.w	(a2)+,(a1)		;Copy bar line colour
	lea	CL_BAR_SIZEOF(a1),a1	;next line
	dbf	d5,.bar1loop


	;draw the remaining bars (one less bar)
	moveq	#NUM_BARS-1-1,d7
.barloop:
	move.l	(a3)+,a0
	bsr	Draw_Bar		;T:d0-d5/a0-a2
	dbf	d7,.barloop
	rts


*****************************************************************************
* Draws a bar at given position.
* IN:		a0, bar address
* OUT:		
* TRASHED:	d0-d5/a1-a2
*****************************************************************************

Draw_Bar:
	; Get a pointer to the current palette
	move.w	BAR_POS(a0),d0
	move.l	CL_Log1_Ptr(pc),a1
	lea	P2_CL_BAR_OFFSET+CL_BAR_CMOVE_COL+2(a1),a1	;Ptr to 1st COLOR value
	mulu	#CL_BAR_SIZEOF,d0	;Find this position in CL
	add.l	d0,a1		

	lea	BAR_PAL(a0),a2		;source palette
	move.w	BAR_LEVEL(a0),d0	;level
	;cmpi.w	#BAR_NUM_LEVELS,d0	;fully black?
	;beq.s	.exit
	mulu	#BAR_PAL_SIZEOF,d0
	add.l	d0,a2			;Find palette for this level

	moveq	#BAR_HEIGHT-1,d5
.line:
	move.w	(a2)+,d0		;Bar colour
	beq.s	.nextloop		;No calc needed
	move.w	(a1),d1			;Screen colour
	bne.s	.notblack
	move.w	d0,(a1)			;Store new colour
	bra.s	.nextloop
.notblack:
	;red
	move.w	d0,d2
	move.w	d1,d3
	lsr.w	#8,d2
	lsr.w	#8,d3
	add.w	d2,d3
	cmp.w	#$f,d3
	ble.s	.rok
	move.w	#$f,d3
.rok:
	move.w	d3,d4			;final
	lsl.w	#8,d4

	;green
	move.w	d0,d2
	move.w	d1,d3
	andi.w	#$0f0,d2
	andi.w	#$0f0,d3
	lsr.w	#4,d2
	lsr.w	#4,d3
	add.w	d2,d3
	cmp.w	#$f,d3
	ble.s	.gok
	move.w	#$f,d3
.gok:
	lsl.w	#4,d3
	or.w	d3,d4			;add to final

	;blue
	move.w	d0,d2
	move.w	d1,d3
	andi.w	#$00f,d2
	andi.w	#$00f,d3
	add.w	d2,d3
	cmp.w	#$f,d3
	ble.s	.bok
	move.w	#$f,d3
.bok:
	or.w	d3,d4			;add to final

	move.w	d4,(a1)			;Store new colour
.nextloop:	
	lea	CL_BAR_SIZEOF(a1),a1	;next line
	dbf	d5,.line
.exit:
	rts


*****************************************************************************
* Updates the animation and level for all bars.
* IN:		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d3/a0
*****************************************************************************

Animate_Bars:
	moveq	#NUM_BARS-1,d3
	lea	Bar_List(pc),a1
.barloop:
	move.l	(a1)+,a0
	bsr	Animate_Bar		;I:a0/a5, T:d0-d2/a0
	dbf	d3,.barloop

	rts


*****************************************************************************
* Sets the bar level and moves position.
* IN:		a0, bar address
*		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d2/a0
*****************************************************************************

Animate_Bar:
	;Update position
	move.w	BAR_POS(a0),d0
	move.w	BAR_VEL(a0),d1
	add.w	d1,d0			;update pos
	cmpi.w	#BAR_MAX_Y,d0
	bls.s	.draw
.clip:
	;reverse velocity and add twice
	neg.w	d1
	add.w	d1,d0
	add.w	d1,d0
	move.w	d1,BAR_VEL(a0)
.draw:
	move.w	d0,BAR_POS(a0)	;save new pos

	;Animate new flashes
	;move.w	CTRL_BAR_FADE_VAL(a5),d0	;Fading
	;bne.s	.animate		;Just animate out

	;Generate random
	movem.l   .state(pc),d0-d1
	move.l	d0,d2
	lsl.l	#2,d0
	eor.l	d2,d0           ; T = A^(A<<2)
	move.l	d1,d2
	lsr.l	#3,d2
	eor.l	d1,d2           ; B^(B>>3)
	eor.l	d0,d2           ; B^(B>>3)^T
	lsr.l	#7,d0
	eor.l	d0,d2           ; B^(B>>3)^T^(T>>7)
	movem.l   d1-d2,.state	;random number in d2.w

	;andi.b	#$ff,d2
	cmp.b	#$f0,d2		;lower is visible more
	bls.s	.animate

	;Flicker bar to max current brightness
	move.w	CTRL_BAR_FADE_VAL(a5),BAR_LEVEL(a0)
	move.w	#BAR_SPEED,BAR_COUNTER(a0)
	rts

.animate:
	move.w	BAR_COUNTER(a0),d0
	bne.s	.dec

	move.w	#BAR_SPEED,BAR_COUNTER(a0)
	move.w	BAR_LEVEL(a0),d1
	cmpi.w	#BAR_NUM_LEVELS-1,d1
	bge.s	.alreadydark
	addq.w	#1,d1
	move.w	d1,BAR_LEVEL(a0)
.alreadydark:
	rts
.dec:
	subq.w	#1,d0			;update counter
	move.w	d0,BAR_COUNTER(a0)
	rts

.state:
	dc.l $9876fedc
	dc.l $abcd1234


*****************************************************************************
* Sets up the palette for a bar based on its brightness
* IN:		a0, bar address
* OUT:		
* TRASHED:	d0-d7/a1-a2
*****************************************************************************

Create_Bar_Palette:
	; Get a pointer to the current palette
	move.w	BAR_COL(a0),d0	;base colour
	lea	BAR_PAL(a0),a1	;initial palette

	moveq	#0,d7			;starting step, 0-14
	move.w	BAR_COL(a0),d5	;base colour
.palette:
	moveq	#BAR_HEIGHT-1,d6
	lea	PAL_Mapping(pc),a2
.loop
	move.w	d5,d0			;source col
	move.w	(a2)+,d2		;step
	add.w	d7,d2			;increase darkness by step
	bpl.s	.positive
	neg.w	d2			;ASSERT: d2<=15
	move.w	#$fff,d1			;dest col is white
	jsr	LIB_RGB12_Interpolate_Fast	;trashes d0-d4
	moveq	#0,d2
.positive:				;d0 is base colour or lightened base colour
	moveq	#0,d1			;dest col
	cmpi.w	#15,d2			;range of LIB_RGB12_Interpolate_Fast
	ble.s	.stepok
	moveq	#15,d2
.stepok:
	jsr	LIB_RGB12_Interpolate_Fast	;trashes d0-d4
	move.w	d0,(a1)+
	dbf	d6,.loop

	addq.w	#1,d7			;next step
	cmpi.w	#BAR_NUM_LEVELS,d7
	bne.s	.palette

	rts


*****************************************************************************
*****************************************************************************
*****************************************************************************

	;SECTION	IntroFramework_PublicData,DATA	;Public data
	EVEN



PAL_Current:
	dcb.w	32,$000	;Palette to be use for CL

PAL_Black32:
	dcb.w	32,$000			;All black palette for fades
PAL_White32:
	dcb.w	32,$fff			;ALl white palette for fades

PAL_Transition:
	dcb.w	32,$134	

*****************************************************************************

	;Number of entries must match BAR_HEIGHT!
PAL_Mapping:
	dc.w	14,14,14
	dc.w	13,13,13
	dc.w	12,12,12
	dc.w	11,11,11
	dc.w	10,10,10
	dc.w	9,9,9
	dc.w	8,8,8
	dc.w	7
	dc.w	7
	dc.w	6
	dc.w	6
	dc.w	0
	dc.w	-7
	dc.w	-13
	dc.w	-13
	dc.w	-13
	dc.w	-13
	dc.w	-13
	dc.w	-13
	dc.w	-7
	dc.w	0
	dc.w	6
	dc.w	6
	dc.w	7
	dc.w	7
	dc.w	8,8,8
	dc.w	9,9,9
	dc.w	10,10,10
	dc.w	11,11,11
	dc.w	12,12,12
	dc.w	13,13,13
	dc.w	14,14,14

	RSRESET
BAR_POS:		rs.w	1		;Pos 0-255 -Bar height
BAR_VEL:		rs.w	1		;Speed and direction
BAR_COL:		rs.w	1		;The base color
BAR_LEVEL:		rs.w	1		;Current level, 0-14, 0=full, 14=almost black
BAR_COUNTER		rs.w	1		
BAR_PAL:		rs.w	BAR_HEIGHT*BAR_NUM_LEVELS	;Current palette, starting with brightest
BAR_SIZEOF:		rs.w	0

BAR_PAL_SIZEOF = BAR_HEIGHT*2	;distance in bytes between each palette
BAR_SPEED = 4


BUF_Bar1:	ds.b	BAR_SIZEOF
BUF_Bar2:	ds.b	BAR_SIZEOF
BUF_Bar3:	ds.b	BAR_SIZEOF
BUF_Bar4:	ds.b	BAR_SIZEOF
BUF_Bar5:	ds.b	BAR_SIZEOF
BUF_Bar6:	ds.b	BAR_SIZEOF

Bar_List:
	dc.l	BUF_Bar1
	dc.l	BUF_Bar2
	dc.l	BUF_Bar3
	dc.l	BUF_Bar4
	dc.l	BUF_Bar5
	dc.l	BUF_Bar6
Bar_List_End
NUM_BARS = (Bar_List_End-Bar_List)/4


*****************************************************************************

	EVEN
BPL_SaxMan_Source:
	INCBIN "AssetsConverted/Precalc_Logo_320x256x5.BPL.nrv2s"
	EVEN

PAL_SaxMan_Source:
	INCLUDE "AssetsConverted/Precalc_Logo_320x256x5.PAL_dcw.i"

	EVEN
BPL_SaxMan_Sil_Source:
	INCBIN "AssetsConverted/Precalc_Logo_Sil_320x256x1.BPL.nrv2s"
	EVEN

BPL_Dedication_Source:
	INCBIN "AssetsConverted/Precalc_Dedication_320x256x5.BPL.nrv2s"
	EVEN

PAL_Dedication_Source:
	INCLUDE "AssetsConverted/Precalc_Dedication_320x256x5.PAL_dcw.i"


*****************************************************************************
*****************************************************************************
*****************************************************************************

	SECTION	IntroFramework_ChipData_Copper,DATA_C	;Chip Data Section for gfx/music

*****************************************************************************

;Dedication CL
; This CL is not double buffered
P0_CL_Phys:
	;Because this routine completes instantly on a4000/040 it can mess up
	;if trying to use a bottom of frame irq.
	IFNE FW_IRQ_TYPE_COPPER
		CMOVE	intreq,INTF_SETCLR|INTF_COPER	;Trigger IRQ, nothing visible
	ENDC

	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	CMOVE 	bplcon0,$5200		;5 bpl
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0000
	CMOVE 	bpl1mod,SCR_BPL1MOD
	CMOVE 	bpl2mod,SCR_BPL1MOD

	;Wait until just before vblank ends so have time to update copper
	;but early enough to set background colors properly
	CWAIT	25-1,$7
	;CWAIT	DIW_V-2,$7		;Additional time for altering Copperlist

P0_CL_Bpl:			;Bitplane pointers playfield 1
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
	CMOVE	tmpcolor00,$0
a set color01
	REPT	BPL_BUF_DED_NUMCOLS-1	
	CMOVE	a,$000
a set a+2
	ENDR

	; Trigger copper interrupt at end of frame
	;IFNE FW_IRQ_TYPE_COPPER
	;	CWAIT	255,$df
	;	CWAIT	(300&$ff),$7
	;	CMOVE	intreq,INTF_SETCLR|INTF_COPER
	;ENDC

	COPPEREND
P0_CL_End:

P0_CL_BPL_OFFSET 		= (P0_CL_Bpl-P0_CL_Phys)
P0_CL_COL_OFFSET 		= (P0_CL_Cols-P0_CL_Phys)
P0_CL_SIZE 			= (P0_CL_End-P0_CL_Phys)


*****************************************************************************

;Silouette CL
; This CL is doublebuffered for smooth bar animiation
P2_CL_Phys:
	;Because this routine completes instantly on a4000/040 it can mess up
	;if trying to use a bottom of frame irq.
	IFNE FW_IRQ_TYPE_COPPER
		CMOVE	intreq,INTF_SETCLR|INTF_COPER	;Trigger IRQ, nothing visible
	ENDC

	;Wait until just before vblank ends so have time to update copper
	;but early enough to set background colors properly
	CWAIT	25-1,$7
	;CWAIT	DIW_V-2,$7		;Additional time for altering Copperlist
	
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	CMOVE 	bplcon0,$1200		;1 bpl
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0000
	CMOVE 	bpl1mod,SCR_BPL1MOD
	CMOVE 	bpl2mod,SCR_BPL1MOD

P2_CL_Bpl:			;Bitplane pointers playfield 1
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

P2_CL_Cols:
	CMOVE	tmpcolor00,$0
a set color01
	REPT	31	
	CMOVE	a,$000
a set a+2
	ENDR


P2_CL_Bars:
a set DIW_V
	REPT	BPL_BUF_SAX_HEIGHT
	CWAIT	a,$7			;0,2
	CMOVE	tmpcolor00,$0		;4,6
	CWAIT	a,$df			;8,10
a set a+1
	ENDR

	;background back to $000 for wrap around
	CWAIT	a,$7
	CMOVE	tmpcolor00,$0

	; Trigger copper interrupt at end of frame
	;IFNE FW_IRQ_TYPE_COPPER
	;	;CWAIT	255,$e1
	;	;CWAIT	(303&$ff),$7
	;	CMOVE	intreq,INTF_SETCLR|INTF_COPER
	;ENDC

	COPPEREND
P2_CL_End:

P2_CL_BPL_OFFSET 		= (P2_CL_Bpl-P2_CL_Phys)
P2_CL_COL_OFFSET 		= (P2_CL_Cols-P2_CL_Phys)
P2_CL_BAR_OFFSET 		= (P2_CL_Bars-P2_CL_Phys)
P2_CL_SIZE 			= (P2_CL_End-P2_CL_Phys)

;This is the repeated structure in the CL for drawing the bars
	RSRESET
CL_BAR_CWAIT_LINESTART:	rs.w	2
CL_BAR_CMOVE_COL:	rs.w	2
CL_BAR_CWAIT_LINEEND:	rs.w	2
CL_BAR_SIZEOF:		rs.w	0


*****************************************************************************

;Sax-man CL
; This CL is not double buffered
P3_CL_Phys:
	;Because this routine completes instantly on a4000/040 it can mess up
	;if trying to use a bottom of frame irq.
	IFNE FW_IRQ_TYPE_COPPER
		CMOVE	intreq,INTF_SETCLR|INTF_COPER	;Trigger IRQ, nothing visible
	ENDC

	;Wait until just before vblank ends so have time to update copper
	;but early enough to set background colors properly
	CWAIT	25-1,$7
	;CWAIT	DIW_V-2,$7		;Additional time for altering Copperlist
	
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	CMOVE 	bplcon0,$5200		;5 bpl
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0000
	CMOVE 	bpl1mod,SCR_BPL1MOD
	CMOVE 	bpl2mod,SCR_BPL1MOD

P3_CL_Bpl:			;Bitplane pointers playfield 1
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

	CWAIT	DIW_V,$7
P3_CL_Cols:
	CMOVE	tmpcolor00,$0
a set color01
	REPT	31	
	CMOVE	a,$000
a set a+2
	ENDR

	CWAIT	255,$df
	CWAIT	(DIW_V+DIW_HEIGHT)&$ff,$7
	CMOVE	tmpcolor00,$000		;Black out screen for wrap around


	; Trigger copper interrupt at end of frame
	;IFNE FW_IRQ_TYPE_COPPER
		;CWAIT	255,$e1
		;CWAIT	(303&$ff),$7
	;	CMOVE	intreq,INTF_SETCLR|INTF_COPER
	;ENDC

	COPPEREND
P3_CL_End:

P3_CL_BPL_OFFSET 		= (P3_CL_Bpl-P3_CL_Phys)
P3_CL_COL_OFFSET 		= (P3_CL_Cols-P3_CL_Phys)
P3_CL_SIZE 			= (P3_CL_End-P3_CL_Phys)


*****************************************************************************
*****************************************************************************
*****************************************************************************

; Map screens to shared chipmem buffers
CUR_CHIP_BUF set FW_Chip_Buffer_1

BPL_Dedication		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_DED_TOTALSIZE	;50KB

BPL_SaxMan_Sil		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_SAX_SIZE		;10KB

BPL_SaxMan		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_SAX_TOTALSIZE	;50KB

P2_CL_Log1		=	CUR_CHIP_BUF		;4KB
CUR_CHIP_BUF set CUR_CHIP_BUF+P2_CL_SIZE

;Total: ~114KB

*****************************************************************************
