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

	INCLUDE "IntroConfig.i"
	INCLUDE	"../Framework/customextra.i"
	INCLUDE "../Framework/CustomMacros.i"
	INCLUDE "../Framework/IntroLibrary.i"

; Additional external symbols 
	xref	FW_CheckUserQuitSignal_A6
	xref	FW_ClearBuffer_BlitCPU_A6
	xref	FW_ClearBuffer_CPU
	xref	FW_CopyBuffer_CPU
	xref	FW_CPUClearBuffer
	xref	FW_GetFrame
	xref	FW_InitCopperBplPtrs
	xref	FW_InitCopperColsFromPalette
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
	xref	BPL_Zoom_Pattern


*****************************************************************************

	SECTION	Pic1_PublicCode,CODE	;Code section in Public memory

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

SCANLINE_EOF		=	P0_DIW_V+P0_DIW_HEIGHT	; Safe to starting clearing after this scanline

*****************************************************************************

*** Screen Definitions ***

;The visible screen area and back buffer
BPL_BUF_WIDTH		=	320
BPL_BUF_BYTEWIDTH	=	BPL_BUF_WIDTH/8		;Bytes
BPL_BUF_WORDWIDTH	=	BPL_BUF_BYTEWIDTH/2	;Words
BPL_BUF_HEIGHT		=	256			;Rule of thirds
BPL_BUF_NUMPLANES	=	5			;3bpl/8 cols
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

*****************************************************************************

PAL_NUMCOLS_MAIN	= BPL_BUF_NUMCOLS		; number of main colour entries in our palettes
PAL_NUMCOLS_DARK	= BPL_BUF_NUMCOLS		; number of dark/refl cols
PAL_NUMCOLS_ALL		= (PAL_NUMCOLS_MAIN+PAL_NUMCOLS_DARK)

*****************************************************************************

;Zoom pattern is 352 width so have to alter offsets to center in our 334x256 window
ZOOM_PATTERN_X_BYTEOFFSET = 2	;352 vs 320 = 2 extra bytes
ZOOM_PATTERN_Y_BYTEOFFSET = 6	;352 vs 256 high = 6 extra bytes to work for a 256 high screen

ZOOM_BPLCON0 = $5200	;5bpl screen


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

	xdef	PIC1_Start
PIC1_Start:
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
	xdef	PIC1_PreCalc_IntroStart
	ENDC
	
SubPartPreCalc_IntroStart:	
PIC1_PreCalc_IntroStart:
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

	; Clear all screen buffers (previous routine must have blanked screen/colors)
	bsr	Clear_ScreenBuffers_CPU

	; Load default palette
	lea	PAL_AllBlack(pc),a0
	moveq	#0,d0
	bsr	Controller_FX_Palette	;I:d0/a0/a5, T:d0/a0-a1

	;Depack picture
	lea	BPL_Pic_Source(pc),a0
	lea	BPL_Phys,a1
	jsr	LIB_NRV2S_Depack	;I:a0-a1, T:d0-d1/a0-a1	

	; Set default zoom
	moveq	#0,d0
	move.w	#0,d1
	bsr	Controller_FX_Zoom	;I:a5,d0,d1

	;Setup phys/logical bpl and copperlist ptrs and load palette
	bsr	P0_CL_InitPtrs		;I:a6, T:d0-d2/a0-a1
	bsr	Write_Palette_To_Copper	;I:a5, T:d0/a0-a2

	;Swap buffers and load copper for next frame
	bsr	P0_ScreenBufferSwap	;T:d0-d1/a0

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

.exit
	movem.l	(sp)+,d2-d7/a2-a4
	rts


*****************************************************************************

P0_Lev3Irq:
	TIMERON
	movem.l	d0-d7/a0-a6,-(sp)

	lea	_custom,a6
	lea	Controller_Info(pc),a5

	jsr	FW_VBlankProxy		;T:d0-d7/a0-a4

	; Read new script lines and perform
	addq.w	#1,CTRL_FRAME_COUNT(a5)	;Update local frame count
	bsr	Controller_ReadCommands	;Read new commands
	bsr	Controller_Perform	;Do any ongoing time-based effects and update angles

	bsr	Do_Copper_Zoom

	;Load current palette into CL if needed
	bsr	Write_Palette_To_Copper	;I:a5, T:d0/a0-a2

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
	jsr	FW_ClearBuffer_CPU	;I:d0/a0, T:d0-d7/a0-a4

	lea	BPL_Log1,a0
	move.w	#(BPL_BUF_TOTALSIZE/2),d0
	jsr	FW_ClearBuffer_CPU	;I:d0/a0, T:d0-d7/a0-a4

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

	cmpi.w	#FX_ZOOM_FLAG,d0
	beq	.fx_zoom

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

.fx_zoom:
	movem.w	(a4)+,d0-d1		;speed,w
	bsr	Controller_FX_Zoom
	bra	.loop	


*****************************************************************************
* Performs any time-based controller routines.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d7/a2-a5
*****************************************************************************

Controller_Perform:

	tst.w	CTRL_PALETTE_ACTIVE(a5)
	beq.s	.zoom
	bsr	Controller_FX_Palette_Perform
.zoom:
	tst.w	CTRL_ZOOM_ACTIVE(a5)
	beq.s	.exit
	bsr	Controller_FX_Zoom_Perform
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

	moveq	#PAL_NUMCOLS_ALL-1,d0	;8 cols * 2
	lea	PAL_Current(pc),a1
.loop:
	move.w	(a0)+,(a1)+
	dbf	d0,.loop

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
	
	REPT PAL_NUMCOLS_ALL
	move.w	(a0)+,(a1)+
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
	move.w	d0,CTRL_ZOOM_ACTIVE(a5)
	move.w	#CL_NUM_BUFFERS,CTRL_ZOOM_LOAD_FLAG(a5)
	rts
.slow:
	move.w	#1,CTRL_ZOOM_ACTIVE(a5)
	move.w	d0,CTRL_ZOOM_SPEED(a5)	
	move.w	d1,CTRL_ZOOM_REQ(a5)	
	;clr.w	CTRL_ZOOM_VEL(a5)		;no

	rts


*****************************************************************************
* Performs move change
* IN:		a5, VEC_Controller_Info
*		a0, current object info
* OUT:
* TRASHED:	d0-d6
*****************************************************************************

Controller_FX_Zoom_Perform:
	;Request copper load this twice for double buffer cl issues
	move.w	#CL_NUM_BUFFERS,CTRL_ZOOM_LOAD_FLAG(a5)

	;min/max zoom level is not limited here. It is limited in the copper routine
	move.w	CTRL_ZOOM(a5),d0	;current value
	move.w	CTRL_ZOOM_REQ(a5),d1	;final value
	move.w	CTRL_ZOOM_SPEED(a5),d2	;speed
	move.w	CTRL_ZOOM_VEL(a5),d3	;vec
	bsr.s	Move2_Coord_Towards

	move.w	d0,CTRL_ZOOM(a5)	;save value
	move.w	d3,CTRL_ZOOM_VEL(a5)	;save value
	cmp.w	d0,d1			;finished?
	bne.s	.exit
	clr.w	CTRL_ZOOM_ACTIVE(a5)	;finished (double buffered)
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

	REPT	PAL_NUMCOLS_MAIN
	move.w	(a0)+,(a1)
	addq.l	#4,a1			;next color
	ENDR
.exit:	
	rts


*****************************************************************************
* Loads the current zoom into the current copperlist if changed.
* IN:		a5, VEC_Controller_Info
* OUT:
* TRASHED:	?
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
	lea	P0_CL_ZOOM_PATTERN_OFFSET+CL_PATTERN_BPLCON0+2(a0),a1

	; Clear vertical bplcon0 values (default is vector visible as normal)
	move.w	#BPL_BUF_HEIGHT-1,d1
	move.w	#ZOOM_BPLCON0,d0		;3bpl (2vector, 1pattern)
	moveq	#CL_PATTERN_SIZEOF,d2
.clearloop:
	move.w	d0,(a1)
	add.l	d2,a1
	dbf	d1,.clearloop	

	;Set vertial scaling to 0
	move.l	CL_Log1_Ptr(pc),a0
	lea	P0_CL_BPL_OFFSET(a0),a0
	moveq	#BPL_BUF_NUMPLANES,d0
	move.l	BPL_Log1_Ptr(pc),d1
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


	;vertial zoom, turn bitplanes on/off to stretch
	;Our zoom bitmap is 352 wide, so we add 2 for each 16 pixels 
	;difference to cur scr or ZOOM_PATTERN_X_BYTEOFFSET	
	moveq	#0,d4			;blank line count
	move.w	d6,d1
	mulu	#ZOOM_PATTERN_BYTEWIDTH,d1
	lea	BPL_Zoom_Pattern+ZOOM_PATTERN_Y_BYTEOFFSET,a0
	add.l	d1,a0			;a0 is the line

	move.l	CL_Log1_Ptr(pc),a1
	lea	P0_CL_ZOOM_PATTERN_OFFSET+CL_PATTERN_BPLCON0+2(a1),a1
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
CTRL_ZOOM:				rs.w 1		;current zoom level
CTRL_ZOOM_ACTIVE:			rs.w 1		;move active, final x,y,z,speed
CTRL_ZOOM_LOAD_FLAG			rs.w 1		;set to >1 to force zoom load
CTRL_ZOOM_REQ:				rs.w 1		;Final zoom level
CTRL_ZOOM_SPEED:			rs.w 1
CTRL_ZOOM_VEL:				rs.w 1
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

	EVEN
BPL_Pic_Source:
	INCBIN "AssetsConverted/PlayItAgainSam_320x256x5_inter.BPL.nrv2s"
	EVEN

PAL_Pic_Source:
	INCLUDE "AssetsConverted/PlayItAgainSam_320x256x5_inter.PAL_dcw.i"


*****************************************************************************

; Screen height mult by screen bytewidth and number of bitplanes (interleaved)
;Mult_SCR_Height_ByteWidth_NumPlanes:
;	ds.w	BPL_BUF_HEIGHT

; The bitmap used for perspective bitmap. Takes too long to generate so needs
; to be in own buffer for precalc. Precalc dony by ExplodeVector.s
;Moved to IntroSharedData.s
;BPL_Zoom_Pattern:	ds.b ZOOM_PATTERN_TOTALSIZE
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
FX_ZOOM_FLAG			=	8

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

FX_ZOOM		MACRO
		dc.w	FX_ZOOM_FLAG
		dc.w	\1		;speed
		dc.w	\2		;new zoom (0-1023)
		ENDM

*****************************************************************************

ControllerScript:
	;First pause needs to allow for depack time
	;FX_PAUSE	10

	;FX_ZOOM		0,0
	FX_PALETTE	2,PAL_Pic_Source

	FX_PAUSE	250-100		;105 was extra time added onto greetz

	FX_ZOOM		16,ZOOM_MAX
	FX_PAUSE	24
	FX_ZOOM		16,0
	FX_PAUSE	32

	FX_ZOOM		16,ZOOM_MAX
	FX_PAUSE	32
	FX_ZOOM		16,0
	FX_PAUSE	32

	FX_ZOOM		16,ZOOM_MAX
	FX_PAUSE	32
	FX_ZOOM		16,0
	FX_PAUSE	32

	FX_ZOOM		64,ZOOM_MAX
	FX_PAUSE	74

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
	CMOVE 	bplcon0,$5200		;5 bpl
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0000
	CMOVE 	bpl1mod,BPL_BPLMOD
	CMOVE 	bpl2mod,BPL_BPLMOD

	;CWAIT	25-1,$7			;Time for altering Copperlist
	;CWAIT	P0_DIW_V-1,$7

P0_CL_Bpl:				;Bitplane pointers
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
a set color01
	REPT	PAL_NUMCOLS_MAIN-1	
	CMOVE	a,$000
a set a+2
	ENDR

P0_CL_Zoom_Pattern:
a set P0_DIW_V
	REPT BPL_BUF_HEIGHT
	CWAIT	(a&$ff),$7		;0,2
	CMOVE 	bplcon0,ZOOM_BPLCON0	;4,6
	CWAIT	(a&$ff),$e1		;8,10
a set a+1
	ENDR

	; Trigger copper interrupt
	IFNE FW_IRQ_TYPE_COPPER
		;IF SCANLINE_EOF>255
		;	CWAIT	255,$e1
		;ENDIF
		;CWAIT	SCANLINE_EOF&$ff,$7
		CMOVE	intreq,INTF_SETCLR|INTF_COPER
	ENDC

	COPPEREND
P0_CL_End:

P0_CL_BPL_OFFSET = (P0_CL_Bpl-P0_CL_Phys)
P0_CL_ZOOM_PATTERN_OFFSET = (P0_CL_Zoom_Pattern-P0_CL_Phys)
P0_CL_COL_OFFSET = (P0_CL_Cols-P0_CL_Phys)
P0_CL_SIZE = P0_CL_End-P0_CL_Phys

	RSRESET
CL_PATTERN_WAIT1:	rs.w	2
CL_PATTERN_BPLCON0:	rs.w	2
CL_PATTERN_WAIT2:	rs.w	2
CL_PATTERN_SIZEOF:	rs.w	0


*****************************************************************************
*****************************************************************************
*****************************************************************************

; Map screens to shared chipmem buffers
CUR_CHIP_BUF set FW_Chip_Buffer_1

BPL_Phys		=	CUR_CHIP_BUF	;3bpl 320x256 = 30720
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_Log1		=	CUR_CHIP_BUF	;3bpl = 30720
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

P0_CL_Log1		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+P0_CL_SIZE

*****************************************************************************

