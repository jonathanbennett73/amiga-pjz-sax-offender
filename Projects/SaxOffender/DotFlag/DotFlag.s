*****************************************************************************

; Name			: DotFlag.s
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com)
; Description		: Logo, starfield and text messages.
; Date last edited	: 04/02/2020
				
*****************************************************************************

RasterTest set 0	;color00 timing bar, 0=off, 1=overall, 2=show blitwaits

	IFEQ	RasterTest
tmpcolor00 set color00	;use tmpcolor00 in place of color00 everywhere so timing is easier
	ELSE
tmpcolor00 set color01
	ENDC

*****************************************************************************

	INCLUDE "hardware/custom.i"
	INCLUDE "hardware/intbits.i"	

	INCLUDE "IntroConfig.i"
	INCLUDE	"../Framework/customextra.i"
	INCLUDE "../Framework/CustomMacros.i"
	INCLUDE "../Framework/IntroLibrary.i"

; Time this frame should start in the full demo, to handle faster amigas and
; timing issues. As this is OCS timings should be based on A500
	IFNE _INTROWRAPPER
FRAME_START = $b40
	ELSE
FRAME_START = 1
	ENDC

; Additional external symbols 
	xref	FW_CheckUserQuitSignal_A6
	xref	FW_ClearBuffer_BlitCPU_A6
	xref	FW_ClearBuffer_CPU
	xref	FW_Quit_Flag
	xref	FW_SetBaseCopperAndLev3Irq_A6
	xref	FW_SetBaseLev3Irq
	xref	FW_SetCopperAndLev3Irq_A6
	xref	FW_WaitFrame
	xref 	FW_VBlankProxy
	xref	FW_IsFrameOver

	xref	LIB_RGB12_Interpolate_Fast
	xref	LIB_RGB12_Interpolate_Fast_Palette

	xref	FW_Chip_Buffer_1
	xref	FW_Chip_Buffer_1_End


*****************************************************************************

	SECTION	DotFlag_PublicCode,CODE	;Code section in Public memory

*****************************************************************************

; Photon:
; Best practice is to start with DDF 38,d0 and DIW 2c81,2cc1 and modify these 
; symmetrically and always in whole 16px steps for compatibility.
; Note DDF start of less than 30 and you start to lose sprites.

*** Changeable Parameters For Display ***

*** Display Window ***
P0_DIW_V		=	$2c+11	;Hardware Vstart ($2c normal, $24 overscan)
P0_DIW_H		=	$81	;Hardware Hstart ($81 normal, $71 overscan)
P0_DIW_WIDTH		=	320	;Pixels		 (multiple of 16, 320 normal, 352 overscan)
P0_DIW_HEIGHT		=	256-11	;Lines		 (256 normal PAL, 272 overscan)

P0_DDF_H		=	$81		;Hardware Hstart ($81 normal, $71 overscan)
P0_DDF_WIDTH		=	320		;Pixels		 (320 normal pal, 352 overscan)
P0_DDF_BYTEWIDTH	=	P0_DDF_WIDTH/8	;Bytes
P0_DDF_WORDWIDTH	=	P0_DDF_BYTEWIDTH/2	;Words

SCANLINE_EOF		=	(P0_DIW_V+P0_DIW_HEIGHT)	; Safe to starting clearing after this scanline

*****************************************************************************

*** Screen Definitions ***

;The visible screen area and back buffer
SCR_BUF_WIDTH		=	320
SCR_BUF_BYTEWIDTH	=	SCR_BUF_WIDTH/8		;Bytes
SCR_BUF_WORDWIDTH	=	SCR_BUF_BYTEWIDTH/2	;Words
SCR_BUF_HEIGHT		=	256		;Lines
SCR_BUF_NUMPLANES	=	2			;2bpl/4 cols
SCR_BUF_NUMCOLS 	= 	(1<<SCR_BUF_NUMPLANES)
SCR_BUF_SIZE		=	SCR_BUF_BYTEWIDTH*SCR_BUF_HEIGHT
SCR_BUF_TOTALSIZE	=	SCR_BUF_SIZE*SCR_BUF_NUMPLANES

;Save some time by not clear top/bottom most lines
SCR_BUF_CLR_YOFFSET	=	11*(SCR_BUF_BYTEWIDTH*SCR_BUF_NUMPLANES)
SCR_BUF_CLR_HEIGHT	=	SCR_BUF_HEIGHT-21	;Clear height (save time)

SCR_BPLMOD		=	(SCR_BUF_BYTEWIDTH-P0_DDF_BYTEWIDTH)+(SCR_BUF_BYTEWIDTH*(SCR_BUF_NUMPLANES-1))

DOT_XOFFSET		=	SCR_BUF_WIDTH/2
DOT_YOFFSET		=	SCR_BUF_HEIGHT/2

*****************************************************************************
;46x46 = 2116
;46*45 = 2070
;43*43 = 1849
DOT_FLAG_WIDTH	= SCR_BUF_WIDTH
DOT_FLAG_HEIGHT = SCR_BUF_HEIGHT

DOT_NUM_X_DOTS	= 40			;must be a multiple of 8
DOT_NUM_Y_DOTS	= 30			;33(p61),28(pre). Max height is 32
DOT_Y_DOT_OFFSET = (32-DOT_NUM_Y_DOTS)/2	;Image is assumed to be 32px high			

DOT_NUM_X_BYTES = DOT_NUM_X_DOTS/8
DOT_X_SPACING	= (DOT_FLAG_WIDTH-SINE_AMPLITUDE)/DOT_NUM_X_DOTS
DOT_Y_SPACING	= (DOT_FLAG_HEIGHT-SINE_AMPLITUDE)/DOT_NUM_Y_DOTS
DOT_X_STARTPOS	= (DOT_FLAG_WIDTH-((DOT_X_SPACING*(DOT_NUM_X_DOTS-1))+SINE_AMPLITUDE))/2
DOT_Y_STARTPOS	= (DOT_FLAG_HEIGHT-((DOT_Y_SPACING*(DOT_NUM_Y_DOTS-1))+SINE_AMPLITUDE))/2

DOT_BLUR_EFFECT	=	1	;use an extra 2bpl to create a blur (lose a couple of dot lines in raster time)

;Output numplanes and output numcolors is different if blur effect on, it doubles
;the number of bitplanes you need colours for
	IFNE DOT_BLUR_EFFECT
DOT_OUTPUT_NUMPLANES 	= SCR_BUF_NUMPLANES*2
	ELSE
DOT_OUTPUT_NUMPLANES 	= SCR_BUF_NUMPLANES
	ENDC
DOT_OUTPUT_NUMCOLS 	= (1<<DOT_OUTPUT_NUMPLANES)

;Override bitmap palettes
DOT_OUTPUT_COL00 	= $000
DOT_OUTPUT_COL00_REFL 	= $001


*****************************************************************************
* Start the effect (usually used for setting up the copper)
* This is called each time the effect is started
* IN:		
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************
	xdef	DOT_Start

	IFEQ _INTROWRAPPER
	xdef	SubPartStart
SubPartStart:	
	ENDC

DOT_Start:
	movem.l	d2-d7/a2-a6,-(sp)	;save
	lea	ControllerScript,a0

	lea	_custom,a6
	lea	Controller_Info(pc),a5

	;Save script pointer
	move.l	a0,CTRL_SCRIPT_PTR(a5)

	; Run intro startup precalc if not already done (only runs once)
	bsr.s	SubPartPreCalc_IntroStart	;T:None

	;Start the P0 routine irq and copper
	bsr	P0_Init			;I:a5-a6, T:d0-d1/a0-a1

	; Continue with main loop outside the irq
	bsr	P0_MainLoop		;I:a5-a6, T:d0-d1/a0-a1

	;May want to do various things here. Leave copperlist active, but use
	;default lev3 irq is common so that easy to transition.
	jsr	FW_SetBaseLev3Irq

	movem.l	(sp)+,d2-d7/a2-a6	;restore
	rts


*****************************************************************************
* To be called during intro startup to do any permanent precalc required.
* IN:	
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

	;Only export one symbol depending if in demo or standalone mode
	IFEQ _INTROWRAPPER
	xdef	SubPartPreCalc_IntroStart
	ELSE
	xdef	DOT_PreCalc_IntroStart
	ENDC
	
SubPartPreCalc_IntroStart:	
DOT_PreCalc_IntroStart:
	tst.w	Controller_Info+CTRL_PRECALC_INTROSTART_DONE
	bne	.exit			;already done

	movem.l	d2-d7/a2-a6,-(sp)	;save

	;Start

	; Convert y sine into premult screen values
	bsr	Precalc_Sine_Y

	; Covert bitmaps into dot color tables
	lea	DOT_BPL_List,a4
.image_loop:
	move.l	(a4)+,a0		;image bpl
	move.l	(a4)+,a1		;output chk
	bsr	Inter_2BPL_To_2BitChunky	;I:a0/a1, T:d0-d4/a0-a3
	cmp.l	#DOT_BPL_List_End,a4
	blt.s	.image_loop

	; Create palettes and refl colours of all palettes we will use.
	lea	DOT_PAL_List,a4
.pal_loop:
	move.l	(a4)+,a0		;image bpl
	move.l	(a4)+,a1		;output chk
	bsr	Create_Palettes		;I:a0/a1, T:a0-a3,d0-d5
	cmp.l	#DOT_PAL_List_End,a4
	blt.s	.pal_loop

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
	;stop all time based effects
	moveq	#0,d0
	move.w	d0,CTRL_FINISHED(a5)
	move.w	d0,CTRL_PAUSE_COUNTER(a5)
	move.w	d0,CTRL_PALETTE_ACTIVE(a5)
	move.l	d0,CTRL_ANIM_PAL_PTR(a5)
	move.l	d0,CTRL_ANIM_IMAGE_PTR(a5)

	;Load initial image anim
	lea	DOT_Anim_CHK_Blank_1_8,a0
	bsr	Controller_FX_Load_Anim_Image

	; Load default palette anim 
	lea	DOT_Anim_PAL_Blank_1_8,a0
	bsr	Controller_FX_Load_Anim_Pal

	;Start black
	lea	PAL_AllBlack(pc),a0
	moveq	#0,d0
	bsr	Controller_FX_Palette	;I:d0/a0/a5, T:d0/a0-a1

	; Init copper pointers
	bsr	P0_CL_InitPtrs		;T:d0-d4/a0
	bsr	Write_Palette_To_Copper	;I:a5, T:d0/a0-a2

	; Clear all screen buffers at EOF and swap copper list assumes
	;previous routine has a transition/blank screen CL in place
	bsr	Clear_ScreenBuffers

	;Swap buffers and load copper for next frame
	bsr	P0_ScreenBufferSwap	;T:d0-d1/a0

	IFEQ FW_RMB_QUIT_SECTION
	;Wait for A500 timing
.frameloop:	
	move.w	#FRAME_START,d0
	jsr	FW_IsFrameOver		;I:d0, O:d0 
	beq.s	.frameloop
	ENDC

	;Initialise our new copper and irq
	lea	P0_CL_Phys,a0
	lea	P0_Lev3Irq(pc),a1
	jsr	FW_SetCopperAndLev3Irq_A6	
.exit:
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

	bsr	Clear_WorkBuffer_A6
	jsr	FW_VBlankProxy			;T:d0-d7/a0-a4

	; Read new script lines and perform
	lea	Controller_Info(pc),a5		;controller info
	bsr	Controller_ReadCommands		;I:a5, T:d0-d7/a0-a4
	bsr	Anim_Image			;I:a5, T:d0-d1/a0-a2
	bsr	Anim_PAL 			;I:a5, T:d0-d1/a0-a2
	bsr	Controller_Perform		;I:a5, T:d0-d7/a0-a2
	bsr	PlotDots_2bpl			;I:a5, T:d0-d7/a0-a4

	;On A4000 it can finish before/during line 44, force a blitwait here which
	;Takes us 1/3 way into frame so safe to change copper values.
	WAITBLIT_A6

	bsr	Write_Palette_To_Copper		;I:a5
	bsr.s	P0_ScreenBufferSwap

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
* Swaps the screens
* IN:		
* OUT:		
* TRASHED:	d0-d4/a0
*****************************************************************************

P0_CL_InitPtrs:
;Init. physical & logical screen addresses

	move.l	#DOT_Scr_Front,BPL_Phys_Ptr		;Physical screen adr
	move.l	#DOT_Scr_Back1_Draw,BPL_Log1_Ptr	;Intermediate screen adr
	move.l	#DOT_Scr_Back2_Clear,BPL_Log2_Ptr	;Logical screen adr

	IFNE	DOT_BLUR_EFFECT
		move.l	#DOT_Scr_Back3_Prev,BPL_Log3_Ptr	;prev screen adr
		move.l	#DOT_Scr_Back4_Prev,BPL_Log4_Ptr	;prev screen adr
	ENDC

        ; Just run double buffer swap to update the copper bitmap pointers
	bsr.s	P0_ScreenBufferSwap

	rts


*****************************************************************************
* Swaps the screens
* IN:		
* OUT:		
* TRASHED:	d0-d4/a0
*****************************************************************************

P0_ScreenBufferSwap:

	;playfield 1
	IFNE DOT_BLUR_EFFECT
		lea	BPL_Phys_Ptr(pc),a0
		movem.l	(a0),d0-d4	;;12+8n, 52
		movem.l	d1-d4,(a0)	;8+8n, 40
		move.l	d0,4*4(a0)	;16
					;108
	ELSE
		lea	BPL_Phys_Ptr(pc),a0
		movem.l	(a0),d0-d2
		move.l	d1,(a0)+
		move.l	d2,(a0)+
		move.l	d0,(a0)+
	ENDC

	;d1=new front
	;d4=previous front if blurring

	moveq	#SCR_BUF_BYTEWIDTH,d2	;Next bitplane (interleaved)

	lea 	P0_CL_Bpl+2,a0		;Adr of copper pointers, skip cmove
	REPT	SCR_BUF_NUMPLANES	;Number of bitplanes
	swap	d1			;Swap high & low words
	move.w	d1,(a0)			;High ptr
	swap	d1			;Swap high & low words
	move.w	d1,4(a0)		;Low ptr
	addq.l	#8,a0			;Next set of ptrs
	add.l	d2,d1			;Next bitplane (interleaved)
	ENDR

	IFNE	DOT_BLUR_EFFECT
		lea 	P0_CL_Bpl_Blur+2,a0	;Adr of copper pointers, skip cmove
		REPT	SCR_BUF_NUMPLANES	;Number of bitplanes
		swap	d4			;Swap high & low words
		move.w	d4,(a0)			;High ptr
		swap	d4			;Swap high & low words
		move.w	d4,4(a0)		;Low ptr
		addq.l	#8,a0			;Next set of ptrs
		add.l	d2,d4			;Next bitplane (interleaved)
		ENDR
	ENDC	

	rts

*****************************************************************************

; Screen buffer pointers - must stay in this order
BPL_Phys_Ptr:		dc.l	0	;physical
BPL_Log1_Ptr:		dc.l	0	;draw
BPL_Log2_Ptr:		dc.l	0	;clear 
BPL_Log3_Ptr:		dc.l	0	;previous frame
BPL_Log4_Ptr:		dc.l	0	;previous frame


*****************************************************************************
* Converts Y sine values into premultipled screen offsets (interleaved)
* IN:		
* OUT:		
* TRASHED:	d0-d2/a0 
*****************************************************************************

Precalc_Sine_Y:
	lea	Sine_Y_128_Words,a0
	move.w	#(SINE_NUMENTRIES*2)-1,d0	; *2 for duplicate table
	move.w	#SCR_BUF_BYTEWIDTH*SCR_BUF_NUMPLANES,d1
.loop:
	move.w	(a0),d2
	mulu	d1,d2
	move.w	d2,(a0)+
	dbf	d0,.loop

	rts


*****************************************************************************
* Converts logos 2 bit interleaved bitplane data into 2 bit chunky.
* IN:		a0, interleave bitplane start
*		a1, output chunky
*		
* OUT:		
* TRASHED:	d0-d4/a0-a3
*****************************************************************************
; bpl1:   101xxxxx
; bpl2:   011xxxxx
; output: 10 01 11
; Read byte from bpl1,2

Inter_2BPL_To_2BitChunky:
	;skip lines if not using full height
	lea	DOT_Y_DOT_OFFSET*SCR_BUF_NUMPLANES*DOT_BPL_BYTEWIDTH(a0),a0
	move.l	a1,a3

	moveq	#DOT_NUM_Y_DOTS-1,d4
.yloop:
	move.l	a0,a1				;1st bpl pointers
	lea	DOT_BPL_BYTEWIDTH(a0),a2	;2nd bpl

	moveq	#DOT_NUM_X_BYTES-1,d3
.byteloop:
	move.b	(a1)+,d0	
	move.b	(a2)+,d1	

	REPT 8
	add.b	d0,d0			;shift source top bit out
	roxl.w	#1,d2			;rotate into lowest dest bit 
	add.b	d1,d1			;and again for 2nd bpl
	roxl.w	#1,d2
	ENDR

	;d2.w now contains 16 bits (8 dots) of packed data - write it out
	move.w	d2,(a3)+
	dbf	d3,.byteloop

	;Next row (+bytewide * 2bpl)
	lea	DOT_BPL_BYTEWIDTH*2(a0),a0
	dbf	d4,.yloop

	rts


*****************************************************************************
* Creates the various palettes for a given bitmap.
* IN:		a0, source palette
*		a1, dest palette
*		
* OUT:		
* TRASHED:	a0-a3,d0-d5
*****************************************************************************

Create_Palettes:
	move.l	a4,-(sp)
	move.l	a5,-(sp)

	;save intput/output palettes
	move.l	a0,a4
	move.l	a1,a5

	;Bright palette
	lea	PAL_Temp_Bright(pc),a1

	;leave color 00 alone
	;move.w	(a0)+,(a1)+

	;moveq	#SCR_BUF_NUMCOLS-1-1,d5
	moveq	#SCR_BUF_NUMCOLS-1,d5
.loop
	move.w	(a0)+,d0		;starting color
	move.w	#$fff,d1		;final color is white
	moveq	#7,d2			;half way (step 0-15)
	jsr	LIB_RGB12_Interpolate_Fast	;trashes d0-d4. Needs RGB12_Interpolate_Fast_BuildTable done at startup.
	move.w	d0,(a1)+

	dbf	d5,.loop


	;Dark palette
	move.l	a4,a0			;input pal
	lea	PAL_Temp_Dark(pc),a1

	;leave color 00 alone
	;move.w	(a0)+,(a1)+

	;moveq	#SCR_BUF_NUMCOLS-1-1,d5
	moveq	#SCR_BUF_NUMCOLS-1,d5
.loop2
	move.w	(a0)+,d0		;starting color
	moveq	#$000,d1		;final color is black
	moveq	#3,d2			;half way (step 0-15)
	jsr	LIB_RGB12_Interpolate_Fast	;trashes d0-d4. Needs RGB12_Interpolate_Fast_BuildTable done at startup.
	move.w	d0,(a1)+

	dbf	d5,.loop2


	;Final 16 color palette (to handle overlapping planes)
;BPL  4321 
;COL

;normal colours (main/top)
;1  - 0001 - 1 normal 
;2  - 0010 - 2
;3  - 0011 - 3

;dark colours (previous frame/blur, no overlap)
;4  - 0100 - 1 dark
;8  - 1000 - 2
;12 - 1100 - 3

;bright colors (main+previous overlap in anyway)
;5  - 0101 - 1 bright
;6  - 0110 - 2
;7  - 0111 - 3
;9  - 1001 - 1
;10 - 1010 - 2
;11 - 1011 - 3
;13 - 1101 - 1
;14 - 1110 - 2
;15 - 1111 - 3

	move.l	a5,a0			;output palette
	move.l	a4,a1			;input palette
	lea	PAL_Temp_Bright(pc),a2
	lea	PAL_Temp_Dark(pc),a3

	movem.w	(a1),d0-d3		;col0-3 normal
	;moveq	#DOT_OUTPUT_COL00,d0	;background
	move.w	d0,(a0)			;0
	move.w	d1,1*2(a0)		;1
	move.w	d2,2*2(a0)		;2
	move.w	d3,3*2(a0)		;3

	IFNE DOT_BLUR_EFFECT
	movem.w	(a3),d0-d3		;col0-3 dark
	move.w	d1,4*2(a0)		;4
	move.w	d2,8*2(a0)		;8
	move.w	d3,12*2(a0)		;12

	movem.w	(a2),d0-d3		;col0-3 bright
	move.w	d1,5*2(a0)		;5
	move.w	d2,6*2(a0)		;6
	move.w	d3,7*2(a0)		;7
	move.w	d1,9*2(a0)		;9
	move.w	d2,10*2(a0)		;10
	move.w	d3,11*2(a0)		;11
	move.w	d1,13*2(a0)		;13
	move.w	d2,14*2(a0)		;14
	move.w	d3,15*2(a0)		;15
	ENDC

	move.l	(sp)+,a5
	move.l	(sp)+,a4

	rts


*****************************************************************************
* Clears the work buffer screen. Every frame.
* IN:		A6(Custom)
* OUT:		
* TRASHED:	a0
*****************************************************************************

Clear_WorkBuffer_A6:

	; Clear screen
	move.l	BPL_Log2_Ptr(pc),a0
	lea	SCR_BUF_CLR_YOFFSET(a0),a0

	WAITBLIT_A6
	move.l	#$01000000,bltcon0(a6)
	move.l	a0,bltdpth(a6)
	move.w	#0,bltdmod(a6)
	move.w	#SCR_BUF_CLR_HEIGHT*64+(SCR_BUF_WORDWIDTH*SCR_BUF_NUMPLANES),bltsize(a6)

	rts

*****************************************************************************
* Clears front and back buffer. Done at startup to clear everything.
* IN:		
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

Clear_ScreenBuffers:
	movem.l	d2-d7/a2-a6,-(sp)

	; a0 = buffer to clear
	; d0 = size in words to clear
	; trashes all regs, so restore a6

	lea	DOT_Scr_Front,a0		;front
	move.l	#SCR_BUF_TOTALSIZE/2,d0
	jsr	FW_ClearBuffer_CPU		;I:d0/a0, T:d0-d7/a0-a4

	lea	DOT_Scr_Back1_Draw,a0		;back1
	move.l	#SCR_BUF_TOTALSIZE/2,d0
	jsr	FW_ClearBuffer_CPU		;I:d0/a0, T:d0-d7/a0-a4

	lea	DOT_Scr_Back2_Clear,a0		;back2
	move.l	#SCR_BUF_TOTALSIZE/2,d0
	jsr	FW_ClearBuffer_CPU		;I:d0/a0, T:d0-d7/a0-a4

	IFNE	DOT_BLUR_EFFECT
		lea	DOT_Scr_Back3_Prev,a0		;back3
		move.l	#SCR_BUF_TOTALSIZE/2,d0
		jsr	FW_ClearBuffer_CPU	;I:d0/a0, T:d0-d7/a0-a4

		lea	DOT_Scr_Back4_Prev,a0		;back3
		move.l	#SCR_BUF_TOTALSIZE/2,d0
		jsr	FW_ClearBuffer_CPU	;I:d0/a0, T:d0-d7/a0-a4
	ENDC

	movem.l	(sp)+,d2-d7/a2-a6
	rts


*****************************************************************************
* Loads the current colors into the current copperlist if requested.
* IN:		a5, Controller_info
* OUT:
* TRASHED:	d0/a0-a1
*****************************************************************************

Write_Palette_To_Copper:
	tst.w	CTRL_PALETTE_LOAD_FLAG(a5)
	beq.s	.exit
	subq.w	#1,CTRL_PALETTE_LOAD_FLAG(a5)

	lea	P0_CL_Phys,a1		;root of CL

	;main
	lea	PAL_Current(pc),a0		;start at col00
	lea	P0_CL_COL_OFFSET+2(a1),a1	;start at col00 in copperlist

	REPT	DOT_OUTPUT_NUMCOLS
	move.w	(a0)+,(a1)
	addq.l	#4,a1			;next color, skipping cmove
	ENDR
.exit:
	rts

*****************************************************************************

PAL_AllWhite:
	dcb.w	DOT_OUTPUT_NUMCOLS,$fff

PAL_AllBlack:
	dcb.w	DOT_OUTPUT_NUMCOLS,0
	;dcb.w	DOT_OUTPUT_NUMCOLS,0

; Master palette poked into the copperlist each frame, 16 cols followed by 16
; more for reflection
PAL_Current:		
	dcb.w	DOT_OUTPUT_NUMCOLS,0
	;dcb.w	DOT_OUTPUT_NUMCOLS,0

; This holds the source palette used during transitions in FX_PALETTE. The
; source value is interpolated from this value to the destination value + step size.
PAL_Current_Src:		
	dcb.w	DOT_OUTPUT_NUMCOLS,0
	;dcb.w	DOT_OUTPUT_NUMCOLS,0

; Temporary palettes used when working out dark and light versions
; this uses same number of colours as source bitmaps
PAL_Temp_Bright:
	dcb.w	SCR_BUF_NUMCOLS,0
PAL_Temp_Dark:
	dcb.w	SCR_BUF_NUMCOLS,0

*****************************************************************************

;Controller info

	RSRESET
CTRL_PAUSE_COUNTER:		rs.w 1		;Pause counter, 0=running
CTRL_SCRIPT_PTR:		rs.l 1		;Script Ptr
CTRL_FINISHED:			rs.w 1		;1 if quitting
CTRL_PRECALC_INTROSTART_DONE	rs.w 1		;1 if intro startup precalcs done
CTRL_ANIM_IMAGE_PTR:		rs.l 1		;Ptr to active image anim
CTRL_ANIM_PAL_PTR:		rs.l 1		;Ptr to active image anim
CTRL_PALETTE_LOAD_FLAG		rs.w 1		;set to 1 to force palette load
CTRL_PALETTE_ACTIVE:		rs.w 1		;Palette change active
CTRL_PALETTE_PTR:		rs.l 1		;src Palette ptr (16 words of colors)
CTRL_PALETTE_COUNTER:		rs.w 1		;Palette counter, speed
CTRL_PALETTE_SPEED:		rs.w 1		;How often to update, higher is slower, 0 = instant
CTRL_PALETTE_STEP		rs.w 1		;Current step to interpolate between current color and final 0-15
DOT_SINE_X:			rs.w 1
DOT_SINE_Y:			rs.w 1
DOT_SINE_X_SPEED:		rs.w 1
DOT_SINE_X_STEP:		rs.w 1
DOT_SINE_Y_SPEED:		rs.w 1	
DOT_SINE_Y_STEP:		rs.w 1
CTRL_SIZE:			rs.w 0

	EVEN
Controller_Info:
				dc.w 0		;Pause counter, 0=running
				dc.l 0		;Script Ptr
				dc.w 0		;1 if quitting
				dc.w 0		;1 if intro startup precalcs done
				dc.l 0		;Ptr to active image anim
				dc.l 0		;Ptr to active image anim
				dc.w 0		;set to 1 to force palette load
				dc.w 0		;Palette change active
				dc.l 0		;src Palette ptr (16 words of colors)
				dc.w 0		;Palette counter, speed
				dc.w 0		;How often to update, higher is slower, 0 = instant
				dc.w 0		;Current step to interpolate between current color and final 0-15
				dc.w 0		;DOT_SINE_X - must be even 
				dc.w SINE_NUMENTRIES/3	;DOT_SINE_Y
				dc.w -2		;DOT_Sine_X_Speed - must be even
				dc.w 2		;DOT_Sine_X_Step - must be even
				dc.w 2		;DOT_Sine_Y_Speed
				dc.w 4		;DOT_Sine_Y_Step


*****************************************************************************
* Runs the controller script.
* Note the commands are read until a FX_PAUSE command is reached. So beware
* of hogging the CPU with too many commands at once.
* IN:		a6, _custom
*		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d7/a0-a4
*****************************************************************************

Controller_ReadCommands:

	;Time to get a new command from the script? Subtract each frame until 0
	move.w	CTRL_PAUSE_COUNTER(a5),d0
	bne	.pausing

	tst.w	CTRL_FINISHED(a5)
	bne.s	.exit

.readcmd:
	; Get current script pointer
	move.l	CTRL_SCRIPT_PTR(a5),a4
.loop:
	move.w	(a4)+,d0

	; subrouteins need to preserve a4/a5
	cmpi.w	#FX_PALETTE_FLAG,d0
	beq.s	.fx_pallete

	cmpi.w	#FX_SCRIPTJMP_FLAG,d0
	beq.s	.fx_scriptjmp

	cmpi.w	#FX_PAUSE_FLAG,d0
	beq.s	.fx_pause

	cmpi.w	#FX_LOAD_ANIM_IMAGE_FLAG,d0
	beq.s	.fx_loadanimimage

	cmpi.w	#FX_LOAD_ANIM_PAL_FLAG,d0
	beq.s	.fx_loadanimpal

	cmpi.w	#FX_SET_SINE_XY_SPEED_FLAG,d0
	beq.s	.fx_set_sine_xy_speed

	;assume end, don't save ptr
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
	bra.s	.loop

.fx_loadanimimage:
	move.l	(a4)+,a0		;New image anim struc
	bsr	Controller_FX_Load_Anim_Image
	bra.s	.loop

.fx_loadanimpal:
	move.l	(a4)+,a0		;New pal anim struc
	bsr	Controller_FX_Load_Anim_Pal
	bra.s	.loop

.fx_pallete:
	move.w	(a4)+,d0		;Speed
	move.l	(a4)+,a0		;New pallete
	bsr.s	Controller_FX_Palette	;I:d0/a0/a5, T:d0/a0-a1
	bra.s	.loop

.fx_set_sine_xy_speed:
	movem.w	(a4)+,d0-d3		;speed,step,speed,step
	movem.w	d0-d3,DOT_SINE_X_SPEED(a5)	;must be in same order
	;move.w	d0,DOT_SINE_X_SPEED(a2)
	;move.w	d1,DOT_SINE_X_STEP(a2)
	;move.w	d2,DOT_SINE_Y_SPEED(a2)
	;move.w	d3,DOT_SINE_Y_STEP(a2)
	bra.s	.loop

	rts


*****************************************************************************
* Performs any time-based controller routines.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	d0-d7/a0-a2
*****************************************************************************

Controller_Perform:

; TODO: Change to bitflag

.pal:
	tst.w	CTRL_PALETTE_ACTIVE(a5)
	beq.s	.exit
	bsr.s	Controller_FX_Palette_Perform

.exit:
	rts


*****************************************************************************
* Sets up the pallet change process.
* IN:		a5, controller info
*		a0, new pallete
*		d0, speed
* OUT:		
* TRASHED:	d0/a0-a1
*****************************************************************************

Controller_FX_Palette:
	;If setting a palette turn off palette animation
	clr.l	CTRL_ANIM_PAL_PTR(a5)

	; If speed is 0 just instastransform
	tst.w	d0
	bne.s	.palette

	move.w	d0,CTRL_PALETTE_ACTIVE(a5)	;disable change, d0 is zero here
	move.w	#CL_NUM_BUFFERS,CTRL_PALETTE_LOAD_FLAG(a5)	;request copper loads immediately twice for double buffer CL issues
	lea	PAL_Current(pc),a1

	REPT	(DOT_OUTPUT_NUMCOLS)/2		;/2 for number of longs, *2 for cols+refl
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
	lea	PAL_Current_Src(pc),a1	;store original active colors

	REPT	(DOT_OUTPUT_NUMCOLS)/2		;/2 for number of longs, *2 for cols+refl
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

	move.w	CTRL_PALETTE_SPEED(a5),CTRL_PALETTE_COUNTER(a5)

	lea	PAL_Current_Src(pc),a0		;starting colors
	move.l	CTRL_PALETTE_PTR(a5),a1		;final colors
	lea	PAL_Current(pc),a2		;active colors
	moveq	#(DOT_OUTPUT_NUMCOLS),d0	;number of colours to touch
	move.w	CTRL_PALETTE_STEP(a5),d1	;step

	;In, d0=numcols, d1=step, a0-a2 palettes
	;Out, d1=step
	;trashes d0/d2-d7,a0-a2
	jsr	LIB_RGB12_Interpolate_Fast_Palette	;I:d0-d1/a0-a2, T:d0/d2-d7/a0-a2
	
	;Request copper load this palette
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
* Sets up the image anim.
* IN:		a5, controller info
*		a0, new anim struct
* OUT:		
* TRASHED:	d0/a0-a1
*****************************************************************************

Controller_FX_Load_Anim_Image:
	move.l	a0,CTRL_ANIM_IMAGE_PTR(a5)
	
	; reset values
	moveq	#0,d0
	move.w	d0,DOT_ANIM_CURFRAME(a0)

	;Framedelay count should be reset so that 1st frame gets shown correctly
	move.w	DOT_ANIM_FRAMEDELAY(a0),DOT_ANIM_FRAMEDELAYCOUNT(a0)

	lea	DOT_ANIM_FRAMESTART(a0),a1	;reset to first frame
	move.l	a1,DOT_ANIM_FRAMESECTIONPTR(a0)

	rts


*****************************************************************************
* Sets up the pallet anim.
* IN:		a5, controller info
*		a0, new anim struct
* OUT:		
* TRASHED:	d0/a0-a2
*****************************************************************************

Controller_FX_Load_Anim_Pal:
	;Have to do this after Controller_FX_Palette
	;move.l	a0,CTRL_ANIM_PAL_PTR(a5)
	
	; reset values
	moveq	#0,d0
	move.w	d0,DOT_ANIM_CURFRAME(a0)

	;Framedelay count should be reset so that 1st frame gets shown correctly
	move.w	DOT_ANIM_FRAMEDELAY(a0),DOT_ANIM_FRAMEDELAYCOUNT(a0)	

	lea	DOT_ANIM_FRAMESTART(a0),a1	;reset to first frame
	move.l	a1,DOT_ANIM_FRAMESECTIONPTR(a0)

	; If there is only 1 frame in this, it won't animate so we
	; have to force it 
	;cmpi.w	#1,DOT_ANIM_NUMFRAMES(a0)
	;bgt.s	.exit
	; Must set new palette active immediately
	; save a0, anim struct
	move.l	a0,a2
	move.l	(a1),a0	;get actual palette in a0, controller a5
	;moveq	#0,d0	;immediate, d0 still 0 from above
	bsr	Controller_FX_Palette	;I:d0/a0/a5, T:d0/a0-a1

	;Controller_FX_Palette will have disabled palette animate, so reenable
	move.l	a2,CTRL_ANIM_PAL_PTR(a5)
.exit:
	rts


*****************************************************************************
* Animates image
* IN:		a5, Controller_Info
*		
* OUT:
* TRASHED:	d0-d1/a0-a2
*****************************************************************************

Anim_Image:

	move.l	CTRL_ANIM_IMAGE_PTR(a5),d0
	beq.s	.exit			;No anim yet
	move.l	d0,a0

	; How many frames?
	move.w	DOT_ANIM_NUMFRAMES(a0),d1
	cmpi.w	#1,d1
	beq.s	.exit			;Static image, ignore.

	; Is it time to change anim?
	subq.w	#1,DOT_ANIM_FRAMEDELAYCOUNT(a0)
	ble.s	.timetochange
	rts

.timetochange:
	; Reset frame count
	move.w	DOT_ANIM_FRAMEDELAY(a0),DOT_ANIM_FRAMEDELAYCOUNT(a0)

	;Increment current frame
	move.w	DOT_ANIM_CURFRAME(a0),d0
	addq.w	#1,d0
	cmp.w	d1,d0
	blt.s	.incok
	clr.w	DOT_ANIM_CURFRAME(a0)	;reset
	move.l	a0,a2
	lea	DOT_ANIM_FRAMESTART(a0),a2	;start of frame section
	move.l	a2,DOT_ANIM_FRAMESECTIONPTR(a0)	;store pointer
	rts

.incok:
	move.w	d0,DOT_ANIM_CURFRAME(a0)	;save frame
	addq.l	#4,DOT_ANIM_FRAMESECTIONPTR(a0)	;point to next entry

.exit:
	rts


*****************************************************************************
* Animates palette
* IN:		a5, Controller_Info
*		
* OUT:
* TRASHED:	d0-d1/a0-a2
*****************************************************************************

Anim_PAL:
	move.l	CTRL_ANIM_PAL_PTR(a5),d0
	beq.s	.exit			;No anim yet

	;If there is a fade active we can't change colours
	tst.w	CTRL_PALETTE_ACTIVE(a5)
	bne.s	.exit

	move.l	d0,a0

	; How many frames?
	move.w	DOT_ANIM_NUMFRAMES(a0),d1
	cmpi.w	#1,d1
	beq.s	.exit			;Static image, ignore.

	; Is it time to change anim?
	subq.w	#1,DOT_ANIM_FRAMEDELAYCOUNT(a0)
	ble.s	.timetochange
.exit:
	rts

.timetochange:
	; Reset frame count
	move.w	DOT_ANIM_FRAMEDELAY(a0),DOT_ANIM_FRAMEDELAYCOUNT(a0)

	;Increment current frame
	move.w	DOT_ANIM_CURFRAME(a0),d0
	addq.w	#1,d0
	cmp.w	d1,d0
	blt.s	.incok
	clr.w	DOT_ANIM_CURFRAME(a0)		;reset
	move.l	a0,a2
	lea	DOT_ANIM_FRAMESTART(a0),a2	;start of frame section
	move.l	a2,DOT_ANIM_FRAMESECTIONPTR(a0)	;store pointer
	bra.s	.changepal
.incok:
	move.w	d0,DOT_ANIM_CURFRAME(a0)	;save frame
	move.l	DOT_ANIM_FRAMESECTIONPTR(a0),a2
	addq.l	#4,a2
	move.l	a2,DOT_ANIM_FRAMESECTIONPTR(a0)	;store pointer

.changepal:
	lea	PAL_Current(pc),a0		;live palette
	move.l	(a2),a2				;new palette
	REPT	(DOT_OUTPUT_NUMCOLS)/2		;/2 for number of longs
	move.l	(a2)+,(a0)+
	ENDR

	;Request copper load this palette immediately
	move.w	#CL_NUM_BUFFERS,CTRL_PALETTE_LOAD_FLAG(a5)
	rts


*****************************************************************************
* Runs the effect.
* IN:		a6, _custom
*		a5, Controller_Info
* OUT:		
* TRASHED:	d0-d7/a0-a5
*
*****************************************************************************

DOTPLOT_FIRST	MACRO
	move.w	(a1)+,d0		;sine x (pixels)
	addi.w	#Xval,d0		;add current x position
	
	move.w	d0,d1			;save for bset
	not.w	d1			;convert to bset value (bset is modulo 8)
	;moveq	#1,d1
	;sub.b	d0,d1

	lsr.w	#3,d0			;x to byte offset
	add.w	(a2)+,d0		;x byte pos + y screen address offset

	move.w	(a5)+,d5		;Get next chunky color (2bits per bpl)
	bpl.s	.pl2\@			;is -ve bit set?
	bset.b	d1,(a0,d0.w)
.pl2\@:
	add.w	d5,d5			;shift left (into negative bit)
	bpl.s	.pl3\@			;is -ve bit set?
	bset.b	d1,SCR_BUF_BYTEWIDTH(a0,d0.w)
.pl3\@:
	ENDM

DOTPLOT_FIRST_Q	MACRO
	moveq	#Xval,d0		;current x pos
	add.w	(a1)+,d0		;sine x (pixels)
	
	move.w	d0,d1			;save for bset
	not.w	d1			;convert to bset value (bset is modulo 8)
	;moveq	#1,d1
	;sub.b	d0,d1
	
	lsr.w	#3,d0			;x to byte offset
	add.w	(a2)+,d0		;x byte pos + y screen address offset

	move.w	(a5)+,d5		;Get next chunky color (2bits per bpl)
	bpl.s	.pl2\@			;is -ve bit set?
	bset.b	d1,(a0,d0.w)
.pl2\@:
	add.w	d5,d5			;shift left (into negative bit)
	bpl.s	.pl3\@			;is -ve bit set?
	bset.b	d1,SCR_BUF_BYTEWIDTH(a0,d0.w)
.pl3\@:
	ENDM

DOTPLOT	MACRO
	move.w	(a1)+,d0		;sine x (pixels)
	addi.w	#Xval,d0		;add current x position
	move.w	d0,d1			;save for bset
	lsr.w	#3,d0			;x to byte offset
	add.w	(a2)+,d0		;x byte pos + y screen address offset
	not.w	d1			;convert to bset value (bset is modulo 8)

	add.w	d5,d5			;shift left (into negative bit)
	bpl.s	.pl2\@			;is -ve bit set?
	bset.b	d1,(a0,d0.w)
.pl2\@:
	add.w	d5,d5
	bpl.s	.pl3\@
	bset.b	d1,SCR_BUF_BYTEWIDTH(a0,d0.w)
.pl3\@:
	ENDM

DOTPLOT_Q	MACRO
	moveq	#Xval,d0		;current x pos
	add.w	(a1)+,d0		;sine x (pixels)
	move.w	d0,d1			;save for bset
	lsr.w	#3,d0			;x to byte offset
	add.w	(a2)+,d0		;x byte pos + y screen address offset
	not.w	d1			;convert to bset value (bset is modulo 8)

	add.w	d5,d5			;shift left (into negative bit)
	bpl.s	.pl2\@			;is -ve bit set?
	bset.b	d1,(a0,d0.w)
.pl2\@:
	add.w	d5,d5
	bpl.s	.pl3\@
	bset.b	d1,SCR_BUF_BYTEWIDTH(a0,d0.w)
.pl3\@:
	ENDM	


PlotDots_2bpl:
	move.w	#SINE_OFFSET_MASK,d4
	
	move.w	DOT_SINE_X(a5),d2
	add.w	DOT_SINE_X_SPEED(a5),d2
	and.w	d4,d2
	move.w	d2,DOT_SINE_X(a5)	;value for next frame

	move.w	DOT_SINE_Y(a5),d3
	add.w	DOT_SINE_Y_SPEED(a5),d3
	and.w	d4,d3
	move.w	d3,DOT_SINE_Y(a5)	;value for next frame

	lea	Sine_X_128_Words,a3
	lea	Sine_Y_128_Words,a4

	move.w	DOT_SINE_X_STEP(a5),a6	;trashed a6
	move.w	DOT_SINE_Y_STEP(a5),d6

	; Get chunky details that we are plotting (trashed a5)
	move.l	CTRL_ANIM_IMAGE_PTR(a5),a5	;Current image anim
	move.l	DOT_ANIM_FRAMESECTIONPTR(a5),a5	;Frame section pointer
	move.l	(a5),a5				;The actual chunky data

	move.l	BPL_Log1_Ptr(pc),a0
	lea	SCR_BUF_BYTEWIDTH*SCR_BUF_NUMPLANES*DOT_Y_STARTPOS(a0),a0
	;move.w	#DOT_NUM_Y_DOTS-1,d7		
.loopy:
	REPT	DOT_NUM_Y_DOTS
	lea	(a3,d2.w),a1		;Get starting offsets
	lea	(a4,d3.w),a2
Xval Set DOT_X_STARTPOS

	REPT	2;DOT_NUM_X_BYTES
	DOTPLOT_FIRST_Q			;Read in color and plot first dot
Xval set Xval+DOT_X_SPACING
	REPT	7
	DOTPLOT_Q			;Plot remaining 7 dots
Xval set Xval+DOT_X_SPACING
	ENDR
	ENDR	;DOT_NUM_X_BYTES

	REPT	DOT_NUM_X_BYTES-2
	DOTPLOT_FIRST			;Read in color and plot first dot
Xval set Xval+DOT_X_SPACING
	REPT	7
	DOTPLOT				;Plot remaining 7 dots
Xval set Xval+DOT_X_SPACING
	ENDR
	ENDR	;DOT_NUM_X_BYTES


	lea	SCR_BUF_BYTEWIDTH*SCR_BUF_NUMPLANES*DOT_Y_SPACING(a0),a0	;next row

	; Update starting offsets and do bounds check
	;add.w	DOT_Sine_X_Step,d2			;in words, even
	;add.w	DOT_Sine_Y_Step,d3
	add.w	a6,d2
	add.w	d6,d3
	and.w	d4,d2			;bounds check
	and.w	d4,d3

	ENDR	;DOT_NUM_Y_DOTS
	;dbf	d7,.loopy

	lea	_custom,a6		;restore
	lea	Controller_Info,a5
	rts


*****************************************************************************

; Note we have 128 words, but we duplicate the table twice so that
; we can skip a lot of bounds checks. We then only check bounds every
; row rather than every dot
; For this to work table must be 2 * DOT_NUM_X_DOTS
; DOT_NUM_X_DOTS = up to 63, means table must be 128

SINE_AMPLITUDE = 31

Sine_X_128_Words:
	include	"sine_0_31_64_words.i"
	include	"sine_0_31_64_words.i"
Sine_X_128_Words2:
	include	"sine_0_31_64_words.i"
	include	"sine_0_31_64_words.i"

Sine_Y_128_Words:
	include	"sine_0_31_128_words.i"
Sine_Y_128_Words2:
	include	"sine_0_31_128_words.i"

SINE_NUMENTRIES = 128	; Must be power of 2
SINE_OFFSET_MASK = ((SINE_NUMENTRIES*2)-2)	; Word offset access into the table


*****************************************************************************

	; Include all object data and scripts
	; Creates a data and bss data section so include at the end of
	; a section
	
	INCLUDE "DOT_ControllerScript.i"

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
CL_NUM_BUFFERS = 1

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
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	IFNE	DOT_BLUR_EFFECT
		CMOVE 	bplcon0,$4200		;4bpl
	ELSE
		CMOVE 	bplcon0,$2200		;2bpl
	ENDC
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0000
	CMOVE 	bpl1mod,SCR_BPLMOD
	CMOVE 	bpl2mod,SCR_BPLMOD

	CWAIT	P0_DIW_V-1,$7		;Additional time for altering Copperlist

P0_CL_Bpl:				;Bitplane pointers playfield 1
	CMOVE	bpl1pth,$0
	CMOVE	bpl1ptl,$0
	CMOVE	bpl2pth,$0
	CMOVE	bpl2ptl,$0
	
	IFNE	DOT_BLUR_EFFECT
P0_CL_Bpl_Blur:				;Bitplane pointers playfield 1
	CMOVE	bpl3pth,$0
	CMOVE	bpl3ptl,$0
	CMOVE	bpl4pth,$0
	CMOVE	bpl4ptl,$0
	ENDC

P0_CL_Cols:				;PF1 (stars) is 8 colours
	CMOVE	tmpcolor00,$000
	CMOVE	color01,$000
	CMOVE	color02,$000
	CMOVE	color03,$000
	IFNE 	DOT_BLUR_EFFECT
	CMOVE	color04,$000
	CMOVE	color05,$000
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
	ENDC

	; Trigger copper interrupt
	IFNE FW_IRQ_TYPE_COPPER
		IF SCANLINE_EOF>255
			CWAIT	255,$df
		ENDIF
		CWAIT	(SCANLINE_EOF&$ff),$df
		CMOVE	intreq,INTF_SETCLR|INTF_COPER
	ENDC

	COPPEREND
P0_CL_End:

P0_CL_BPL_OFFSET 		= (P0_CL_Bpl-P0_CL_Phys)
	IFNE	DOT_BLUR_EFFECT
P0_CL_BPL_BLUR_OFFSET 		= (P0_CL_Bpl_Blur-P0_CL_Phys)
	ENDC
P0_CL_COL_OFFSET 		= (P0_CL_Cols-P0_CL_Phys)
P0_CL_SIZE 			= (P0_CL_End-P0_CL_Phys)

					
*****************************************************************************
*****************************************************************************
*****************************************************************************

; Map screens to shared chipmem buffers
CUR_CHIP_BUF set FW_Chip_Buffer_1

DOT_Scr_Front		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+SCR_BUF_TOTALSIZE

DOT_Scr_Back1_Draw	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+SCR_BUF_TOTALSIZE

DOT_Scr_Back2_Clear	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+SCR_BUF_TOTALSIZE

DOT_Scr_Back3_Prev	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+SCR_BUF_TOTALSIZE

DOT_Scr_Back4_Prev	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+SCR_BUF_TOTALSIZE

*****************************************************************************

