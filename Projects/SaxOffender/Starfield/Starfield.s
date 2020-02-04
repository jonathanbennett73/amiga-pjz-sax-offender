*****************************************************************************

; Name			: Starfield.s
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com)
; Description		: Starfield.
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

; Time this frame should start in the full demo, to handle faster amigas and
; timing issues. As this is OCS timings should be based on A500
	IFNE _INTROWRAPPER
FRAME_START = $8ef
	ELSE
FRAME_START = 1
	ENDC

; Additional external symbols 
	xref	FW_CheckUserQuitSignal_A6
	xref	FW_ClearBuffer_CPU
	xref	FW_CopyBuffer_CPU
	xref	FW_GetFrame
	xref	FW_InitCopperBplPtrs
	xref	FW_InitCopperColsFromPalette
	xref	FW_IsFrameOver
	xref	FW_SetBaseCopperAndLev3Irq_A6
	xref	FW_SetBaseLev3Irq
	xref	FW_SetCopper_A6
	xref	FW_SetCopperAndLev3Irq_A6
	xref	FW_WaitFrame
	xref	FW_WaitRaster_A6
	xref 	FW_VBlankProxy

	xref	LIB_RGB12_Interpolate_Fast
	xref	LIB_RGB12_Interpolate_Fast_Palette
	xref	LIB_NRV2S_Depack

	xref	FW_Chip_Buffer_1
	xref	BPL_Zoom_Pattern


*****************************************************************************

	SECTION	Starfield_PublicCode,CODE	;Code section in Public memory

*****************************************************************************
*** Display Window ***

; Photon:
; Best practice is to start with DDF 38,d0 and DIW 2c81,2cc1 and modify these 
; symmetrically and always in whole 16px steps for compatibility.
; Note DDF start of less than 30 and you start to lose sprites.

DIW_V			=	$2c	;Hardware Vstart ($2c normal, $24 overscan)
DIW_H			=	$81	;Hardware Hstart ($81 normal, $71 overscan)
DIW_WIDTH		=	320	;Pixels		 (multiple of 16, 320 normal, 352 overscan)
DIW_HEIGHT		=	256	;Lines		 (256 normal PAL, 272 overscan)

DDF_H			=	$81	;Hardware Hstart ($81 normal, $71 overscan)
DDF_WIDTH		=	320	;Pixels		 (320 normal pal, 352 overscan)
DDF_BYTEWIDTH		=	DDF_WIDTH/8	;Bytes
DDF_WORDWIDTH		=	DDF_BYTEWIDTH/2	;Words

SCANLINE_EOF		=	DIW_V+DIW_HEIGHT	; Safe to starting clearing after this scanline

*****************************************************************************

*** Screen Definitions ***

;The visible screen area and back buffer (the stars)
BPL_BUF_WIDTH		=	320
BPL_BUF_BYTEWIDTH	=	BPL_BUF_WIDTH/8		;Bytes
BPL_BUF_WORDWIDTH	=	BPL_BUF_BYTEWIDTH/2	;Words
BPL_BUF_HEIGHT		=	256	;Lines
BPL_BUF_NUMPLANES	=	2	; 3bpl/8 cols
BPL_BUF_NUMCOLS 	= 	(1<<BPL_BUF_NUMPLANES)
BPL_BUF_SIZE		=	BPL_BUF_BYTEWIDTH*BPL_BUF_HEIGHT
BPL_BUF_TOTALSIZE	=	BPL_BUF_SIZE*BPL_BUF_NUMPLANES

; The logo. Intially in 1 bpl along with stars in dualplayfield
; Then stars fade out. Switch to standard 4 bpl
BPL_LOGO_BUF_WIDTH	=	320
BPL_LOGO_BUF_BYTEWIDTH	=	BPL_LOGO_BUF_WIDTH/8		;Bytes
BPL_LOGO_BUF_WORDWIDTH	=	BPL_LOGO_BUF_BYTEWIDTH/2	;Words
BPL_LOGO_BUF_HEIGHT	=	256	;Lines
BPL_LOGO_BUF_NUMPLANES	=	4	; 4bpl/16 cols
BPL_LOGO_BUF_NUMCOLS 	= 	(1<<BPL_LOGO_BUF_NUMPLANES)
BPL_LOGO_BUF_SIZE	=	BPL_LOGO_BUF_BYTEWIDTH*BPL_LOGO_BUF_HEIGHT
BPL_LOGO_BUF_TOTALSIZE	=	BPL_LOGO_BUF_SIZE*BPL_LOGO_BUF_NUMPLANES

P0_PAL_NUMCOLS		=	16	;Palette code works with 16 colours in CL

; In P0 the 16 color logo is interleaved
P0_BPL_BPL1MOD 		= 	(BPL_LOGO_BUF_BYTEWIDTH-DDF_BYTEWIDTH)+(BPL_LOGO_BUF_BYTEWIDTH*(BPL_LOGO_BUF_NUMPLANES-1))
P0_BPL_BPL2MOD 		= 	(BPL_LOGO_BUF_BYTEWIDTH-DDF_BYTEWIDTH)+(BPL_LOGO_BUF_BYTEWIDTH*(BPL_LOGO_BUF_NUMPLANES-1))
P0_BPL_BPLMOD_REPTLINE	=	-DDF_BYTEWIDTH

; In P1 the starfield (PF1) is interleaved. The picture outline is 1bpl so not interleaved.
P1_BPL_BPL1MOD 		= 	(BPL_BUF_BYTEWIDTH-DDF_BYTEWIDTH)+(BPL_BUF_BYTEWIDTH*(BPL_BUF_NUMPLANES-1))
P1_BPL_BPL2MOD 		= 	0

COL_TRANS_IN		=	$134	;Ending background color of previous routine

*****************************************************************************

STF_XOFFSET		=	BPL_BUF_WIDTH/2
STF_YOFFSET		=	BPL_BUF_HEIGHT/2
STF_ZOFFSET		=	64	;must be >= 0
STF_Z_NUMENTRIES	=	1024	;power of 2, num entries in reciprocal table

STF_NUMSTARS set 344			; Max possible stars, no music
	IF FW_MUSIC_TYPE=1
STF_NUMSTARS set STF_NUMSTARS-7
	ENDIF
	IF FW_MUSIC_TYPE=3
STF_NUMSTARS set STF_NUMSTARS-66	
	ENDIF


*****************************************************************************

;Zoom pattern is 352 width so have to alter offsets to center in our 320x256 window
ZOOM_PATTERN_X_BYTEOFFSET = 2	;352 vs 320 = 2 extra bytes
ZOOM_PATTERN_Y_BYTEOFFSET = 6	;352 vs 256 high = 6 extra bytes to work for a 256 high screen

ZOOM_BPLCON0 = $4200	;4bpl screen


*****************************************************************************
* Start the effect (usually used for setting up the copper)
* This is called each time the effect is started
* IN:		a0(text message ptr),d0(pre pause)
* OUT:		
* TRASHED:	d0
*****************************************************************************

	IFEQ _INTROWRAPPER
	xdef	SubPartStart
SubPartStart:	
	ENDC

	xdef	STF_Start
STF_Start:
	movem.l	d2-d7/a2-a6,-(sp)	;save
	lea	STF_ControllerScript(pc),a0

	lea	_custom,a6
	lea	Controller_Info(pc),a5

	;Save script pointer
	move.l	a0,CTRL_SCRIPT_PTR(a5)

	; Run intro startup precalc if not already done (only runs once)
	bsr.s	SubPartPreCalc_IntroStart	;T:None

	; Save script ptr and reset everything to defaults
	bsr.s	P0_Init		;I:a5-a6, T:d0-d1/a0-a1

	;Start main loop
	bsr	P0_MainLoop

	; Restore copper for framework
	;jsr	FW_SetBaseCopperAndLev3Irq_A6
	;jsr	FW_WaitFrame

	;Just restore the base Irq and leave the screen/copper alone so
	;we can go straight into next effect
	jsr	FW_SetBaseLev3Irq

	movem.l	(sp)+,d2-d7/a2-a6	;restore
	rts


*****************************************************************************
* To be called during INTRO startup to do any permanent precalc required.
* i.e. the buffers used are unique for this effect.
* IN:		
* OUT:		
* TRASHED:	
*****************************************************************************

	;Only export one symbol depending if in demo or standalone mode
	IFEQ _INTROWRAPPER
	xdef	SubPartPreCalc_IntroStart
	ELSE
	xdef	STF_PreCalc_IntroStart
	ENDC
	
SubPartPreCalc_IntroStart:	
STF_PreCalc_IntroStart:
	tst.w	Controller_Info+CTRL_PRECALC_INTROSTART_DONE
	bne.s	.exit			;already done

	movem.l	d0-d7/a0-a6,-(sp)	;save

	;Start
	bsr	Calc_PreMult_ScreenHeight
	bsr	RandomizeStars
	bsr	Calc_Perspective_ReciprocalTable
	bsr	Calc_Perspective_FadeTable

	IFEQ _INTROWRAPPER
	;Done by ExplodeVector.s in full demo
	bsr	Calc_Zoom_Bitmaps
	ENDC

	;End

	move.w	#1,Controller_Info+CTRL_PRECALC_INTROSTART_DONE

	movem.l	(sp)+,d0-d7/a0-a6	;restore
.exit:
	rts


*****************************************************************************
* Called everytime the routine is started. Can be used for one time setups or
* to reset defaults each time routine is called. Will also call
* _PreCalc if it's not already been done (for safety)
* Use for one-time setup
* IN:		a0, script ptr
*		a5, Controller_Info
* OUT:		
* TRASHED:	all
*****************************************************************************

P0_Init:
	movem.l	d2-d7/a2-a4,-(sp)

	;stop all time based effects
	moveq	#0,d0
	move.w	d0,CTRL_FINISHED(a5)
	move.w	d0,CTRL_FRAME_COUNT(a5)	
	move.w	d0,CTRL_PAUSE_COUNTER(a5)
	move.w	d0,CTRL_PALETTE_ACTIVE(a5)

	; Set default zoom
	moveq	#0,d0
	moveq	#0,d1
	bsr	Controller_FX_Zoom	;I:a5,d0,d1

	;Default sine values (everything else assumed 0)
	move.w	#SINE_NUMENTRIES/2,CTRL_SINE_Y(a5)
	move.w	#SINE_NUMENTRIES/3,CTRL_SINE_Z(a5)
	move.w	#3,CTRL_SINE_X_STEP(a5)
	move.w	#2,CTRL_SINE_Y_STEP(a5)
	move.w	#1,CTRL_SINE_Z_STEP(a5)

	;Start black
	;lea	PAL_AllBlack(pc),a0
	;Start in transition color
	lea	PAL_Logo_Full_TransIn1(pc),a0
	moveq	#0,d0
	bsr	Controller_FX_Palette	;I:d0/a2/a5 T:d0/a2/a3

	;Clear all data, assumes we are coming from Miami routine which 
	;has a transition screen such that we can clear the screen without
	;seeing it happen
	bsr	Clear_ScreenBuffers_CPU	;d0-d7/a0-a4

	; Init copper pointers
	bsr	P0_CL_InitPtrs		;d0-d2/a0-a1

	bsr	Write_Palette_To_Copper	;I:a5, T:a0-a1

	IFEQ FW_RMB_QUIT_SECTION
	;Wait for A500 timing
.frameloop:	
	move.w	#FRAME_START,d0
	jsr	FW_IsFrameOver		;I:d0, O:d0 
	beq.s	.frameloop
	ENDC

	;Initialise our new copper and irq (starts the routine)
	lea	P0_CL_Phys,a0
	lea	P0_Lev3Irq(pc),a1	
	jsr	FW_SetCopperAndLev3Irq_A6

	;Let 2 frames pass so we are sure new CL is in effect before 
	jsr	FW_WaitFrame
	jsr	FW_WaitFrame

	;Copy/depack some logos just in time
	;Have to tune script so that you don't see this with FX_PAUSE
	lea	BPL_Logo_Full_Source(pc),a0
	lea	BPL_Logo_4Bpl,a1
	jsr	LIB_NRV2S_Depack	;I:a0/a1, T:d0-d1/a0-a1

	lea	BPL_Logo_Sil_Source(pc),a0
	lea	BPL_Logo_Sil_1BPL,a1
	jsr	LIB_NRV2S_Depack	;I:a0/a1, T:d0-d1/a0-a1

;debug, check if depacked :)
;.loop	move.w	#$fff,color00+_custom
;	bra.s	.loop

	movem.l	(sp)+,d2-d7/a2-a4

	rts


*****************************************************************************
* Loop outside of the main irq. 
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

*****************************************************************************

; Used when just the logo is visible (no stars at all running)
P0_Lev3Irq:
	TIMERON
	movem.l	d0-d7/a0-a6,-(sp)

	lea	_custom,a6
	
	;btst	#5,INTREQR+1(a6)	;Is it a vblank interrupt? Could be a Copper/Blitter
	;beq.s	.notvb

	; Read new script lines and perform
	lea	Controller_Info(pc),a5	;controller info
	addq.w	#1,CTRL_FRAME_COUNT(a5)	;Update local frame count

	bsr	Controller_ReadCommands	;Read new commands
	bsr	Controller_Perform	;Do any ongoing time-based effects (fades)
	bsr	Do_Copper_Zoom

	bsr	Write_Palette_To_Copper	;I:a5, T:a0-a1

	jsr	FW_VBlankProxy		;T:d0-d7/a0-a4

	;Reset interrupt
	IFNE FW_IRQ_TYPE_COPPER
		moveq	#INTF_COPER,d0
	ELSE
		moveq	#INTF_VERTB,d0
	ENDC
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)			;A4000 compat
.notvb:	
	movem.l	(sp)+,d0-d7/a0-a6
	TIMEROFF
	rte


*****************************************************************************

; Used when stars running with 1 bpl logo in front
P1_Lev3Irq:	
	TIMERON
	movem.l	d0-d7/a0-a6,-(sp)

	lea	_custom,a6
	
	;btst	#5,INTREQR+1(a6)	;Is it a vblank interrupt? Could be a Copper/Blitter
	;beq.s	.notvb

	bsr	P0_Clear_Screen_A6
	
	; Read new script lines and perform
	lea	Controller_Info(pc),a5	;controller info
	addq.w	#1,CTRL_FRAME_COUNT(a5)	;Update local frame count

	bsr	Controller_ReadCommands	;Read new commands
	bsr	Controller_Perform	;Do any ongoing time-based effects (fades)

	; Transform and plot stars
	bsr	Update_Stars		;T:d0-d7/a0-a1
	bsr	Perspective_And_Plot	;T:d0-d7/a0-a5
	lea	Controller_Info(pc),a5	;restore controller info

	;On A4000 it can finish before/during line 44, force a blitwait here which
	;Takes us 1/3 way into frame so safe to change copper values.
	WAITBLIT_A6

	bsr	Write_Palette_To_Copper	;I:a5, T:a0-a1
	bsr	P0_TripleBufferSwap	;Swap buffers

	jsr	FW_VBlankProxy		;T:d0-d7/a0-a4

	;Reset interrupt
	IFNE FW_IRQ_TYPE_COPPER
		moveq	#INTF_COPER,d0
	ELSE
		moveq	#INTF_VERTB,d0
	ENDC
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)			;A4000 compat
.notvb:	
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
	;Init. physical & logical screen addresses
	lea	BPL_Phys,a0
	move.l	a0,BPL_PF1_Phys_Ptr		;Physical screen adr

	lea	BPL_Log1,a1
	move.l	a1,BPL_PF1_Log1_Ptr		;Intermediate screen adr

	lea	BPL_Log2,a1
	move.l	a1,BPL_PF1_Log2_Ptr		;Logical screen adr

	;Init blank bitplane
	lea	P1_CL_PF2_Bpl,a0	;copper bpl pointer block
	moveq	#1,d0
	move.l	#BPL_Logo_Blank_1BPL,d1	;in d1 for InitCopperBplPtrs
	move.l 	#BPL_LOGO_BUF_SIZE,d2	;non-interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	; Init P1 copper list with 4 bpl 
	lea	P0_CL_Bpl,a0	;copper bpl pointer block
	moveq	#4,d0			;4 bpls
	move.l	#BPL_Logo_4Bpl,d1	;in d1 for InitCopperBplPtrs
	moveq 	#BPL_LOGO_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

        ; Just run double buffer swap to update the copper bitmap pointers
	;bsr	P0_TripleBufferSwap

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
* Creates table of screen height * screenwidth bytes (non-interleaved) or
* screen height * (screenwidth bytes * num bitplanes) (interleaved)
* Use for one-time setup
* IN:		
* OUT:		
* TRASHED:	d0-d3/a0
*****************************************************************************

Calc_PreMult_ScreenHeight:

	;interleaved
	lea	Mult_Height_PF1_NumPlanes,a0
	move.w	#BPL_BUF_BYTEWIDTH*BPL_BUF_NUMPLANES,d1
	
	;d1 = byte width per line
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
* Loads the current colors into the current copperlist
* IN:		a5, VEC_Controller_Info
* OUT:
* TRASHED:	?
*****************************************************************************

Do_Copper_Zoom:
	lea	Controller_Info(pc),a5

	; Active?
	tst.w	CTRL_ZOOM_ACTIVE(a5)
	beq.s	.exit

	;if no live zoom then reset everything (required because of double buffer delay)
	move.w	CTRL_ZOOM(a5),d6	;0 is nozoom
	bne.s	.zooming

	;TODO:move.l	VEC_Work_CL_Ptr(pc),a0
	lea	P0_CL_Phys,a0
	lea	P0_CL_PATTERN_OFFSET+CL_PATTERN_BPL1MOD+2(a0),a1

	; Clear vertical bplcon0 values (default is vector visible as normal)
	move.w	#BPL_LOGO_BUF_HEIGHT-1,d1
	move.w	#P0_BPL_BPL1MOD,d0	
	move.w	#CL_PATTERN_SIZEOF,a2
.clearloop:
	move.w	d0,(a1)			;bpl1mod
	move.w	d0,4(a1)		;bpl2mod
	add.l	a2,a1
	dbf	d1,.clearloop	

	;Set vertial scaling to 0
	;TODO:move.l	VEC_Work_CL_Ptr(pc),a0
	lea	P0_CL_Phys,a0
	lea	P0_CL_BPL_OFFSET(a0),a0
	moveq	#BPL_LOGO_BUF_NUMPLANES,d0
	move.l	#BPL_Logo_4Bpl,d1
	moveq 	#BPL_LOGO_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo
.exit:
	rts


.zooming
	;d7=zoom level
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
	
	;TODO:move.l	VEC_Work_CL_Ptr(pc),a1
	lea	P0_CL_Phys,a1

	lea	P0_CL_PATTERN_OFFSET+CL_PATTERN_BPL1MOD+2(a1),a1
	move.w	#P0_BPL_BPLMOD_REPTLINE,d2	;show previous
	move.w	#P0_BPL_BPL1MOD,d3	;show next

	move.w	#(BPL_LOGO_BUF_HEIGHT/16)-1,d6	;number of 16bit words in the height
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
	move.w	d1,(a1)			;bpl1mod
	move.w	d1,4(a1)		;bpl2mod
	add.l	a2,a1			;next copper entry
	dbf	d5,.xloop2		;next bit
	dbf	d6,.yloop2

	;d4 = total number of blank lines for entire screen, divide by two to get y offset
	;note:interleaved
	lsr.w	#1,d4
	mulu	#BPL_LOGO_BUF_BYTEWIDTH*BPL_LOGO_BUF_NUMPLANES,d4	;pointer to first line to show

	;Increase the y offset by this many lines
	;TODO: move.l	VEC_Work_CL_Ptr(pc),a0
	lea	P0_CL_Phys,a0

	lea	P0_CL_BPL_OFFSET(a0),a0
	moveq	#BPL_LOGO_BUF_NUMPLANES,d0
	
	;TODO:move.l	VEC_Work_Ptr(pc),d1		;in d1 for InitCopperBplPtrs
	move.l	#BPL_Logo_4Bpl,d1

	add.l	d4,d1			
	moveq 	#BPL_LOGO_BUF_BYTEWIDTH,d2	;interleaved
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	rts

*****************************************************************************

; Screen buffer pointers
BPL_PF1_Phys_Ptr:		dc.l	0	;physical
BPL_PF1_Log1_Ptr:		dc.l	0	;intermediate
BPL_PF1_Log2_Ptr:		dc.l	0	;logical


*****************************************************************************
* Swaps the screens
* IN:		
* OUT:		
* TRASHED:	d0-d1/a0
*****************************************************************************

P0_TripleBufferSwap:

	;playfield 1
	lea	BPL_PF1_Phys_Ptr(pc),a0
	movem.l	(a0),d0-d2
	movem.l	d1-d2,(a0)
	move.l 	d0,8(a0)

	;d1=new physical

	lea 	P1_CL_PF1_Bpl+2,a0	;Adr of copper pointers PTH
	moveq	#BPL_BUF_BYTEWIDTH,d0

	REPT	BPL_BUF_NUMPLANES
	swap	d1			;Swap high & low words
	move.w	d1,(a0)			;High ptr
	swap	d1			;Swap high & low words
	move.w	d1,4(a0)		;Low ptr
	addq.l	#8,a0			;Next set of ptrs
	add.l	d0,d1			;Next bitplane (interleaved)
	ENDR

	rts

*****************************************************************************
* Clears the work buffer screen. Every frame.
* IN:		A6(Custom)
* OUT:		
* TRASHED:	d0
*****************************************************************************

P0_Clear_Screen_A6:

	; Clear stars screen
	WAITBLIT_NASTY_A6
	move.l	#$01000000,bltcon0(a6)
	move.l	BPL_PF1_Log2_Ptr(pc),bltdpth(a6)
	move.w	#0,bltdmod(a6)
	move.w	#BPL_BUF_HEIGHT*64+(BPL_BUF_WORDWIDTH*BPL_BUF_NUMPLANES),bltsize(a6)

	rts

*****************************************************************************
* Clears front and back buffer. Done at startup to clear everything.
* IN:
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

Clear_ScreenBuffers_CPU:
	movem.l	d2-d7/a2-a6,-(sp)

	;Clear with CPU to avoid hassle of clearing 5*256*40 in OCS bltsize
	; a0 = buffer to clear
	; d0 = size in words to clear
	; trashes all so restore a6
	lea	BPL_Phys,a0
	move.l	#BPL_BUF_TOTALSIZE/2,d0
	jsr	FW_ClearBuffer_CPU	;I:d0/a0, T:d0-d7/a0-a4

	lea	BPL_Log1,a0
	move.l	#BPL_BUF_TOTALSIZE/2,d0
	jsr	FW_ClearBuffer_CPU	;I:d0/a0, T:d0-d7/a0-a4

	lea	BPL_Log2,a0
	move.l	#BPL_BUF_TOTALSIZE/2,d0
	jsr	FW_ClearBuffer_CPU	;I:d0/a0, T:d0-d7/a0-a4

	lea	BPL_Logo_Blank_1BPL,a0
	move.l	#BPL_LOGO_BUF_SIZE/2,d0
	jsr	FW_ClearBuffer_CPU	;I:d0/a0, T:d0-d7/a0-a4

	movem.l	(sp)+,d2-d7/a2-a6

	rts


*****************************************************************************

	RSRESET
CTRL_PAUSE_COUNTER:			rs.w 1		;Pause counter, 0=inactive
CTRL_FRAME_COUNT			rs.w 1		;Local (effect) frame counter
CTRL_ISFRAMEOVER_COUNTER:		rs.w 1		;Waiting for frame, 0=inactive
CTRL_ISMASTERFRAMEOVER_COUNTER:		rs.w 1		;Waiting for frame, 0=inactive
CTRL_SCRIPT_PTR:			rs.l 1		;Script Ptr
CTRL_FINISHED:				rs.w 1		;1 if quitting
CTRL_PRECALC_INTROSTART_DONE		rs.w 1		;1 if intro precalc done
CTRL_PALETTE_LOAD_FLAG			rs.w 1		;set to >0 to force palette load x times
CTRL_PALETTE_ACTIVE:			rs.w 1		;Palette change active
CTRL_PALETTE_PTR:			rs.l 1		;src Palette ptr (16 words of colors)
CTRL_PALETTE_COUNTER:			rs.w 1		;Palette counter, speed
CTRL_PALETTE_SPEED:			rs.w 1		;How often to update, higher is slower, 0 = instant
CTRL_PALETTE_STEP			rs.w 1		;Current step to interpolate between current color and final 0-15
CTRL_SINE_X:				rs.w 1
CTRL_SINE_Y:				rs.w 1
CTRL_SINE_Z:				rs.w 1
CTRL_SINE_X_SPEED:			rs.w 1
CTRL_SINE_Y_SPEED:			rs.w 1
CTRL_SINE_Z_SPEED:			rs.w 1
CTRL_SINE_X_STEP:			rs.w 1
CTRL_SINE_Y_STEP:			rs.w 1
CTRL_SINE_Z_STEP:			rs.w 1
CTRL_SINE_X_COUNT:			rs.w 1
CTRL_SINE_Y_COUNT:			rs.w 1
CTRL_SINE_Z_COUNT:			rs.w 1
CTRL_ZOOM:				rs.w 1		;current zoom level
CTRL_ZOOM_ACTIVE:			rs.w 1		;move active, final x,y,z,speed
CTRL_ZOOM_REQ:				rs.w 1		;Final zoom level
CTRL_ZOOM_SPEED:			rs.w 1
CTRL_ZOOM_VEL:				rs.w 1
CTRL_SIZE:				rs.w 0

	EVEN
Controller_Info:
	dcb.b	CTRL_SIZE,0			;Zeroed
	EVEN

FX_END_FLAG			= 	0
FX_PAUSE_FLAG			=	1
FX_GET_MASTERFRAME_FLAG		=	2
FX_ISMASTERFRAMEOVER_FLAG	=	3
FX_ISFRAMEOVER_FLAG		=	4
FX_SCRIPTJMP_FLAG		=	5
FX_PALETTE_FLAG			=	6
FX_SET_SINE_XY_SPEED_FLAG	=	7
FX_SET_STF_FLAG			=	8
FX_SET_4BPL_LOGO_FLAG		=	9
FX_ZOOM_FLAG			=	10
FX_SET_STF_AND_1BPL_LOGO_FLAG	=	11

FX_PALETTE	MACRO
		dc.w	FX_PALETTE_FLAG
		dc.w	\1		;speed
		dc.l	\2		;new palette
		ENDM

FX_PAUSE	MACRO
		dc.w	FX_PAUSE_FLAG
		dc.w	\1		;frames to pause
		ENDM

FX_ISMASTERFRAMEOVER	MACRO
		dc.w	FX_ISMASTERFRAMEOVER_FLAG
		dc.w	\1		;frames wait for (global timing)
		ENDM

FX_ISFRAMEOVER	MACRO
		dc.w	FX_ISFRAMEOVER_FLAG
		dc.w	\1		;frames wait for (local timing)
		ENDM

FX_END	MACRO
		dc.w	FX_END_FLAG
		ENDM

FX_GET_MASTERFRAME	MACRO
		dc.w	FX_GET_MASTERFRAME_FLAG
		ENDM

FX_SET_STF	MACRO
		dc.w	FX_SET_STF_FLAG
		ENDM

FX_SET_STF_AND_1BPL_LOGO	MACRO
		dc.w	FX_SET_STF_AND_1BPL_LOGO_FLAG
		ENDM

FX_SET_4BPL_LOGO	MACRO
		dc.w	FX_SET_4BPL_LOGO_FLAG
		ENDM

FX_SCRIPTJMP	MACRO
		dc.w	FX_SCRIPTJMP_FLAG
		dc.l	\1		;new script address
		ENDM

FX_SET_SINE_XY_SPEED	MACRO
		dc.w	FX_SET_SINE_XY_SPEED_FLAG
		dc.w	\1,\2,\3,\4	;new speed (must be even)
					;x speed, x step, y speed, y step
		ENDM

FX_ZOOM		MACRO
		dc.w	FX_ZOOM_FLAG
		dc.w	\1		;new zoom (0-1023)
		dc.w	\2		;speed
		ENDM


*****************************************************************************
* Runs the controller script.
* Note the commands are read until a FX_PAUSE command is reached. So beware
* of hogging the CPU with too many commands at once.
* IN:		a6, _custom
*		a5, Controller_Info
* OUT:		
* TRASHED:	
*****************************************************************************

Controller_ReadCommands:

	;First check if any types of pause/wait are happening
	move.w	CTRL_PAUSE_COUNTER(a5),d0
	bne.s	.pausing
	
	move.w	CTRL_ISFRAMEOVER_COUNTER(a5),d0
	bne.s	.isframeover

	move.w	CTRL_ISMASTERFRAMEOVER_COUNTER(a5),d0
	bne.s	.ismasterframeover


	; Get current script pointer
.readcmd:
	move.l	CTRL_SCRIPT_PTR(a5),a4
.loop:
	move.w	(a4)+,d0

	; subroutines need to preserve a4/a5
	cmpi.w	#FX_PALETTE_FLAG,d0
	beq	.fx_pallete

	cmpi.w	#FX_SET_SINE_XY_SPEED_FLAG,d0
	beq	.fx_set_sine_xy_speed

	cmpi.w	#FX_SCRIPTJMP_FLAG,d0
	beq	.fx_scriptjmp

	cmpi.w	#FX_SET_STF_FLAG,d0
	beq	.fx_set_stf

	cmpi.w	#FX_SET_4BPL_LOGO_FLAG,d0
	beq	.fx_set_4bpl_logo

	cmpi.w	#FX_SET_STF_AND_1BPL_LOGO_FLAG,d0
	beq	.fx_set_stf_and_1bpl_logo

	cmpi.w	#FX_PAUSE_FLAG,d0
	beq.s	.fx_pause

	cmpi.w	#FX_ISMASTERFRAMEOVER_FLAG,d0
	beq.s	.fx_ismasterframeover

	cmpi.w	#FX_ISFRAMEOVER_FLAG,d0
	beq.s	.fx_isframeover

	cmpi.w	#FX_ZOOM_FLAG,d0
	beq	.fx_zoom

	cmpi.w	#FX_GET_MASTERFRAME_FLAG,d0
	beq	.fx_get_masterframe

	;assume end of script, don't save ptr
	move.w	#1,CTRL_FINISHED(a5)	;exit
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
	move.w	(a4)+,d0
	move.w	d0,CTRL_PAUSE_COUNTER(a5)
	move.l	a4,CTRL_SCRIPT_PTR(a5)
	rts					;exit when starting pause

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

.fx_pallete:
	move.w	(a4)+,d0		;Speed
	move.l	(a4)+,a0		;New pallete
	bsr	Controller_FX_Palette	
	bra	.loop

.fx_set_sine_xy_speed:
;	movem.w	(a4)+,d0-d3		;speed,step,speed,step
;	movem.w	d0-d3,STF_SINE_X_SPEED(a5)	;must be in same order
	;move.w	d0,STF_SINE_X_SPEED(a2)
	;move.w	d1,STF_SINE_X_STEP(a2)
	;move.w	d2,STF_SINE_Y_SPEED(a2)
	;move.w	d3,STF_SINE_Y_STEP(a2)
	bra	.loop

.fx_set_stf:
	lea	P1_CL_PF2_Bpl,a0	;copper bpl pointer block
	moveq	#1,d0
	move.l	#BPL_Logo_Blank_1BPL,d1	;in d1 for InitCopperBplPtrs
	move.l 	#BPL_LOGO_BUF_SIZE,d2	;1bpl only
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	lea	P1_CL_Phys,a0
	lea	P1_Lev3Irq(pc),a1
	jsr	FW_SetCopperAndLev3Irq_A6	

	bra	.loop

.fx_set_stf_and_1bpl_logo:
	lea	P1_CL_PF2_Bpl,a0	;copper bpl pointer block
	moveq	#1,d0
	move.l	#BPL_Logo_Sil_1BPL,d1	;in d1 for InitCopperBplPtrs
	move.l 	#BPL_LOGO_BUF_SIZE,d2	;1bpl only
	jsr	FW_InitCopperBplPtrs	;a0=bpl ptr block, d0=numplanes, d1=scr buffer, d2=modulo

	lea	P1_CL_Phys,a0
	lea	P1_Lev3Irq(pc),a1
	jsr	FW_SetCopperAndLev3Irq_A6
	bra	.loop

.fx_set_4bpl_logo:
	lea	P0_CL_Phys,a0
	lea	P0_Lev3Irq(pc),a1
	jsr	FW_SetCopperAndLev3Irq_A6
	bra	.loop

.fx_zoom:
	movem.w	(a4)+,d0-d1		;w,speed
	bsr	Controller_FX_Zoom
	bra	.loop	

	rts

.fx_get_masterframe:
	jsr	FW_GetFrame		;Get master frame count
	tst.w	d0			;Set breakpoint here
	move.w	CTRL_FRAME_COUNT(a5),d0
	rts

.fx_scriptjmp:
	move.l	(a4)+,a4		;New script
	move.l	a4,CTRL_SCRIPT_PTR(a5)
	bra	.loop


*****************************************************************************
* Performs any time-based controller routines.
* IN:		a5, Controller_Info
*		a0, current object
* OUT:
* TRASHED:	d0-d7/a2-a5
*****************************************************************************

Controller_Perform:

; TODO: Change to bitflag


.pal:
	tst.w	CTRL_PALETTE_ACTIVE(a5)
	beq.s	.zoom
	bsr.s	Controller_FX_Palette_Perform
.zoom:
	tst.w	CTRL_ZOOM_ACTIVE(a5)
	beq.s	.exit
	bsr	Controller_FX_Zoom_Perform
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
	; If speed is 0 just instastransform
	tst.w	d0
	bne.s	.palette

	move.w	d0,CTRL_PALETTE_ACTIVE(a5)	;disable change, d0 is zero here
	move.w	#CL_NUM_BUFFERS,CTRL_PALETTE_LOAD_FLAG(a5)	;request copper loads immediately twice for double buffer CL issues
	lea	PAL_Current(pc),a1

	REPT	P0_PAL_NUMCOLS/2		;/2 for number of longs
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

	REPT	P0_PAL_NUMCOLS/2		;/2 for number of longs
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
	moveq	#P0_PAL_NUMCOLS,d0		;number of colours to touch
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
* 		d0-d1, new w,speed
* OUT:		
* TRASHED:	d0-d1
*****************************************************************************

Controller_FX_Zoom:
	;min/max zoom level is not limited here. It is limited in the copper routine

	tst.w	d1
	bne.s	.slow
	move.w	d0,CTRL_ZOOM(a5)		;change zoom immediately
	move.w	d0,CTRL_ZOOM_ACTIVE(a5)		;change zoom immediately
	clr.w	CTRL_ZOOM_VEL(a5)		;reset velocities - hard stop
	rts
.slow:
	;1 because when we deactivate we use this as a count for coping
	;with doublebuffered copper
	move.w	#1,CTRL_ZOOM_ACTIVE(a5)	;enable change
	movem.w	d0-d1,CTRL_ZOOM_REQ(a5)	;store params in controller info
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
	subq.w	#1,CTRL_ZOOM_ACTIVE(a5)	;finished (double buffered)
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

PAL_AllBlack:
	dcb.w	P0_PAL_NUMCOLS,0

; Master palette poked into the copperlist each frame
PAL_Current:		
	dcb.w	P0_PAL_NUMCOLS,0

; This holds the source palette used during transitions in DOT_FX_PALETTE. The
; source value is interpolated from this value to the destination value + step size.
PAL_Current_Src:		
	dcb.w	P0_PAL_NUMCOLS,0


*****************************************************************************
* Loads the current colors into the current copperlist if requested.
* IN:		a5, Controller_Info
* OUT:
* TRASHED:	a0-a1
*****************************************************************************

Write_Palette_To_Copper:
	;Load flag may be > 1 if using double buffered CL or changing
	;between CLs
	tst.w	CTRL_PALETTE_LOAD_FLAG(a5)
	beq	.exit
	subq.w	#1,CTRL_PALETTE_LOAD_FLAG(a5)

	;Change colours in both CLs so we have an easier time during transitions

	;main - p0/STF and Pic CL
	lea	PAL_Current(pc),a0	;start at col00
	lea	P1_CL_Cols+2,a1	;start at col00 in copperlist
	REPT	P0_PAL_NUMCOLS
	move.w	(a0)+,(a1)
	addq.l	#4,a1			;next color, skipping cmove
	ENDR

	;main (P1 CL) - pic only
	lea	PAL_Current(pc),a0	;start at col00
	lea	P0_CL_Cols+2,a1	;start at col00 in copperlist
	REPT	P0_PAL_NUMCOLS
	move.w	(a0)+,(a1)
	addq.l	#4,a1			;next color, skipping cmove
	ENDR

.exit:
	rts


*****************************************************************************
* Does some precalcs for perspective to change two divs into 2 muls.
* IN:		
* OUT:		
* TRASHED:	d0-d3/a0
*****************************************************************************
; Our normal perspective is
; (256(scale) * x) / z + 256(screen dist)
; example x = 100, z = 500
; x * 256 / 500 = 51
; example x = 50, z = 2000
; x * 256 / 2000 = 6

; In our table z 500 = 32768 / 500 = 65
; example x = 100, z = 500
; 100 * 65 = 6500, then right shift 7 (/128) = 50
; In our table z 2000 = 32768 / 2000 = 16
; example x = 50, z = 2000
; 50 * 16= 400, then right shift 7 (/128) = 6

Calc_Perspective_ReciprocalTable:

	lea	Perspective_ReciprocalTable,a0

	; If first z entry is 0, use the max value as it breaks div
	move.l	#32767,d2

	IFNE STF_ZOFFSET
		move.w	#STF_ZOFFSET,d0		
		move.w	#(STF_Z_NUMENTRIES)-1,d4	
	ELSE
		move.w	d2,(a0)+		
		moveq	#1,d0			;already stored 0
		move.w	#((STF_Z_NUMENTRIES)-1)-1,d4	
	ENDC
.loop	
	move.l	d2,d1			;32767
	divu	d0,d1			;32767 / z
	move.w	d1,(a0)+
	addq.w	#1,d0			;next z
	dbf	d4,.loop

	rts


*****************************************************************************
* Updates star positions.
* IN:		
* OUT:		
* TRASHED:	d0-d7/a0-a1
*****************************************************************************

Update_Stars:
	lea	Controller_Info(pc),a5
	lea	XYZpts,a0	;rot pts 3d
	lea	Sine_256_Byte(pc),a1

	move.w	CTRL_SINE_X(a5),d3
	move.w	CTRL_SINE_X_COUNT(a5),d0
	subq.w	#1,d0
	bpl.s	.skipupdatex
	move.w	CTRL_SINE_X_SPEED(a5),d0
	add.w	CTRL_SINE_X_STEP(a5),d3
	andi.w	#SINE_OFFSET_MASK,d3
	move.w	d3,CTRL_SINE_X(a5)
.skipupdatex:
	move.b	(a1,d3.w),d3
	ext.w	d3
	asl.w	#3,d3			;make values bigger as 255 is too slow
	move.w	d0,CTRL_SINE_X_COUNT(a5)
	
	move.w	CTRL_SINE_Y(a5),d4
	move.w	CTRL_SINE_Y_COUNT(a5),d0
	subq.w	#1,d0
	bpl.s	.skipupdatey
	move.w	CTRL_SINE_Y_SPEED(a5),d0
	add.w	CTRL_SINE_Y_STEP(a5),d4
	andi.w	#SINE_OFFSET_MASK,d4
	move.w	d4,CTRL_SINE_Y(a5)
.skipupdatey:
	move.b	(a1,d4.w),d4
	ext.w	d4
	asl.w	#3,d4			;make values bigger as 255 is too slow
	move.w	d0,CTRL_SINE_Y_COUNT(a5)

	move.w	CTRL_SINE_Z(a5),d5
	move.w	CTRL_SINE_Z_COUNT(a5),d0
	subq.w	#1,d0
	bpl.s	.skipupdatez
	move.w	CTRL_SINE_Z_SPEED(a5),d0
	add.w	CTRL_SINE_Z_STEP(a5),d5
	andi.w	#SINE_OFFSET_MASK,d5
	move.w	d5,CTRL_SINE_Z(a5)
.skipupdatez:
	move.b	(a1,d5.w),d5
	ext.w	d5
	asr.w	#3,d5			;sine is in 0-128 to 128 range, too fast for z
	move.w	d0,CTRL_SINE_Z_COUNT(a5)


	; Now actually update everything
	move.w	#(STF_Z_NUMENTRIES*2)-2,d6	;keep in range, and even for word access
	move.w	#STF_NUMSTARS-1,d7	;num pts-1
.loop:
	movem.w	(a0),d0-d2		;x,y,z

	;Because we using the full -32768-32767 range we get automatic clipping/wraparound
	;for x and y values
	add.w	d3,d0			;update x
	add.w	d4,d1			;update y

	;Z is kept in 0-1024 (*2 for access recip table in words)
	add.w	d5,d2			;update z
	and.w	d6,d2			;keep in postive range and even

	movem.w	d0-d2,(a0)		;store new x,y,z
	addq.l	#6,a0
	dbf	d7,.loop

	rts


*****************************************************************************
* Updates z values, does perspective and plots.
* IN:		
* OUT:		
* TRASHED:	d0-d7/a0-a5
*****************************************************************************

Perspective_And_Plot_Vars1:
	dc.w	STF_XOFFSET		;d4
	dc.w	STF_YOFFSET		;d5
	dc.w	BPL_BUF_WIDTH	;d6	;clip x max
	dc.w	STF_NUMSTARS-1		;d7
	dc.w	BPL_BUF_HEIGHT	;a5	;clip y max

Perspective_And_Plot_Vars2:
	dc.l	XYZpts			;a0		;rot pts 3d
	dc.l	Mult_Height_PF1_NumPlanes	;a1
	dc.l	Perspective_ReciprocalTable	;a2	
	dc.l	Perspective_FadeTable	;a3

Perspective_And_Plot:
	movem.w	Perspective_And_Plot_Vars1(pc),d4-d7/a5
	movem.l	Perspective_And_Plot_Vars2(pc),a0-a3
	move.l	BPL_PF1_Log1_Ptr(pc),a4

.persloop1:
	movem.w	(a0)+,d0-d2		;get x,y,z

	;x,y in the range -32768-32767
	;z in the range 0-2046 (even only)

	; Y is slightly more likely to clip with our -256-256 range so test first
	muls	(a2,d2.w),d1		;y
	;asr.l	#7,d1			;/128
	;asr.l	#7,d1			;/16384
	asl.l	#2,d1			;swap method of div 16384 :)
	swap	d1
	add.w	d5,d1			;add y offset, check if clipping
	cmp.w	a5,d1
	bhs.s	.nodraw			;unsigned test, higher or same. Traps <0

	muls	(a2,d2.w),d0		;x
	;asr.l	#7,d0			;/128
	;asr.l	#7,d0			;/16384
	asl.l	#2,d0			;swap method of div 16384
	swap	d0
	add.w	d4,d0			;add x offset, check if clipping
	cmp.w	d6,d0
	bhs.s	.nodraw			;unsigned test, higher or same. Traps <0

	move.w	(a3,d2.w),d2		;get color for this z value
	;d0-d2 = x,y,col

	move.w	d0,d3			;save x
	asr.w	#3,d0			;x to byte offset
	add.w	d1,d1			;access in words
	add.w	(a1,d1.w),d0		;add premultiplied line byte offset into screen
	not.w	d3			;convert to bset value

.pl1:	btst	#0,d2
	beq.s	.pl2
	bset.b	d3,(a4,d0.w)
.pl2:	
	btst	#1,d2
	beq.s	.pl3
	bset.b	d3,BPL_BUF_BYTEWIDTH(a4,d0.w)
.pl3:	
.nodraw:
	dbf	d7,.persloop1
	rts


*****************************************************************************
* Calculates the color for all the perspective z entries
* IN:		
* OUT:		
* TRASHED:	d0-d7/a0-a5
*****************************************************************************

Calc_Perspective_FadeTable:

	lea	Perspective_FadeTable,a0

	; first entry is 0 and 1, use the max value as it breaks div
	moveq	#0,d0
	move.w	#STF_Z_NUMENTRIES-1,d2
.loop	
	move.l	d0,d1
	divu	#(STF_Z_NUMENTRIES/((1<<BPL_BUF_NUMPLANES)-1)),d1
	addq.w	#1,d1			;change range to 1-7
	cmpi.w	#(1<<BPL_BUF_NUMPLANES)-1,d1
	ble.s	.colok
	moveq	#(1<<BPL_BUF_NUMPLANES)-1,d1
.colok:
	move.w	d1,(a0)+		;store colour
	addq.l	#1,d0			;next z
	dbf	d2,.loop

	rts


*****************************************************************************
* Random number routines. Various.
* IN:		
* OUT:		
* TRASHED:	d0
*****************************************************************************

RandomizeStars:			;initialize x,y,z coordinate values
	lea	XYZpts,a1
	move.w	#STF_NUMSTARS-1,d7
.l1:
	bsr.s	STF_PRNG
	ext.l	d2			;-32768 to 32767
	move.w 	d2,(a1)+		

	bsr.s	STF_PRNG
	ext.l	d2			;-32768 to 32767
	move.w 	d2,(a1)+		

	bsr.s	STF_PRNG
	ext.l	d2			;-32768 to 32767		
	move.w 	d2,(a1)+		

	dbf d7,.l1	
	rts


STF_RndW:
	bsr.s	STF_RndB
	rol.w	#8,d0
STF_RndB:
	move.b	$dff007,d0		;Hpos
	move.b	$bfd800,d1		;event counter
	eor.b	d1,d0
	rts

STF_Random:				;A simple mathematical generator. d0=seed
	rol.w	d0,d0
	eor.w	#18565,d0
	rts				;returns next RND number/seed in d0

STF_PRNG:    
	movem.l   STF_PRNG_State(pc),d0-d1

	move.l	d0,d2
	lsl.l	#2,d0
	eor.l	d2,d0           ; T = A^(A<<2)

	move.l	d1,d2
	lsr.l	#3,d2
	eor.l	d1,d2           ; B^(B>>3)

	eor.l	d0,d2           ; B^(B>>3)^T
	lsr.l	#7,d0
	eor.l	d0,d2           ; B^(B>>3)^T^(T>>7)

	movem.l   d1-d2,STF_PRNG_State
	rts                        ; return random number in D2

STF_PRNG_State:
	;dc.l      2                ; initialize once to non zero
	dc.l $9876fedc
	dc.l $abcd1234


*****************************************************************************
*****************************************************************************
*****************************************************************************

	;SECTION	IntroFramework_PublicData,DATA	;Public data section for variables
	EVEN
	
*****************************************************************************

STF_ControllerScript:
	;Starts with PAL_Logo_Full_TransIn1
	;Allow BPL depack to finish!
	FX_PAUSE	10

	FX_PALETTE	1,PAL_Logo_Full_TransIn2
	FX_PAUSE	1*16

	; Small bounce
	FX_ZOOM		ZOOM_MAX,16
	FX_PAUSE	24
	FX_ZOOM		0,16
	FX_PAUSE	32

	; Into color
	FX_PALETTE	4,PAL_Logo_Full_Source

	FX_ZOOM		ZOOM_MAX,16
	FX_PAUSE	32
	FX_ZOOM		0,16
	FX_PAUSE	32

;	FX_ZOOM		ZOOM_MAX,16
;	FX_PAUSE	32
;	FX_ZOOM		0,16
;	FX_PAUSE	32


	;All colours to white
	FX_PAUSE	75
	FX_PALETTE	2,PAL_Stars4
	FX_PAUSE	16*2

	;Palette to pic in white, and everything else black (ready to fade in)
	; Starfield on with 1bpl logo in white
	FX_SET_STF_AND_1BPL_LOGO
	FX_PALETTE	0,PAL_Stars3
	FX_PAUSE	2

	FX_PALETTE	2,PAL_Stars2
	FX_PAUSE	16*2
	FX_PALETTE	2,PAL_Stars1
	FX_PAUSE	16*2
	
	;Just stars
	FX_SET_STF	
	
	FX_PAUSE	248

	;FX_PALETTE	1,PAL_AllWhite
	FX_PALETTE	1,PAL_AllBlack
	FX_PAUSE	16

	; Wait for specific frame to start
	;FX_ISFRAMEOVER	4700
	;FX_GET_MASTERFRAME

	FX_END

*****************************************************************************

PAL_AllWhite:
	dc.w	$fff,$fff,$fff,$fff,$fff,$fff,$fff,$fff
	dc.w	$fff,$fff,$fff,$fff,$fff,$fff,$fff,$fff

PAL_AllWhite2:
	dc.w	$888,$888,$888,$888,$888,$888,$888,$888
	dc.w	$888,$888,$888,$888,$888,$888,$888,$888

PAL_Stars1:
	dc.w	$000,$fff,$888,$444,$000,$000,$000,$000
	dc.w	$000,$000,$000,$000,$000,$000,$000,$000

PAL_Stars2:
	dc.w	$000,$fff,$888,$444,$000,$000,$000,$000
	dc.w	$000,$fff,$000,$000,$000,$000,$000,$000

PAL_Stars3:
	dc.w	$000,$000,$000,$000,$000,$000,$000,$000
	dc.w	$000,$fff,$000,$000,$000,$000,$000,$000

PAL_Stars4:
	dc.w	$000,$fff,$fff,$fff,$fff,$fff,$fff,$fff
	dc.w	$fff,$fff,$fff,$fff,$fff,$fff,$fff,$fff

*****************************************************************************

Sine_256_Byte:
	include	"sine_-128_127_1024_bytes.i"

SINE_NUMENTRIES = 1024	; Must be power of 2
SINE_OFFSET_MASK = (SINE_NUMENTRIES-1)	; Byte offset access into the table

*****************************************************************************

	EVEN
BPL_Logo_Full_Source:
	INCBIN "AssetsConverted/PJZ_logo2019_320x256x4_inter.BPL.nrv2s"
	EVEN

PAL_Logo_Full_Source:
	INCLUDE "AssetsConverted/PJZ_logo2019_320x256x4_inter.PAL_dcw.i"

; This is the background in the incoming transition color
; all the other colors are black (so the logo is in siloutte)
PAL_Logo_Full_TransIn1:
	dcb.w	P0_PAL_NUMCOLS,COL_TRANS_IN

PAL_Logo_Full_TransIn2:
	dc.w	COL_TRANS_IN
	dcb.w	P0_PAL_NUMCOLS-1,$000

BPL_Logo_Sil_Source:
	INCBIN "AssetsConverted/PJZ_logo2019_Sil_320x256x1.BPL.nrv2s"
	EVEN

*****************************************************************************

; The bitmap used for perspective bitmap. Takes too long to generate so needs
; to be in own buffer for precalc. Precalc dony by ExplodeVector.s
;Moved to IntroSharedData.s
;BPL_Zoom_Pattern:	ds.b ZOOM_PATTERN_TOTALSIZE
	EVEN


*****************************************************************************
*****************************************************************************
*****************************************************************************

	;SECTION	IntroFramework_PublicBss,BSS	;Public blank memory

*****************************************************************************

Mult_Height_PF1_NumPlanes:
	ds.w	BPL_BUF_HEIGHT

XYZpts:
	dcb.w	STF_NUMSTARS*3,0	;x,y,z

Perspective_ReciprocalTable:
	dcb.w	STF_Z_NUMENTRIES,0

Perspective_FadeTable:
	dcb.w	STF_Z_NUMENTRIES,0


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

DIW_VSTART set (DIW_V&$ff)<<8
DIW_VSTOP set ((DIW_V+DIW_HEIGHT)&$ff)<<8
DIW_HSTART set DIW_H&$ff
DIW_HSTOP set (DIW_HSTART+DIW_WIDTH)&$ff
DIW_START set DIW_VSTART!DIW_HSTART
DIW_STOP set DIW_VSTOP!DIW_HSTOP
DDF_START set (DDF_H/2)-8
DDF_STOP set DDF_START+((DDF_WORDWIDTH-DDF_INCREMENT)*8)

; 4 Bitplane full color logo
P0_CL_Phys:
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	CMOVE 	bplcon0,$4200		;3bpl dual playfield to start
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0000		;PF2 priority (pic on top)
	CMOVE 	bpl1mod,P0_BPL_BPL1MOD
	CMOVE 	bpl2mod,P0_BPL_BPL2MOD

	CWAIT	DIW_V-1,$7		;Time for altering Copperlist ptrs
P0_CL_Bpl:				;Bitplane pointers
	CMOVE	bpl1pth,$0
	CMOVE	bpl1ptl,$0
	CMOVE	bpl2pth,$0
	CMOVE	bpl2ptl,$0
	CMOVE	bpl3pth,$0
	CMOVE	bpl3ptl,$0
	CMOVE	bpl4pth,$0
	CMOVE	bpl4ptl,$0

	CWAIT	DIW_V,$7
P0_CL_Cols:	
	CMOVE	tmpcolor00,$000
	CMOVE	color01,$fff		
	CMOVE	color02,$fff
	CMOVE	color03,$fff
	CMOVE	color04,$fff
	CMOVE	color05,$fff
	CMOVE	color06,$fff
	CMOVE	color07,$fff
	CMOVE	color08,$fff		;PF2 cols
	CMOVE	color09,$fff
	CMOVE	color10,$fff
	CMOVE	color11,$fff
	CMOVE	color12,$fff
	CMOVE	color13,$fff
	CMOVE	color14,$fff
	CMOVE	color15,$fff

P0_CL_Zoom_Pattern:
a set DIW_V
	REPT BPL_LOGO_BUF_HEIGHT
	CWAIT	(a&$ff),$7		;0,2
	CMOVE 	bpl1mod,P0_BPL_BPL1MOD	;4,6
	CMOVE 	bpl2mod,P0_BPL_BPL1MOD	;8,10
	CWAIT	(a&$ff),$df		;12,14
a set a+1
	ENDR


	;CWAIT	255,$e1
	CWAIT	(DIW_V+DIW_HEIGHT)&$ff,$7
	CMOVE	tmpcolor00,$000		;Black out screen for wrap around

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
P0_CL_PATTERN_OFFSET = (P0_CL_Zoom_Pattern-P0_CL_Phys)
P0_CL_COL_OFFSET = (P0_CL_Cols-P0_CL_Phys)
P0_CL_SIZE = P0_CL_End-P0_CL_Phys

	RSRESET
CL_PATTERN_WAIT1:	rs.w	2
CL_PATTERN_BPL1MOD:	rs.w	2
CL_PATTERN_BPL2MOD:	rs.w	2
CL_PATTERN_WAIT2:	rs.w	2
CL_PATTERN_SIZEOF:	rs.w	0

*****************************************************************************

P1_CL_Phys:
	CMOVE 	diwstrt,DIW_START
	CMOVE 	diwstop,DIW_STOP
	CMOVE 	ddfstrt,DDF_START
	CMOVE 	ddfstop,DDF_STOP
	CMOVE 	bplcon0,$3600		;3bpl dual playfield to start
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0040		;PF2 priority (pic on top)
	CMOVE 	bpl1mod,P1_BPL_BPL1MOD
	CMOVE 	bpl2mod,P1_BPL_BPL2MOD

	;CWAIT	25-1,$7			;Time for altering Copperlist ptrs

P1_CL_PF1_Bpl:				;Bitplane pointers playfield 1
	CMOVE	bpl1pth,$0
	CMOVE	bpl1ptl,$0
	CMOVE	bpl3pth,$0
	CMOVE	bpl3ptl,$0
P1_CL_PF2_Bpl:				;Bitplane pointers playfield 2
	CMOVE	bpl2pth,$0
	CMOVE	bpl2ptl,$0
	CMOVE	bpl4pth,$0
	CMOVE	bpl4ptl,$0

	;CWAIT	DIW_V,$7
P1_CL_Cols:	
	CMOVE	tmpcolor00,$000
	CMOVE	color01,$fff		
	CMOVE	color02,$fff
	CMOVE	color03,$fff
	CMOVE	color04,$fff
	CMOVE	color05,$fff
	CMOVE	color06,$fff
	CMOVE	color07,$fff
	CMOVE	color08,$fff		;PF2 cols
	CMOVE	color09,$fff
	CMOVE	color10,$fff
	CMOVE	color11,$fff
	CMOVE	color12,$fff
	CMOVE	color13,$fff
	CMOVE	color14,$fff
	CMOVE	color15,$fff

	;CWAIT	255,$e1
	;CWAIT	(DIW_V+DIW_HEIGHT)&$ff,$7
	;CMOVE	tmpcolor00,$000		;Black out screen for wrap around

	; Trigger copper interrupt
	IFNE FW_IRQ_TYPE_COPPER
		IF SCANLINE_EOF>255
			CWAIT	255,$e1
		ENDIF
		CWAIT	(SCANLINE_EOF-1)&$ff,$df
		CMOVE	intreq,INTF_SETCLR|INTF_COPER
	ENDC
	COPPEREND

*****************************************************************************
*****************************************************************************
*****************************************************************************

; Map screens to shared chipmem buffers. Put stuff that is no longer used by
; the end of the routine last. So that next routine (allocating from the end)
; doesn't trip over. Last thing shown is 4BPL.
; TOTAL: 150KB
CUR_CHIP_BUF set FW_Chip_Buffer_1

BPL_Logo_4Bpl		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_LOGO_BUF_TOTALSIZE

BPL_Phys		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_Log1		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_Log2		=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_BUF_TOTALSIZE

BPL_Logo_Blank_1BPL	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_LOGO_BUF_SIZE

BPL_Logo_Sil_1BPL	=	CUR_CHIP_BUF
CUR_CHIP_BUF set CUR_CHIP_BUF+BPL_LOGO_BUF_SIZE


