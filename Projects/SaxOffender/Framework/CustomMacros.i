	IFND _CUSTOMMACROS_I
_CUSTOMMACROS_I SET 1

*********************************************
* Custom Macro File (c)1989-2019 Antiriad / *
* Jonathan Bennett <jon@autoitscript.com    *
*********************************************

;Macros. Coded to work with customextra.i and CBM includes.

;RasterTest colours defaults
	IFND RasterTest
RasterTest set 0	
	ENDC

	IFND RASTERTEST_COL_BLITWAIT
RASTERTEST_COL_BLITWAIT set $600
	ENDC

	IFND RASTERTEST_COL_ON
RASTERTEST_COL_ON set $111
	ENDC

	IFND RASTERTEST_COL_OFF
RASTERTEST_COL_OFF set $000
	ENDC

;to stop here in WinUAE, enter "w 4 4 4 w" in the debugger window (shift+f12) 
;to place the breakpoint, and enter "w 4" to remove it
WinUAEBreakpoint	MACRO
			move.l	4.w,4.w
			ENDM

TIMERON		MACRO
		IFNE	RasterTest
		move.w	#RASTERTEST_COL_ON,color00+_custom
		ENDC
		ENDM

TIMEROFF	MACRO
		IFNE	RasterTest
		move.w	#0,color00+_custom
		ENDC
		ENDM

TIMERON2	MACRO
		IFNE	RasterTest
		move.w	#\1,color00+_custom
		ENDC
		ENDM


SAVEREGS	MACRO
		movem.l	a0-a6/d0-d7,-(sp)
		ENDM
		
LOADREGS	MACRO
		movem.l	(sp)+,a0-a6/d0-d7
		ENDM

WAITBLIT_A1000_A6	MACRO
		tst.w	dmaconr(a6)		;for compatibility on A1000 with fastmem
.bw\@		btst.b	#6,dmaconr(a6)		;=bit 14 of dmaconr!
		bne.s	.bw\@
		ENDM

WAITBLIT_A1000	MACRO
		tst.w	dmaconr+_custom		;for compatibility on A1000 with fastmem
.bw\@		btst.b	#6,dmaconr+_custom	;=bit 14 of dmaconr!
		bne.s	.bw\@
		ENDM

BLIT_NASTY_ON_A6	MACRO
		move.w	#$8400,dmacon(a6)	;Blitter nasty on
		ENDM

BLIT_NASTY_OFF_A6	MACRO
		move.w	#$0400,dmacon(a6)	;Blitter nasty off
		ENDM

BLIT_NASTY_ON	MACRO
		move.w	#$8400,dmacon+_custom	;Blitter nasty on
		ENDM

BLIT_NASTY_OFF	MACRO
		move.w	#$0400,dmacon+_custom	;Blitter nasty off
		ENDM


WAITBLIT_A6	MACRO
		IFEQ RasterTest-2
		move.w	#RASTERTEST_COL_BLITWAIT,color00(a6)
		ENDC

.bw\@		btst.b	#6,dmaconr(a6)		;=bit 14 of dmaconr!
		bne.s	.bw\@

		IFEQ RasterTest-2
		move.w	#RASTERTEST_COL_ON,color00(a6)
		ENDC
		ENDM

WAITBLIT_NASTY_A6	MACRO
		IFEQ RasterTest-2
		move.w	#RASTERTEST_COL_BLITWAIT,color00(a6)
		ENDC

		move.w	#$8400,dmacon(a6)	;Blitter nasty on
.bw\@		btst.b	#6,dmaconr(a6)		;=bit 14 of dmaconr!
		bne.s	.bw\@
		move.w	#$0400,dmacon(a6)	;Blitter nasty off

		IFEQ RasterTest-2
		move.w	#RASTERTEST_COL_ON,color00(a6)
		ENDC
		ENDM

WAITBLIT_NASTY	MACRO
		IFEQ RasterTest-2
		move.w	#RASTERTEST_COL_BLITWAIT,color00+_custom
		ENDC

		move.w	#$8400,dmacon+_custom	;Blitter nasty on
.bw\@		btst.b	#6,dmaconr+_custom	;=bit 14 of dmaconr!
		bne.s	.bw\@
		move.w	#$0400,dmacon+_custom	;Blitter nasty off

		IFEQ RasterTest-2
		move.w	#RASTERTEST_COL_ON,color00+_custom
		ENDC
		ENDM

WAITBLIT_NASTY_EARLYEXIT_A6	MACRO
		IFEQ RasterTest-2
		move.w	#RASTERTEST_COL_BLITWAIT,color00(a6)
		ENDC

		btst.b	#6,dmaconr(a6)		;=bit 14 of dmaconr!
		beq.s	.earlyexit\@
		move.w	#$8400,dmacon(a6)	;Blitter nasty on
.bw\@		btst.b	#6,dmaconr(a6)		;=bit 14 of dmaconr!
		bne.s	.bw\@
		move.w	#$0400,dmacon(a6)	;Blitter nasty off
.earlyexit\@:
		IFEQ RasterTest-2
		move.w	#RASTERTEST_COL_ON,color00(a6)
		ENDC
		ENDM

WAITBLIT	MACRO
.bw\@		btst.b	#6,dmaconr+_custom	;=bit 14 of dmaconr!
		bne.s	.bw\@
		ENDM

WAITRASTER	MACRO
.lo\@:		move.l	vposr+_custom,d0	;Wait for scanline or greater. trashes d0
		lsr.l	#1,d0
		lsr.w	#7,d0
		cmp.w	\1,d0
		blt.s	.lo\@			;wait until it matches (eq)

WAITRASTER_A6	MACRO
.lo\@:		move.l	vposr(a6),d0		;Wait for scanline or greater. trashes d0
		lsr.l	#1,d0
		lsr.w	#7,d0
		cmp.w	\1,d0
		blt.s	.lo\@			;wait until it matches (eq)

WAITRASTER_EXACT	MACRO				;trashes d0
.wr\@		move.l	vposr+_custom,d0
		andi.l	#$1ff00,d0
		cmpi.l	#(\1)<<8,d0
		bne.s	.wr\@			;wait until it matches (eq)
		ENDM

WAITRASTER_EXACT_A6	MACRO			;trashes d0
.wr\@		move.l	vposr(a6),d0
		andi.l	#$1ff00,d0
		cmpi.l	#(\1)<<8,d0
		bne.s	.wr\@			;wait until it matches (eq)
		ENDM

WAITTOF_A6	MACRO
.vsync\@:	
	btst	#0,vposr+1(a6)
	beq.b	.vsync\@			;wait while in 0-255 range if bit is 0
.vsync2\@: 
	btst	#0,vposr+1(a6)			;wait while in 256+ range
	bne.b	.vsync2\@
		ENDM

AllocMem	MACRO				;SIZE, TYPE, WHERE
		move.l	(_ExecBase).w,a6
		move.l	\1,d0
		move.l	\2,d1			;TYPE
		jsr	_LVOAllocMem(a6)
		move.l	d0,\3
		ENDM

AllocAbs	MACRO				;SIZE, ADR, WHERE
		move.l	(_ExecBase).w,a6
		move.l	\1,d0
		move.l	\2,a1
		jsr	_LVOAllocAbs(a6)
		move.l	d0,\3
		ENDM

FreeMem		MACRO				;SIZE,WHERE
		move.l	(_ExecBase).w,a6
		move.l	\1,d0
		move.l	\2,a1
		jsr	_LVOFreeMem(A6)
		ENDM

LEFTMOUSEWAIT	MACRO
.wait\@		btst.b	#6,$bfe001
		bne.s	.wait\@
		ENDM

RIGHTMOUSEWAIT	MACRO
.wait\@		btst.b	#10-8,$dff016
		bne.s	.wait\@
		ENDM

NO_BLITMASK_A6	MACRO
		move.l	#-1,bltafwm(a6)
		ENDM

ENABLE_COPPER_DANGER_A6	MACRO
		move.w	#2,copcon(a6)
		ENDM

ENABLE_INT_VERTB_A6	MACRO
		move.w	#$c020,intena(a6)
		ENDM

DISABLE_INT_VERTB_A6	MACRO
		move.w	#$0020,intena(a6)
		ENDM

ENABLE_INT_VERTB	MACRO
		move.w	#$c020,intena+_custom
		ENDM

DISABLE_INT_VERTB	MACRO
		move.w	#$0020,intena+_custom
		ENDM

RESET_INT_VERTB	MACRO
		move.w	#$0020,intreq+_custom
		move.w	#$0020,intreq+_custom	;twice for A4000 compat
		ENDM

RESET_INT_VERTB_A6	MACRO
		move.w	#$0020,intreq(a6)
		move.w	#$0020,intreq(a6)	;twice for A4000 compat
		ENDM
		

;(Kalms explained the method itself in the other thread.) Now, to use that you'd simply 
;enter the macro like any other instruction, specifying a source register, a destination
; register and an available scratch register like so:
; ABS_W d1,d2,d6 ; d2.w <- abs(d1.w-d2.w), trashes d6.w
ABS_W	MACRO
	sub.w \1,\2
	subx.w \3,\3
	eor.w \3,\2
	sub.w \3,\2
	ENDM

; Aligns a value to a 64KB boundary. You need to allocate MEM+65536 for this to work.
ALIGN_64KB_DX_L	MACRO
	add.l #65536,\1
	and.l #$3ff0000,\1
	ENDM


AGA_Palette_Load	MACRO

;\1=Palette Adr
;\2=Copper palette Adr
;\3=Number of colours

		move.l	\1,a0			;Palette data
		move.l	\2,a1			;Copper palette data
	
		move.w	\3/32-1,d7		;Number of colours
.ColLoop1\@
		addq.l	#4,a1			;miss bank select

		move.w	#32-1,d6
.ColLoop2\@
		move.w	(a0)+,2(a1)		;high bit
		move.w	(a0)+,132+2(a1)		;low bit
		addq.l	#4,a1
		dbf	d6,.ColLoop2\@

		lea	132(a1),a1
		dbf	d7,.ColLoop1\@

		ENDM


;Copper macros

CWAIT		MACRO
		dc.w	(\1&$ff)*256+(\2&$fe|1)
		dc.w	$fffe
		ENDM

CWAITBLIT	MACRO
		dc.w    $0001,$7ffe
		ENDM
		
COPPEREND	MACRO
		dc.w $ffff,$fffe
		ENDM

CSKIP		MACRO
		dc.b	\1,\2,$ff,$ff	
		ENDM
		
CMOVE		MACRO
		dc.w	\1,\2			
		ENDM

COPPERNOP	MACRO
		dc.w	$1fe,$0
		ENDM


COL24BIT	MACRO			;Assumes bplcon3 =$0c00
					;High bits, Low bits
		CMOVE	\1,((\2&$f00000)>>12)!((\2&$f000)>>8)!((\2&$f0)>>4)
		CMOVE	bplcon3,$0e00
		CMOVE	\1,((\2&$f0000)>>8)!((\2&$f00)>>4)!(\2&$0f)
		CMOVE	bplcon3,$0c00
		ENDM

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
* TRASHED:	d0-d3
*****************************************************************************

GET_SPRITE_CTRL_WORDS	MACRO		
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
	ENDM


ALL_COLS	MACRO

		CMOVE	color00,$000
		CMOVE	color01,$000
		CMOVE	color02,$000
		CMOVE	color03,$000
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
		CMOVE	color16,$000
		CMOVE	color17,$000
		CMOVE	color18,$000
		CMOVE	color19,$000
		CMOVE	color20,$000
		CMOVE	color21,$000
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

		ENDM

		
;AGA_Copper_Palette	MACRO
;
;a SET $0c00
;b SET $0e00
;		REPT	(\1)/32
;	
;		CMOVE	bplcon3,a		;bank x, high bits
;		All_Cols
;		CMOVE	bplcon3,b		;bank x, low bits
;		All_Cols
;a SET a+$2000
;b SET b+$2000
;		ENDR
;		
;		ENDM
		
	ENDC	;CUSTOMMACROS_I