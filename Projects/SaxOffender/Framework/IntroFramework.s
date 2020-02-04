*****************************************************************************

; Name			: IntroFramework.s
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com)
; Description		: Shared framework.
; Date last edited	: 04/02/2020
				
*****************************************************************************

	INCLUDE "hardware/custom.i"
	INCLUDE "hardware/cia.i"
	INCLUDE "hardware/dmabits.i"
	INCLUDE "hardware/intbits.i"

	INCLUDE "exec/execbase.i"
	INCLUDE "exec/exec_lib.i"
	INCLUDE "graphics/gfxbase.i"
	INCLUDE "graphics/graphics_lib.i"

	INCLUDE "IntroConfig.i"
	INCLUDE "IntroFramework.i"
	INCLUDE	"CustomExtra.i"
	INCLUDE "CustomMacros.i"

*****************************************************************************

	IFEQ FW_MUSIC_TYPE-1		;p61
		xref	P61_End
		xref	P61_Init
		xref	P61_VBR
		xref	P61Module
		xref	P61Samples
		IFEQ FW_MUSIC_VBLANK-1
			xref	P61_Music
		ENDC
	ENDC

	IFEQ FW_MUSIC_TYPE-2		;ahx
		INCLUDE "MusicReplay/AHX-Offsets.i"
		xref	ahxMyModule
	ENDC

	IFEQ FW_MUSIC_TYPE-3		;prt
		INCLUDE "MusicReplay/PreTracker_Offsets.i"

		; Song/buffers from IntroSharedData.s
		xref	prtChipBuf
		xref	prtPlayerBuf
		xref	prtSong
		xref	prtSongBuf
	ENDC

; AHX needs a CIA interrupt if not in VBlank mode (which never works anyway)
FW_SETUPCIAINTERRUPT set 0
	IFNE FW_MUSIC_VBLANK-1
		IFEQ FW_MUSIC_TYPE-2	;ahx
FW_SETUPCIAINTERRUPT set 1
		ENDC
	ENDC

*****************************************************************************

	SECTION	IntroFramework_PublicCode,CODE	;Code section in Public memory

*****************************************************************************

*****************************************************************************
* Gets sys details like VBR on 68010+ and if AA chipset. Run first as it gets
* critical details like VBR.
* IN:		
* OUT:		d0.l, VBR
* TRASHED:	d0/a5-a6
*****************************************************************************

	xdef	FW_GetSysDetails
FW_GetSysDetails:

;AA check
	cmpi.b	#$f8,lisaid+1+_custom		;$fc=ECS, $f8=AA
	bne.s	.notAA
	move.w	#1,FW_Vars+FW_AA_CHIPSET
.notAA:		

;VBR check
	move.l	(_ExecBase).w,a6
	moveq	#0,d0			;default VBR at $0
	btst.b	#0,AttnFlags+1(a6)	;68000 CPU?
	beq.s	.yes68k
	lea	.GetVBR(pc),a5		;else fetch vector base address
	jsr	_LVOSupervisor(a6)	;Go Supervisor mode to run a5 and return VBR
.yes68k:
	move.l	d0,FW_Vars+FW_VBRPTR	;d0 is vbr

	rts

	; From call to Supervisor()
	; Get VBR in d0.l
.GetVBR		
	;dc.w $4e7a,$c801		; movec vbr,a4
	dc.w $4e7a,$0801 		; movec vbr,d0
	rte


*****************************************************************************
* Kills the system.
*
* MUST RUN FW_GetSysDetails first!
*
* IN:		
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

	xdef	FW_KillSys
FW_KillSys:
	movem.l	d2-d7/a2-a6,-(sp)	;save

	move.l	(_ExecBase).w,a6
	jsr	_LVOForbid(a6)		;Disable multitasking

	lea	FW_Vars(pc),a5		;Storage
	lea	FW_GFXNAME(a5),a1	;open graphics.library
	jsr	_LVOOldOpenLibrary(a6)	;open
	move.l	d0,a6
	move.l	a6,FW_GFXBASE(a5)	;Save adr	

	move.l	gb_ActiView(a6),FW_OLDGFXVIEW(a5)

	jsr	_LVOOwnBlitter(a6)	;Take over blitter
	jsr	_LVOWaitBlit(a6)	;and let it finish

	sub.l	a1,a1			;null view
	jsr	_LVOLoadView(a6)	;reset display
	jsr	_LVOWaitTOF(a6)		;let interlaced displays stop
	jsr	_LVOWaitTOF(a6)
	
	lea	_custom,a6		
	move.w	dmaconr(a6),FW_SYSDMACON(a5)	;save sys stuff
	move.w	intenar(a6),FW_SYSINTENA(a5)
	move.w	intreqr(a6),FW_SYSINTREQ(a5)
	move.w	adkconr(a6),FW_SYSADKCON(a5)	
	
	;Stop interrupts before waiting for TOF so we don't get sprite corruption
	move.w	#$7fff,d0
	move.w	d0,intena(a6)		
	
	bsr	FW_WaitTOF_A6		;T:None

	;Disable everything else
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)		;twice for A4000/040 compat
	move.w	d0,dmacon(a6)
	move.w	d0,adkcon(a6)

	;move.b	#(~(CIAICRF_SETCLR))&$ff,_ciaa+ciaicr	;disable CiaA interrupts (lev2 INTB_PORTS)

	move.l	FW_VBRPTR(a5),a0
	move.l	_Level3Vector(a0),FW_OLDLEVEL3VECTOR(a5)	; Save old lev3

	;AHX Cia nonsense
	IFEQ FW_SETUPCIAINTERRUPT-1
		move.l	_Level6Vector(a0),FW_OldLevel6Vector	;Save old CIA handler

		lea	FW_OldCIABSettings(pc),a0		;Save old CIA settings
		move.b	_ciab+ciatblo,(a0)
		move.b	_ciab+ciatbhi,1(a0)
		move.b	_ciab+ciacrb,2(a0)
		
		move.b	#(~(CIAICRF_SETCLR))&$ff,_ciab+ciaicr	;disable CiaB interrupts (lev6 INTB_EXTER)
		move.b	#0,_ciab+ciacrb 
		tst.b	_ciab+ciaicr
	ENDIF


	; Point sprites at dummy sprite in base copper list
	;lea	FW_CL_Sprites,a0
	;moveq	#8,d0
	;bsr	FW_ClrCopperSprPtrs
	
	movem.l	(sp)+,d2-d7/a2-a6	;restore
	rts


*****************************************************************************
* Restores the system.
*
* IN:		
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

	xdef	FW_RestoreSys
FW_RestoreSys:
	movem.l	d2-d7/a2-a6,-(sp)	;save

	lea	_custom,a6
	lea	FW_Vars(pc),a5		;Storage

	;Stop interrupts before waiting for EOF as slow music players running
	;in the lev3 irq can cause line position checks to never finish
	move.w	#$7fff,d0
	move.w	d0,intena(a6)

	;Load a completely blank copperlist so we don't have anything running when
	;we swap back to OS lists (our default CL disables things, and can be executed
	;after we try to turn DMA back on but before system CL restored)
	move.l	#FW_CL_Blank,cop1lch(a6)

	;Wait for any blits and TOF to safely disable sprite DMA
	bsr	FW_WaitBlit_A6		;I:a6, T:None
	bsr	FW_WaitTOF_A6		;I:a6, T:None

	;Disable everything else
	move.w	#$7fff,d0
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)		;twice for A4000 compat
	move.w	d0,dmacon(a6)
	move.w	d0,adkcon(a6)

	;move.b	#(~(CIAICRF_SETCLR))&$ff,_ciaa+ciaicr	;$7f, disable CiaA interrupts (lev2 INTB_PORTS)

	move.l	FW_VBRPTR(a5),a0
	move.l	FW_OLDLEVEL3VECTOR(a5),_Level3Vector(a0)

	;AHX Cia nonsense
	IFEQ FW_SETUPCIAINTERRUPT-1
		move.b	#(~(CIAICRF_SETCLR))&$ff,_ciab+ciaicr	;$7f, disable CiaB interrupts (lev6 INTB_EXTER)
		move.b	#0,_ciab+ciacrb 
		tst.b	_ciab+ciaicr

		move.l	FW_OldLevel6Vector(pc),_Level6Vector(a0)

		lea	FW_OldCIABSettings(pc),a0
		move.b	(a0),_ciab+ciatblo
		move.b	1(a0),_ciab+ciatbhi
		or.b	#CIACRBF_LOAD,2(a0)
		move.b	2(a0),_ciab+ciacrb
		;move.b	#$9b,_ciab+ciaicr	;ciaB interrupts on
	ENDC	

	;move.b	#$9b,_ciaa+ciaicr	;ciaA interrupts on
	
	move.w	#$8000,d0		;Dma etc.. back on
	or.w	d0,FW_SYSADKCON(a5)
	or.w	d0,FW_SYSDMACON(a5)		
	or.w	d0,FW_SYSINTREQ(a5)
	or.w	d0,FW_SYSINTENA(a5)
	move.w	FW_SYSADKCON(a5),adkcon(a6)
	move.w	FW_SYSDMACON(a5),dmacon(a6)
	move.w	FW_SYSINTREQ(a5),intreq(a6)
	move.w	FW_SYSINTREQ(a5),intreq(a6)	;twice for A4000 compat
	move.w	FW_SYSINTENA(a5),intena(a6)

	move.l	FW_GFXBASE(a5),a6	;gfx library
	move.l	gb_copinit(a6),_custom+cop1lch	;Old copper adr

	move.l	FW_OLDGFXVIEW(a5),a1	;get old view
	jsr	_LVOLoadView(a6)	;activate view

	jsr	_LVODisownBlitter(a6)	;free the blitter

	move.l	a6,a1			;gfx base
	move.l	(_ExecBase).w,a6
	jsr	_LVOCloseLibrary(a6)	;close

	jsr	_LVOPermit(a6)		;multitask on

	movem.l	(sp)+,d2-d7/a2-a6	;restore
	rts


*****************************************************************************
* Init music routines that must be done before system killed. AHX.
* IN:		
* OUT:		d0.w, 0=ok (eq)
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

	xdef	FW_MusicInit_Sys
FW_MusicInit_Sys:
	movem.l	d2-d7/a2-a6,-(sp)	;save

	IFEQ FW_MUSIC_TYPE-2		;AHX
	
	IFNE FW_MUSIC_VBLANK-1		;ahx cia
.InitCIA	
	lea	ahxSetTempo(pc),a0
	moveq	#1,d0
	bsr	ahxReplayer+ahxInitCIA
	tst.w	d0			;ok?
	bne.s	.exit

	ENDC				;ahx CIA

.InitPlayer	
	sub.l	a0,a0			;auto-allocate public (fast)
	sub.l	a1,a1			;auto-allocate chip
	moveq	#1,d0			;0=load waves, 1=precalc waves
	moveq	#0,d1
	bsr	ahxReplayer+ahxInitPlayer
	tst.w	d0			;ok?
	bne.s	.exit

.InitModule	
	lea	ahxMyModule,a0	;module
	bsr	ahxReplayer+ahxInitModule
	tst.w	d0			;ok?
	bne.s	.exit

	move.w	#1,FW_Vars+FW_MUSICINITIALISED
	ENDC				;ahx

	moveq	#0,d0			;Everything OK 
.exit:
	movem.l	(sp)+,d2-d7/a2-a6	;restore
	rts


*****************************************************************************
* Init music routines that can be done after system killed. P61 etc
* IN:		
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

	xdef	FW_MusicInit_NoSys
FW_MusicInit_NoSys:
	movem.l	d2-d7/a2-a6,-(sp)	;save

	IFEQ FW_MUSIC_TYPE-1		;p61
		move.l	FW_Vars+FW_VBRPTR(pc),P61_VBR
		lea	P61Module,a0
		lea	P61Samples,a1
		sub.l	a2,a2
		moveq	#0,d0
		bsr	P61_Init

		move.w	#1,FW_Vars+FW_MUSICINITIALISED
	ENDC				;p61


	IFEQ FW_MUSIC_TYPE-2		;AHX
		IFNE FW_MUSIC_VBLANK-1
			move.b	#1,ahxIsStarted
			move.w	ahxCurrentTempo(pc),d0
			bsr	ahxSetTempo
		ENDC

		moveq	#0,d0		;Subsong #0 = Mainsong
		moveq	#0,d1		;Play immediately
		bsr	ahxReplayer+ahxInitSubSong

		move.w	#1,FW_Vars+FW_MUSICINITIALISED
	ENDIF				;AHX


	IFEQ FW_MUSIC_TYPE-3		;prt
		lea	prtPlayer(pc),a6
		lea	prtPlayerBuf,a0
		lea	prtSongBuf,a1
		lea	prtSong,a2
		add.l	(prtSongInit,a6),a6
		jsr	(a6)		; songInit

		lea	prtPlayer(pc),a6
		lea	prtPlayerBuf,a0
		lea	prtChipBuf,a1
		lea	prtSongBuf,a2
		add.l	(prtPlayerInit,a6),a6
		jsr	(a6)		; playerInit

		move.w	#1,FW_Vars+FW_MUSICINITIALISED
	ENDC				;prt


	IFEQ FW_SETUPCIAINTERRUPT-1
		;Enable interrupt handler
		move.l	FW_Vars+FW_VBRPTR(pc),a2
		move.l	#FW_IrqHandlerLev6,_Level6Vector(a2)	;Setup new level 6

		move.b	#(CIACRBF_START),_ciab+ciacrb 		;Start timer B
		move.b	#CIAICRF_SETCLR|CIAICRF_TB,_ciab+ciaicr	;Enable timer B interrupts
		tst.b	_ciab+ciaicr

		move.w	#1,FW_Vars+FW_MUSICINITIALISED
	ENDC

	;Flag that music has precalced and ready
	;IFNE FW_MUSIC_TYPE 
	;	move.w	#1,FW_Vars+FW_MUSICINITIALISED
	;ENDC

	movem.l	(sp)+,d2-d7/a2-a6	;restore

	rts


*****************************************************************************
* Stop music routines that can be done after system killed. P61 etc
* Runs any cleanup as well.
* IN:		
* OUT:		
* TRASHED:	d0-a1/a0-a1
*****************************************************************************

	xdef	FW_MusicStop_NoSys
FW_MusicStop_NoSys:
	movem.l	d2-d7/a2-a6,-(sp)	;save

	; Finish music
	IFEQ FW_MUSIC_TYPE-1
		bsr	P61_End
		clr.w	FW_Vars+FW_MUSICINITIALISED
	ENDC

	IFEQ FW_MUSIC_TYPE-2		;ahx
		bsr	ahxReplayer+ahxStopSong
		clr.b	ahxIsStarted
		clr.w	FW_Vars+FW_MUSICINITIALISED
	ENDC
	
	IFEQ FW_MUSIC_TYPE-3		;prt
		clr.w	FW_Vars+FW_MUSICINITIALISED
	ENDIF
	
	movem.l	(sp)+,d2-d7/a2-a6	;restore

	rts

*****************************************************************************
* Stop music routines that can be done after system restored. AHX etc
* Runs any cleanup as well.
* IN:		
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

	xdef	FW_MusicStop_Sys
FW_MusicStop_Sys:
	IFEQ FW_MUSIC_TYPE-2		;ahx
	movem.l	d2-d7/a2-a6,-(sp)	;save

		bsr	ahxReplayer+ahxKillPlayer
		
		IFNE FW_MUSIC_VBLANK-1
			bsr	ahxReplayer+ahxKillCIA
		ENDC

	movem.l	(sp)+,d2-d7/a2-a6	;restore	
	ENDC

	rts
	
*****************************************************************************

; Normal 320x320 starting at line 44($2c) means EOF would be 44+256 = 300
; Overscan 352x272 starting at line 36($24) means EOF would be 36+272 = 308
; For interlaced PAL:
;   vblank begins at vpos 312 hpos 1 and ends at vpos 25 hpos 1
;   vblank begins at vpos 311 hpos 1 and ends at vpos 25 hpos 1
; So safest last visible line to wait for before vblank interrupt would be 310
; I like to start clearing the screen as soon as possible after the last visible line
; which means that the blitter is usually clearing already by the time the vblank happens
; and then there is a nice mix of CPU code happening while clearing.
;
	xdef	FW_WaitEOFExact_A6
FW_WaitEOFExact_A6:			;wait for end of frame, IN: a6=_custom, trashes d0
.lo:	move.l	vposr(a6),d0
	andi.l	#$1ff00,d0		;16
	cmpi.l	#303<<8,d0		;14		
	bne.s	.lo			;wait until it matches (eq)
	rts

	xdef	FW_WaitEOF_A6		
FW_WaitEOF_A6:				;wait for end of frame or greater, IN: a6=_custom, trashes d0
.lo:	move.l	vposr(a6),d0
	lsr.l	#1,d0			;10
	lsr.w	#7,d0			;20
	cmpi.w	#303,d0			
	blt.s	.lo			;EOF or later
	rts

	xdef	FW_WaitRasterExact_A6
FW_WaitRasterExact_A6:		
.lo:	move.l	vposr(a6),d1		;Wait for scanline. IN: A6=custom, d0=scanline, trashes d1
	lsr.l	#1,d1
	lsr.w	#7,d1
	cmp.w	d0,d1
	bne.s	.lo			;wait until it matches (eq)
	rts

	xdef	FW_WaitRaster_A6
FW_WaitRaster_A6:
.lo:	move.l	vposr(a6),d1		;Wait for scanline or greater. IN: A6=custom, d0=scanline, trashes d1
	lsr.l	#1,d1
	lsr.w	#7,d1
	cmp.w	d0,d1
	blt.s	.lo			;wait until it matches or later
	rts

	xdef	FW_WaitTOF
FW_WaitTOF:
.vsync:	
	btst	#0,_custom+vposr+1
	beq.b	.vsync			;wait while in 0-255 range if bit is 0
.vsync2: 
	btst	#0,_custom+vposr+1	;wait while in 256+ range
	bne.b	.vsync2

	;just started new frame, and will only really return after any VBL processing
	;because the code will be blocked during that.
	rts				

	xdef	FW_WaitTOF_A6
FW_WaitTOF_A6:
.vsync:	
	btst	#0,vposr+1(a6)
	beq.b	.vsync			;wait while in 0-255 range if bit is 0
.vsync2: 
	btst	#0,vposr+1(a6)		;wait while in 256+ range
	bne.b	.vsync2

	;just started new frame, and will only really return after any VBL processing
	;because the code will be blocked during that.
	rts	

	
*****************************************************************************
	
	xdef	FW_WaitBlit_A6
FW_WaitBlit_A6:				;wait until blitter is finished, IN: A5=_custom
	tst.w	dmaconr(a6)		;for compatibility with A1000
.loop:	btst.b	#6,dmaconr(a6)
	bne.s	.loop
	rts


*****************************************************************************
* Clears a buffer using the Blitter and CPU.
* Adapted from source from Photon/Scoopex.
* Max 256KB
* IN:		a6, _custom/$dff000
*		a0 (buffer to clear) (even address)
*		d0.w, number of words to clear
*		d1.w, number of bytes to clear with blitter (tune to suit)
*			0=all CPU, 65535=99% blitter
* OUT:		a0, next address after the cleared buffer
*		a6, _custom
* TRASHED:	d0-d7/a0-a4
*****************************************************************************

FW_ClearBuffer_BlitCPU_BlkSize=2	;movem-repetitions, 2*13.l = 104 bytes per loop

	xdef	FW_ClearBuffer_BlitCPU_A6
FW_ClearBuffer_BlitCPU_A6:
	move.l	a5,-(sp)		;save

	moveq	#0,d2
	move.w	d0,d2
	mulu	d1,d0			;blit part of the clr, 34250/65536 for 6bpdma
	swap	d0
	move.l	d2,d1

	and.w	#-64,d0			;bltsize
	beq.s	.skipb
	sub.w	d0,d1			;rest (words)
	
	;Start the blit
	WAITBLIT_NASTY_A6
	move.l	#$01000000,bltcon0(a6)	;Clear
	clr.w	bltdmod(a6)
	move.l	a0,bltdpth(a6)
	move.w	d0,bltsize(a6)
.skipb:	
	; Work out how many movem loops we need
	divu	#13*2*FW_ClearBuffer_BlitCPU_BlkSize,d1
.cpu:	
	add.l	d2,d2			;words to bytes
	add.l	d2,a0			;find end of buffer for pre-decrement
	movem.l FW_ClearBuffer_Zero13(pc),d0/d2-d7/a1-a6	;zeroes, 13 regs, d0 already 0
	subq.w	#1,d1			;-1 for dbf
	bmi.s	.nor2
.mvml:	
	REPT 	FW_ClearBuffer_BlitCPU_BlkSize
	movem.l	d0/d2-d7/a1-a6,-(a0)		;13 regs (52 bytes)
	ENDR
	dbf 	d1,.mvml
.nor2:
	;Clear any remaining in single 13 long blocks
	swap	d1				;rest
	bra.s	.cont1
.bigl:	
	movem.l	d0/d2-d7/a1-a6,-(a0)	;13 regs
.cont1:	
	sub.w	#13*2,d1		;13 regs
	bpl.s	.bigl
	add.w	#13*2,d1		;13 regs

      	;Clear final remaining in single long/word loop
	lsr.w	#1,d1
	bcc.s	.now
	move.w	d0,-(a0)
.now:	subq.w	#1,d1
	bmi.s	.nor1
.l:	move.l	d0,-(a0)
	dbf	d1,.l
.nor1:
	lea	_custom,a6		;restore a6
	move.l	(sp)+,a5

	rts

FW_ClearBuffer_Zero13:
	dcb.l	13,0			;13 regs
	;dcb.l	13,$aaaaaaaa


*****************************************************************************
* Clears a buffer using the CPU.
* Adapted from source from Photon/Scoopex.
* Max 256KB
* IN:		a0 (buffer to clear) (even address)
*		d0.w, number of words to clear
* OUT:		a0, next address after the cleared buffer
* TRASHED:	d0-d7/a0-a4
*****************************************************************************

	xdef	FW_ClearBuffer_CPU
FW_ClearBuffer_CPU:
	move.l	a5,-(sp)		;save
	move.l	a6,-(sp)

	moveq	#0,d2			;clear top of d2
	move.w	d0,d2			;d2=words to clr (cleared top)
	move.l	d2,d1			;d1=words to clr (cleared top)

	; Work out how many movem loops we need (how many words per loop)
	divu	#13*2*FW_ClearBuffer_BlitCPU_BlkSize,d1
.cpu:	
	add.l	d2,d2			;words to bytes
	add.l	d2,a0			;find end of buffer for pre-decrement
	movem.l FW_ClearBuffer_Zero13(pc),d0/d2-d7/a1-a6	;zeroes, 13 regs, d0 already 0
	subq.w	#1,d1			;-1 for dbf
	bmi.s	.nor2
.mvml:	
	REPT 	FW_ClearBuffer_BlitCPU_BlkSize
	movem.l	d0/d2-d7/a1-a6,-(a0)		;13 regs (52 bytes)
	ENDR
	dbf 	d1,.mvml
.nor2:
	;Clear any remaining in single 13 long blocks
	swap	d1				;rest
	bra.s	.cont1
.bigl:	
	movem.l	d0/d2-d7/a1-a6,-(a0)	;13 regs
.cont1:	
	sub.w	#13*2,d1		;13 regs
	bpl.s	.bigl
	add.w	#13*2,d1		;13 regs

      	;Clear final remaining in single long/word loop
	lsr.w	#1,d1
	bcc.s	.now
	move.w	d0,-(a0)
.now:	subq.w	#1,d1
	bmi.s	.nor1
.l:	move.l	d0,-(a0)
	dbf	d1,.l
.nor1:
	move.l	(sp)+,a6		;restore
	move.l	(sp)+,a5

	rts


*****************************************************************************
* Copies a buffer using the CPU.
* Max 256KB
* IN:		a0 (source) (even address)
*		a1 (dest)
*		d0.w, number of words to copy
* OUT:		
* TRASHED:	d0-d7/a0-a2
*****************************************************************************

	xdef	FW_CopyBuffer_CPU
FW_CopyBuffer_CPU:
FW_CopyBuffer_BlkSize=4	;movem-repetitions, 4*8.l = 128 bytes per loop

	;Copying 8 longs at a time
	ext.l	d0			;clear top
	divu.w	#8*2*FW_CopyBuffer_BlkSize,d0
	subq.w	#1,d0			;dbf
	bmi.s	.nor2

.copy:	
	REPT	FW_CopyBuffer_BlkSize
	movem.l	(a0)+,d1-d7/a2
	movem.l	d1-d7/a2,(a1)
	lea	8*4(a1),a1
	ENDR
	dbf	d0,.copy

.nor2:
	;Clear any remaining (get remainder from highword)
	swap	d0	

	;Copy final remaining in single long/word loop
	lsr.w	#1,d0
	bcc.s	.now
	move.w	(a0)+,(a1)+
.now:	subq.w	#1,d0
	bmi.s	.nor1
.l:	move.l	(a0)+,(a1)+
	dbf	d0,.l
.nor1:
	rts


*****************************************************************************
* Sets all colors to $000 (for startup or non-copper list routines)
* IN:
* OUT:
* TRASHED:	d0-d1/a0
*****************************************************************************

	xdef	FW_SetColorsBlack_A6
FW_SetColorsBlack_A6:
	lea	color00(a6),a0
	moveq	#32-1,d0
	moveq	#0,d1
.loop:	
	move.w	d1,(a0)+
	dbf	d0,.loop

	rts


*****************************************************************************
* Sets the copper list and Lev3 IRQ
* Doesn't touch Dma so set separately.
* IN:		a6, _custom
*		a0, ptr to new copperlist
*		a1, ptr to lev3 irq
* OUT:		
* TRASHED:	a0
*****************************************************************************

	xdef	FW_SetCopperAndLev3Irq_A6
FW_SetCopperAndLev3Irq_A6:
	move.l 	a0,cop1lch(a6)
	move.l	FW_Vars+FW_VBRPTR(pc),a0
	move.l	a1,_Level3Vector(a0)

	rts


*****************************************************************************
* Sets the base CL.
* Doesn't touch Dma so set separately.
* IN:
* OUT:
* TRASHED:	a0
*****************************************************************************

	xdef	FW_SetBaseCopper_A6
FW_SetBaseCopper_A6:
	move.l 	#FW_CL,cop1lch(a6)

	rts


*****************************************************************************
* Sets the base CL and Lev3 Irq.
* Doesn't touch Dma so set separately.
* IN:
* OUT:
* TRASHED:	a0
*****************************************************************************

	xdef	FW_SetBaseCopperAndLev3Irq_A6
FW_SetBaseCopperAndLev3Irq_A6:
	move.l 	#FW_CL,cop1lch(a6)
	move.l	FW_Vars+FW_VBRPTR(pc),a0
	move.l	#FW_IrqHandlerLev3,_Level3Vector(a0)

	rts


*****************************************************************************
* Sets Lev3 IRQ safely on 68000+. Sets to FW default (just music player)
* IN:		
* OUT:
* TRASHED:	a0
*****************************************************************************

	xdef	FW_SetBaseLev3Irq
FW_SetBaseLev3Irq:
	move.l	FW_Vars+FW_VBRPTR(pc),a0
	move.l	#FW_IrqHandlerLev3,_Level3Vector(a0)

	rts


*****************************************************************************
* Sets Lev3 IRQ safely on 68000+. Sets it immediately so handle any
* timing/waiting externally.
* IN:		a0, new Irq
* OUT:
* TRASHED:	a1
*****************************************************************************

	xdef	FW_SetLev3Irq
FW_SetLev3Irq:
	move.l	FW_Vars+FW_VBRPTR(pc),a1
	move.l	a0,_Level3Vector(a1)

	rts


*****************************************************************************
* Sets the copper list and enables DMA (Blitter,Copper,Bitplane)
* Use after exiting a subpart to ensure blank screen. 
* IN:		a6, _custom
* OUT:
* TRASHED:	d0/a0-a1
*****************************************************************************

	xdef	FW_SetBaseCopperAndDma_A6
FW_SetBaseCopperAndDma_A6:
	lea     FW_CL,a0
	lea	FW_IrqHandlerLev3(pc),a1

	bra	FW_SetCopperAndDma_A6	;should be optimized out by assembler


*****************************************************************************
* Sets the copper list and enables DMA (Blitter,Copper,Bitplane)
* IN:		a6, _custom
*		a0, ptr to new copperlist
*		a1, ptr to lev3 irq
* OUT:		
* TRASHED:	d0/a0
*****************************************************************************

	xdef	FW_SetCopperAndDma_A6
FW_SetCopperAndDma_A6:
	;move.w	#INTF_INTEN,intena(a6)	;Interrupts off while we switch
	;bsr	FW_WaitBlit_A6		;Finish off blits

	;Can't use a EOF vposr check here as music player may run over the frame
	;Also can't use framesync counter as irq may not be running yet.
	;Instead just check for going into the 256+ (bit V8 is bit 0 of vposr)
	;territory and then back to 0-255.
	;this usually handles IRQs that are waiting for line 0, or 256+
	;without clashing with long running music players
	;bsr	FW_WaitTOF

	move.l 	a0,cop1lch(a6)
	move.l	FW_Vars+FW_VBRPTR(pc),a0
	move.l	a1,_Level3Vector(a0)

	; Enable DMA and interrupts
BaseDMA set DMAF_SETCLR|DMAF_MASTER|DMAF_RASTER|DMAF_COPPER|DMAF_BLITTER

	IFNE FW_IRQ_TYPE_COPPER
BaseINT set INTF_SETCLR|INTF_INTEN|INTF_COPER
	ELSE
BaseINT set INTF_SETCLR|INTF_INTEN|INTF_VERTB
	ENDC

	IFEQ FW_MUSIC_TYPE-2
	;AHX needs INTF_EXTER (Lev6) interrupts and audio enabling
BaseDMA set BaseDMA|DMAF_AUDIO
		IFNE FW_MUSIC_VBLANK-1
BaseINT set BaseINT|INTF_EXTER
		ENDC
	ENDC

	move.w 	#BaseDMA,dmacon(a6)	
	move.w	#BaseINT,intena(a6)
	;clr.w	COPJMP1(a6)

	rts


*****************************************************************************
* Safely disable sprite DMA. Can be used outside of a vblank with no garbage.
* Note: ross approved :)
* IN:		a6, _custom
* OUT:		
* TRASHED:	d0
*****************************************************************************

	xdef	FW_SafeDisableSpriteDma_A6
FW_SafeDisableSpriteDma_A6
	move.w	#DMAF_SPRITE,dmacon(a6)
	moveq	#0,d0
	move.w	d0,spr0ctl(a6)		;SPRxCTL
	move.w	d0,spr1ctl(a6)
	move.w	d0,spr2ctl(a6)
	move.w	d0,spr3ctl(a6)
	move.w	d0,spr4ctl(a6)
	move.w	d0,spr5ctl(a6)
	move.w	d0,spr6ctl(a6)
	move.w	d0,spr7ctl(a6)

	rts


*****************************************************************************
* Pokes the bitplane pointers in the copperlist
* IN:		a0, CopperPtr to bpl1pth
*		d1, ScreenPtr
*		d0, num bitplanes
*		d2, modulo in bytes, screen_bytewidth for interleaved)
*		screen_size for normal) Optional if d0=1
* OUT:		
* TRASHED:	d0-d1/a0
*****************************************************************************

	xdef	FW_InitCopperBplPtrs
FW_InitCopperBplPtrs:
	subq.w	#1,d0			;-1 for dbf
	ext.l	d2			;Make d2 safe for longword addition
	addq.l	#2,a0			;Skip bplpth to save time below (a0) instead of 2(a0)
.makecl
	swap	d1			;Swap high & low words
	move.w	d1,(a0)			;High ptr
	swap	d1			;Swap high & low words
	move.w	d1,4(a0)		;Low ptr
	addq.l	#8,a0			;Next set of ptrs
	add.l	d2,d1			;Next bitplane
	dbf	d0,.makecl

	rts


*****************************************************************************
* Pokes the colors in the copperlist stored at the end of an image (normal RAW)
* IN:		a0, CopperPtr to color00,
*		a1, ImagePtr
*		d0, num colors
*		d1, image size
* OUT:		
* TRASHED:	d0/a0-a1
*****************************************************************************

	xdef	FW_InitCopperColsFromRawImage
FW_InitCopperColsFromRawImage:
	addq.l	#2,a0			;Skip color00 entry
	subq.w	#1,d0			;-1 for dbf

	add.l	d1,a1			;Skip to the color map
	
.loop	move.w	(a1)+,(a0)
	addq.l	#4,a0			;Next copper instruction
	dbf	d0,.loop
	rts


*****************************************************************************
* Pokes the colors in the copperlist using a source palette (just list of dc.w color values)
* IN:		a0(CopperPtr to color00),a1(palette),d0(num colors)
* OUT:		
* TRASHED:	d0/a0-a1
*****************************************************************************
	
	xdef	FW_InitCopperColsFromPalette
FW_InitCopperColsFromPalette:
	addq.l	#2,a0			;Skip CMOVE entry
	subq.w	#1,d0			;-1 for dbf
	
.loop	move.w	(a1)+,(a0)
	addq.l	#4,a0			;Next copper instruction
	dbf	d0,.loop
	rts


*****************************************************************************
* Clears copper list block of n sprites
* IN:		a0, ptr to block of n sprites in copperlist
*			CMOVE	spr0pth,$0
*			CMOVE	spr0ptl,$0
*			CMOVE	spr1pth,$0
*			CMOVE	spr1ptl,$0
*		d0.w, number of sprites to clear, 1-8
*
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

FW_Blank_SpriteList:
	dcb.l	8,FW_Blank_Sprite	;8 blank sprite ptrs

	xdef	FW_ClrCopperSprPtrs
FW_ClrCopperSprPtrs:
	lea	FW_Blank_SpriteList(pc),a1

	;fallthrough
	;bra.s	FW_InitCopperSprPtrs


*****************************************************************************
* Init copper list block of sprites
* IN:		a0, ptr to block of 8 sprites in copperlist
*			CMOVE	spr0pth,$0
*			CMOVE	spr0ptl,$0
*			CMOVE	spr1pth,$0
*			CMOVE	spr1ptl,$0
*		a1, ptr to 8 longwords which point to sprites
*		d0.w, number of sprites
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

	xdef	FW_InitCopperSprPtrs
FW_InitCopperSprPtrs:
	addq.l	#2,a0			;PTH
	subq.w	#1,d0			;dbf
.loop
	move.l	(a1)+,d1
	move.w	d1,4(a0)
	swap	d1
	move.w	d1,(a0)
	addq.l	#8,a0
	dbf	d0,.loop

	rts


*****************************************************************************
* Waits for a new frame.
* IN:		
* OUT:		
* TRASHED:	d0	
*****************************************************************************

	xdef	FW_WaitFrame
FW_WaitFrame:
	move.w	FW_Vars+FW_MASTERFRAMECOUNTER(pc),d0
	
.loop	cmp.w	FW_Vars+FW_MASTERFRAMECOUNTER(pc),d0
	beq.s	.loop
	rts


*****************************************************************************
* Waits for a vertical blank for a number of frames (to get constant 25fps for example)
* If the period has already been missed when it starts it will wait for the next vblank.
* IN:		d0.w (number of frames to wait, 1=50fps,2=25fps)	
* OUT:		
* TRASHED:	d0
*****************************************************************************

	xdef	FW_WaitFrames
FW_WaitFrames:
	add.w	FW_Vars+FW_MASTERFRAMECOUNTER(pc),d0	;count to wait for

.loop	cmp.w	FW_Vars+FW_MASTERFRAMECOUNTER(pc),d0
	bhi.s	.loop
	rts


*****************************************************************************
* Returns 1 if the frame counter has passed given frame number. Unsigned word
* so range of 36 mins. 
* IN:		d0.w the frame number to check for
* OUT:		d0.w, 0=not yet, 1=yes
* TRASHED:	d0
*****************************************************************************

	xdef	FW_IsFrameOver
FW_IsFrameOver:
	IFNE	FW_GETFRAME_BREAKPOINT
		move.w	d0,-(sp)
		move.w	FW_Vars+FW_MASTERFRAMECOUNTER(pc),d0
		WinUAEBreakpoint
		move.w	(sp)+,d0
	ENDC

	cmp.w	FW_Vars+FW_MASTERFRAMECOUNTER(pc),d0
	bhi.s	.notyet
	moveq	#1,d0
	rts
.notyet:
	moveq	#0,d0
	rts


*****************************************************************************
* Returns current frame number for demo timing.
* IN:		
* OUT:		d0.w, frame number
* TRASHED:	d0
*****************************************************************************

	xdef	FW_GetFrame
FW_GetFrame:
	move.w	FW_Vars+FW_MASTERFRAMECOUNTER(pc),d0

	IFNE	FW_GETFRAME_BREAKPOINT
		WinUAEBreakpoint
	ENDC

	rts


*****************************************************************************
* VBI happens at line 0 and lasts for 25 lines on PAL. Last visible line is 311
* If you need more processing time you can make the copper trigger an interrupt 
* after the last visible line instead. For example if using normal 320x256
* starting at line 44($2c) then you could trigger interrupt at line 300 instead.
*****************************************************************************

; If VBlank-based music player:
; 	- If targeting 1 frame:
;		- Use copper IRQ and trigger after last visible line ~300-308
;		- Run routine code
;		- After checking FrameSync not zero (will change after IRQ) 
;		  - Clear FrameSync
;		  - Swap buffers / copperlist entries
;		  - Start clearing screen
;		  - Loop back to routine code start
;		- Don't call FW_VBlankProxy from IRQ
;		- Call FW_VBlankProxy during main code after a large blit
;		  e.g. after frame clear, or during screen fill
;
;	- If targeting 2+ frame:
;		- Use a VBlank or Copper IRQ (trigger at line 0)
;		- IRQ calls FW_VBlankProxy
;		- Run routine code
;		- Check FrameSync for 2+ (2 frames) and then use vposr to wait
;		  for last visible line ~300-308 (FW_WaitRaster_A6)
;		  - Clear FrameSync
;		  - Swap buffers / copperlists
;		  - Start clearing screen
;		  - Loop back to routine code start
;
; If CIA-based music player:
; 	- If targeting 1 frame:
;		- Same as vblank above except no calls to FW_VblankProxy
;		- Use copper IRQ and trigger after last visible line ~300-308
;		- There is a chance if CIA routine hits at line 299 (before irq) and 
;		  takes all rastertime until next frame line 25+ that you will miss
;		  the frame. A CWAIT at the top of the copperlist will help, i.e. only
;		  start writing pointers just before DIW start.
;		  CWAIT	DIW_V-2,$7		;Time for altering Copperlist
;
;	- If targeting 2+ frame:
;		- Use copper IRQ and trigger after last visible line ~300-308
;		- Run routine code
;		- Check FrameSync for 2+ (2 frames) 
;		  - Clear FrameSync
;		  - Swap buffers / copperlists
;		  - Start clearing screen
;		  - Loop back to routine code start

; General Irq used by FW_SetBase... - designed to be used in between
; parts for minimal distruption. Setup to handle all combinations of VERTB vs
; COPER vs BLIT irqs and music players.
FW_IrqHandlerLev3:			;Blank template VERTB interrupt
	movem.l	d0-d7/a0-a6,-(sp)

	lea	_custom,a6

	move.w	intreqr(a6),d0
	btst	#INTB_VERTB,d0
	bne.s	.vertb
	btst	#INTB_COPER,d0
	bne.s	.coper
	;Must be blitter irq

; ----

.blit:
	moveq	#INTF_BLIT,d0
	bra.s	.exit

; ----

.coper:
	IFNE	FW_IRQ_TYPE_COPPER
	bsr.s	FW_VBlankProxy		;Play music and update frame count, TRASHES ALL
	lea	_custom,a6
	ENDC

	moveq	#INTF_COPER,d0
	bra.s	.exit

; ----

.vertb:
	IFEQ	FW_IRQ_TYPE_COPPER
	bsr.s	FW_VBlankProxy		;Play music and update frame count, TRASHES ALL
	lea	_custom,a6
	ENDC

	moveq	#INTF_VERTB,d0

; ----

.exit	;a6=_custom, d0.w = interrupt to reset
	move.w	d0,intreq(a6)
	move.w	d0,intreq(a6)		;A4000 compat
	movem.l	(sp)+,d0-d7/a0-a6	;restore
	rte


*****************************************************************************
* Call this every frame in VBL or Copper interrupt. Will update the frame
* counter and call music routine if vblank based player routine.
* TRASHES ALL BUT a5/a6 REGS - THIS IS FOR SPEED 
* In most of my code I'll usually have a5/a6 in use when
* calling this so don't want to lose cycles saving all.
* IN:		
* OUT:		
* TRASHED:	d0-d7/a0-a4
*****************************************************************************

	xdef	FW_VBlankProxy
FW_VBlankProxy:

	;If no vblank based music then smallest possible code
	IFEQ FW_MUSIC_VBLANK
		addq.w	#1,FW_MASTERFRAMECOUNTER+FW_Vars
		rts
	ELSE

		move.l	a5,-(sp)
		move.l	a6,-(sp)

		lea	FW_Vars(pc),a5
		addq.w	#1,FW_MASTERFRAMECOUNTER(a5)	;Increase master frame counter

		;Is our music actually ready to play?
		IFNE FW_MUSIC_TYPE
		tst.w	FW_MUSICINITIALISED(a5)
		beq.s	.exit		;nope
		ENDC

		; Any CIA based blank music (AHK only)?
		IFEQ FW_SETUPCIAINTERRUPT-1
		IFEQ FW_MUSIC_VBLANK-2
			tst.b	FW_CIATimerHits
			beq	.exit
		ENDC
		ENDC

.loop:
		; P61 Vblank music
		IFEQ FW_MUSIC_TYPE-1
		IFEQ FW_MUSIC_VBLANK-1
			lea	_custom,a6
			bsr	P61_Music	;IN: a6=custom
		ENDC
		ENDC

		; AHX VBlank music or CIA timer-vblank
		IFEQ FW_MUSIC_TYPE-2
		IFNE FW_MUSIC_VBLANK-0
			jsr	ahxReplayer+ahxInterrupt
		ENDC
		ENDC

		; PRT VBlank music
		IFEQ FW_MUSIC_TYPE-3
		IFEQ FW_MUSIC_VBLANK-1
			lea	prtPlayer(pc),a1
			lea	prtPlayerBuf,a0
			add.l	(prtPlayerTick,a1),a1
			jsr	(a1)		; playerTick
		ENDC
		ENDC

		IFEQ FW_SETUPCIAINTERRUPT-1
		IFEQ FW_MUSIC_VBLANK-2
			subq.b	#1,FW_CIATimerHits
			bne.s	.loop
		ENDC
		ENDIF

.exit:
		move.l	(sp)+,a6
		move.l	(sp)+,a5
		rts
	ENDC


*****************************************************************************
* Check for a user quit signal. RMB quits section, lmb quits intro.
* IN:		a6, _custom
* OUT:		d0, 0=ok, 1=quit (zero flag will be set appropriately)
* TRASHED:	d0
*****************************************************************************
	
	xdef	FW_CheckUserQuitSignal_A6
FW_CheckUserQuitSignal_A6:
	IFNE FW_RMB_QUIT_SECTION
	btst.b	#10-8,potgor(a6)	;rmb quits section
	beq.s	.section_quit
	ENDC

	btst.b 	#6,$bfe001		;lmb quits intro
	beq.s 	.intro_quit
.ok:
	moveq	#0,d0			;clr d0
	rts

.intro_quit:
	move.w	#1,FW_Quit_Flag		;quit entire intro
.section_quit:
	moveq	#1,d0
	rts


*****************************************************************************
* CiaB handler. Used for AHX
* IN:		
* OUT:		
* TRASHED:	
*****************************************************************************

	IFEQ FW_SETUPCIAINTERRUPT-1
FW_OldLevel6Vector:
		dc.l	0		;Old CIA

FW_IrqHandlerLev6:
		tst.b	_ciab+ciaicr
		move.w	#INTF_EXTER,intreq+_custom
		move.w	#INTF_EXTER,intreq+_custom

		IFEQ FW_MUSIC_VBLANK-2
			addq.b	#1,FW_CIATimerHits
		ELSE
			IFEQ FW_MUSIC_TYPE-2
				movem.l	d0-d7/a0-a6,-(sp)
				bsr	ahxReplayer+ahxInterrupt
				movem.l	(sp)+,d0-d7/a0-a6
			ENDC
		ENDC
		rte

FW_OldCIABSettings:
		ds.b	3

		IFEQ FW_MUSIC_VBLANK-2
FW_CIATimerHits:
		dc.b	0
		ENDC
		EVEN
	ENDC


*****************************************************************************

*** System stuff ***

	RSRESET
FW_SYSDMACON:		rs.w	1
FW_SYSADKCON:		rs.w	1
FW_SYSINTENA:		rs.w	1
FW_SYSINTREQ:		rs.w	1
FW_AA_CHIPSET		rs.w	1		;-1=AA, 0=ECS/OCS
FW_MUSICINITIALISED:	rs.w	1		;Set to 1 after music init routines (precalc etc done)
FW_MASTERFRAMECOUNTER:	rs.w	1		;Number of VBlank interrupts since startup (unsigned, 36mins demo timing)
FW_VBRPTR:		rs.l	1		;0 or VBR on 68010+
FW_OLDLEVEL3VECTOR:	rs.l	1		;Old level 3
FW_GFXBASE		rs.l	1		;Address of Gfx Library
FW_OLDGFXVIEW		rs.l	1		;Address of Old Viewport
FW_GFXNAME		rs.b	18		;"graphics.library",0,0

FW_Vars:
			dc.w	0		;FW_SYSDMACON
			dc.w	0		;FW_SYSADKCON
			dc.w	0		;FW_SYSINTENA
			dc.w	0		;FW_SYSINTREQ
			dc.w	0		;FW_AA_CHIPSET
			dc.w	0		;FW_MUSICINITIALISED
			dc.w	0		;FW_MASTERFRAMECOUNTER
			dc.l	0		;FW_VBRPTR
			dc.l	0		;FW_OLDLEVEL3VECTOR
			dc.l	0		;FW_GFXBASE
			dc.l	0		;FW_OLDGFXVIEW
			dc.b	"graphics.library",0,0	;FW_GFXNAME

*** Intro Variables ***

	xdef	FW_Quit_Flag
FW_Quit_Flag	dc.w	0		;1=Quit


*****************************************************************************

*** Music replayer code ***

	IFEQ FW_MUSIC_TYPE-2		;AHX

ahxSetTempo:
	move.w	d0,ahxCurrentTempo
	tst.b	ahxIsStarted
	beq.s	.notStarted

	move.b	d0,_ciab+ciatblo
	lsr.w	#8,d0
	move.b	d0,_ciab+ciatbhi
	or.b	#CIACRBF_START,_ciab+ciacrb
.notStarted:
	rts

ahxCurrentTempo:
		dc.w	0
ahxIsStarted:
		dc.b	0
	EVEN

ahxReplayer:
	incbin	"MusicReplay/AHX-Replayer000.BIN"

	ENDC	; AHX


;PRT music
	IFEQ FW_MUSIC_TYPE-3		;prt
prtPlayer:
	INCBIN "MusicReplay/PreTracker_Replayer.bin"

	ENDC				;prt

*****************************************************************************
*****************************************************************************
*****************************************************************************

	SECTION	IntroFramework_ChipData_Copper,DATA_C	;Chip Data Section for gfx/music

*****************************************************************************

FW_MemoryFetchMode	=	0	;0,1 or 3 	
	
; Copper horizontal blanking notes from Photon/Scoopex
; Best practice is to start with DDF 38,d0 and DIW 2c81,2cc1 and modify these 
; symmetrically and always in whole 16px steps for compatibility.
; Note DDF start of less than 30 and you start to lose sprites.
; As established, positions $e7...$03 are not usable. If you're writing a simple 
; copperlist with no need for tight timing, positions $df and $07 are conventionally 
; used for the positions on either side of the horizontal blanking, and for 
; compatibility across chipsets use increments of 4 from these, resulting in 
; positions $db, $0b, and so on.

; The FW_CL sets the basic custom registers. It has bitplanes turned off.
; It is used during initial intro startup and shutdown and can also be 
; triggered between routines provide a safe transition screen.
FW_CL:
	CMOVE	fmode,FW_MemoryFetchMode	;Chip Ram fetch mode (0=OCS)
	
	CMOVE	beamcon0,$0020		;Pal mode ( $0000=NTSC )
	CMOVE 	diwstrt,$2c81
	CMOVE 	diwstop,$2cc1
	CMOVE 	ddfstrt,$0038
	CMOVE 	ddfstop,$00d0
	CMOVE 	bplcon0,$0200		;Bitplanes off
	CMOVE 	bplcon1,$0000
	CMOVE	bplcon2,$0000
	CMOVE	bplcon3,$0c00		;AGA compat, dual playfield related
	CMOVE	bplcon4,$0011
	CMOVE 	bpl1mod,0
	CMOVE 	bpl2mod,0

	CMOVE	dmacon,DMAF_SPRITE	;Disable sprite DMA

	; In case sprite DMA is enabled, point to blank sprites
;FW_CL_Sprites:			        ;Sprite pointers
;	CMOVE	spr0pth,$0
;	CMOVE	spr0ptl,$0
;	CMOVE	spr1pth,$0
;	CMOVE	spr1ptl,$0
;	CMOVE	spr2pth,$0
;	CMOVE	spr2ptl,$0
;	CMOVE	spr3pth,$0
;	CMOVE	spr3ptl,$0
;	CMOVE	spr4pth,$0
;	CMOVE	spr4ptl,$0
;	CMOVE	spr5pth,$0
;	CMOVE	spr5ptl,$0
;	CMOVE	spr6pth,$0
;	CMOVE	spr6ptl,$0
;	CMOVE	spr7pth,$0
;	CMOVE	spr7ptl,$0

	; Trigger copper interrupt
	IFNE FW_IRQ_TYPE_COPPER
		IFGT FW_IRQ_COPPER_SCANLINE-255
			CWAIT	255,$df
		ENDC
		CWAIT	(FW_IRQ_COPPER_SCANLINE&$ff),$7
		CMOVE	intreq,INTF_SETCLR|INTF_COPER
	ENDC

FW_CL_Blank:				;Ptr to empty CL
	COPPEREND

*****************************************************************************

	xdef	FW_Blank_Sprite
FW_Blank_Sprite:
	dcb.l	1,0

*****************************************************************************

; This is a blank word that we use for bltdpth in blitter linedraws. The first
; pixel of a blitter linedraw is written to bltdpth. We use this quirk to reduce
; the line length by 1 which allows the object to be blitter filled correctly.
	xdef	FW_LineDraw_Scratch
FW_LineDraw_Scratch:
	dcb.w	1,0

*****************************************************************************
