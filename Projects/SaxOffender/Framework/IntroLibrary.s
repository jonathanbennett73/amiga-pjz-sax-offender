*****************************************************************************

; Name			: IntroLibrary.s
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com>)
; Description		: Shared useful functions that can be turned on from IntroConfig.i.
; Date last edited	: 04/02/2020
				
*****************************************************************************

	IFND _CUSTOMMACROS_I
		INCLUDE "CustomMacros.i"
	ENDC

	INCLUDE "IntroConfig.i"
	INCLUDE "IntroLibrary.i"

*****************************************************************************

	SECTION	IntroFramework_PublicCode,CODE	;Code section in Public memory

*****************************************************************************
*****************************************************************************


*****************************************************************************
* Runs precalc code for any library functions that are enabled.
* IN:		
* OUT:		
* TRASHED:	d0-a1/a0-a1
*****************************************************************************

	xdef	LIB_PreCalc
LIB_PreCalc:
	movem.l	d2-d7/a2-a6,-(sp)

	IFD LIB_ENABLE_RGB12_LERP
	bsr.s	LIB_RGB12_Interpolate_Fast_BuildTable
	ENDC

	movem.l	(sp)+,d2-d7/a2-a6
	rts

*****************************************************************************

;LIB_RGB12_Interpolate_Small:
;	movem.l	d3-d6,-(a7)		; 4
;	moveq	#$0000000f,d3		; 2
;	moveq	#$ffffffff,d4		; 2
;.LoopRGB
;	move.w	d0,d5			; 2
;	move.w	d1,d6			; 2
;	and.w	d3,d5			; 2
;	and.w	d3,d6			; 2

;	sub.w	d5,d6			; 2
;	muls.w	d2,d6			; 2
;	divs.w	#15,d6			; 4
;	and.w	d4,d6			; 2
;	add.w	d6,d0			; 2

;	asl.w	#4,d4			; 2
;	lsl.w	#4,d3			; 2
;	bgt.s	.LoopRGB		; 2

;	movem.l	(a7)+,d3-d6		; 4
;	rts

	IFD LIB_ENABLE_RGB12_LERP

LIB_RGB12_Interpolate_Fast_BuildTable:
	lea	LIB_RGB12_Interpolate_Table(pc),a0
	moveq	#-15,d7
.LoopColor
	moveq	#0,d6
.LoopScale
	move.w	d7,d0
	muls.w	d6,d0
	divs.w	#15,d0
	move.b	d0,(a0)+
	asl.b	#4,d0
	move.b d0,(a0)+
	addq.w	#1,d6
	cmp.w	#16,d6
	bne.b	.LoopScale
	addq.w	#1,d7
	cmp.w	#16,d7
	bne.b	.LoopColor
	rts

*****************************************************************************
* Interpolates between 2 12 bit colors. Use for fades.
* From post on eab.
* IN:		d0, color source
*		d1, color dest
*		d2, step (0-15)
* OUT:		d0, new color
* TRASHED:	d0-d4
*****************************************************************************

	xdef	LIB_RGB12_Interpolate_Fast
LIB_RGB12_Interpolate_Fast:
;	move.w	d3,-(a7)		; 8
;	move.w	d4,-(a7)		; 8
	cmp.w	d0,d1
	beq.s	.exit			;nothing to do

	move.w	#$0f0,d3		; 8
	add.w	d3,d2			; 4
	add.w	d2,d2			; 4

	move.w	d3,d4			; 4
	and.w	d0,d3			; 4
	and.w	d1,d4			; 4
	sub.w	d3,d4			; 4
	add.w	d4,d4			; 4
	add.w	d2,d4			; 4
	add.b	LIB_RGB12_Interpolate_Table+1(pc,d4.w),d0	; 14

	;move.w	#$00f,d3		; 8
	moveq	#$00f,d3		; 4
	move.w	d3,d4			; 4
	and.w	d0,d3			; 4
	and.w	d1,d4			; 4
	sub.w	d3,d4			; 4
	asl.w	#4+1,d4			; 6+2*5=16
	add.w	d2,d4			; 4
	add.b	LIB_RGB12_Interpolate_Table(pc,d4.w),d0	; 14

	move.w	d0,d3			; 4
	clr.b	d3			; 4
	clr.b	d1			; 4
	sub.w	d3,d1			; 4
	asr.w	#8-(4+1),d1		; 6+2*3=12
	add.w	d2,d1			; 4
	move.w	LIB_RGB12_Interpolate_Table(pc,d1.w),d1	; 14
	clr.b	d1			; 4
	add.w	d1,d0			; 4

;	move.w	(a7)+,d4		; 8
;	move.w	(a7)+,d3		; 8
.exit:
	rts				; 16


*****************************************************************************
* Interpolates between 2 12 bit colors. Use for fades.
* From post on eab.
* This version does multiple colors at once from a list of RGB words
* IN:		a0, color source palette
*		a1, color dest palette
*		a2, resulting palette
*		d0, num colours to process
*		d1, step (0-15)
* OUT:		d1, step
* TRASHED:	d0-d6/a0-a2
*****************************************************************************

	xdef	LIB_RGB12_Interpolate_Fast_Palette
LIB_RGB12_Interpolate_Fast_Palette:
	subq	#1,d0			;correct dbf
.loop:
	move.w	d1,d2			;restore step
	move.w	(a0)+,d5		;source
	move.w	(a1)+,d6		;dest
	cmp.w	d5,d6
	beq.s	.store			;already correct

	move.w	#$0f0,d3		; 8
	add.w	d3,d2			; 4
	add.w	d2,d2			; 4

	move.w	d3,d4			; 4
	and.w	d5,d3			; 4
	and.w	d6,d4			; 4
	sub.w	d3,d4			; 4
	add.w	d4,d4			; 4
	add.w	d2,d4			; 4
	add.b	LIB_RGB12_Interpolate_Table+1(pc,d4.w),d5	; 14

	;move.w	#$00f,d3		; 8
	moveq	#$00f,d3		; 4
	move.w	d3,d4			; 4
	and.w	d5,d3			; 4
	and.w	d6,d4			; 4
	sub.w	d3,d4			; 4
	asl.w	#4+1,d4			; 6+2*5=16
	add.w	d2,d4			; 4
	add.b	LIB_RGB12_Interpolate_Table(pc,d4.w),d5	; 14

	move.w	d5,d3			; 4
	clr.b	d3			; 4
	clr.b	d6			; 4
	sub.w	d3,d6			; 4
	asr.w	#8-(4+1),d6		; 6+2*3=12
	add.w	d2,d6			; 4
	move.w	LIB_RGB12_Interpolate_Table(pc,d6.w),d6	; 14
	clr.b	d6			; 4
	add.w	d6,d5			; 4

.store:
	move.w	d5,(a2)+		;store result color
	dbf	d0,.loop

	rts

LIB_RGB12_Interpolate_Table:	DS.W	31*16		; [-15..15]x[0..15], 992bytes

	ENDC				; LIB_ENABLE_RGB12_LERP


*****************************************************************************
* Arj m7 depacker. 
* arjbeta a -m7 -jm __temp__archive__.arj in.bin
* arj2raw __temp__archive__.arj in.bin out.am7
* IN:		a0, packed data
*		a1, destination
* OUT:		a1, ptr to end of packed data
* TRASHED:	d0-d1/a0-a1
*****************************************************************************
	
	IFD	LIB_ENABLE_ARJM7
	xdef	LIB_ArjM7_Depack
LIB_ArjM7_Depack:
	;routine wants src=a1,dest=a0
	exg	a0,a1		

	;a2, buffer (11312 bytes / LIB_ARJM7_BUFFER_SIZE)
	lea	LIB_Depack_Buffer,a2

	INCLUDE "Depack/ArjM7_Depack_ross.asm"
	ENDC


*****************************************************************************
* PackFire depacker (large model). 
* packfire -l in.bin out.pkf
* IN:		a0, packed data
*		a1, destination
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************
	
	IFD	LIB_ENABLE_PACKFIRE_LARGE
	xdef	LIB_PackFire_Large_Depack
LIB_PackFire_Large_Depack:
	movem.l	d2-d7/a2-a6,-(sp)
	;a2, buffer (15980 bytes / LIB_PACKFIRE_BUFFER_SIZE)
	lea	LIB_Depack_Buffer,a2
	bsr.s	packfire_large_depack
	movem.l	(sp)+,d2-d7/a2-a6
	rts

	INCLUDE "Depack/PackFire_Depack_Large.asm"
	ENDC


*****************************************************************************
* LZ4 depacker. 
* Use lz4.exe -9 --no-frame-crc in.bin out.lz4
* IN:		a0, packed data
*		a1, destination
*		d0.l, size of packed data in bytes
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************
	
	IFD	LIB_ENABLE_LZ4
	xdef	LIB_LZ4_Depack
LIB_LZ4_Depack:
	movem.l	d2-d7/a2-a6,-(sp)
	bsr.s	lz4_frame_depack
	movem.l	(sp)+,d2-d7/a2-a6
	rts

	INCLUDE "Depack/LZ4_Depack_Frame.asm"
	ENDC


*****************************************************************************
* Cranker depacker. 
* Use cranker -cd -f in.bin -o out.cra
*
* In-place decrunching: Align the crunched data to the end of the
*	destination area PLUS overhead.
*
* IN:		a0, packed data
*		a1, destination
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************
	
	IFD	LIB_ENABLE_CRANKER
	xdef	LIB_Cranker_Depack
LIB_Cranker_Depack:
	;Note that the cruncher produces a 4 byte header, $b0 followed by the
	;24bit length of the decrunched data. Strip off or skip these first
	;four bytes from the crunched data when passing them to this routine.
	addq.l	#4,a0	

	INCLUDE "Depack/Cranker_Depack.asm"
	ENDC


*****************************************************************************
* Doynamite68k depacker. 
* Use doynamite68k_lz.exe -o output.doy intput.bin
* IN:		a0, packed data (must be even aligned)
*		a1, destination
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************
	
	IFD	LIB_ENABLE_DOYNAMITE68K
	xdef	LIB_Doynamite68k_Depack
LIB_Doynamite68k_Depack:
	movem.l	d2-d7/a2-a6,-(sp)
	bsr.s	doynax_depack
	movem.l	(sp)+,d2-d7/a2-a6
	rts

	INCLUDE "Depack/doynamite68k_Depack.asm"
	ENDC


*****************************************************************************
* NRV2S depacker.
* IN:		a0, packed data
*		a1, destination
* OUT:		a0, dest start
*		a1, dest end
* TRASHED:	d0-d1/a0-a1
*****************************************************************************
	
	IFD	LIB_ENABLE_NRV2S
	xdef	LIB_NRV2S_Depack
LIB_NRV2S_Depack:
	INCLUDE "Depack/nrv2s_Depack.asm"
	ENDC


*****************************************************************************
* NRV2S depacker (in-place with no offset needed).
* IN:		a0, packed data
* OUT:		a1, dest start
* TRASHED:	d0-d1/a0-a1
*****************************************************************************
	
	IFD	LIB_ENABLE_NRV2R
	xdef	LIB_NRV2R_Depack
LIB_NRV2R_Depack:
	INCLUDE "Depack/nrv2r_Depack.asm"
	ENDC


*****************************************************************************
*
* IN:		a0, packed data
*		a1, destination
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************
	
	IFD	LIB_ENABLE_SHRINKLER
	xdef	LIB_Shrinkler_Depack
LIB_Shrinkler_Depack:
	movem.l	a2-a3,-(sp)			;everything else saved by routine
	sub.l	a2,a2				;no callback
	bsr.s	Shrinkler_Depack
	movem.l	(sp)+,a2-a3
	rts

	INCLUDE "Depack/Shrinkler_Depack.asm"
	ENDC


*****************************************************************************
*****************************************************************************

	SECTION	IntroFramework_PublicData,DATA	;Code section in Public memory

*****************************************************************************

	IFD LIB_ENABLE_SIN_Q15
;
; Q15 (15 bits used for fractional, 1 bit sign)
; Note the table is in words from -32768 to 32767
; After multiplying
; add.l d0,d0
; swap d0
; = /32768
	xdef	LIB_SIN_Q15_1024W_Table
	xdef	LIB_COS_Q15_1024W_Table

LIB_SIN_Q15_1024W_Table:
	dc.w	$0000,$00c9,$0192,$025b,$0324,$03ed,$04b6,$057f,$0648,$0711,$07d9,$08a2,$096a,$0a33,$0afb,$0bc4
	dc.w	$0c8c,$0d54,$0e1c,$0ee3,$0fab,$1072,$113a,$1201,$12c8,$138f,$1455,$151c,$15e2,$16a8,$176e,$1833
	dc.w	$18f9,$19be,$1a82,$1b47,$1c0b,$1ccf,$1d93,$1e57,$1f1a,$1fdd,$209f,$2161,$2223,$22e5,$23a6,$2467
	dc.w	$2528,$25e8,$26a8,$2767,$2826,$28e5,$29a3,$2a61,$2b1f,$2bdc,$2c99,$2d55,$2e11,$2ecc,$2f87,$3041
	dc.w	$30fb,$31b5,$326e,$3326,$33df,$3496,$354d,$3604,$36ba,$376f,$3824,$38d9,$398c,$3a40,$3af2,$3ba5
	dc.w	$3c56,$3d07,$3db8,$3e68,$3f17,$3fc5,$4073,$4121,$41ce,$427a,$4325,$43d0,$447a,$4524,$45cd,$4675
	dc.w	$471c,$47c3,$4869,$490f,$49b4,$4a58,$4afb,$4b9d,$4c3f,$4ce0,$4d81,$4e20,$4ebf,$4f5d,$4ffb,$5097
	dc.w	$5133,$51ce,$5268,$5302,$539b,$5432,$54c9,$5560,$55f5,$568a,$571d,$57b0,$5842,$58d3,$5964,$59f3
	dc.w	$5a82,$5b10,$5b9c,$5c28,$5cb3,$5d3e,$5dc7,$5e4f,$5ed7,$5f5d,$5fe3,$6068,$60eb,$616e,$61f0,$6271
	dc.w	$62f1,$6370,$63ee,$646c,$64e8,$6563,$65dd,$6656,$66cf,$6746,$67bc,$6832,$68a6,$6919,$698b,$69fd
	dc.w	$6a6d,$6adc,$6b4a,$6bb7,$6c23,$6c8e,$6cf8,$6d61,$6dc9,$6e30,$6e96,$6efb,$6f5e,$6fc1,$7022,$7083
	dc.w	$70e2,$7140,$719d,$71f9,$7254,$72ae,$7307,$735f,$73b5,$740a,$745f,$74b2,$7504,$7555,$75a5,$75f3
	dc.w	$7641,$768d,$76d8,$7722,$776b,$77b3,$77fa,$783f,$7884,$78c7,$7909,$794a,$7989,$79c8,$7a05,$7a41
	dc.w	$7a7c,$7ab6,$7aee,$7b26,$7b5c,$7b91,$7bc5,$7bf8,$7c29,$7c59,$7c88,$7cb6,$7ce3,$7d0e,$7d39,$7d62
	dc.w	$7d89,$7db0,$7dd5,$7dfa,$7e1d,$7e3e,$7e5f,$7e7e,$7e9c,$7eb9,$7ed5,$7eef,$7f09,$7f21,$7f37,$7f4d
	dc.w	$7f61,$7f74,$7f86,$7f97,$7fa6,$7fb4,$7fc1,$7fcd,$7fd8,$7fe1,$7fe9,$7ff0,$7ff5,$7ff9,$7ffd,$7ffe

LIB_COS_Q15_1024W_Table:
	dc.w	$7fff,$7ffe,$7ffd,$7ff9,$7ff5,$7ff0,$7fe9,$7fe1,$7fd8,$7fcd,$7fc1,$7fb4,$7fa6,$7f97,$7f86,$7f74
	dc.w	$7f61,$7f4d,$7f37,$7f21,$7f09,$7eef,$7ed5,$7eb9,$7e9c,$7e7e,$7e5f,$7e3e,$7e1d,$7dfa,$7dd5,$7db0
	dc.w	$7d89,$7d62,$7d39,$7d0e,$7ce3,$7cb6,$7c88,$7c59,$7c29,$7bf8,$7bc5,$7b91,$7b5c,$7b26,$7aee,$7ab6
	dc.w	$7a7c,$7a41,$7a05,$79c8,$7989,$794a,$7909,$78c7,$7884,$783f,$77fa,$77b3,$776b,$7722,$76d8,$768d
	dc.w	$7641,$75f3,$75a5,$7555,$7504,$74b2,$745f,$740a,$73b5,$735f,$7307,$72ae,$7254,$71f9,$719d,$7140
	dc.w	$70e2,$7083,$7022,$6fc1,$6f5e,$6efb,$6e96,$6e30,$6dc9,$6d61,$6cf8,$6c8e,$6c23,$6bb7,$6b4a,$6adc
	dc.w	$6a6d,$69fd,$698b,$6919,$68a6,$6832,$67bc,$6746,$66cf,$6656,$65dd,$6563,$64e8,$646c,$63ee,$6370
	dc.w	$62f1,$6271,$61f0,$616e,$60eb,$6068,$5fe3,$5f5d,$5ed7,$5e4f,$5dc7,$5d3e,$5cb3,$5c28,$5b9c,$5b10
	dc.w	$5a82,$59f3,$5964,$58d3,$5842,$57b0,$571d,$568a,$55f5,$5560,$54c9,$5432,$539b,$5302,$5268,$51ce
	dc.w	$5133,$5097,$4ffb,$4f5d,$4ebf,$4e20,$4d81,$4ce0,$4c3f,$4b9d,$4afb,$4a58,$49b4,$490f,$4869,$47c3
	dc.w	$471c,$4675,$45cd,$4524,$447a,$43d0,$4325,$427a,$41ce,$4121,$4073,$3fc5,$3f17,$3e68,$3db8,$3d07
	dc.w	$3c56,$3ba5,$3af2,$3a40,$398c,$38d9,$3824,$376f,$36ba,$3604,$354d,$3496,$33df,$3326,$326e,$31b5
	dc.w	$30fb,$3041,$2f87,$2ecc,$2e11,$2d55,$2c99,$2bdc,$2b1f,$2a61,$29a3,$28e5,$2826,$2767,$26a8,$25e8
	dc.w	$2528,$2467,$23a6,$22e5,$2223,$2161,$209f,$1fdd,$1f1a,$1e57,$1d93,$1ccf,$1c0b,$1b47,$1a82,$19be
	dc.w	$18f9,$1833,$176e,$16a8,$15e2,$151c,$1455,$138f,$12c8,$1201,$113a,$1072,$0fab,$0ee3,$0e1c,$0d54
	dc.w	$0c8c,$0bc4,$0afb,$0a33,$096a,$08a2,$07d9,$0711,$0648,$057f,$04b6,$03ed,$0324,$025b,$0192,$00c9
	dc.w	$0000,$ff37,$fe6e,$fda5,$fcdc,$fc13,$fb4a,$fa81,$f9b8,$f8ef,$f827,$f75e,$f696,$f5cd,$f505,$f43c
	dc.w	$f374,$f2ac,$f1e4,$f11d,$f055,$ef8e,$eec6,$edff,$ed38,$ec71,$ebab,$eae4,$ea1e,$e958,$e892,$e7cd
	dc.w	$e707,$e642,$e57e,$e4b9,$e3f5,$e331,$e26d,$e1a9,$e0e6,$e023,$df61,$de9f,$dddd,$dd1b,$dc5a,$db99
	dc.w	$dad8,$da18,$d958,$d899,$d7da,$d71b,$d65d,$d59f,$d4e1,$d424,$d367,$d2ab,$d1ef,$d134,$d079,$cfbf
	dc.w	$cf05,$ce4b,$cd92,$ccda,$cc21,$cb6a,$cab3,$c9fc,$c946,$c891,$c7dc,$c727,$c674,$c5c0,$c50e,$c45b
	dc.w	$c3aa,$c2f9,$c248,$c198,$c0e9,$c03b,$bf8d,$bedf,$be32,$bd86,$bcdb,$bc30,$bb86,$badc,$ba33,$b98b
	dc.w	$b8e4,$b83d,$b797,$b6f1,$b64c,$b5a8,$b505,$b463,$b3c1,$b320,$b27f,$b1e0,$b141,$b0a3,$b005,$af69
	dc.w	$aecd,$ae32,$ad98,$acfe,$ac65,$abce,$ab37,$aaa0,$aa0b,$a976,$a8e3,$a850,$a7be,$a72d,$a69c,$a60d
	dc.w	$a57e,$a4f0,$a464,$a3d8,$a34d,$a2c2,$a239,$a1b1,$a129,$a0a3,$a01d,$9f98,$9f15,$9e92,$9e10,$9d8f
	dc.w	$9d0f,$9c90,$9c12,$9b94,$9b18,$9a9d,$9a23,$99aa,$9931,$98ba,$9844,$97ce,$975a,$96e7,$9675,$9603
	dc.w	$9593,$9524,$94b6,$9449,$93dd,$9372,$9308,$929f,$9237,$91d0,$916a,$9105,$90a2,$903f,$8fde,$8f7d
	dc.w	$8f1e,$8ec0,$8e63,$8e07,$8dac,$8d52,$8cf9,$8ca1,$8c4b,$8bf6,$8ba1,$8b4e,$8afc,$8aab,$8a5b,$8a0d
	dc.w	$89bf,$8973,$8928,$88de,$8895,$884d,$8806,$87c1,$877c,$8739,$86f7,$86b6,$8677,$8638,$85fb,$85bf
	dc.w	$8584,$854a,$8512,$84da,$84a4,$846f,$843b,$8408,$83d7,$83a7,$8378,$834a,$831d,$82f2,$82c7,$829e
	dc.w	$8277,$8250,$822b,$8206,$81e3,$81c2,$81a1,$8182,$8164,$8147,$812b,$8111,$80f7,$80df,$80c9,$80b3
	dc.w	$809f,$808c,$807a,$8069,$805a,$804c,$803f,$8033,$8028,$801f,$8017,$8010,$800b,$8007,$8003,$8002
	dc.w	$8001,$8002,$8003,$8007,$800b,$8010,$8017,$801f,$8028,$8033,$803f,$804c,$805a,$8069,$807a,$808c
	dc.w	$809f,$80b3,$80c9,$80df,$80f7,$8111,$812b,$8147,$8164,$8182,$81a1,$81c2,$81e3,$8206,$822b,$8250
	dc.w	$8277,$829e,$82c7,$82f2,$831d,$834a,$8378,$83a7,$83d7,$8408,$843b,$846f,$84a4,$84da,$8512,$854a
	dc.w	$8584,$85bf,$85fb,$8638,$8677,$86b6,$86f7,$8739,$877c,$87c1,$8806,$884d,$8895,$88de,$8928,$8973
	dc.w	$89bf,$8a0d,$8a5b,$8aab,$8afc,$8b4e,$8ba1,$8bf6,$8c4b,$8ca1,$8cf9,$8d52,$8dac,$8e07,$8e63,$8ec0
	dc.w	$8f1e,$8f7d,$8fde,$903f,$90a2,$9105,$916a,$91d0,$9237,$929f,$9308,$9372,$93dd,$9449,$94b6,$9524
	dc.w	$9593,$9603,$9675,$96e7,$975a,$97ce,$9844,$98ba,$9931,$99aa,$9a23,$9a9d,$9b18,$9b94,$9c12,$9c90
	dc.w	$9d0f,$9d8f,$9e10,$9e92,$9f15,$9f98,$a01d,$a0a3,$a129,$a1b1,$a239,$a2c2,$a34d,$a3d8,$a464,$a4f0
	dc.w	$a57e,$a60d,$a69c,$a72d,$a7be,$a850,$a8e3,$a976,$aa0b,$aaa0,$ab37,$abce,$ac65,$acfe,$ad98,$ae32
	dc.w	$aecd,$af69,$b005,$b0a3,$b141,$b1e0,$b27f,$b320,$b3c1,$b463,$b505,$b5a8,$b64c,$b6f1,$b797,$b83d
	dc.w	$b8e4,$b98b,$ba33,$badc,$bb86,$bc30,$bcdb,$bd86,$be32,$bedf,$bf8d,$c03b,$c0e9,$c198,$c248,$c2f9
	dc.w	$c3aa,$c45b,$c50e,$c5c0,$c674,$c727,$c7dc,$c891,$c946,$c9fc,$cab3,$cb6a,$cc21,$ccda,$cd92,$ce4b
	dc.w	$cf05,$cfbf,$d079,$d134,$d1ef,$d2ab,$d367,$d424,$d4e1,$d59f,$d65d,$d71b,$d7da,$d899,$d958,$da18
	dc.w	$dad8,$db99,$dc5a,$dd1b,$dddd,$de9f,$df61,$e023,$e0e6,$e1a9,$e26d,$e331,$e3f5,$e4b9,$e57e,$e642
	dc.w	$e707,$e7cd,$e892,$e958,$ea1e,$eae4,$ebab,$ec71,$ed38,$edff,$eec6,$ef8e,$f055,$f11d,$f1e4,$f2ac
	dc.w	$f374,$f43c,$f505,$f5cd,$f696,$f75e,$f827,$f8ef,$f9b8,$fa81,$fb4a,$fc13,$fcdc,$fda5,$fe6e,$ff37
	dc.w	$0000,$00c9,$0192,$025b,$0324,$03ed,$04b6,$057f,$0648,$0711,$07d9,$08a2,$096a,$0a33,$0afb,$0bc4
	dc.w	$0c8c,$0d54,$0e1c,$0ee3,$0fab,$1072,$113a,$1201,$12c8,$138f,$1455,$151c,$15e2,$16a8,$176e,$1833
	dc.w	$18f9,$19be,$1a82,$1b47,$1c0b,$1ccf,$1d93,$1e57,$1f1a,$1fdd,$209f,$2161,$2223,$22e5,$23a6,$2467
	dc.w	$2528,$25e8,$26a8,$2767,$2826,$28e5,$29a3,$2a61,$2b1f,$2bdc,$2c99,$2d55,$2e11,$2ecc,$2f87,$3041
	dc.w	$30fb,$31b5,$326e,$3326,$33df,$3496,$354d,$3604,$36ba,$376f,$3824,$38d9,$398c,$3a40,$3af2,$3ba5
	dc.w	$3c56,$3d07,$3db8,$3e68,$3f17,$3fc5,$4073,$4121,$41ce,$427a,$4325,$43d0,$447a,$4524,$45cd,$4675
	dc.w	$471c,$47c3,$4869,$490f,$49b4,$4a58,$4afb,$4b9d,$4c3f,$4ce0,$4d81,$4e20,$4ebf,$4f5d,$4ffb,$5097
	dc.w	$5133,$51ce,$5268,$5302,$539b,$5432,$54c9,$5560,$55f5,$568a,$571d,$57b0,$5842,$58d3,$5964,$59f3
	dc.w	$5a82,$5b10,$5b9c,$5c28,$5cb3,$5d3e,$5dc7,$5e4f,$5ed7,$5f5d,$5fe3,$6068,$60eb,$616e,$61f0,$6271
	dc.w	$62f1,$6370,$63ee,$646c,$64e8,$6563,$65dd,$6656,$66cf,$6746,$67bc,$6832,$68a6,$6919,$698b,$69fd
	dc.w	$6a6d,$6adc,$6b4a,$6bb7,$6c23,$6c8e,$6cf8,$6d61,$6dc9,$6e30,$6e96,$6efb,$6f5e,$6fc1,$7022,$7083
	dc.w	$70e2,$7140,$719d,$71f9,$7254,$72ae,$7307,$735f,$73b5,$740a,$745f,$74b2,$7504,$7555,$75a5,$75f3
	dc.w	$7641,$768d,$76d8,$7722,$776b,$77b3,$77fa,$783f,$7884,$78c7,$7909,$794a,$7989,$79c8,$7a05,$7a41
	dc.w	$7a7c,$7ab6,$7aee,$7b26,$7b5c,$7b91,$7bc5,$7bf8,$7c29,$7c59,$7c88,$7cb6,$7ce3,$7d0e,$7d39,$7d62
	dc.w	$7d89,$7db0,$7dd5,$7dfa,$7e1d,$7e3e,$7e5f,$7e7e,$7e9c,$7eb9,$7ed5,$7eef,$7f09,$7f21,$7f37,$7f4d
	dc.w	$7f61,$7f74,$7f86,$7f97,$7fa6,$7fb4,$7fc1,$7fcd,$7fd8,$7fe1,$7fe9,$7ff0,$7ff5,$7ff9,$7ffd,$7ffe

	ENDC				; LIB_ENABLE_SIN_Q15



	IFD LIB_ENABLE_SIN_Q14
;
; Q14 (14 bits used for fractional, 1 bit sign 1 bit int)
; Note the table is in words from -16384 to 16384, $c000 to $4000
; After multiplying
; add.l d0,d0
; add.l d0,d0
; add.w	d0,d0
; swap d0
; (if extra precision needed addx.w dx,d0 where dx=0)
; = /16384
	xdef	LIB_SIN_Q14_1024W_Table
	xdef	LIB_COS_Q14_1024W_Table

LIB_SIN_Q14_1024W_Table:
	dc.w	$0000,$0065,$00c9,$012e,$0192,$01f7,$025b,$02c0,$0324,$0388,$03ed,$0451,$04b5,$051a,$057e,$05e2
	dc.w	$0646,$06aa,$070e,$0772,$07d6,$0839,$089d,$0901,$0964,$09c7,$0a2b,$0a8e,$0af1,$0b54,$0bb7,$0c1a
	dc.w	$0c7c,$0cdf,$0d41,$0da4,$0e06,$0e68,$0eca,$0f2b,$0f8d,$0fee,$1050,$10b1,$1112,$1173,$11d3,$1234
	dc.w	$1294,$12f4,$1354,$13b4,$1413,$1473,$14d2,$1531,$1590,$15ee,$164c,$16ab,$1709,$1766,$17c4,$1821
	dc.w	$187e,$18db,$1937,$1993,$19ef,$1a4b,$1aa7,$1b02,$1b5d,$1bb8,$1c12,$1c6c,$1cc6,$1d20,$1d79,$1dd3
	dc.w	$1e2b,$1e84,$1edc,$1f34,$1f8c,$1fe3,$203a,$2091,$20e7,$213d,$2193,$21e8,$223d,$2292,$22e7,$233b
	dc.w	$238e,$23e2,$2435,$2488,$24da,$252c,$257e,$25cf,$2620,$2670,$26c1,$2711,$2760,$27af,$27fe,$284c
	dc.w	$289a,$28e7,$2935,$2981,$29ce,$2a1a,$2a65,$2ab0,$2afb,$2b45,$2b8f,$2bd8,$2c21,$2c6a,$2cb2,$2cfa
	dc.w	$2d41,$2d88,$2dcf,$2e15,$2e5a,$2e9f,$2ee4,$2f28,$2f6c,$2faf,$2ff2,$3034,$3076,$30b8,$30f9,$3139
	dc.w	$3179,$31b9,$31f8,$3236,$3274,$32b2,$32ef,$332c,$3368,$33a3,$33df,$3419,$3453,$348d,$34c6,$34ff
	dc.w	$3537,$356e,$35a5,$35dc,$3612,$3648,$367d,$36b1,$36e5,$3718,$374b,$377e,$37b0,$37e1,$3812,$3842
	dc.w	$3871,$38a1,$38cf,$38fd,$392b,$3958,$3984,$39b0,$39db,$3a06,$3a30,$3a59,$3a82,$3aab,$3ad3,$3afa
	dc.w	$3b21,$3b47,$3b6d,$3b92,$3bb6,$3bda,$3bfd,$3c20,$3c42,$3c64,$3c85,$3ca5,$3cc5,$3ce4,$3d03,$3d21
	dc.w	$3d3f,$3d5b,$3d78,$3d93,$3daf,$3dc9,$3de3,$3dfc,$3e15,$3e2d,$3e45,$3e5c,$3e72,$3e88,$3e9d,$3eb1
	dc.w	$3ec5,$3ed9,$3eeb,$3efd,$3f0f,$3f20,$3f30,$3f40,$3f4f,$3f5d,$3f6b,$3f78,$3f85,$3f91,$3f9c,$3fa7
	dc.w	$3fb1,$3fbb,$3fc4,$3fcc,$3fd4,$3fdb,$3fe1,$3fe7,$3fec,$3ff1,$3ff5,$3ff8,$3ffb,$3ffd,$3fff,$4000

LIB_COS_Q14_1024W_Table:
	dc.w	$4000,$4000,$3fff,$3ffd,$3ffb,$3ff8,$3ff5,$3ff1,$3fec,$3fe7,$3fe1,$3fdb,$3fd4,$3fcc,$3fc4,$3fbb
	dc.w	$3fb1,$3fa7,$3f9c,$3f91,$3f85,$3f78,$3f6b,$3f5d,$3f4f,$3f40,$3f30,$3f20,$3f0f,$3efd,$3eeb,$3ed9
	dc.w	$3ec5,$3eb1,$3e9d,$3e88,$3e72,$3e5c,$3e45,$3e2d,$3e15,$3dfc,$3de3,$3dc9,$3daf,$3d93,$3d78,$3d5b
	dc.w	$3d3f,$3d21,$3d03,$3ce4,$3cc5,$3ca5,$3c85,$3c64,$3c42,$3c20,$3bfd,$3bda,$3bb6,$3b92,$3b6d,$3b47
	dc.w	$3b21,$3afa,$3ad3,$3aab,$3a82,$3a59,$3a30,$3a06,$39db,$39b0,$3984,$3958,$392b,$38fd,$38cf,$38a1
	dc.w	$3871,$3842,$3812,$37e1,$37b0,$377e,$374b,$3718,$36e5,$36b1,$367d,$3648,$3612,$35dc,$35a5,$356e
	dc.w	$3537,$34ff,$34c6,$348d,$3453,$3419,$33df,$33a3,$3368,$332c,$32ef,$32b2,$3274,$3236,$31f8,$31b9
	dc.w	$3179,$3139,$30f9,$30b8,$3076,$3034,$2ff2,$2faf,$2f6c,$2f28,$2ee4,$2e9f,$2e5a,$2e15,$2dcf,$2d88
	dc.w	$2d41,$2cfa,$2cb2,$2c6a,$2c21,$2bd8,$2b8f,$2b45,$2afb,$2ab0,$2a65,$2a1a,$29ce,$2981,$2935,$28e7
	dc.w	$289a,$284c,$27fe,$27af,$2760,$2711,$26c1,$2670,$2620,$25cf,$257e,$252c,$24da,$2488,$2435,$23e2
	dc.w	$238e,$233b,$22e7,$2292,$223d,$21e8,$2193,$213d,$20e7,$2091,$203a,$1fe3,$1f8c,$1f34,$1edc,$1e84
	dc.w	$1e2b,$1dd3,$1d79,$1d20,$1cc6,$1c6c,$1c12,$1bb8,$1b5d,$1b02,$1aa7,$1a4b,$19ef,$1993,$1937,$18db
	dc.w	$187e,$1821,$17c4,$1766,$1709,$16ab,$164c,$15ee,$1590,$1531,$14d2,$1473,$1413,$13b4,$1354,$12f4
	dc.w	$1294,$1234,$11d3,$1173,$1112,$10b1,$1050,$0fee,$0f8d,$0f2b,$0eca,$0e68,$0e06,$0da4,$0d41,$0cdf
	dc.w	$0c7c,$0c1a,$0bb7,$0b54,$0af1,$0a8e,$0a2b,$09c7,$0964,$0901,$089d,$0839,$07d6,$0772,$070e,$06aa
	dc.w	$0646,$05e2,$057e,$051a,$04b5,$0451,$03ed,$0388,$0324,$02c0,$025b,$01f7,$0192,$012e,$00c9,$0065
	dc.w	$0000,$ff9b,$ff37,$fed2,$fe6e,$fe09,$fda5,$fd40,$fcdc,$fc78,$fc13,$fbaf,$fb4b,$fae6,$fa82,$fa1e
	dc.w	$f9ba,$f956,$f8f2,$f88e,$f82a,$f7c7,$f763,$f6ff,$f69c,$f639,$f5d5,$f572,$f50f,$f4ac,$f449,$f3e6
	dc.w	$f384,$f321,$f2bf,$f25c,$f1fa,$f198,$f136,$f0d5,$f073,$f012,$efb0,$ef4f,$eeee,$ee8d,$ee2d,$edcc
	dc.w	$ed6c,$ed0c,$ecac,$ec4c,$ebed,$eb8d,$eb2e,$eacf,$ea70,$ea12,$e9b4,$e955,$e8f7,$e89a,$e83c,$e7df
	dc.w	$e782,$e725,$e6c9,$e66d,$e611,$e5b5,$e559,$e4fe,$e4a3,$e448,$e3ee,$e394,$e33a,$e2e0,$e287,$e22d
	dc.w	$e1d5,$e17c,$e124,$e0cc,$e074,$e01d,$dfc6,$df6f,$df19,$dec3,$de6d,$de18,$ddc3,$dd6e,$dd19,$dcc5
	dc.w	$dc72,$dc1e,$dbcb,$db78,$db26,$dad4,$da82,$da31,$d9e0,$d990,$d93f,$d8ef,$d8a0,$d851,$d802,$d7b4
	dc.w	$d766,$d719,$d6cb,$d67f,$d632,$d5e6,$d59b,$d550,$d505,$d4bb,$d471,$d428,$d3df,$d396,$d34e,$d306
	dc.w	$d2bf,$d278,$d231,$d1eb,$d1a6,$d161,$d11c,$d0d8,$d094,$d051,$d00e,$cfcc,$cf8a,$cf48,$cf07,$cec7
	dc.w	$ce87,$ce47,$ce08,$cdca,$cd8c,$cd4e,$cd11,$ccd4,$cc98,$cc5d,$cc21,$cbe7,$cbad,$cb73,$cb3a,$cb01
	dc.w	$cac9,$ca92,$ca5b,$ca24,$c9ee,$c9b8,$c983,$c94f,$c91b,$c8e8,$c8b5,$c882,$c850,$c81f,$c7ee,$c7be
	dc.w	$c78f,$c75f,$c731,$c703,$c6d5,$c6a8,$c67c,$c650,$c625,$c5fa,$c5d0,$c5a7,$c57e,$c555,$c52d,$c506
	dc.w	$c4df,$c4b9,$c493,$c46e,$c44a,$c426,$c403,$c3e0,$c3be,$c39c,$c37b,$c35b,$c33b,$c31c,$c2fd,$c2df
	dc.w	$c2c1,$c2a5,$c288,$c26d,$c251,$c237,$c21d,$c204,$c1eb,$c1d3,$c1bb,$c1a4,$c18e,$c178,$c163,$c14f
	dc.w	$c13b,$c127,$c115,$c103,$c0f1,$c0e0,$c0d0,$c0c0,$c0b1,$c0a3,$c095,$c088,$c07b,$c06f,$c064,$c059
	dc.w	$c04f,$c045,$c03c,$c034,$c02c,$c025,$c01f,$c019,$c014,$c00f,$c00b,$c008,$c005,$c003,$c001,$c000
	dc.w	$c000,$c000,$c001,$c003,$c005,$c008,$c00b,$c00f,$c014,$c019,$c01f,$c025,$c02c,$c034,$c03c,$c045
	dc.w	$c04f,$c059,$c064,$c06f,$c07b,$c088,$c095,$c0a3,$c0b1,$c0c0,$c0d0,$c0e0,$c0f1,$c103,$c115,$c127
	dc.w	$c13b,$c14f,$c163,$c178,$c18e,$c1a4,$c1bb,$c1d3,$c1eb,$c204,$c21d,$c237,$c251,$c26d,$c288,$c2a5
	dc.w	$c2c1,$c2df,$c2fd,$c31c,$c33b,$c35b,$c37b,$c39c,$c3be,$c3e0,$c403,$c426,$c44a,$c46e,$c493,$c4b9
	dc.w	$c4df,$c506,$c52d,$c555,$c57e,$c5a7,$c5d0,$c5fa,$c625,$c650,$c67c,$c6a8,$c6d5,$c703,$c731,$c75f
	dc.w	$c78f,$c7be,$c7ee,$c81f,$c850,$c882,$c8b5,$c8e8,$c91b,$c94f,$c983,$c9b8,$c9ee,$ca24,$ca5b,$ca92
	dc.w	$cac9,$cb01,$cb3a,$cb73,$cbad,$cbe7,$cc21,$cc5d,$cc98,$ccd4,$cd11,$cd4e,$cd8c,$cdca,$ce08,$ce47
	dc.w	$ce87,$cec7,$cf07,$cf48,$cf8a,$cfcc,$d00e,$d051,$d094,$d0d8,$d11c,$d161,$d1a6,$d1eb,$d231,$d278
	dc.w	$d2bf,$d306,$d34e,$d396,$d3df,$d428,$d471,$d4bb,$d505,$d550,$d59b,$d5e6,$d632,$d67f,$d6cb,$d719
	dc.w	$d766,$d7b4,$d802,$d851,$d8a0,$d8ef,$d93f,$d990,$d9e0,$da31,$da82,$dad4,$db26,$db78,$dbcb,$dc1e
	dc.w	$dc72,$dcc5,$dd19,$dd6e,$ddc3,$de18,$de6d,$dec3,$df19,$df6f,$dfc6,$e01d,$e074,$e0cc,$e124,$e17c
	dc.w	$e1d5,$e22d,$e287,$e2e0,$e33a,$e394,$e3ee,$e448,$e4a3,$e4fe,$e559,$e5b5,$e611,$e66d,$e6c9,$e725
	dc.w	$e782,$e7df,$e83c,$e89a,$e8f7,$e955,$e9b4,$ea12,$ea70,$eacf,$eb2e,$eb8d,$ebed,$ec4c,$ecac,$ed0c
	dc.w	$ed6c,$edcc,$ee2d,$ee8d,$eeee,$ef4f,$efb0,$f012,$f073,$f0d5,$f136,$f198,$f1fa,$f25c,$f2bf,$f321
	dc.w	$f384,$f3e6,$f449,$f4ac,$f50f,$f572,$f5d5,$f639,$f69c,$f6ff,$f763,$f7c7,$f82a,$f88e,$f8f2,$f956
	dc.w	$f9ba,$fa1e,$fa82,$fae6,$fb4b,$fbaf,$fc13,$fc78,$fcdc,$fd40,$fda5,$fe09,$fe6e,$fed2,$ff37,$ff9b
	dc.w	$0000,$0065,$00c9,$012e,$0192,$01f7,$025b,$02c0,$0324,$0388,$03ed,$0451,$04b5,$051a,$057e,$05e2
	dc.w	$0646,$06aa,$070e,$0772,$07d6,$0839,$089d,$0901,$0964,$09c7,$0a2b,$0a8e,$0af1,$0b54,$0bb7,$0c1a
	dc.w	$0c7c,$0cdf,$0d41,$0da4,$0e06,$0e68,$0eca,$0f2b,$0f8d,$0fee,$1050,$10b1,$1112,$1173,$11d3,$1234
	dc.w	$1294,$12f4,$1354,$13b4,$1413,$1473,$14d2,$1531,$1590,$15ee,$164c,$16ab,$1709,$1766,$17c4,$1821
	dc.w	$187e,$18db,$1937,$1993,$19ef,$1a4b,$1aa7,$1b02,$1b5d,$1bb8,$1c12,$1c6c,$1cc6,$1d20,$1d79,$1dd3
	dc.w	$1e2b,$1e84,$1edc,$1f34,$1f8c,$1fe3,$203a,$2091,$20e7,$213d,$2193,$21e8,$223d,$2292,$22e7,$233b
	dc.w	$238e,$23e2,$2435,$2488,$24da,$252c,$257e,$25cf,$2620,$2670,$26c1,$2711,$2760,$27af,$27fe,$284c
	dc.w	$289a,$28e7,$2935,$2981,$29ce,$2a1a,$2a65,$2ab0,$2afb,$2b45,$2b8f,$2bd8,$2c21,$2c6a,$2cb2,$2cfa
	dc.w	$2d41,$2d88,$2dcf,$2e15,$2e5a,$2e9f,$2ee4,$2f28,$2f6c,$2faf,$2ff2,$3034,$3076,$30b8,$30f9,$3139
	dc.w	$3179,$31b9,$31f8,$3236,$3274,$32b2,$32ef,$332c,$3368,$33a3,$33df,$3419,$3453,$348d,$34c6,$34ff
	dc.w	$3537,$356e,$35a5,$35dc,$3612,$3648,$367d,$36b1,$36e5,$3718,$374b,$377e,$37b0,$37e1,$3812,$3842
	dc.w	$3871,$38a1,$38cf,$38fd,$392b,$3958,$3984,$39b0,$39db,$3a06,$3a30,$3a59,$3a82,$3aab,$3ad3,$3afa
	dc.w	$3b21,$3b47,$3b6d,$3b92,$3bb6,$3bda,$3bfd,$3c20,$3c42,$3c64,$3c85,$3ca5,$3cc5,$3ce4,$3d03,$3d21
	dc.w	$3d3f,$3d5b,$3d78,$3d93,$3daf,$3dc9,$3de3,$3dfc,$3e15,$3e2d,$3e45,$3e5c,$3e72,$3e88,$3e9d,$3eb1
	dc.w	$3ec5,$3ed9,$3eeb,$3efd,$3f0f,$3f20,$3f30,$3f40,$3f4f,$3f5d,$3f6b,$3f78,$3f85,$3f91,$3f9c,$3fa7
	dc.w	$3fb1,$3fbb,$3fc4,$3fcc,$3fd4,$3fdb,$3fe1,$3fe7,$3fec,$3ff1,$3ff5,$3ff8,$3ffb,$3ffd,$3fff,$4000

	ENDC				; LIB_ENABLE_SIN_Q14


*****************************************************************************
*****************************************************************************
*****************************************************************************

	SECTION	IntroFramework_PublicBss,BSS	;BSS section in Public memory

*****************************************************************************

; Packfire is largest so if multiple depack routines use this
;LIB_PACKFIRE_BUFFER_SIZE = 15980
;LIB_ARJM7_BUFFER_SIZE = 11312

	IFD	LIB_ENABLE_PACKFIRE_LARGE
LIB_Depack_Buffer:
		ds.b	LIB_PACKFIRE_BUFFER_SIZE
		EVEN
	ELSE
		IFD	LIB_ENABLE_ARJM7
LIB_Depack_Buffer:
			ds.b	LIB_ARJM7_BUFFER_SIZE
			EVEN
		ENDC
	ENDC

