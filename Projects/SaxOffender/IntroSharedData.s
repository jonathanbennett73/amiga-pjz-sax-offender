
*****************************************************************************

; Name			: IntroSharedData.i
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com)
; Description		: Resources that need to be shared between parts.
;			  generally music files, screen buffers, and fonts.
; Date last edited	: 24/05/2019

; CPU Required		: MC68000 or better
; ChipSet Required	: OCS or better
				
*****************************************************************************


*****************************************************************************


	INCLUDE "IntroConfig.i"
	INCLUDE "Framework/CustomMacros.i"


; Additional External symbols

; Include biggest data sections first so hunks are processed first.

*****************************************************************************
*****************************************************************************
*****************************************************************************

	SECTION	IntroFramework_ChipBss_Screen,BSS_C	;BSS Chip data section - screens etc

*****************************************************************************

; Max chip needed is GlenzBobs
;256 BOB Frames = 256x384 = 98304, 96KB
;Front+back buffers = 320x256x3bplx2 = 60KB
;1KB for copper.
;=156KB

	xdef FW_Chip_Buffer_1
	xdef FW_Chip_Buffer_1_End
FW_Chip_Buffer_1:
	ds.b	176*1024	;~320 is max on 512/512 config and cold boot
FW_Chip_Buffer_1_End:


*****************************************************************************
*****************************************************************************
*****************************************************************************

;	SECTION	IntroFramework_PublicBss,BSS

*****************************************************************************

; Max public needed is...
;	xdef FW_Public_Buffer_1
;	xdef FW_Public_Buffer_1_End
;FW_Public_Buffer_1:
;	ds.b	32*1024
;FW_Public_Buffer_1_End:


*****************************************************************************
*****************************************************************************
*****************************************************************************

;P61 Music
	IFEQ FW_MUSIC_TYPE-1		;p61

	SECTION	IntroFramework_PublicCode,CODE		;Player code part of IntroFramework

	xdef	P61_End
	xdef	P61_Init
	xdef	P61_VBR
	xdef	P61_Music

	IFEQ FW_MUSIC_VBLANK-0
P61mode	= 1	;1=cia, 2=vblank
	ELSE
P61mode	= 2	;1=cia, 2=vblank
	ENDC
usecode	set 0
	INCLUDE	"Assets/usecode.!retromancer.i"
	INCLUDE	"Framework/P61Settings.i"
	INCLUDE	"Framework/MusicReplay/P6112-Play_hr.asm"		;Because of usecode have to include player here


	SECTION	IntroFramework_PublicData,DATA		;Music module (public data)
	xdef	P61Module
P61Module:
	INCBIN	"Assets/P61.!retromancer"


	SECTION	IntroFramework_ChipData_Music,DATA_C	;Music samples (chip data)
	xdef	P61Samples
P61Samples:
	INCBIN	"Assets/SMP.!retromancer"

	ENDIF				;Music p61

*****************************************************************************
*****************************************************************************

;PRT music
	IFEQ FW_MUSIC_TYPE-3		;prt

	SECTION	IntroFramework_PublicData,DATA		;Music module (public data)
	xdef	prtSong
prtSong:
	INCBIN	"Assets/terminalmassage.prt"

	xdef	prtSongBuf
prtSongBuf:	ds.w	16*1024/2	;16kb
	xdef	prtPlayerBuf
prtPlayerBuf:	ds.l	16*1024/4	;16kb


	SECTION	IntroFramework_ChipBss_Music,BSS_C	;Data buffers (chip data)
	xdef	prtChipBuf
prtChipBuf:
	;NOTE: This size must be larger than what is shown in pretracker UI
	;for required chipmem
	ds.b	48*1024			;48KB

	ENDC				;prt

*****************************************************************************
*****************************************************************************

;AHX music
	IFEQ FW_MUSIC_TYPE-2		;ahx
	SECTION	IntroFramework_PublicData,DATA
	xdef	ahxMyModule
ahxMyModule:
	INCBIN "Assets/thx.Tachyon"
	;INCBIN "Assets/THX.EVERYTHING IS CONNECTED"
	ENDC				;ahx
	
*****************************************************************************
*****************************************************************************

	SECTION	IntroFramework_ChipBss,BSS_C	;BSS Chip data section - screens etc

*****************************************************************************

;Shared zoom effect 1bpl bitmap
	xdef	BPL_Zoom_Pattern
BPL_Zoom_Pattern:	ds.b ZOOM_PATTERN_TOTALSIZE
	EVEN
