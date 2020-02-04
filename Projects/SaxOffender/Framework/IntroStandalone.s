*****************************************************************************

; Name			: IntroStandalone.s
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com)
; Description		: Simulates framework so individual parts can be tested.
; Date last edited	: 04/02/2020
				
*****************************************************************************

	INCLUDE "hardware/custom.i"
	INCLUDE "hardware/cia.i"

	INCLUDE "../IntroConfig.i"
	INCLUDE	"../Framework/CustomExtra.i"
	INCLUDE "../Framework/CustomMacros.i"
	INCLUDE "../Framework/IntroLibrary.i"	

; Additional External symbols 
	xref	FW_GetSysDetails
	xref	FW_KillSys
	xref	FW_MusicInit_NoSys
	xref	FW_MusicInit_Sys
	xref	FW_MusicStop_NoSys
	xref	FW_MusicStop_Sys
	xref	FW_Quit_Flag
	xref	FW_RestoreSys
	xref	FW_SetBaseCopperAndDma_A6
	xref	FW_SetBaseCopperAndLev3Irq_A6
	xref	FW_SetBaseLev3Irq
	xref	FW_SetColorsBlack_A6
	xref	FW_WaitFrame
	
	IFNE FW_PRECALC_LONG
	xref	PRC_Init
	xref 	PRC_CL
	xref	PRC_Irq
	xref	PRC_PreCalc_Done
	xref	PRC_Finished
	ENDC

	xref	LIB_PreCalc

	xref	SubPartStart
	xref	SubPartPreCalc_IntroStart
	xref	SubPartPreCalc_EffectStart

*****************************************************************************

	SECTION	IntroFramework_PublicCode,CODE	;Code section in Public memory

*****************************************************************************

IntroStartup:
	bsr	FW_GetSysDetails	;Get VBR/AA details
	
	bsr	FW_MusicInit_Sys	;Init music that needs the system
	tst.w	d0
	bne.s	.exit			;Ran out of memory or Bad Things...

	bsr	FW_KillSys		;Kill system	

	lea	_custom,a6
	bsr	FW_SetColorsBlack_A6	;So black...
	bsr	FW_SetBaseCopperAndDma_A6 ;Blank screen and copper but DMA on
	bsr	FW_WaitFrame		;Let it become active
	bsr	FW_WaitFrame

	;Shared library precalcs. The PRC_Init routine also requires these 
	bsr	LIB_PreCalc		;T:d0-d1/a0-a1

	;Execute precalc routine if needed (runs in VBL while we continue here)
	IFNE	FW_PRECALC_LONG
		jsr	PRC_Init	;T:d0-d1/a0-a1
	ENDC

	;Init musicroutines that work with no system (P61)
	bsr	FW_MusicInit_NoSys	;T:d0-d1/a0-a1

	;Do intro start precalcs
	bsr.s	PreCalc			;T:d0-d1/a0-a1

	IFNE FW_PRECALC_LONG
	move.w	#1,PRC_PreCalc_Done	;Signal precalc routine to finish
.wait:
	tst.w	PRC_Finished		;Has it finished?
	beq.s	.wait
	ENDC

	; Leave precalc CL and switch the default irq
	;bsr	FW_SetBaseCopperAndLev3Irq_A6
	bsr	FW_SetBaseLev3Irq
	bsr	FW_WaitFrame

	bsr.s	IntroMain		;Run the intro

	bsr	FW_MusicStop_NoSys	;Stop music and cleanup for players that work with no system
	bsr	FW_RestoreSys		;Restore system
	bsr	FW_MusicStop_Sys	;Stop music and cleanup for players that work with system

	moveq	#0,d0			;Keep cli happy
.exit:
	rts				;Return code in d0


*****************************************************************************
* Does any intro start precalcs by calling external precalc routines.
* All precalc routines must preserve d2-d7/a2-a6
* IN:		
* OUT:		
* TRASHED:	d0-d1/a0-a1
*****************************************************************************

PreCalc:
	jsr	SubPartPreCalc_IntroStart
	rts

*****************************************************************************

IntroMain:

	clr.w	FW_Quit_Flag		;Don't quit just yet
.MainLoop1

	jsr	SubPartStart	

	tst.w	FW_Quit_Flag
	beq.s	.MainLoop1

	rts				;exit


*****************************************************************************
*****************************************************************************
