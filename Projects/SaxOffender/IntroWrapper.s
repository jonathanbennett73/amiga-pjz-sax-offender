
*****************************************************************************

; Name			: IntroWrapper.s
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com)
; Description		: The main wrapper for the entire intro.
; Date last edited	: 24/05/2019

; CPU Required		: MC68000 or better
; ChipSet Required	: OCS or better
				
*****************************************************************************

WBStartupCode	=	0	;Add startup code from icon (needs amigaos includes)
				;Doesn't work with Cranker exe on KS 1.3

*****************************************************************************

	IFNE WBStartupCode
		INCLUDE "exec\exec_lib.i"
		INCLUDE	"exec\exec.i"
		INCLUDE	"libraries\dosextens.i"
	ENDC

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
	xref	FW_WaitFrames
	
	IFNE FW_PRECALC_LONG
	xref	PRC_Init
	xref 	PRC_CL
	xref	PRC_Irq
	xref	PRC_PreCalc_Done
	xref	PRC_Finished
	ENDC

	xref	LIB_PreCalc

	xref	MIA_Start
	xref	MIA_PreCalc_IntroStart

	xref	VECEXP_Start
	xref	VECEXP_PreCalc_IntroStart
	
	xref	VECBOB_Start
	xref	VECBOB_PreCalc_IntroStart
	
	xref	DOT_Start
	xref	DOT_PreCalc_IntroStart
	
	xref	STF_Start
	xref	STF_PreCalc_IntroStart

	xref	PIC1_Start
	xref	PIC1_PreCalc_IntroStart

	xref	GTZ_Start
	xref	GTZ_PreCalc_IntroStart

	xref	END_Start
	xref	END_PreCalc_IntroStart

*****************************************************************************

	SECTION	IntroFramework_PublicCode,CODE	;Code section in Public memory

*****************************************************************************

IntroIconStartup:
	IFNE WBStartupCode
	;This handles startup from an icon cleanly.
	movem.l	d0/a0,-(sp)

	sub.l	a1,a1
	move.l  4.w,a6
	jsr	_LVOFindTask(a6)

	move.l	d0,a4

	tst.l	pr_CLI(a4)		; was it called from CLI?
	bne.s   .fromCLI		; if so, skip out this bit...

	lea	pr_MsgPort(a4),a0
	move.l  4.w,a6
	jsr	_LVOWaitPort(A6)
	lea	pr_MsgPort(a4),a0
	jsr	_LVOGetMsg(A6)
	move.l	d0,.returnMsg

.fromCLI
	movem.l	(sp)+,d0/a0
	ENDC

	bsr.s	IntroStartup           	; Calls your code..
	
	IFNE WBStartupCode
	move.l	d0,-(sp)
	tst.l	.returnMsg		; Is there a message?
	beq.s	.exitToDOS		; if not, skip...

	move.l	4.w,a6
        jsr	_LVOForbid(a6)          ; note! No Permit needed!
	move.l	.returnMsg(pc),a1
	jsr	_LVOReplyMsg(a6)
.exitToDOS:
	move.l	(sp)+,d0		; exit code
	ENDC

	rts

.returnMsg:	dc.l	0

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
	jsr	MIA_PreCalc_IntroStart
	jsr	VECEXP_PreCalc_IntroStart
	jsr	VECBOB_PreCalc_IntroStart
	jsr	STF_PreCalc_IntroStart
	jsr	PIC1_PreCalc_IntroStart
	jsr	GTZ_PreCalc_IntroStart
	jsr	END_PreCalc_IntroStart
	rts

*****************************************************************************

IntroMain:

	clr.w	FW_Quit_Flag		;Don't quit just yet
.MainLoop1
	tst.w	FW_Quit_Flag
	bne.w	.quit
	
	lea	Controller_Info(pc),a5
	move.l	CTRL_SCRIPT_PTR(a5),a0

	;Get a new command from the script
	move.w	(a0)+,d0
	
	cmpi.w	#FX_PAUSE,d0
	beq.s	.fx_pause

	cmpi.w	#FX_MIAMI,d0
	beq.s	.fx_miami

	cmpi.w	#FX_EXPLODEVECTOR,d0
	beq.s	.fx_explodevector

	cmpi.w	#FX_GLENZBOBS,d0
	beq.s	.fx_glenzbobs

	cmpi.w	#FX_STARFIELD,d0
	beq	.fx_starfield

	cmpi.w	#FX_DOTFLAG,d0
	beq	.fx_dotflag

	cmpi.w	#FX_PIC1,d0
	beq	.fx_pic1

	cmpi.w	#FX_GREETZ,d0
	beq	.fx_greetz

	cmpi.w	#FX_ENDCREDITS,d0
	beq	.fx_endcredits

	;assume end
	bra.s	.quit

.restartscript
	move.l	#ControllerScript,CTRL_SCRIPT_PTR(a5)
	bra.s	.tstmouse

.fx_pause:
	move.w	(a0)+,d0		;pause time in frames
	move.l	a0,CTRL_SCRIPT_PTR(a5)	;Store script ptr
	bsr	FW_WaitFrames
	bra.s	.tstmouse

.fx_miami:
	move.l	a0,CTRL_SCRIPT_PTR(a5)	;Store script ptr
	jsr	MIA_Start
	bra.s	.tstmouse

.fx_explodevector:
	move.l	a0,CTRL_SCRIPT_PTR(a5)	;Store script ptr
	jsr	VECEXP_Start
	bra.s	.tstmouse

.fx_glenzbobs:
	move.l	a0,CTRL_SCRIPT_PTR(a5)	;Store script ptr
	jsr	VECBOB_Start
	bra.s	.tstmouse

.fx_dotflag:
	move.l	a0,CTRL_SCRIPT_PTR(a5)	;Store script ptr
	jsr	DOT_Start
	bra.s	.tstmouse

.fx_starfield:
	move.l	a0,CTRL_SCRIPT_PTR(a5)	;Store script ptr
	jsr	STF_Start
	bra.s	.tstmouse

.fx_pic1:
	move.l	a0,CTRL_SCRIPT_PTR(a5)	;Store script ptr
	jsr	PIC1_Start
	bra	.tstmouse

.fx_greetz:
	move.l	a0,CTRL_SCRIPT_PTR(a5)	;Store script ptr
	jsr	GTZ_Start
	bra	.tstmouse

.fx_endcredits:
	move.l	a0,CTRL_SCRIPT_PTR(a5)	;Store script ptr
	jsr	END_Start
	bra	.tstmouse


;---
.tstmouse
	IFNE FW_RMB_QUIT_SECTION
	btst.b	#10-8,potgor+_custom	;rmb quits section so stay here until it is released
	beq.s	.tstmouse		
	ENDC

	btst.b 	#6,$bfe001		;L.m.b. pressed?
	bne.w 	.MainLoop1

.quit	
	rts				;exit
			

*****************************************************************************
*****************************************************************************


*****************************************************************************
*****************************************************************************

;	SECTION	IntroFramework_PublicData,DATA	;Public data section for variables
	
*****************************************************************************

*** Demo Sequencing ***
; Has to go last to have all the labels from the individual parts available

FX_END			= 	$ff
FX_PAUSE		=	$f0
FX_MIAMI		=	$f1
FX_STARFIELD		=	$f2
FX_EXPLODEVECTOR	=	$f3
FX_GLENZBOBS		=	$f4
FX_DOTFLAG		=	$f5
FX_PIC1			=	$f6
FX_GREETZ		=	$f7
FX_ENDCREDITS		=	$f8

	RSRESET
CTRL_SCRIPT_PTR:		rs.l 1		;0 - Script Ptr
CTRL_SIZE:			rs.w 0

	EVEN
Controller_Info:
	dc.l	ControllerScript	;CTRL_SCRIPT_PTR


ControllerScript:
	dc.w	FX_MIAMI
	dc.w	FX_STARFIELD
	dc.w	FX_DOTFLAG
	dc.w	FX_GREETZ
	dc.w	FX_PIC1
	dc.w	FX_EXPLODEVECTOR
	dc.w	FX_GLENZBOBS
	dc.w	FX_ENDCREDITS

;	dc.w	FX_PAUSE,50
;	dc.w	FX_PAUSE,50
;	dc.w	FX_PAUSE,50
;	dc.w	FX_PAUSE,50

ControllerScriptEnd:
	dc.w	FX_END

