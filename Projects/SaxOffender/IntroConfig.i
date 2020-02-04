	IFND _INTROCONFIG_I
_INTROCONFIG_I SET 1

*****************************************************************************

; Name			: IntroConfig.i
; Coded by		: Antiriad (Jonathan Bennett <jon@autoitscript.com)
; Description		: Global config like music type.
; Date last edited	: 24/05/2019

; CPU Required		: MC68000 or better
; ChipSet Required	: OCS or better
				
*****************************************************************************

; Configure things that control how the framework is setup.

; Allow right mouse to quit section?
FW_RMB_QUIT_SECTION	=	0

; Cause WinUAE breakpoint on calling FW_IsFrameOver and FW_GetFrame
; (enter "w 4 4 4 w" in the debugger to catch)
FW_GETFRAME_BREAKPOINT	=	0

; Do we use a long precalc routine?
FW_PRECALC_LONG		set	0

; Framework can use either a vblank lev3 irq or a copper lev3. For copper
; you must manually trigger the irq from your copperlist but this gives
; more control.
; Must call FW_VBlankProxy once per frame in this interrupt for timing/vblank music.
FW_IRQ_TYPE_COPPER	=	1	;1=Copper, 0=vblank
FW_IRQ_COPPER_SCANLINE	=	303	;Line to interrupt, scanline/copper v coord

; Music player type and timing
FW_MUSIC_TYPE		=	3	;0=none,1=p61,2=ahx,3=prt	
FW_MUSIC_VBLANK		set	0	;0=CIA,1=VBlank,2=Cia timed vblank (AHX)

*****************************************************************************

	IFEQ FW_MUSIC_TYPE
FW_MUSIC_VBLANK set 0			; Save CPU in FW_VblankProxy if no music
	ENDC

	IFEQ FW_MUSIC_TYPE-3
FW_PRECALC_LONG	set 1
FW_MUSIC_VBLANK set 1			; PRT only uses VBlank
	ENDC

*****************************************************************************

; To avoid syntax errors in visual studio code
	IFND _INTROWRAPPER
_INTROWRAPPER set 0		
	ENDIF

	IFND _DEBUG
_DEBUG set 0		
	ENDIF

*****************************************************************************

; Enable LIB functions (reduce size by commenting out unused)

LIB_ENABLE_RGB12_LERP		= 1	;RBG12 interpolation (fast color fades etc)
;LIB_ENABLE_SIN_Q14		= 1	;Q14 SIN/COS table (3d vector tables)
LIB_ENABLE_SIN_Q15		= 1	;Q15 SIN/COS table (3d vector tables)

;Compression libraries
;Compression (best to worst): PKF,SHR,AM7,NRV2S,DOY,CRA,LZ4
;Speed (quickest to slowest): LZ4,CRA,DOY,NRV2S,AM7,SHR,PKF
;Notes: nrv/cra/shr can do in-place with offset.

;Packers available, lz4 is reference for speed/size comparison
;LZ4		(1x speed, 100% size)
;Cranker	(1.3x, 93%)
;Doynamite68k	(1.8x, 88%)
;nrv2s		(2.0x, 85%)
;Arj m7		(3.4x, 79%)
;Shrinkler	(23x, 71%)
;PackFire	(46x, 70%)
;
;All basic routines take parameters
;a0, source/packed data
;a1, destination

;LIB_ENABLE_LZ4			= 1	;(LZ4) LZ4 
;LIB_ENABLE_CRANKER		= 1	;(CRA) LZO depacking
;LIB_ENABLE_DOYNAMITE68K		= 1	;(DOY) Doynamite68k
LIB_ENABLE_NRV2S		= 1	;(NRV) Ross's nrv2s
;LIB_ENABLE_NRV2R		= 1	;(NRV) Ross's nrv2r (in place no offset)
;LIB_ENABLE_ARJM7		= 1	;(AM7) Arj m7
;LIB_ENABLE_PACKFIRE_LARGE	= 1	;(PKF) Packfire LZMA depacking
;LIB_ENABLE_SHRINKLER		= 1	;(SHR) Shrinkler LZMA


*****************************************************************************

; To avoid syntax errors in visual studio code
	IFND _INTROWRAPPER
_INTROWRAPPER set 0		
	ENDIF


*****************************************************************************

;Shared data values and definitions
BPM_FRAMES = 112			;Number of frames per beat of music


;Some common colors
COLOR_NEON_PINK		= $f09
COLOR_NEON_PINK_LIGHT	= $f2b
COLOR_NEON_PINK_DARK	= $d07

COLOR_NEON_YELLOW	= $ee1
COLOR_NEON_YELLOW_LIGHT	= $ff3
COLOR_NEON_YELLOW_DARK	= $cc0

COLOR_NEON_PURPLE	= $70d
COLOR_NEON_PURPLE_LIGHT	= $93f
COLOR_NEON_PURPLE_DARK	= $50b

COLOR_NEON_GREEN	= $8f3
COLOR_NEON_GREEN_LIGHT	= $af5
COLOR_NEON_GREEN_DARK	= $6d1

COLOR_NEON_ORANGE	= $f90
COLOR_NEON_ORANGE_LIGHT	= $fb0
COLOR_NEON_ORANGE_DARK	= $f70

COLOR_NEON_RED		= $f00
COLOR_NEON_RED_LIGHT	= $f22
COLOR_NEON_RED_DARK	= $d00


;Our zoom pattern is 352 wide so we can use it for screen sizes from 320-352
;We make the adjustments for the actual screen we are using in 
;Zoom_X_Axis
;Do_Copper_Zoom
;Shared constants in IntroConfig.i and IntroSharedData.s

ZOOM_VALUES_NUM		=	256-1	;total number of values, including 0 (fully visible)
ZOOM_VALUES_INC 	= 	1	;Perspective increment
ZOOM_VALUES_START_Z	=	256
ZOOM_MAX		=	ZOOM_VALUES_NUM-1	;highest number to use in controller script

; In any code that uses this need to alter the offset of actual screen
; width so that it is centered in this 352 definition
ZOOM_PATTERN_WIDTH	=	352
ZOOM_PATTERN_BYTEWIDTH	=	ZOOM_PATTERN_WIDTH/8
ZOOM_PATTERN_WORDWIDTH	=	ZOOM_PATTERN_BYTEWIDTH/2
ZOOM_PATTERN_TOTALSIZE	=	ZOOM_PATTERN_BYTEWIDTH*(ZOOM_VALUES_NUM+1)
ZOOM_XOFFSET		=	ZOOM_PATTERN_WIDTH/2

	
	ENDC				; _INTROCONFIG_I