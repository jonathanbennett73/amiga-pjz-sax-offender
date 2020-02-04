	IFND _CUSTOMEXTRA_I
_CUSTOMEXTRA_I SET 1

*******************************************&**
* Custom Hardware File (c)1989-2019 Antiriad *
* Jonathan Bennett <jon@autoitscript.com     *
**********************************************

;Additonal hardware related values not present in CBM includes
_custom 	= $dff000
_ciaa		= $bfe001
_ciab		= $bfd000

_ExecBase	= 4
_Kickstart_Version = $f8000c

;Hardware registers, OCS
potgor	=$16

dskpth	=$20
dskptl	=$22

bltcpth	=$48
bltcptl	=$4a
bltbpth	=$4c
bltbptl	=$4e
bltapth	=$50
bltaptl	=$52
bltdpth	=$54
bltdptl	=$56

cop1lch	=$80
cop1lcl	=$82
cop2lch	=$84
cop2lcl	=$86

aud0lch	=$a0
aud0lcl	=$a2
aud0len	=$a4
aud0per	=$a6
aud0vol	=$a8
aud0dat	=$aa
aud1lch	=$b0
aud1lcl	=$b2
aud1len	=$b4
aud1per	=$b6
aud1vol	=$b8
aud1dat	=$ba
aud2lch	=$c0
aud2lcl	=$c2
aud2len	=$c4
aud2per	=$c6
aud2vol	=$c8
aud2dat	=$ca
aud3lch	=$d0
aud3lcl	=$d2
aud3len	=$d4
aud3per	=$d6
aud3vol	=$d8
aud3dat	=$da

bpl1pth	=$e0
bpl1ptl	=$e2
bpl2pth	=$e4
bpl2ptl	=$e6
bpl3pth	=$e8
bpl3ptl	=$ea
bpl4pth	=$ec
bpl4ptl	=$ee
bpl5pth	=$f0
bpl5ptl	=$f2
bpl6pth	=$f4
bpl6ptl	=$f6
bpl7pth	=$f8
bpl7ptl	=$fa
bpl8pth	=$fc
bpl8ptl	=$fe

spr0pth	=$120
spr0ptl	=$122
spr1pth	=$124
spr1ptl	=$126
spr2pth	=$128
spr2ptl	=$12a
spr3pth	=$12c
spr3ptl	=$12e
spr4pth	=$130
spr4ptl	=$132
spr5pth	=$134
spr5ptl	=$136
spr6pth	=$138
spr6ptl	=$13a
spr7pth	=$13c
spr7ptl	=$13e

spr0pos	=$140
spr0ctl	=$142
spr0data=$144
spr0datb=$146
spr1pos	=$148
spr1ctl	=$14a
spr1data=$14c
spr1datb=$14e
spr2pos	=$150
spr2ctl	=$152
spr2data=$154
spr2datb=$156
spr3pos	=$158
spr3ctl	=$15a
spr3data=$15c
spr3datb=$15e
spr4pos	=$160
spr4ctl	=$162
spr4data=$164
spr4datb=$166
spr5pos	=$168
spr5ctl	=$16a
spr5data=$16c
spr5datb=$16e
spr6pos	=$170
spr6ctl	=$172
spr6data=$174
spr6datb=$176
spr7pos	=$178
spr7ctl	=$17a
spr7data=$17c
spr7datb=$17e


;ecs and aa regs
sprhdat	=$078
bplhdat	=$07a
lisaid	=$07c
bplhmod	=$1e6
sprhpth	=$1e8
sprhptl	=$1ea
bplhpth	=$1ec
bplhptl	=$1ee


;colors
color00	=$180
color01	=$182
color02	=$184
color03	=$186
color04	=$188
color05	=$18a
color06	=$18c
color07	=$18e
color08	=$190
color09	=$192
color10	=$194
color11	=$196
color12	=$198
color13	=$19a
color14	=$19c
color15	=$19e
color16	=$1a0
color17	=$1a2
color18	=$1a4
color19	=$1a6
color20	=$1a8
color21	=$1aa
color22	=$1ac
color23	=$1ae
color24	=$1b0
color25	=$1b2
color26	=$1b4
color27	=$1b6
color28	=$1b8
color29	=$1ba
color30	=$1bc
color31	=$1be

;Blitter logic (not)
;move.w	#BLTEN_ACD+(BLT_A|BLT_C),bltcon0
;vasm operators:
;& (bitwise and) 
;^ (bitwise exclusive-or) 
;| (bitwise inclusive-or) 
;! (bitwise not)
;BLTCON0
BLT_SRC_A	= $0800
BLT_SRC_B	= $0400
BLT_SRC_C	= $0200
BLT_SRC_D	= $0100
BLT_SRC_AD	= (BLT_SRC_A|BLT_SRC_D)
BLT_SRC_ABD	= (BLT_SRC_A|BLT_SRC_B|BLT_SRC_D)
BLT_SRC_ACD	= (BLT_SRC_A|BLT_SRC_C|BLT_SRC_D)
BLT_SRC_ABCD	= (BLT_SRC_A|BLT_SRC_B|BLT_SRC_C|BLT_SRC_D)

BLT_A	= %11110000
BLT_B	= %11001100
BLT_C	= %10101010

;BLTCON1
BLT_LINEMODE	= $1
BLT_FILL_OR	= $8
BLT_FILL_XOR	= $10
BLT_FILL_CARRYIN= $4
BLT_ONEDOT	= $2
BLT_OVFLAG	= $20
BLT_SIGNFLAG	= $40
BLT_BLITREVERSE	= $2


;Flags

; dmacon
;15	SET/CLR	Set/Clear control bit. Determines if bits written with a 1 get set or cleared Bits written with a zero are unchanged
;14	BBUSY	Blitter busy status bit (read only)
;13	BZERO	Blitter logic zero status bit (read only)
;12	X	

;11	X	
;10	BLTPRI	Blitter DMA priority (over CPU micro) (also called "blitter nasty") (disables /BLS pin, preventing micro from stealing any bus cycles while blitter DMA is running)
;09	DMAEN	Enable all DMA below (also UHRES DMA)
;08	BPLEN	Bit plane DMA enable

;07	COPEN	Coprocessor DMA enable
;06	BLTEN	Blitter DMA enable
;05	SPREN	Sprite DMA enable
;04	DSKEN	Disk DMA enable

;03	AUD3EN	Audio channel 3 DMA enable
;02	AUD2EN	Audio channel 2 DMA enable
;01	AUD1EN	Audio channel 1 DMA enable
;00	AUD0EN	Audio channel 0 DMA enable
;DMAF_SETCLR	= $8000
;DMAF_BLTPRI	= $0400
;DMAF_DMAEN	= $0200
;DMAF_BPLEN	= $0100
;DMAF_COPEN	= $0080
;DMAF_BLTEN	= $0040
;DMAF_SPREN	= $0020
;DMAF_DSKEN	= $0010
;DMAF_AUDEN	= $000f


; INTENA
;15	SET/CLR		Set/clear control bit. Determines if bits written with a 1 get set or cleared. Bits written with a zero are always unchanged.
;14	INTEN		Master interrupt (enable only, no request)
;13	EXTER	6	External interrupt (CiaB interrupts)
;12	DSKSYN	5	Disk sync register (DSKSYNC) matches disk

;11	RBF	5	Serial port receive buffer full
;10	AUD3	4	Audio channel 3 block finished
;09	AUD2	4	Audio channel 2 block finished
;08	AUD1	4	Audio channel 1 block finished

;07	AUD0	4	Audio channel 0 block finished
;06	BLIT	3	Blitter has finished
;05	VERTB	3	Start of vertical blank
;04	COPER	3	Coprocessor

;03	PORTS	2	I/O Ports and timers (CiaA interrupts)
;02	SOFT	1	Reserved for software initiated interrupt.
;01	DSKBLK	1	Disk block finished
;00	TBE	1	Serial port transmit buffer empty
;INTF_SETCLR 	= $8000
;INTF_INTEN	= $4000
;INTF_EXTER	= $2000
;INTF_BLIT	= $0040
;INTF_VERTB	= $0020
;INTF_COPER	= $0010
;INTF_PORTS	= $0008
;INTF_SOFT	= $0004
		

;Traps and vectors
_Bus_Error	=$08
_Address_Error	=$0c
_Illegal_Inst	=$10
_Div_By_Zero	=$14
_CHK_Inst	=$18
_TRAPV_Inst	=$1c
_Privilege_Vio	=$20
_Trace		=$24
_LineA_Emu	=$28
_LineF_Emu	=$2c

_Level1Vector	=$64
_Level2Vector	=$68
_Level3Vector	=$6c
_Level4Vector	=$70
_Level5Vector	=$74
_Level6Vector	=$78
_Level7Vector	=$7c

_Trap0		=$80
_Trap1		=$84
	
; Cache Control Register ( CACR )
;
; 	BIT#	FUNCTION
;	0	Inst Cache Enable
;	1	Freeze Inst Cache
;	2	Clear Entry in Inst Cache
;	3	Clear Inst Cache
;	4	Inst Burst Enable
;	5	0
;	6	0	
;	7	0
;	8	Data Cache Enable
;	9	Freeze Data Cache
;	10	Clear Entry in Data Cache
;	11	Clear Data Cache
;	12	Data Burst Enable
;	13	Write Allocate (always set - I think...)
;
;	movec.l	dn,CACR


	ENDC	;EXTRA_I