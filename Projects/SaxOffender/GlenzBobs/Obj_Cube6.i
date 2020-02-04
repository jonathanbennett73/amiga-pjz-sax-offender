	IFND _OBJ_CUBE6
_OBJ_CUBE6 SET 1
	ELSE
_OBJ_CUBE6 SET _OBJ_CUBE6+1
	ENDC


OBJ_CUBE6_NUMPTS = 8
Obj_Cube6_Info:
	dc.w	0			; initialised (happens on first load)
	dc.w	0,0,0			; pos, x,y,z
	dc.w	0,0,0			; current rotation, x,y,z
	dc.w	1,2,3			; Rotation step, x,y,z
	dc.w	1			; Complex 1/0
	dc.w	1			; Num frames min
	dc.l	Obj_Cube6_PtsBuffer	; Pts ptr (in use/buffer)
	dc.l	Obj_Cube6_Pts_CubeSmall	; Initial points ptr
	dc.l	Obj_Cube6_Facelist	; Facelist ptr
	dc.w	0,0,0			; Morph active flag, counter, speed

; Points are loaded to here from Ob1_Pts. So can do transforms without
; trashing original points.
Obj_Cube6_PtsBuffer:
	ds.w	1
	ds.w	3*OBJ_CUBE6_NUMPTS

Obj_Cube6_Pts_CubeSmall:
	dc.w	OBJ_CUBE6_NUMPTS-1
	dc.w -50,50,50		;0
	dc.w 50,50,50		;1
	dc.w 50,-50,50		;2
	dc.w -50,-50,50		;3

	dc.w -50,50,-50		;4
	dc.w 50,50,-50		;5
	dc.w 50,-50,-50		;6
	dc.w -50,-50,-50	;7

Obj_Cube6_Facelist:
	dc.w	6-1 ;6-1
	dc.l	Obj_Cube6_f1
	dc.l	Obj_Cube6_f2
	dc.l	Obj_Cube6_f3
	dc.l	Obj_Cube6_f4
	dc.l	Obj_Cube6_f5
	dc.l	Obj_Cube6_f6

Obj_Cube6_f1		;back
	VEC_FACE	4-1,1,1,-1,-1
	VEC_VERTEX5	3,2,1,0,3

Obj_Cube6_f2:		;front
	VEC_FACE	4-1,1,1,-1,-1
	VEC_VERTEX5	4,5,6,7,4

Obj_Cube6_f3		;top
	VEC_FACE	4-1,2,2,-1,-1
	VEC_VERTEX5	4,0,1,5,4

Obj_Cube6_f4		;bottom
	VEC_FACE	4-1,2,2,-1,-1
	VEC_VERTEX5	7,6,2,3,7	

Obj_Cube6_f5		;left
	VEC_FACE	4-1,3,3,-1,-1
	VEC_VERTEX5	0,4,7,3,0

Obj_Cube6_f6		;right
	VEC_FACE	4-1,3,3,-1,-1
	VEC_VERTEX5	5,1,2,6,5


