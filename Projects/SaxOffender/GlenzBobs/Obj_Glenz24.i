	IFND _OBJ_GLENZ24
_OBJ_GLENZ24 SET 1
	ELSE
_OBJ_GLENZ24 SET _OBJ_GLENZ24+1
	ENDC

;Glenz vectors
;3 bitplanes
;
; color01 := $eef = off white    
; color02 := $306 = dark blue
; color05 := $fff = pure white    
; color06 := $63d = light blue    
;
;Front white face:
; overlapping with back transparent faces is off white   - color01  bpl 1 0 0
; overlapping with back white face is pure white         - color05  bpl 1 0 1
; DRAW ON BPL 1 (color01)
;
;Back white face
; overlapping with front transparent face is light blue  - color06  bpl 0 1 1
; overlapping with front white face is pure white        - color05  bpl 1 0 1
; DRAW ON BPL 3 (color04)
;
;Front transparent face: = 0 1 0 = 2
; overlapping back transparent is dark blue              - color02  bpl 0 1 0
; overlapping back white face is light blue              - color06  bpl 0 1 1
; DRAW ON BPL 2 (color02)
;
;Back transparent face: = 0 0 0 = 0
; overlapping front transparent is dark blue             - color02  bpl 0 1 0
; overlapping front white faceis off white               - color01  bpl 1 0 0
; DON'T DRAW

;Use color 1 (white), 2 (trans) for visible faces
;Use color 4 (undefined), 0 for back faces

;This means that each face is actually drawn on ONE bitplane.

;My vector routine allows a different colour to be set for front/back faces
;instead of just culling. Makes it easy to turn it into a glenz routine.

;80 pt at 192width, z pos = -6 without clipping
;80 pt at 224 width, z pos = -37 without clipping 
;85 pt at 224 width,  z pos = -37 without clipping 
OBJ_GLENZ24_NUMPTS = 14
Obj_Glenz24_Info:
	dc.w	0			; initialised (happens on first load)
	dc.w	0,0,0			; pos, x,y,z
	dc.w	0,0,0			; current rotation, x,y,z
	dc.w	0,0,0			; Rotation step, x,y,z
	dc.w	0			; Complex 1/0
	dc.w	1			; Num frames max
	dc.l	Obj_Glenz24_PtsBuffer	; 22 - Pts ptr (in use/buffer)
	dc.l	Obj_Glenz24_Pts_ClassicSphere	; 26 - Initial points ptr
	dc.l	Obj_Glenz24_Facelist	; 30 - Facelist ptr
	dc.w	0,0,0			; Morph active flag, counter, speed

; Points are loaded to here from Obj_Glenz24_Pts. So can do transforms without
; trashing original points.
Obj_Glenz24_PtsBuffer:
	ds.w	1
	ds.w	3*OBJ_GLENZ24_NUMPTS

Obj_Glenz24_Pts_Cube:	;CUBE
	; points defined looking at object from the front
	; postitive x = up, y = right, z = forward
	dc.w	OBJ_GLENZ24_NUMPTS-1
	dc.w	-50,50,-50	;0 front top left
	dc.w	50,50,-50	;1 front top right
	dc.w	50,-50,-50	;2 front bottom right
	dc.w	-50,-50,-50	;3 front bottom left

	dc.w	-50,50,50	;4 back top left
	dc.w	50,50,50	;5 back top right
	dc.w	50,-50,50	;6 back bottom right
	dc.w	-50,-50,50	;7 back bottom left

	dc.w	0,0,-50		;8 front middle
	dc.w	0,0,50		;9 back middle
	dc.w	0,50,0		;10 top middle
	dc.w	0,-50,0		;11 bottom middle
	dc.w	50,0,0		;12 right middle
	dc.w	-50,0,0		;13 left middle

Obj_Glenz24_Pts_CubeSmall:	;CUBE
	; points defined looking at object from the front
	; postitive x = up, y = right, z = forward
	dc.w	OBJ_GLENZ24_NUMPTS-1
	dc.w	-40,40,-40	;0 front top left
	dc.w	40,40,-40	;1 front top right
	dc.w	40,-40,-40	;2 front bottom right
	dc.w	-40,-40,-40	;3 front bottom left

	dc.w	-40,40,40	;4 back top left
	dc.w	40,40,40	;5 back top right
	dc.w	40,-40,40	;6 back bottom right
	dc.w	-40,-40,40	;7 back bottom left

	dc.w	0,0,-40		;8 front middle
	dc.w	0,0,40		;9 back middle
	dc.w	0,40,0		;10 top middle
	dc.w	0,-40,0		;11 bottom middle
	dc.w	40,0,0		;12 right middle
	dc.w	-40,0,0		;13 left middle

Obj_Glenz24_Pts2:
	; points defined looking at object from the front
	; postitive x = up, y = right, z = forward
	dc.w	OBJ_GLENZ24_NUMPTS-1
	dc.w	-50,12,-50	;0 front top left
	dc.w	50,12,-50	;1 front top right
	dc.w	50,-12,-50	;2 front bottom right
	dc.w	-50,-12,-50	;3 front bottom left

	dc.w	-50,12,50	;4 back top left
	dc.w	50,12,50	;5 back top right
	dc.w	50,-12,50	;6 back bottom right
	dc.w	-50,-12,50	;7 back bottom left

	dc.w	0,0,-50		;8 front middle
	dc.w	0,0,50		;9 back middle
	dc.w	0,50,0		;10 top middle
	dc.w	0,-50,0		;11 bottom middle
	dc.w	50,0,0		;12 right middle
	dc.w	-50,0,0		;13 left middle

Obj_Glenz24_Pts_Pyramid:	;pyramid ish
	dc.w	OBJ_GLENZ24_NUMPTS-1
	dc.w	-45,45,-45
	dc.w	45,45,-45
	dc.w	45,-45,-45
	dc.w	-45,-45,-45

	dc.w	-20,20,45
	dc.w	20,20,45
	dc.w	20,-20,45
	dc.w	-20,-20,45

	dc.w	0,0,-75
	dc.w	0,0,75
	dc.w	0,40,0
	dc.w	0,-40,0
	dc.w	40,0,0
	dc.w	-40,0,0

Obj_Glenz24_Pts_Pyramid2:	;pyramid ish
	dc.w	OBJ_GLENZ24_NUMPTS-1
	dc.w	-45,45,-45
	dc.w	45,45,-45
	dc.w	45,-45,-45
	dc.w	-45,-45,-45

	dc.w	-20,20,75
	dc.w	20,20,75
	dc.w	20,-20,75
	dc.w	-20,-20,75

	dc.w	0,0,-45
	dc.w	0,0,75
	dc.w	0,40,0
	dc.w	0,-40,0
	dc.w	40,0,0
	dc.w	-40,0,0


Obj_Glenz24_Pts_ClassicSphere:	;tetraicosahedron (classic ball)
	dc.w	OBJ_GLENZ24_NUMPTS-1
	dc.w	-50,50,-50
	dc.w	50,50,-50
	dc.w	50,-50,-50
	dc.w	-50,-50,-50
	dc.w	-50,50,50
	dc.w	50,50,50
	dc.w	50,-50,50
	dc.w	-50,-50,50
	dc.w	0,0,-80
	dc.w	0,0,80
	dc.w	0,80,0
	dc.w	0,-80,0
	dc.w	80,0,0
	dc.w	-80,0,0

Obj_Glenz24_Pts_Disc:	;flat disc
	dc.w	OBJ_GLENZ24_NUMPTS-1
	dc.w	-20,50,-50
	dc.w	20,50,-50
	dc.w	20,-50,-50
	dc.w	-20,-50,-50
	
	dc.w	-20,50,50
	dc.w	20,50,50
	dc.w	20,-50,50
	dc.w	-20,-50,50

	dc.w	0,0,-80
	dc.w	0,0,80
	dc.w	0,80,0
	dc.w	0,-80,0
	dc.w	20,0,0
	dc.w	-20,0,0

Obj_Glenz24_Pts_Box:
	; points defined looking at object from the front
	; postitive x = up, y = right, z = forward
	dc.w	OBJ_GLENZ24_NUMPTS-1
	dc.w	-48,20,-70	;0 front top left
	dc.w	48,20,-70	;1 front top right
	dc.w	48,-20,-70	;2 front bottom right
	dc.w	-48,-20,-70	;3 front bottom left

	dc.w	-48,20,70	;4 back top left
	dc.w	48,20,70	;5 back top right
	dc.w	48,-20,70	;6 back bottom right
	dc.w	-48,-20,70	;7 back bottom left

	dc.w	0,0,-70		;8 front middle
	dc.w	0,0,70		;9 back middle
	dc.w	0,48,0		;10 top middle
	dc.w	0,-48,0		;11 bottom middle
	dc.w	48,0,0		;12 right middle
	dc.w	-48,0,0		;13 left middle

Obj_Glenz24_Pts_FlatSquare:	;flat square
	dc.w	OBJ_GLENZ24_NUMPTS-1
	dc.w	-10,50,-50
	dc.w	10,50,-50
	dc.w	10,-50,-50
	dc.w	-10,-50,-50
	dc.w	-10,50,50
	dc.w	10,50,50
	dc.w	10,-50,50
	dc.w	-10,-50,50
	dc.w	0,0,-50
	dc.w	0,0,50
	dc.w	0,50,0
	dc.w	0,-50,0
	dc.w	10,0,0
	dc.w	-10,0,0

Obj_Glenz24_Facelist:
	dc.w	24-1 ;6-1
	dc.l	Obj_Glenz24_f1
	dc.l	Obj_Glenz24_f2
	dc.l	Obj_Glenz24_f3
	dc.l	Obj_Glenz24_f4
	dc.l	Obj_Glenz24_f5
	dc.l	Obj_Glenz24_f6
	dc.l	Obj_Glenz24_f7
	dc.l	Obj_Glenz24_f8
	dc.l	Obj_Glenz24_f9
	dc.l	Obj_Glenz24_f10
	dc.l	Obj_Glenz24_f11
	dc.l	Obj_Glenz24_f12
	dc.l	Obj_Glenz24_f13
	dc.l	Obj_Glenz24_f14
	dc.l	Obj_Glenz24_f15
	dc.l	Obj_Glenz24_f16
	dc.l	Obj_Glenz24_f17
	dc.l	Obj_Glenz24_f18
	dc.l	Obj_Glenz24_f19
	dc.l	Obj_Glenz24_f20
	dc.l	Obj_Glenz24_f21
	dc.l	Obj_Glenz24_f22
	dc.l	Obj_Glenz24_f23
	dc.l	Obj_Glenz24_f24

; Connections should be defined in as if you were facing that side in clockwise order
Obj_Glenz24_f1:	;front, top tri, white
	VEC_FACE	3-1,1,1,4,4
	VEC_VERTEX4	8,0,1,8

Obj_Glenz24_f2: ;front, right tri, trans
	VEC_FACE	3-1,2,2,-1,-1
	VEC_VERTEX4	8,1,2,8

Obj_Glenz24_f3:	;front, bot tri, white
	VEC_FACE	3-1,1,1,4,4
	VEC_VERTEX4	8,2,3,8

Obj_Glenz24_f4: ;front, left tri, trans
	VEC_FACE	3-1,2,2,-1,-1
	VEC_VERTEX4	8,3,0,8

Obj_Glenz24_f5:	;back, top tri, white
	VEC_FACE	3-1,1,1,4,4
	VEC_VERTEX4	9,5,4,9

Obj_Glenz24_f6: ;back, right tri, trans
	VEC_FACE	3-1,2,2,-1,-1
	VEC_VERTEX4	9,4,7,9

Obj_Glenz24_f7:	;back, bot tri, white
	VEC_FACE	3-1,1,1,4,4
	VEC_VERTEX4	9,7,6,9

Obj_Glenz24_f8: ;back, left tri, trans
	VEC_FACE	3-1,2,2,-1,-1
	VEC_VERTEX4	9,6,5,9

Obj_Glenz24_f9:	;top, top tri, trans
	VEC_FACE	3-1,2,2,-1,-1
	VEC_VERTEX4	10,4,5,10

Obj_Glenz24_f10: ;top, right tri, white
	VEC_FACE	3-1,1,1,4,4
	VEC_VERTEX4	10,5,1,10

Obj_Glenz24_f11:	;top, bot tri, trans
	VEC_FACE	3-1,2,2,-1,-1
	VEC_VERTEX4	10,1,0,10

Obj_Glenz24_f12: ;top, left tri, white
	VEC_FACE	3-1,1,1,4,4
	VEC_VERTEX4	10,0,4,10

Obj_Glenz24_f13:	;bot, top tri, trans
	VEC_FACE	3-1,2,2,-1,-1
	VEC_VERTEX4	11,3,2,11

Obj_Glenz24_f14: ;bot, right tri, white
	VEC_FACE	3-1,1,1,4,4
	VEC_VERTEX4	11,2,6,11

Obj_Glenz24_f15:	;bot, bot tri, trans
	VEC_FACE	3-1,2,2,-1,-1
	VEC_VERTEX4	11,6,7,11

Obj_Glenz24_f16: ;bot, left tri, white
	VEC_FACE	3-1,1,1,4,4
	VEC_VERTEX4	11,7,3,11

Obj_Glenz24_f17: ;right, top tri, trans
	VEC_FACE	3-1,2,2,-1,-1
	VEC_VERTEX4	12,1,5,12

Obj_Glenz24_f18: ;right, right tri, white
	VEC_FACE	3-1,1,1,4,4
	VEC_VERTEX4	12,5,6,12

Obj_Glenz24_f19: ;right, bot tri, trans
	VEC_FACE	3-1,2,2,-1,-1
	VEC_VERTEX4	12,6,2,12

Obj_Glenz24_f20: ;right, left tri, white
	VEC_FACE	3-1,1,1,4,4
	VEC_VERTEX4	12,2,1,12

Obj_Glenz24_f21: ;left, top tri, trans
	VEC_FACE	3-1,2,2,-1,-1
	VEC_VERTEX4	13,4,0,13

Obj_Glenz24_f22: ;left, right tri, white
	VEC_FACE	3-1,1,1,4,4
	VEC_VERTEX4	13,0,3,13

Obj_Glenz24_f23: ;left, bot tri, trans
	VEC_FACE	3-1,2,2,-1,-1
	VEC_VERTEX4	13,3,7,13

Obj_Glenz24_f24: ;left, left tri, white
	VEC_FACE	3-1,1,1,4,4
	VEC_VERTEX4	13,7,4,13


