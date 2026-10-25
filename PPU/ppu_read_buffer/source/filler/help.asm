org 0x6CF0

	dw HelpString

GenerateNTSCtables:
	; xbegins is a too large a table to be stored in the ROM.
	; We must generate it at runtime.
	; The formula is:
	;	xcenter = x * (282*8)/320 + 4
	;	xbegin  = xcenter-6
	;	int_xbegin = floor(xbegin + 0.5)
	;	result = 4 * min(int_xbegin,  8*282-1)
	mov bp, 282*8
	mov bx, 320
	mov di, si ;xbegins
	mov cx, bx
	cld
.loop:
	 mov ax, di
	 sub ax, si ;xbegins
	 shr ax, 1 ; ax=x
	 mul bp
	 add ax, 160 ; for rounding
	 adc dx, 0
	 div bx
	 add ax, 4-6
	 cmp ax, bp
	 jl .ok
	 lea ax, [bp-1]
.ok:	 sal ax, 2
	 stosw
	loop .loop
	
	; While we are at it, we could also generate
	; the sincos table.
	; It is 24 elements long, and composed of: sin(pi/6 * x)
	mov ax, 0
.index  EQU $-2
	mov cx, 24

	;mov bx, sincos-4
	lea bx, [si + 320*4 - 4]
	
.sincosloop:
	fldpi
	fild word [.index]
	fmulp st1
	add bx, 4
	fmul dword [.div6]
	fsin
	inc word [.index]
	fstp dword [bx]
	loop .sincosloop
	ret
.div6	dd 0.16666666666666666667


HelpString:
