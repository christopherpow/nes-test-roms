section .const

opnames: db "bitxseinynophphaneorandeyjmplasreshyshseclinxsharranclclvRESNMINTtaxbnebeqbccldybmisbxjsrtincmpbplpbvcpxstyadcpybvsedexstaydecsaxstxaslosbcshxldaroldxrortsxlsrlaxbrk"
opmatch: ; Which mnemonic for each opcode
	db 162,  18, 132, 133,   9,  18, 132, 133,  11,  18, 132,  50,   9,  18, 132, 133
	db  95,  18, 132, 133,   9,  18, 132, 133,  52,  18,   9, 133,   9,  18, 132, 133
	db  86,  20, 145, 158,   0,  20, 145, 158,  96,  20, 145,  50,   0,  20, 145, 158
	db  80,  20, 145, 158,   9,  20, 145, 158,  38,  20,   9, 158,   9,  20, 145, 158
	db  88,  17, 156,  30,   9,  17, 156,  30,  13,  17, 156,  29,  25,  17, 156,  30
	db  99,  17, 156,  30,   9,  17, 156,  30,  40,  17,   9,  30,   9,  17, 156,  30
	db 152, 107, 150,  48,   9, 107, 150,  48,  27, 107, 150,  47,  25, 107, 150,  48
	db 112, 107, 150,  48,   9, 107, 150,  48,   4, 107,   9,  48,   9, 107, 150,  48
	db 104, 119,   9, 126, 104, 119, 129, 126,  22, 119, 130,  15, 104, 119, 129, 126
	db  74, 119, 129,  45, 104, 119, 129, 126, 105, 119,   2,  36,  33, 119, 139,  45
	db  77, 142, 147, 159,  77, 142, 147, 159, 120, 142,  65, 159,  77, 142, 147, 159
	db 137, 142, 147, 159,  77, 142, 147, 159,  54, 142, 153,  28,  77, 142, 147, 159
	db 109,  92,   9, 108, 109,  92, 123, 108,   6,  92, 116,  83, 109,  92, 123, 108
	db  68,  92, 123, 108,   9,  92, 123, 108,  76,  92,   9, 108,   9,  92, 123, 108
	db 101, 136,   9,  82, 101, 136,  90,  82,  42, 136,   9, 136, 101, 136,  90,  82
	db  71, 136,  90,  82,   9, 136,  90,  82, 114, 136,   9,  82,   9, 136,  90,  82
	db  60,  57,  62
unofficial:
	db 00111001b,00011001b,00111001b,00111001b,00110001b,00010001b,00111001b,00111001b
	db 00111001b,00010001b,00111001b,00111001b,00111001b,00010001b,00111001b,00111001b
	db 10110001b,00010001b,00110001b,00011011b,00010001b,00010001b,00110001b,00010001b
	db 00110001b,00010001b,00111001b,00111001b,00110001b,00000001b,00111001b,00111001b
	db 00100000b
modetable: ; Which addressing mode for each opcode
	db 0x06,0x16,0x33,0x33,0x01,0x01,0x88,0x88,0x27,0x77,0x44,0x44,0x0A,0x0A,0x99,0x99
	db 0x86,0x16,0x33,0x33,0x01,0x01,0x88,0x88,0x27,0x77,0x44,0x44,0x0A,0x0A,0x99,0x99
	db 0x06,0x16,0x33,0x33,0x01,0x01,0x88,0x88,0x27,0x77,0x44,0x44,0x0A,0x0A,0x99,0x99
	db 0x06,0x16,0x33,0x33,0x01,0x01,0xB8,0x88,0x27,0x77,0x44,0x44,0x0A,0x0A,0x99,0x99
	db 0x16,0x16,0x33,0x33,0x01,0x01,0x88,0x88,0x27,0x77,0x44,0x55,0x0A,0x0A,0x99,0xAA
	db 0x16,0x16,0x33,0x33,0x01,0x01,0x88,0x88,0x27,0x77,0x44,0x55,0x0A,0x0A,0x99,0xAA
	db 0x16,0x16,0x33,0x33,0x01,0x01,0x88,0x88,0x27,0x77,0x44,0x44,0x0A,0x0A,0x99,0x99
	db 0x16,0x16,0x33,0x33,0x01,0x01,0x88,0x88,0x27,0x77,0x54,0x44,0x0A,0x0A,0xA9,0x99
	db 0x00,0x00
pfxtable EQU $-2
	db 0,0 ;,0,0
	db "#",0,0,0
	db "(",0,0,0
	db ",x",0,0
	db ",y",0,0
	db ",x)",0
	db "),y",0
	db ")" ;,0,0,0
sfxtable EQU pfxtable
pfxmul:	  db 0,4*1,4*0,4*0,4*0,4*0,4*2,4*2,4*0,4*0,4*0,4*2
sfxmul:   db 0,4*0,4*0,4*0,4*3,4*4,4*5,4*6,4*0,4*3,4*4,4*7
	  ;  0 1 2 3 4 5 6 7 8 9 A B
sizetable:db 0,1,1,1,1,1,1,1,2,2,2,2
	  ;  | | | | | | | | | | | |
	  ;  | | | | | | | | | | | |
	  ;  | | | | | | | | | | | |
	  ;  | | | | | | | | | | | |
	  ;  | | | | | | | | | | | |
	  ;  | | | | | | | | | | | |___________ jump
	  ;  | | | | | | | | | | |___________ absy
	  ;  | | | | | | | | | |___________ absx
	  ;  | | | | | | | | |___________ abs
	  ;  | | | | | | | |___________ indy
	  ;  | | | | | | |___________ indx
	  ;  | | | | | |___________ zpy
	  ;  | | | | |___________ zpx
	  ;  | | | |___________ zp
	  ;  | | |___________ relative (branch)
	  ;  | |___________ immediate
	  ;  |___________ implied/A

section .text

DEBUG:
	ret ; Replaced by Commandline if necessary
	pusha
	 mov ax, [R_PC]
	 call PrintHexWord
	 mov ax, 2
	 call PrintSpaces
	 mov ax, [C_OPCODE]
	 call PrintHexByte
	 
	 mov bx, [C_OPCODE]
	 ; ^ BX = opcode
	 push bx
	  shr bx, 1
	  mov ax, [modetable+bx]
	  jc .lower
	  shr al, 4
.lower:
	  and ax, 15
	  mov [.MODE], al
	  mov dx, ax
	  ; ^ DX = mode. Leave AH=0.
	 pop bx
	 mov al, [opmatch+bx] ; Depends on AH=0
	 add ax, opnames
	 mov [.INSNAME], ax ; insname = opnames + opmatch[opcode]
	 mov ax, dx ; mode
	  mov bx, pfxmul
	  xlatb
	  cbw
	  add ax, pfxtable
	  mov [.PFX], ax
	 mov ax, dx
	  mov bx, sfxmul
	  xlatb
	  cbw
	  add ax, sfxtable
	  mov [.SFX], ax
	 mov ax, dx
	  mov bx, sizetable
	  xlatb
	  cbw
	  mov [.SIZE], ax ; n
	 mov bx, 1
.HexLoop:
	  push bx
	   call .AddSpace
	   cmp bx, word 0xAAAA
.SIZE	   EQU $-2
	   ja .NotHex
	   mov ax, [R_PC]
	   add ax, bx
	   call DebugRB
	   call PrintHexByte
	   jmp .MaybeDidHex
.NotHex:
	   mov ax, 2
	   call PrintSpaces
.MaybeDidHex:
	  pop bx
	  inc bx
	  cmp bx, 3
	  jb .HexLoop
	 call .AddSpace

	 mov bx, [C_OPCODE]
	 shr bx, 3
	 mov cl, [C_OPCODE]
	 and cl, 7
	 inc cx
	 mov al, [unofficial+bx]
	 shl al, cl
	 mov dl, ' '
	 jnc .NotUnofficial
	 mov dl, '*'
.NotUnofficial:	
	 call ConsolePutc
	 xor di, di
	 mov bx, 0
.INSNAME EQU $-2
	 mov dl, [bx+0]
	 call .AddChar
	 mov dl, [bx+1]
	 call .AddChar
	 mov dl, [bx+2]
	 call .AddChar
	 call .AddSpace
	 mov bx, 0
.PFX	 EQU $-2
.PfxLoop:mov dl, [bx]
	 or dl, dl
	 jz .DidPfx
	 inc bx
	 call .AddChar
	 jmp .PfxLoop
.DidPfx: cmp [.SIZE], word 0
	 je .NotHexC
	 mov dl, '$'
	 call .AddChar
.NotHexC:mov al, [C_OPCODE]
	 and al, 0x9F
	 cmp al, 0x0A
	 jne .NotImplied
	 call .AddSpace
	 mov dl, 'a'
	 call .AddChar
.NotImplied:
	 xor ax, ax
	 mov [.ADDR], ax
	 mov ax, 0x0000
.MODE	 EQU $-2
	 cmp al, 2
	 jne .DispAddr
	 ; Do branch
	 mov ax, [R_PC]
	 inc ax
	 call DebugRB
	 cbw
	 add ax, [R_PC]
	 inc ax
	 inc ax
	 mov [.ADDR], ax
	 call PrintHexWord
	 add di, 4
	 jmp .SkipDispAddr
.DispAddr:
	 mov ax, [.SIZE]
.LoopDispAddr:
	 cmp ax, 1
	 jb .SkipDispAddr
	 push ax
	  add ax, [R_PC]
	  call DebugRB
	  call PrintHexByte
	  add di, 2
	  mov ah, [.ADDR]
	  mov [.ADDR], ax
	 pop ax
	 dec ax
	 jmp .LoopDispAddr
.SkipDispAddr:
	 mov bx, 0
.SFX	 EQU $-2
.SfxLoop:mov dl, [bx]
	 or dl, dl
	 jz .DidSfx
	 inc bx
	 call .AddChar
	 jmp .SfxLoop
.DidSfx:
	 ; Print spaces until N characters
	 call .AddSpace
	 cmp di, 20
	 jb .DidSfx
	 ; Print register contents
	 mov dl, 'A'
	 mov al, [R_A]
	 call .PrintReg
	 mov dl, 'X'
	 mov al, [R_X]
	 call .PrintReg
	 mov dl, 'Y'
	 mov al, [R_Y]
	 call .PrintReg
	 mov dl, 'P'
	 mov al, [R_P]
	 or al, 0x20
	 call .PrintReg
	 mov dl, 'S'
	 mov al, [R_S]
	 call .PrintReg
	 
	 ; newline
	 call PrintNewline
	popa
	ret
.PrintReg:
	call ConsolePutc
	mov dl, ':'
	call ConsolePutc
	call PrintHexByte
.AddSpace:
	mov dl, ' '
	;jmp ConsolePutc
.AddChar:
	call ConsolePutc
	inc di
	ret

.ADDR dw 0

section .const
HEXBYTES db '0123456789ABCDEF'
section .text

PrintHexByte:
	pusha
	 mov bx, HEXBYTES
	 push ax
	  shr al, 4
	  xlatb
	  mov dl, al
	  call ConsolePutc
	 pop ax
	 and al, 15
	 xlatb
	 mov dl, al
	 call ConsolePutc
	popa
	ret
	 
PrintHexWord:
	push ax
	 xchg al, ah
	 call PrintHexByte
	 xchg al, ah
	 call PrintHexByte
	pop ax
	ret
PrintSpaces:
	pusha
	 mov dl, ' '
	 xchg cx, ax
	 jcxz .done
.more:	 call ConsolePutc
	 loop .more
.done:	popa
	ret

DebugRB:
	cmp ax, 0x2000
	jb RB
	cmp ax, 0x8000
	jae RB
	mov al, 0xFF
	ret

PrintNewline:
	mov dl, 13
	call ConsolePutc
	mov dl, 10
	call ConsolePutc
	;jmp ConsoleFlush
ConsoleFlush:
	mov ah, 68h
	mov bx, 1
	int 21h
	ret
