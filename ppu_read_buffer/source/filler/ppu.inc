

P_SYSCTRL	db 0
P_DISPCTRL	db 0
P_STATUS	db 0
PPU_READ_BUFFER	db 0

P_SCANLINE	dw 0
	vseg	dw 0xA000 ; VGA video segment
	; vseg is here so that ShowScanline can use the "les" instruction.

P_SPRINPOS	dw 0
P_SPRRENPOS	dw 0

section .text


PPU_tick:
	mov al, 0
    P_VBL_STATE EQU $-1
	cmp al, 0
	jz .VBLstate0
	jl .VBLminus
.VBLplus:
	cmp al, 2
	jne .VBLplus2
	or byte [P_STATUS], 0x80	; set invblank flag
.VBLplus2:
	dec ax
	mov [P_VBL_STATE], al
	jmp .VBLstateDef
.VBLminus:
	cmp al, -5
	jne .VBLminus2
	mov byte [P_STATUS], 0x00
.VBLminus2:
	inc ax	
	mov [P_VBL_STATE], al
	jmp .VBLstateDef
.VBLstate0:
	; NMI = status & sysctrl & 80h
	mov al, [P_STATUS]
	and al, [P_SYSCTRL]
	and al, 80h
	;shr al, 7
	mov [C_NMI], al
.VBLstateDef:
	cmp word [P_SCANLINE], 240
	jge .NoRendering

	;; rendering...

         ;if(reg.ShowBGSP) rendering_tick();
         mov al, [P_DISPCTRL]
         test al, 3 << 3
         jz .NoRenderingTick
         call PPU_RenderingTick
.NoRenderingTick:
         ;if(scanline >= 0 && x < 256) render_pixel();
         cmp word [P_SCANLINE], 0
         jl .DoneRendering
         cmp byte [P_X+1], 0
         jnz .DoneRendering
         call PPU_RenderPixel
.DoneRendering:
	;; end rendering...

.NoRendering:
	inc word [P_X]
	
	; NTSC phase is incremented by 8 after every pixel,
	; rendered or not.
	mov bl, 0
    P_NTSC_PHASE EQU $-1
        call NTSC_phase_inc_bl
	mov [P_NTSC_PHASE], bl

	mov ax, [P_X]
	cmp ax, 341
    P_SCANLINE_END EQU $-2
	jb .ScanlineUnfinished

	mov word [P_SCANLINE_END], 341
	mov [P_X], word 0

	mov ax, [P_SCANLINE]
	cmp ax, 240
	jae .DontShow

	call ShowScanline
	mov ax, [P_SCANLINE]
.DontShow:
	inc ax
	mov [P_SCANLINE], ax
	cmp ax, 241
	je .VBLbegin
	cmp ax, 0
	je .EndOfPreRenderLine
	cmp ax, 261 
	jl .ScanlineUnfinished
.VBLend:mov [P_VBL_STATE], byte -5
	mov [P_SCANLINE],  word -1
	xor [P_EVENODD], byte 1
%if 0	
	mov ax, 0
    P_FRAMECOUNT EQU $-2
	dec ax
	jns .NotReset
	mov ax, 0
    P_FRAME_SKIP EQU $-2
.NotReset:
	mov [P_FRAMECOUNT], ax
%endif
	jmp short .ScanlineUnfinished
.VBLbegin:
	mov [P_VBL_STATE], byte 2
	jmp short .ScanlineUnfinished
.EndOfPreRenderLine:
	; FIXME: This should happen at x=304 (small timing difference):
	test byte [P_DISPCTRL], 15*2
	jz .ScanlineUnfinished
	; Only do vaddr=taddr if _some_ part of rendering is enabled
	mov ax, [TADDR_BUF]
	mov [VADDR_BUF], ax

	; The 340-length scanline also happens only if rendering is enabled
	mov ax, word 341
	sub al, byte 0
    P_EVENODD  EQU $-1
	mov [P_SCANLINE_END], ax
.ScanlineUnfinished:
	ret



PPU_write:
	; dl = byte
	; ax = index (0..7), already clamped
	mov [PPU_OPEN_BUS], dl
	cmp ax, 1
	jb .WriteSysCtrl
	je .WriteDispCtrl
	cmp ax, 3
	je .WriteOAMaddr
	jb .WriteNothing
	cmp ax, 5
	jb .WriteOAM
	je .WriteScroll
	cmp ax, 7
	je .WriteMemory
	jb .WriteMemoryPosition
.WriteNothing:
	ret
.WriteSysCtrl:
	mov [P_SYSCTRL], dl
	; scroll.basenta = reg.BaseNTA
	mov ax, [TADDR_BUF]
	and ax, ~(3 << 10)
	and dx, 3
	shl dx, 10
	or ax, dx
	mov [TADDR_BUF], ax
	ret
.WriteDispCtrl:
	mov [P_DISPCTRL], dl
	; Build attenuation mask for generating
	; the color de-emphasis bits in NTSC signal
	xor eax, eax
	shl dl, 1
	jnc .Not4
	or eax, 00111111000000111111000000111111b
.Not4:	shl dl, 1
	jnc .Not2
	or eax, 11110000001111110000001111110000b
.Not2:	shl dl, 1
	jnc .Not1
	or eax, 00000011111100000011111100000011b
.Not1:	mov [P_ATTENUATION_MASK], eax
	ret
.WriteOAMaddr:
	mov [P_OAMADDR], dl
	ret
.WriteOAM:
	push bx
	 mov bx, word 0x0000
P_OAMADDR EQU $-2
	 mov [OAM+bx], dl
	 inc byte [P_OAMADDR]
	pop bx
	ret
.WriteScroll:
	mov al, [P_OFFSETTOGGLE]
	cmp al, 0
	mov ax, [TADDR_BUF]
	jnz .SHi
.SLo:
	; Set xfine and xcoarse
	and al, ~31
	mov dh, dl
	shr dh, 3
	or al, dh
	and dx, 7
	mov [XFINE], dx
	jmp short .Sdone
.SHi:
	; Set yfine and ycoarse
	and ax, ~((7 << 12) | (31 << 5))
	mov dh, dl
	and dh, 7    ; yfine   = dl&7
	shl dh, 4
	or ah, dh
	shl dx, 2    ; ycoarse = dl>>3  (placed at bitpos 5)
	and dx, 31<<5
	or ax, dx ; y
	;jmp short .Sdone
.Sdone:
	mov [TADDR_BUF], ax
	jmp short .DidWriteToggle
.WriteMemoryPosition:
	;pusha
	; mov dx, .WMP
	; mov ah, 9
	; int 21h
	;popa
	;pusha
	; mov al, [PPU_OPEN_BUS]
	; call PrintHexByte
	; call PrintNewline
	; jmp .WMP2
	; .WMP: db 'Set mem pos $'
	; .WMP2:
	;popa
	mov al, byte 00h
P_OFFSETTOGGLE EQU $-1
	cmp al, 0
	jnz .MLo
.MHi:
	and dl, 0x3F
	mov [TADDR_BUF+1], dl
	jmp .DidWriteToggle
.MLo:	
	mov ah, [TADDR_BUF+1]
	mov al, dl
	mov [TADDR_BUF+0], al
	mov [VADDR_BUF+0], ax
	;jmp .DidWriteToggle
.DidWriteToggle:
	not byte [P_OFFSETTOGGLE]
	;pusha
	; mov dx, .WMP3
	; mov ah, 9
	; int 21h
	;popa
	;pusha
	; mov ax, [VADDR_BUF]
	; call PrintHexWord
	; call PrintNewline
	; jmp .WMP4
	; .WMP3: db 'Begets $'
	; .WMP4:
	;popa
	ret
.WriteMemory:
	push bx
	push dx
	 mov ax, [VADDR_BUF]
	 call PPU_mmap ; bx = t
	 jc .DoneWriteMemory ; Don't write to read-only memory (0000-1FFF). 2000-3FFF is ok.
	  cmp ax, 0x3F00
	  jae .PaletteWrite
	  mov al, [PPU_OPEN_BUS]
	  mov [bx], al
	  jmp short .DoneWriteMemory
.PaletteWrite:
	  mov bx, ax
	  test al, 3
	  jnz .NotZero
	  and bx, 0x0F ; x10, x14, x18 and x1C must be mirrors of x00, x04, x08 and x0C
	.NotZero:
	  and bx, 0x1F
	  mov al, [PPU_OPEN_BUS]
	  and al, 0x3F
	  mov [PALETTE+bx], al
	;  pusha
	;   mov dx, .PPPT
	;   mov ah, 9
	;   int 21h
	;   mov al, bl
	;   call PrintHexByte
	;   mov dl, ' '
	;   call ConsolePutc
	;   mov al, [PPU_OPEN_BUS]
	;   and al, 0x3F
	;   call PrintHexByte
	;   call PrintNewline
	;   jmp .PPPT2
	;   .PPPT: db 'Wrote palette $'
	;   .PPPT2:
	;  popa
.DoneWriteMemory:
	 call PPU_IncAddr
	pop dx
	pop bx
	ret

PPU_read:
	; ax = index (0..7), already clamped
	cmp ax, 2
	jb .DoneRead
	je .ReadStatus
	cmp ax, 4
	jb .DoneRead
	je .ReadOAM
	cmp ax, 7
	je .ReadMemory
.DoneRead:
	mov al, 0x00
PPU_OPEN_BUS EQU $-1
	ret
.ReadOAM:
	push bx
	 mov bx, [P_OAMADDR]
	 mov al, [OAM+bx]
	 mov [PPU_OPEN_BUS], al; FIXME: For %4=2, update only &0xE3
	pop bx
	ret
.ReadStatus:
	mov al, [PPU_OPEN_BUS]
	and al, 0x1F
	or al, [P_STATUS]
	mov [PPU_OPEN_BUS], al
	 and byte [P_STATUS], 0x7F ; clear invblank flag
	 mov byte [P_OFFSETTOGGLE], 0
	 cmp [P_VBL_STATE], byte -5
	 je .DontResetVBLstate
	 mov [P_VBL_STATE], byte 0 ; this may also cancel the setting of InVBlank
.DontResetVBLstate:	 
	ret
.ReadMemory:
	push bx
	push dx
	 mov ax, [VADDR_BUF]
	 call PPU_mmap ; bx = t; ax = vaddr_raw
	 mov dl, [bx]
	 xchg dl, [PPU_READ_BUFFER]	; Put memory data in read-buffer
	 ; The read-buffer thing happens even if the address is of palette.
	; pusha
	;  pusha
	;   mov dx, .BBB
	;   mov ah, 9
	;   int 21h
	;  popa
	;  call PrintHexWord
	;  call PrintNewline
	;  jmp .RBB2
	;  .BBB: db 'Reads from $'
	;  .RBB2:
	; popa
	  cmp ax, 0x3F00
	  jae short .PaletteRead
	  mov [PPU_OPEN_BUS], dl	; Old contents of read-buffer
	  jmp short .DoneReadMemory
.PaletteRead:
	  mov bx, PALETTE
	;  mov [.PPPR3], al
	  test al, 3
	  jnz .NotZero
	  and al, 0x0F ; x10, x14, x18 and x1C must be mirrors of x00, x04, x08 and x0C
	.NotZero:
	  and al, 0x1F
	  xlatb                  ; mov al, [PALETTE+al]
	  mov ah, [PPU_OPEN_BUS]
	  and ax, 0xC03F ; update only &0x3F
	  or al, ah
	  mov [PPU_OPEN_BUS], al
	;  pusha
	;   mov dx, .PPPR
	;   mov ah, 9
	;   int 21h
	;   mov al, byte 00h
	;   .PPPR3 EQU $-1
	;   call PrintHexByte
	;   mov dl, ' '
	;   call ConsolePutc
	;   mov al, [PPU_OPEN_BUS]
	;   call PrintHexByte
	;   call PrintNewline
	;   jmp .PPPR2
	;   .PPPR: db 'Read palette $'
	;   .PPPR2:
	;  popa
.DoneReadMemory:
	 call PPU_IncAddr
	pop dx
	pop bx
	jmp .DoneRead


PPU_mmap:
	; In:  AX = address in PPU's memory
	; Out: BX = address of physical data in emulator's memory
	;      CF = address is VROM (not writable)
	; Preserves AX LOW 14 BITS; PRESERVES CX-DI
	and ax, 0x3FFF
	test ax, 0x2000
	jz .VBankRead
	; Nametable read
	mov bx, ax
	shr bx, 10-1
	and bx, 3*2
	mov bx, [C_NTA + bx]
	push ax
	 and ax, 0x3FF
	 add bx, ax
	pop ax
	clc ; clear carry flag (content is RAM)
	ret
.VBankRead:
	; FOR NOW, OUR GRANULARITY IS 8k (CNROM)
	mov bx, ax
	;and bx, 0x1FFF
	add bx, [C_VROMPAGE]
	stc ; set carry flag to indicate the content is ROM
	ret

PPU_IncAddr:
	inc word [VADDR_BUF]  ; add 1
	test byte [P_SYSCTRL], 4
	jnz .IncBy32
	ret
.IncBy32:
	add word [VADDR_BUF], 31 ; add 32
	ret


PPU_RenderingTick:
	mov cx, 0x0000
    P_X EQU $-2
	mov bx, cx

	; tile_decode_mode = x<256 || (x >= 320 && x < 336)
	shr cx, 4	; cl = x/16
	mov edx, 1
	shl edx, cl	; edx = 1 << (x/16)
	and edx, 0x10FFFF ;tile_decode_mode = edx<>0

	and bx, 7	; x % 8
	shl bx, 1
	jmp [.Mod8Table + bx]
.Mod8Table:
	dw .Mod8_0, .Mod8_1, .Mod8_2, .Mod8_3, .Mod8_4, .Mod8_5, .Mod8_6, .Mod8_7

.Mod8_2: ; Point to attribute table
	;ioaddr = 0x23C0 + 0x400*reg.vaddr_basenta + 8*(reg.vaddr_ycoarse/4) + (reg.vaddr_xcoarse/4);
	mov ax, [VADDR_BUF]
	mov cx, ax
	mov bx, ax
	;shr bx, 7
	;and bx, 7
	;shl bx, 3	; bx = (ycoarse/4)*8
	shr bx, 4
	and bx, (7<<3)  ; bx = (ycoarse/4)*8
	shr ax, 2	
	and ax, 7	; ax = xcoarse/4
	;add ax, bx
	and cx, 0xC00	; cx = basenta*0x400
	lea ax, [0x23C0 + ebx + eax]
	add ax, cx
	;add ax, 0x23C0
	mov [P_IOADDR], ax
	or edx, edx
	jnz .Mod8_break ;passthru if zero (sprite mode)

.Mod8_0: ;Point to nametable
	mov ax, word 0x0000
    VADDR_BUF EQU $-2
	and ax, 0xFFF
	or ax, 0x2000
	mov [P_IOADDR], ax
	; Reset sprite data
	xor ax, ax
	mov bx, [P_X]
	or bx, bx
	jz .Mod8_0_0
	cmp bx, 256
	jnz .Mod8_break
.Mod8_0_256:
	mov [P_SPRRENPOS], al
	jmp .Mod8_break
.Mod8_0_0:
	mov [P_SPRINPOS], al
	mov [P_SPROUTPOS], al
	mov [P_OAMADDR], byte 0x00
	jmp .Mod8_break

.Mod8_1: ;Name table access
	; pat_addr = 0x1000*reg.BGaddr + 16*mmap(ioaddr) + reg.vaddr_yfine
	mov ax, word 0x0000
    P_IOADDR EQU $-2
	call PPU_mmap
	mov bl, [bx]
	mov bh, 0
	shl bx, 4	; bx = 16*mmap(ioaddr)
	mov al, [VADDR_BUF+1]
	shr al, 4
	and ax, 7	; ax = yfine
	add bx, ax
	mov al, 0
	mov ah, [P_SYSCTRL]
	and ah, 0x10	; ax = bgaddr*0x1000 (bgaddr happens to be sysctrl&0x10)
	add ax, bx
	mov [P_PATADDR], ax
	or edx, edx
	jnz .Mod8_1_continues
	 ; Not tile mode? Check special actions
	 cmp word [P_X], 257
	 jne .Mod8_break
	 ; copy xcoarse, basenta_h from scroll to vaddr
	 mov ax, word 0x0000
    TADDR_BUF EQU $-2
	 and ax,                ((1 << 10) | (31 << 0))
	 and word [VADDR_BUF], ~((1 << 10) | (31 << 0))
	 or word [VADDR_BUF], ax
	 jmp .Mod8_break
.Mod8_1_continues:
        ;// Push the current tile into shift registers.
        ;// The bitmap pattern is 16 bits, while the attribute is 2 bits, repeated 8 times.
        ;misc.bg_shift_pat  = (misc.bg_shift_pat  >> 16) + 0x00010000 * tilepat;
        ;misc.bg_shift_attr = (misc.bg_shift_attr >> 16) + 0x55550000 * tileattr;
        mov eax, [P_BG_SHIFT_PAT]
        mov ax, word 0xAAAA
    P_TILEPAT EQU $-2
        ror eax, 16
        mov [P_BG_SHIFT_PAT], eax

        mov eax, [P_BG_SHIFT_ATTR]
        mov bx, word 0xAAAA
    P_TILEATTR EQU $-2
        mov si, bx ; bx+si = bx*2
        mov ax, [.AttrTable + bx+si]
        ror eax, 16
        mov [P_BG_SHIFT_ATTR], eax
	jmp .Mod8_break
		section .const
		.AttrTable:
			; This lookup table translates
			; a 2-bit value into 16-bit value
			; by duplicating it 8 times.
			dw 0000000000000000b
			dw 0101010101010101b
			dw 1010101010101010b
			dw 1111111111111111b
		section .text
.Mod8_3: ;attribute table access
	or edx, edx
	jnz .Mod8_3_tilemode
	cmp word [P_X], 335
	jae .Mod8_3_done
.Mod8_3_spritemode:
	; pat_addr = 0x1000 * reg.SPaddr
	mov al, [P_SYSCTRL]
	and ax, 8
	shl ax, 9
	mov [P_PATADDR], ax
	mov bx, [P_SPRRENPOS] ; sno
	cmp bl, [P_SPROUTPOS]
	jae .Mod8_3_done
	; Select sprite pattern instead of background pattern
	mov al, [OAM2_sprindex + bx]
	mov ah, [OAM2_attr     + bx]
	mov cl, [OAM2_x        + bx]
	mov [OAM3_sprindex + bx], al
	mov [OAM3_attr     + bx], ah
	mov [OAM3_x        + bx], cl
	; y = scanline - OAM2_y[sno]
	mov al, [OAM2_y + bx]
	mov ah, 0
	sub ax, [P_SCANLINE]
	neg ax	; ax = y
	mov cx, [OAM2_index + bx]
	test byte [P_SYSCTRL], 32 ; 16-tall?
	jz .Mod8_3_sprite_8
.Mod8_3_sprite_16:
	; Deal with 16-tall sprites
	test byte [OAM3_attr + bx], 0x80
	jz .NoYflip_16
	 xor al, 15
.NoYflip_16:
	 shl cx, 12
	 and cx, 0x1000
	 mov [P_PATADDR], cx
	 mov cx, [OAM2_index + bx]
	 and cx, 0xFE
	 jmp short .ChoseSprite
.Mod8_3_sprite_8:
	; Deal with 8-tall sprites
	test byte [OAM3_attr + bx], 0x80
	jz .NoYflip_8
	 xor al, 7
.NoYflip_8:
	 mov ch, 0 ;and cx, 0xFF
.ChoseSprite:
	 shl cx, 4
	 add [P_PATADDR], cx
.Mod8_3_sprite_done:
	 mov bx, ax ; bx, ax = y
	 and bx, 8 
	 shl bx, 1  ; bx = (y&8)*2
	 and ax, 7
	 add ax, bx ; ax = (y&7) + (y&8)*2
	 add [P_PATADDR], ax
	jmp .Mod8_3_done
.Mod8_3_tilemode:
	;tileattr = (mmap(ioaddr) >> ((reg.vaddr_xcoarse&2) + 2*(reg.vaddr_ycoarse&2))) & 3
	mov al, [VADDR_BUF]	; fedcba9876543210
	mov cl, al		; .........4....2.
	and cl, 2		; cl = xcoarse&2
	shr al, 4
	and al, 2<<1		; al = (ycoarse&2)*2
	add cl, al		; cl = (xcoarse&2) + 2*(ycoarse&2)
	mov ax, [P_IOADDR]
	call PPU_mmap
	mov al, [bx]
	shr al, cl
	;mov al, 2	; TEST
	and ax, 3
	mov [P_TILEATTR], ax

	; Go to the next tile horizontally (and switch nametable if it wraps)
	;
	; Increment xcoarse (0..31 at bitpos 0).
	; If it wraps, toggle basenta_h (0..1 at bitpos 10).
	mov ax, [VADDR_BUF]
	mov cl, al
	inc cx
	and cl, 31
	jnz .DidntWrapHoriz
	xor ah, 1 << (10-8) ; Toggle horizontal nametable index
.DidntWrapHoriz:
	and al, ~31
	or al, cl

	cmp word [P_X], 251
	jne .Mod8_3_tilemode_done
	; At the edge of the screen, do the same but vertically
	;
	; Increment yfine (0..7 at bitpos 12).
	; If it wraps, increment ycoarse (0..31 at bitpos 5).
	;               If ycoarse hits 30, set ycoarse=0
	;               and toggle basenta_v (0..1 at bitpos 11).
	;
	add ah,       1 << (12-8)
	and ah, 0x7F
	test ah,      7 << (12-8)
	jnz .Mod8_3_tilemode_done
	; ++ycoarse
	mov cx, ax
	add cx, 1  << 5
	and cx, 31 << 5
	cmp cx, 30 << 5
	jne .DidntWrapVert
	xor cx, cx ;ycoarse=0
	xor ah, 1 << (11-8)	; Toggle vertical nametable index
.DidntWrapVert:
	and ax, ~(31 << 5)
	or ax, cx
.Mod8_3_tilemode_done:	
	mov [VADDR_BUF], ax
	;jmp .Mod8_3_done
.Mod8_3_done:
	mov [P_IOADDR], word 0xAAAA
    P_PATADDR EQU $-2
	jmp .Mod8_break

.Mod8_5:
	; Read first byte of tile pattern
	mov ax, [P_IOADDR]
	call PPU_mmap
	mov al, [bx]
	mov [P_TILEPAT], al
	jmp .Mod8_break

.Mod8_7:
	; Read second byte of tile pattern
	mov ax, [P_IOADDR]
	or ax, 8
	call PPU_mmap
	mov ah, [bx]	    ; high byte (now read)
	mov al, [P_TILEPAT] ; low byte (previously read)
	; interleave the bits of the two pattern bytes
	mov bx, ax
	mov cx, ax
	and ax, 0xF00F ; AAAAbbbbccccAAAA   FEDCBA9876543210
	and bx, 0x0F00 ; becomes
	and cx, 0x00F0 ; AAAAccccbbbbAAAA   FEDC7654BA983210
	shr bx, 4
	shl cx, 4
	or ax, bx
	or ax, cx
	mov bx, ax
	mov cx, ax
	and ax, 0xC3C3 ; AAbbccAAAAbbccAA   FEDC7654BA983210
	and bx, 0x3030 ; becomes
	and cx, 0x0C0C ; AAccbbAAAAccbbAA   FE76DC54BA329810
	shr bx, 2
	shl cx, 2
	or ax, bx
	or ax, cx
	mov bx, ax
	mov cx, ax
	and ax, 0x9999 ; AbcAAbcAAbcAAbcA   FE76DC54BA329810
	and bx, 0x4444 ; becomes
	and cx, 0x2222 ; AcbAAcbAAcbAAcbA   F.E.D.C.B.A.9.8.
	shr bx, 1      ;                     7 6 5 4 3 2 1 0
	shl cx, 1
	or ax, bx
	or ax, cx
	mov [P_TILEPAT], ax ; save 16-bit tile
        ; When decoding sprites, save the sprite graphics and move to next sprite
	or edx, edx
	jnz .Mod8_break
	mov bx, [P_SPRRENPOS]
	cmp bl, [P_SPROUTPOS]
	jae .Mod8_break
	inc bx
	mov [P_SPRRENPOS], bx
	shl bx, 1
	mov [OAM3_pattern + bx-2], ax
.Mod8_6:
.Mod8_4:
.Mod8_break:
	
	mov ax, [P_X]
	cmp ax, 64
	jb .DoneRenderTick
	cmp ax, 256
	jae .DoneRenderTick
	; THIS PART USES SIMPLER CODE FROM YOUTUBE VIDEO
	; Rather than the complex one that supports
	; the crazy 9-sprite malfunction.

	mov bx, [P_OAMADDR]
	test ax, 1
	jz .SpriteAccessOAM

	inc byte [P_OAMADDR]
	and ebx, 3
	mov al, 0xAA
    P_SPR_DATA EQU $-1
	mov si, word 0x0000
    P_SPROUTPOS EQU $-2
	jmp [.SpriteCases + ebx*2]
.SpriteAccessOAM:
	mov bh, 0
	mov al, [OAM+bx]
	mov [P_SPR_DATA], al
	jmp .DoneRenderTick
.SpriteCases:
	dw .SpriteCase0, .SpriteCase1, .SpriteCase2, .SpriteCase3
.SpriteCase0:
	cmp byte [P_SPRINPOS], 64
	jae .SpriteDone
	inc byte [P_SPRINPOS] ; next sprite
	cmp si, 8
	jae .Already8
	 mov [OAM2_y+si], al
	 mov ah, [P_OAMADDR]
	 mov [OAM2_sprindex+si], ah
.Already8:
	; if(!(scanline >= y1 && scanline < y2 ))
	; if(scanline < y1 || scanline >= y2 )

	mov dx, [P_SCANLINE]
	; ax = y1
	mov ah, 0
	cmp dx, ax
	jl .SpriteNotInRange
	; make y2
	mov ah, [P_SYSCTRL]
	and ah, 32 ; 0 or 32
	shr ah, 2  ; 0 or 8
	add ah, 8
	add al, ah
	mov ah, 0
	cmp dx, ax
	jl short .DoneRenderTick ; Sprite in range, will go to next case.
.SpriteNotInRange:
	add byte [P_OAMADDR], 3
	jmp .SpriteCase3_cont
.SpriteCase1:
	cmp si, 8
	jae short .DoneRenderTick
	mov [OAM2_index+si], al
	jmp short .DoneRenderTick
.SpriteCase2:
	cmp si, 8
	jae short .DoneRenderTick
	mov [OAM2_attr+ si], al
	jmp short .DoneRenderTick
.SpriteCase3:
	cmp si, 8
	jae .SpriteOverflow
	mov [OAM2_x+    si], al
	inc word [P_SPROUTPOS]
	jmp .SpriteCase3_cont
.SpriteOverflow:
	or byte [P_STATUS], 0x20
.SpriteCase3_cont:
	cmp byte [P_SPRINPOS], 2
	jne short .DoneRenderTick
	mov byte [P_OAMADDR], 8
.DoneRenderTick:
	ret
.SpriteDone:
	mov byte [P_OAMADDR], 0
	ret

PPU_RenderPixel:


	mov cx, [P_X]
	mov dh, cl
	add dh, 8 		; dh = u8(x+8)
	; xpos = ~((x&7) + (reg.taddr_xfine&7) + ((x&7) ? 8 : 0)) & 15
	and cx, 7 ; x&7
	jz .zero
	or cl, 8  ; x&7 + ((x&7)?8:0)
.zero:	add cx, word 0xAAAA	;0-7 really
    XFINE EQU $-2
	not cx
	and cx, 15
	shl cl, 1 		; cl = xpos*2
	; showbg and showsp:
	mov al, [P_DISPCTRL]
	mov dl, 0
	test al, 8+2	; No BG/BG8 = deny
	jz .Showbg_false
	test al, 8	; Yes BG = allow
	jnz .Showbg_true
	cmp dh, 16	; In edge = deny
	jb .Showbg_false
.Showbg_true:
	inc dx			; dl&1 = showbg
.Showbg_false:
	test al, 16+4
	jz .Showsp_false
	test al, 16
	jnz .Showsp_true
	cmp dh, 16
	jb .Showsp_false
.Showsp_true:
	inc dx
	inc dx 			; dl&2 = showsp
.Showsp_false:
	; Pick a pixel from the shift registers, if BG is allowed
	;
	xor si, si	; si = pixel
	xor di, di	; di = attr
	test dl, 1
	jz .BGdisabled
	 mov esi, dword 0xAAAAAAAA
    P_BG_SHIFT_PAT EQU $-4
	 shr esi, cl
	 and si, 3	; pixel
	 jz .BGchosen	; Keep zero attribute if pixel=0
	 mov edi, dword 0xAAAAAAAA
    P_BG_SHIFT_ATTR EQU $-4
	 shr edi, cl
	 and di, 3	; attr
	 jmp .BGchosen
.BGdisabled:
	mov ax, [VADDR_BUF]
	push ax
	 and ax, 0x3F00
	 cmp ax, 0x3F00
	pop ax
	jne .BGchosen
	test byte [P_DISPCTRL], 2+4+8+16
	jnz .BGchosen	; only set bg from palette if BG/BG8/SP/SP8 are all false
	mov si, ax	; pixel
.BGchosen:
	test dl, 2
	jz .DoneRenderingSprites
	; Overlay the sprites
	xor ebx, ebx
	not bx
	;mov bx, -1
.OverlaySpritesLoop:
	inc bx
	cmp bl, [P_SPRRENPOS]
	jae .DoneRenderingSprites
	 ; Check if the sprite is horizontally in range
	 mov ax, [P_X]
	 mov ch, 0
	 mov cl, [OAM3_x + bx]
	 sub ax, cx              ; xdiff = x - oam3_x[sno]
	 cmp ax, 8
	 jae .OverlaySpritesLoop ; ax = xdiff
	 ; Determine which pixel to display; skip transparent pixels
	 test [OAM3_attr+bx], byte 0x40
	 jnz .NoXflip
	 xor al, 7   ; ax = 7-ax
.NoXflip:
	 ; spritepixel = (misc.OAM3_pattern[sno] >> (xdiff*2)) & 3
	 mov cl, al
	 shl cl, 1               ; cl = xdiff*2
	 mov ax, [OAM3_pattern + ebx*2]
	 shr ax, cl
	 and ax, 3               ; ax = spritepixel
	 jz .OverlaySpritesLoop  ; spritepixel 0 is always transparent
	 ; Register sprite-0 hit if applicable
	 cmp word [P_X], 255 ; x must be < 255
	 jae .NoSprite0hit
	 test si, si         ; background pixel must be non-0
	 jz .NoSprite0hit
	 cmp byte [OAM3_sprindex + bx], 4 ; sprite index must be 0
	 jae .NoSprite0hit
	 or byte [P_STATUS], 0x40 ; set sp0hit flag
.NoSprite0hit:
	 ; Render the pixel unless behind-background placement wanted
	 mov cl, [OAM3_attr + bx]
	 test si, si
	 jz .DoRenderSpritePixel ; background=0? Render
	 test cl, 0x20
	 jnz .DoneRenderingSprites ; 0x20 not set? Render -- 0x20 set = don't render
.DoRenderSpritePixel:
	 and cx, 3	; attribute
	 add cx, 4
	 mov di, cx	; attr = (s.attr & 3) + 4
	 mov si, ax	; pixel = spritepixel
	 ; Only process the first non-transparent sprite pixel.
.DoneRenderingSprites:
	; map pixel through palette
	;mov di, 1*4+2
	lea di, [esi + edi*4] ; pixel + attr*4
	and di, 0x1F
	mov al, [PALETTE+di]
	test byte [P_DISPCTRL], 1
	jz .Notgrayscale
	and al, 0x30
.Notgrayscale:
	; Plot pixel (al=pixel, +use emphasis attributes)

	mov di, [P_X]

NTSC_SYNTHESIS_DISABLE:
	jmp short .DoNTSC ; REPLACED WITH 2*NOP if necessary

	; NO NTSC SIM: Just store the raw pixel.
	;mov ax, di
	add al,16
	mov [NTSCline + di], al
.DontGenerate:
	ret

.DoNTSC:
%if 0
	cmp word [P_FRAMECOUNT], 0
	jnz .DontGenerate
%endif
	; DO NTSC
	and di, 0xFF	; Just to make sure we don't do buffer-overflow.
	jnz .DontMakeBorders

	; Generate borders while at it.
	; Our NTSCline is 282*8 samples long. This means 26*8 is reserved for edges.
	; We are supposed to render 15 pixels of edge at left, 11 pixels of edge at right.
	push di
	 push ax
	  mov al, [P_NTSC_PHASE]
	  mov bl, al
	  cbw
	  shl ax, 2
	  mov [P_NTSC_PHASE_LINEBEGIN], ax
	  
	  ;sub bl, 15*8 ; which is 10*12. No effect.

	  mov di, NTSCline
	 .LeftLoop:
	   mov al, [PALETTE+0]
	   call NTSC_synthesize
	   cmp di, NTSCline + 15*8*4
	   jb .LeftLoop
	  mov di, NTSCline + 15*8*4 + 256*8*4

	  ;call NTSC_phase_inc_bl
	  add bl, 8 ; 256*8 mod 12

	 .RightLoop:
	   mov al, [PALETTE+0]
	   call NTSC_synthesize
	   cmp di, NTSCline + 282*8*4
	   jb .RightLoop
	 pop ax
	pop di

.DontMakeBorders:
	shl di, 3+2	; 8 floats per pixel, 4 bytes per float; 32 bytes per pixel
	add di, NTSCline + 15*8*4
	mov bl, [P_NTSC_PHASE]
	jmp NTSC_synthesize ; tail-call

NTSC_synthesize_with_offset:
	lea di, [NTSCline + ebx*4]
	;passthru
NTSC_synthesize:
	; DI = Pointer to NTSCline (Out: incremented by 8*4)
	; BL = NTSC phase (Out: incremented by 8)
	; Uses AX, CX, EDX, BP, SI

	movzx dx, al	; level
	and ax, 0x0F	; color
	shr dl, 4
	cmp al, 13
	jbe .Not1415
	mov dl, 1	; For colors 14..15, level 1 is forced.
.Not1415:
	;add di, 8*4	; TEMPORARY
	;ret		; TEMPORARY

	; AX = color
	; DX = level
	lea si, [NTSC_levels + edx*4]

	; Level has been handled. What remains still is AX = color

	; right: phase
	; down:  color
	; 1 1 1 1 1 1 1 1 1 1 1 1 
	; 1 1 1 1 1 0 0 0 0 0 0 1 
	; 1 1 1 1 0 0 0 0 0 0 1 1 
	; 1 1 1 0 0 0 0 0 0 1 1 1 
	; 1 1 0 0 0 0 0 0 1 1 1 1 
	; 1 0 0 0 0 0 0 1 1 1 1 1 
	; 0 0 0 0 0 0 1 1 1 1 1 1 
	; 0 0 0 0 0 1 1 1 1 1 1 0 
	; 0 0 0 0 1 1 1 1 1 1 0 0 
	; 0 0 0 1 1 1 1 1 1 0 0 0 
	; 0 0 1 1 1 1 1 1 0 0 0 0 
	; 0 1 1 1 1 1 1 0 0 0 0 0 
	; 1 1 1 1 1 1 0 0 0 0 0 0 
	; 0 0 0 0 0 0 0 0 0 0 0 0 
	; 0 0 0 0 0 0 0 0 0 0 0 0 
	; 0 0 0 0 0 0 0 0 0 0 0 0 
	xor dx, dx
	cmp ax, 12
	ja .BeginNTSCloop      ; For colors 13..15, signal low is forced (000000000000)
	not dx
	test ax, ax
	jz .BeginNTSCloop      ; For color 0, signal high is forced (111111111111, from "not dx")
	add al, bl             ; NTSC phase, 0..20
	;aam 12	; modulo 12
	mov cl, al
.mod12:	mov edx, 00111111000000111111000000111111b
	shr edx, cl
.BeginNTSCloop:
	mov ebp, dword 0
    P_ATTENUATION_MASK EQU $-4
        mov cl, bl
        shr ebp, cl
        mov ecx, ebp

	cld
	; Using %rep and %endrep costs some ROM space, but it relieves CX as a register.
    %rep 8
	xor bp, bp
	; Determine whether to add 4*4 or not, by judging color & phase
	; 4 * (color <= 12 * ((color+phase)%12 < 6))
	; TODO: Determine whether to add 8*4 or not, by judging
	; the color emphasis bits and the phase.

	rcr cx, 1
	rcl bp, 1 ; Cf becomes 0x01

	rcr dx, 1
	rcl bp, 5 ; previous Cf becomes 0x20, Cf becomes 0x10

	; flag  = (0451326 >> (phase/2*3)) & emphasisbits
	mov eax, [bp+si]
	stosd
    %endrep

	;jmp NTSC_phase_inc_bl
NTSC_phase_inc_bl:
	add bl, 8
	push ax
	 mov ax, bx
	 aam 12 ; al = al mod 12
	 mov bl, al
	pop ax
	ret


section .const
NTSC_levels:
        ; Prenormalized values.
        ; We don't support de-emphasis bits for now.
        ; Calculated as:
        ;	normalized_value = (%1 - 0.518) / (1.962 - 0.518)
        ;	factored_value   = normalized_value * brightness / 12
        ;	with brightness = 1

	dd -0.00969529 ;0.350
	dd  0.00000000 ;0.518
	dd  0.02562327 ;0.962
	dd  0.05955679 ;1.550 ; Signal low
	dd  0.03324100 ;1.094
	dd  0.05701754 ;1.506
	dd  0.08333333 ;1.962
	dd  0.08333333 ;1.962 ; Signal high
	; The same, but attenuated by a factor of 0.746 before normalization
	dd -0.01482572
	dd -0.00759303
	dd  0.01152193
	dd  0.03683633
	dd  0.01720476
	dd  0.03494206
	dd  0.05457364
	dd  0.05457364

saturation: dd 1.7

bayer4x4:
	db  0, 12,  3, 15
	db  8,  4, 11,  7
	db  2, 14,  1, 13
	db 10,  6,  9,  5

	; YIQ matrix multiplied by (16*5, 16*6 and 16*8)
;y_r	dd 1.0
i_r	dd  0.946882
q_r	dd  0.623357
;y_g	dd 1.0
i_g	dd -0.274788
q_g	dd -0.635691
;y_b	dd 1.0
i_b	dd -1.108545
q_b	dd  1.709007

section .text

;VESA_Granularity_kB    dw 0
VESA_Granularity_bytes dd 0

ShowScanline:
%if 0
	cmp word [P_FRAMECOUNT], 0
	jz .DoRender
	ret
.DoRender:
%endif
	push es
	 ; MODE-X:
	 ;   Memory position for (x,y) = (y*320+x)/4
	 ;   Into port 3C4h, put xx02h where xx = 1 << (x%4).
	 xor cx, cx	; Plane index

MODEX_RENDERING_ENABLE:
	 jmp short .ModeXrendering

.TrueColorRendering:
	 xor edi, edi
	 xor edx, edx
	 les di, [P_SCANLINE]

	 shl edi, 8
	 lea edi, [edi + edi*4] ; y = 320*4*scanline = 1280*scanline = 256*scanline + 1024*scanline

	 mov eax, edi
	 mov ebp, edi
	 div dword [VESA_Granularity_bytes]
	 
	 ; eax = bank number, edx = starting address in this bank

	 ; Figure out which granularity-page this scanline begins from
	 mov di, dx         ; Modulo bytes = starting address

	 call .SetVESAbank
	 
	 ; Figure out the beginning of the next bank
	 inc ax
	 mul dword [VESA_Granularity_bytes]
	 ; eax = beginning of next bank

	 xor si, si
	 cld
	 
	 ; Figure out if a seam goes in the middle of this scanline
	 sub eax, ebp
	 sar eax, 2   ; from dwords into pixels
	 cmp ax, 320
	 jge .ScanlineLoop2

	 mov [.FirstLimit], ax

.ScanlineLoop:
	 call DoOnePix
	 stosd
	 cmp si, 320
   .FirstLimit EQU $-2
	 jb .ScanlineLoop

	 mov ax, [.LastBank]
	 inc ax
	 call .SetVESAbank
	 xor di, di
	  
	 ; continue with new bank
	 ;jmp .skip
.ScanlineLoop2:
	 call DoOnePix
	 stosd
	 cmp si, 320
	 jb .ScanlineLoop2

	pop es
	ret

.SetVESAbank:
	cmp ax, 0xAAAA
    .LastBank EQU $-2
	je .Done
	mov [.LastBank], ax
	mov dx, ax
	push ax
	 mov ax, 0x4F05
	 xor bx,bx
	 int 10h
	pop ax
.Done:	ret
	


.ModeXrendering:
AllPlanesLoop:
	 les di, [P_SCANLINE]
	 mov ax, di
	 and ax, 3
	 shl ax, 2
	 add ax, bayer4x4
	 mov word [bayer_base], ax

	 shl di, 4
	 lea di, [edi + edi*4] ; We'll begin at this address four times.
	 add di, byte 0  ; When NTSC is not disabled, add 32-pix margin by offseting the coordinate.
SCREEN_MARGIN EQU $-1

	  mov ax, 0x0102
	  shl ah, cl
	  mov dx, 0x3C4
	  out dx, ax	; Set plane index
	  mov si, cx	; Source pixel index (0..3), will cover to 320

	  ; TODO: Calculate 80 pixels (320/4), in groups of 4 (20 loops)
OnePlaneLoop:
	  ; Calculate four pixels at once before writing to VGA RAM
	  call DoOnePix
	  call DoOnePix
	  call DoOnePix
	  call DoOnePix

	  ; Send four pixels
	  stosd
	  cmp si, 320
	RENDER_WIDTH EQU $-2
	  jb OnePlaneLoop

	 inc cx		; Go to next plane
	 cmp cl, 3
	 jbe AllPlanesLoop

	pop es
	ret



DoOnePix:
	;db 0xBB ;mov bx,,..
	jmp short DecodeNTSCpixel
NTSC_DECODE_DISABLE EQU $-2

	mov al, [NTSCline + si]
	jmp DecodeNTSC_return

DecodeNTSCpixel:
	;mov dx, si
	;shr dx, 2
	;mov al, dl
	;add al, 16
	;;mov al, 4*(6*8) + 5*(8) + 0
	;jmp DecodeNTSC_return

	  ; Translate si (0..292) into begin,end (both 0..2047)
	  ; Center = si*2048/292+4
	  ; Begin  = Center-6
	  ; End    = Center+6
	   
	  ; Begin = si*2048/292-2
	  ; End   = si*2048/292+10

	  mov bx, si ; bx+si = si*2
	  mov bx, word [xbegins +bx+si] ; Begin

	  push eax ; backup eax (the four pixels)

	   ; bp = sintable + 4*(bx % 12)
	   ; bx is already pre-multiplied by 4.
	   lea ax, [bx + 24*4  + 4*4]
	   add ax, word 0x1111
	P_NTSC_PHASE_LINEBEGIN EQU $-2
	   cwd
	   mov bp, 12*4
	   div bp		; modulo 12 (times 4)
	   add bx, NTSCline
	   lea bp, [sincos + edx]

	   lea dx, [bx + 12*4] ; End
	   cmp bx, NTSCline
	   jge .NotZero
	   mov bx, NTSCline
	.NotZero:

	   ; y=i=q=0
	   ; while(bx < dx)
	   ; {
	   ;   value = signal[bx] * factor
	   ;   y += value
	   ;   value *= saturation
	   ;   i += value * cos((pi/6) * (phase+bx))
	   ;   q += value * sin((pi/6) * (phase+bx))
	   ; }
	   
	   call NTSCdecodeIntoYUV
MODEX_DECODE_ENABLE:
	   jmp short .MakePalettedVersion
	   nop
	   nop

.MakeTrueColorVersion:
	  pop ebx
	  call NTSCdecodeMakeR
	  call .TrueColorHelper

	  call NTSCdecodeMakeG
	  call .TrueColorHelper

	  call NTSCdecodeMakeB
	  call .TrueColorHelper
	  
	  xchg ebx, eax
	  inc si ; Jump 1 pixel ahead
	  ret

	.TrueColorHelper:
	  call FloatToPositiveIntWithClamp
	  dd 255.49
	  dw 255
	  shl ebx, 8
	  mov bl, al
	  ret

.MakePalettedVersion:
	   xor ax, ax

	   call NTSCdecodeMakeR
           call NTSCdecodeMakeLinear
           dd 64.0 ; 16*4

           mov bx, 4

	   call YIQcalc

	   call NTSCdecodeMakeG
           call NTSCdecodeMakeLinear
           dd 112.0 ; 16*7

           mov bx, 7
	   mul bx
	   
	   ;mov bx, ax
	   ;sal ax, 3
	   ;sub ax, bx
	   ;mov bx, 7
	   
	   ;shl ax, 8
	   ;aad 7

	   call YIQcalc

	   call NTSCdecodeMakeB
           call NTSCdecodeMakeLinear
           dd 144.0 ; 16*9

           mov bx, 9
	   mul bx

	   ;mov bx, ax
	   ;sal ax, 3
	   ;add ax, bx
	   ;mov bx, 9

	   ;shl ax, 8
	   ;aad 9

	   call YIQcalc

	.Bypass:
	   ;mov ax, si;4*(6*8) + 5*(8) + 0 ;test, should make a yellow pixel
	   add al, 4
	   xchg bx, ax
	   ;lea bx, [eax+4]
	  pop eax
	  mov al, bl

DecodeNTSC_return:
	add si, 4 ; jump 4 pixels ahead
	ror eax, 8
	ret

NTSCdecodeIntoYUV:
	   fld dword [saturation]
	   
	   fldz		;i
	   fldz		;q
	   fldz		;y
	   jmp .L2
	 .L3:
	   fld dword [bx]
	   add bx, 4	; next sample from scanline

	   fadd to st1	;y(st1)     += value
	   
	   fmul st4	;value(st0) *= saturation

	   fld dword [bp + 12]   ; cos(x) = sin(x+3) when unit is pi/6

	   fmul st1	;st0 = cos()*value

	   faddp st3	;i += st0

	   fmul dword [bp] ;value*=sin
	   add bp, 4	; next cell in sincos table
	   faddp st3	;q += st0
	 .L2:
	   cmp bx, dx
	   jb .L3
	   fstp st3 ; forget the dummy saturation value
	   fxch st1
	   fxch st2
	ret

NTSCdecodeMakeR:
	fld dword [i_r]
	fmul st2
	fadd st1
	fld dword [q_r]
	jmp fmul_st4_faddp_st1_return
NTSCdecodeMakeG:
	fld dword [i_g]
	fmul st2
	fadd st1
	fld dword [q_g]
fmul_st4_faddp_st1_return:
	fmul st4
	jmp faddp_st1_return
NTSCdecodeMakeB:
	fxch st1
	fmul dword [i_b]
	faddp st1
	fxch st1
	fmul dword [q_b]
faddp_st1_return:
	faddp st1
	ret
NTSCdecodeMakeLinear:
	pop bp
	; Convert gamma-corrected RGB into linear RGB
	; For simplicity, we assume gamma of 2.0. It's close enough.
	ftst
	xchg bx,ax
	 fnstsw ax
	 test ah, 69
	xchg bx,ax
	je .notzero
	fstp st0	; Replace value with zero
	fldz
	add bp, 4
	jmp bp
.notzero:
NTSC_DECODE_POWER2:
	fmul st0	; x^2
	fmul dword [bp]
	add bp, 4
	jmp bp


FloatToPositiveIntWithClamp:
	pop bp
	ftst
	fnstsw ax
	test ah, 69
	jne .zero
	fmul dword [bp]
	fistp word [.temp]
	fwait
	mov ax, 0
    .temp EQU $-2
	cmp ax, [bp+4]
	jbe short .truedone
	mov ax, [bp+4]
	jmp short .truedone
.zero:	fstp st0
	xor ax, ax
.truedone:
	add bp,6
	jmp bp


YIQcalc:
	fistp dword [YIQ_temp]
	xor edx, edx
	mov bp, si
	and bp, 3
	mov dl, [bayer4x4 + bp]
bayer_base EQU $-2
	;and dl, 0x08
	; Our video is not RGB, and our palette is not
	; the NES palette. We use dithering to compensate.
	fwait
	add edx, dword 0xAAAAAAAA
YIQ_temp equ $-4
	sar dx, 4	; Divide by 16
	cmp dx, bx
	jb .ok
	lea dx, [bx-1]
.ok:	
	add ax, dx
;.ZeroPix:
	ret

