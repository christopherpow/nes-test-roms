bits 16
org 0x110+0x4000+0x2000*3

ROM	EQU	0x110 ; ROM begins from this address. No exceptions.

;cpu 386
cpu pentium	; For rdtsc

GenerateNTSCtables EQU 0x6CF0+2 ; This is stored in VROM of the TEST ROM

main:
	mov si, xbegins
	call GenerateNTSCtables ; Run this routine before possibly loading another ROM

	push cs
	pop es
	push cs
	pop ds
	mov si, 80h
	cld
	lodsb
	movzx cx, al
	inc cx
.NextByte:
	dec cx
	jz .DoneCommandLine
	lodsb
	mov ah, 90h ; NOP
	cmp al, 'A'
	jb .NotUpper
	cmp al, 'Z'
	ja .NotUpper
	or al, 32 ; tolower
.NotUpper:	
	cmp al, ' '
	jbe .NextByte
	cmp al, '/'
	je .NextByte
	cmp al, '-'
	je .NextByte
	;cmp al, '?'
	;je .EnableHelp
	;cmp al, 'h'
	;je .EnableHelp
	cmp al, 'd'
	je .EnableDisasm
	cmp al, 'g'
	je .DisableGraphics
	cmp al, 't'
	je .DisableGraphics
	cmp al, 'n'
	je .DisableNTSCsimulation
	cmp al, 'p'
	je .DisableNTSCsimulation
	cmp al, 'm'
	je .DisableVESA
	cmp al, ':'
	je .LoadROM
	;jmp .EnableHelp
.EnableHelp:
	mov byte [PrintHelp], ah ; NOP
	jmp .DoneCommandLine
.EnableDisasm:
	mov byte [DEBUG], ah ; NOP
	jmp .NextByte
.DisableGraphics:
	mov al, 0xC3 ; RET
	mov [InitGraphics], al ; Disable graphics mode
	mov [ShowScanline], al ; Disable graphics rendering
	jmp .NextByte
.DisableNTSCsimulation:
	mov al, ah ; mov ax, 0x9090
	mov word [NTSC_PALETTE_DISABLE], ax    ; nop, so it will do predefined palette
	mov word [NTSC_SYNTHESIS_DISABLE], ax ; nop, so it won't jump to NTSC encoder
	mov word [NTSC_DECODE_DISABLE], ax    ; nop, so it won't jump to NTSC decoder
	; Change NTSCdecodeMakeLinear so it won't do un-gammacorrection
	mov word [NTSC_DECODE_POWER2], ax
	;
	mov byte [SCREEN_MARGIN], 8
	mov word [RENDER_WIDTH], 256
	jmp .NextByte
.DisableVESA:
	mov byte [VESA_mode+1], ah
	jmp .NextByte

.LoadROM:
	; Super secret function: Load another ROM!
	; Makes testing so much easier...
	; And we still fit in 6000 bytes (compressed).
	mov dx, si

	rep lodsb
	cmp al, 13
	jne .NotCR
	dec si
.NotCR:
	mov ax, 0x3D00
	mov [si], al ; Place a nul terminator
	int 21h
	jc .FileError
	xchg bx, ax	; file handle
	mov ah, 3Fh
	mov cx, main-0x100 ; read everything that we can fit
	mov dx, 0x100
	int 21h
	jc .FileError
	mov ah, 3Eh
	int 21h
	jmp .DoneCommandLine
.FileError:
	mov dx, ErrorMsg
	mov ah, 9
	int 21h
	jmp HelpTerminate

.DoneCommandLine:
	call PrintHelp

	mov byte [C_RESET], 1

	; Setup ROM mappings:
	mov al, [0x104]	; 16k ROM count
	cmp al, 2
	ja .FileError ; too large
	shl ax, 14  ; multiply by 16384
	; Assume we have VRAM...
	mov dx, VRAM
	; Set a pointer to VROM (it's after the ROM)
	cmp byte [0x105], 0 ; 8k VROM count
	jz .OnlyVRAM
	; Oh, we have VROM instead.
	lea dx, [0x110 + eax]
.OnlyVRAM:
	mov [VROM],       dx
	mov [C_VROMPAGE], dx
	; Set a pointer to the last ROM page
	add ax, ROM - 16384
	mov [C_ROMPAGES+2], ax

	mov bx, CIRAM+0x0000
	mov cx, CIRAM+0x0400
	mov al, [0x106]; ctrlbyte
	test al, 1
	jz .MirrorHorizontal
.MirrorVertical:
	xchg bx, cx
.MirrorHorizontal:
	mov [C_NTA+2], bx
	mov [C_NTA+4], cx

	call WaitSpeed
	call InitGraphics
	call APU_init
	call JOY_init
opcode_loop:
	call ExecuteOpcode
EscHitFlag:
	jmp short opcode_loop
	xor dx, dx
	jmp ExitEmulation


PrintHelp:
	ret	; Patched with NOP where necessary
	mov si, [GenerateNTSCtables-2] ; pointer to HelpString is here
	cld
.HelpTextLoop:
	; Now this help text, because it will be stored in plain
	; text without any compression whatsoever, is deliberately
	; hidden by simply toggling the high bit of each byte.
	lodsb
	and al, 7Fh
	jz HelpTerminate
	xchg dx, ax
	call ConsolePutc
	jmp short .HelpTextLoop
HelpTerminate:
	mov ax, 4C00h
	int 21h

WaitSpeed:
	call _CPUinfo
	fmul dword [const1em3]
	fistp dword [.khzspeed]
	mov eax, dword 0
    .khzspeed EQU $-4
        cmp eax, 200000
        jae .speed_is_ok
        mov cx, 8
        std
        mov di, CPUspeedText+7
      .digitloop:
         test eax, eax
         jz .zeroloop
         mov ebx, 10
         xor edx, edx
         div ebx
         push eax
          lea ax, ['0' + edx]
          stosb
         pop eax
        loop .digitloop
        jmp .past_zeroloop
      .zeroloop:
         mov al, ' '
         stosb
         loop .zeroloop
      .past_zeroloop:
        mov dx, SlowCPUwarning
        mov ah, 9
        int 21h
        mov ah, 1
        int 21h
        cmp al, 3
        je HelpTerminate
.speed_is_ok:
	ret

	db 'Joel Yliluoma'


section .const
	; This text cannot go to VROM, because it can be accessed
	; after a different ROM might have been loaded.
SlowCPUwarning:
	db 'Warning: Your CPU is slower than 200 MHz: about'
   CPUspeedText:
        db '         kHz in fact.',13,10
	db 'Suggest running this program in a faster computer.',13,10
	db 'If you are using DOSBox, hit Ctrl-F12 to increase the number',13,10
	db 'of cycles until the limit is >200k. Even more is better.',13,10
	db 'Press a key to continue. Ctrl-C to terminate.',13,10,'$'

ErrorMsg:
	db 'ERROR     ',13,10,'$'

const1em3 dd 1e-3

section .text

puhh:
	; This dummy string is here to thwart people who
	; hexdump the ROM. They do not suspect this is actual
	; 8086 assembler code. The '$' however is a clue.
	;
	;db 'Test failed!',13,10,'$'
	;
	; Ok, on second thought, it's maybe too obscure.
	; Let's go the other way: Perplex with a seemingly
	; completely irrelevant string. It's a clue.
	;
	;db $1A,'Incorrect MS-DOS version',13,10
	;
	; ^ Moved into decompressor stub
	;

section .data

C_ROMPAGES	dw ROM, ROM
C_NTA:		dw CIRAM+0x0000, CIRAM+0x0400, CIRAM+0x0000, CIRAM+0x0400

section .text

	;;;;;;; MEMORY READING ;;;;;;;;
RB:	; In:  AX = address
	; Out: AL = byte
	call tick
	cmp ax, 0x2000
	jae .NotRAM
	 push bx
	  and ax, 0x7FF
	  mov bx, ax
	  mov al, [RAM+bx]
	 pop bx
.RB_done:
	 mov [C_OPENBUS], al
	 ret
.NotRAM:
	cmp ax, 0x4000
	jae .NotPPUread
	and ax, 7
	call PPU_read
	jmp short .RB_done
.NotPPUread:
	cmp ax, 0x4015
	jb .ReadOpenBUS
	je APU_read
	cmp ax, 0x4017
	ja .maybeROMread
	call JOY_read
	jmp short .RB_done
.maybeROMread:
	cmp ax, 0x8000
	jb .ReadOpenBUS
	push bx
	 mov bx, [C_ROMPAGES+0]
	 cmp ax, 0xC000
	 jb .lowpage
	 mov bx, [C_ROMPAGES+2]
	.lowpage:
	 and ax, 0x3FFF
	 add bx, ax
	 mov al, [bx]
	pop bx
	jmp short .RB_done
.ReadOpenBUS:
	mov al, byte 0x00
C_OPENBUS	equ $-1
	ret

	;;;;;;; MEMORY WRITING ;;;;;;;;
WB:	; In: AX = address
	; In: DL = byte
	cmp byte [C_RESET], 0
	jnz RB
	call tick
	mov [C_OPENBUS], dl
	cmp ax, 0x2000
	jae .NotRAM
	 push bx
	  and ax, 0x7FF
	  mov bx, ax
	  mov byte [RAM+bx], dl
	 pop bx
	 ret
.NotRAM:
	cmp ax, 0x4000
	jae .NotPPUwrite
	and ax, 7
	jmp PPU_write
.NotPPUwrite:
	cmp ax, 0x8000
	jae .MapperWrite
	cmp ax, 0x6000
	jae .PRAM_write
	cmp ax, 0x4014
	jb APU_write ; 4000..4013
	je DMA_write ; 4014
	cmp ax, 0x4016
	je JOY_write ; 4016
	jb APU_write ; 4015
	ret
.MapperWrite:
	; for now, assume we've got mapper 3 (CNROM)
	movzx ax, dl
	and al, 3
	shl ax, 13  ; mul by 0x2000
	add ax, word 0xAAAA
    VROM EQU $-2
	mov [C_VROMPAGE], ax
	ret
.PRAM_write:
	cmp ax, 0x6004
	jae ConsolePutc
	cmp ax, 0x6000
	je .MaybeExitCode
.NoWrite:
	ret
.MaybeExitCode:
	test dl, 0x80
	jne .NoWrite
ExitEmulation:
	mov al, dl
	mov ah, 4Ch
	push ax
	 call Terminate
	pop ax
	int 21h
	
ConsolePutc:
	or dl, dl
	jz .DontOutputNul
	push ax
	 ; Output ASCII character to STDOUT
	 mov ah, 2 
	 int 21h ; dl = character
	pop ax
.DontOutputNul:
	ret

	;;;;;;; CPU ;;;;;;;;;;;;;;
section .data

C_RESET      db 0
C_NMI        db 0
C_NMI_EDGE   db 0
C_IRQ        db 0

section .text

%include "exec.inc"

tick:	
	pusha
	 call PPU_tick
	 call PPU_tick
	 call PPU_tick
	 call APU_tick
	popa
	ret


DMA_write:
	push dx
	 mov al, 0
	 mov ah, dl
.DMA_loop:
	 push ax
	  call RB
	  mov dl, al
	  mov ax, 0x2004
	  call WB
	 pop ax
	 inc al
	 jnz .DMA_loop
	pop dx
	;jmp tick ; one extra tick?
	ret

%include "ppu.inc"
%include "apu.inc"
%include "joypad.inc"

section .const
IOsequence:
	dw 0x3C4
	dw 0x0604	;chain 4 mode off
	dw 0x0100	;async reset
	dw 0,0x3C2
	dw 0xE3E3	;clock suitable for 320x240
	dw 0,0x3C4
	dw 0x0300	;restart sequence
	dw 0,0x3D4
	dw 0x2C11	;mask out write-protect
	dw 0x0D06
	dw 0x3E07
	dw 0xEA10
	dw 0xAC11
	dw 0xDF12
	dw 0xE715
	dw 0x0616
	dw 0x0014	;dword mode off
	dw 0xE317	;byte mode on
	dw 0x2813	;width:320
	dw 0,0x3C4
	dw 0x0F02
IOsequenceEnd:
section .text


InitModeX:
	mov ax, 13h
	int 10h
	
	mov si, IOsequence
	cld
.NewPort:
	lodsw
	xchg dx, ax
.IOloop:
	cmp si, IOsequenceEnd
	je .IOloopEnd	
	lodsw
	test ax,ax
	jz .NewPort
	out dx, ax
	jmp .IOloop
.IOloopEnd:
	ret

InitGraphics:
	nop
NTSC_PALETTE_DISABLE:
	jmp short InitNTSCmode

	call InitModeX

	; NOT NTSC.
	; Because our ROM (the .com file) is not large enough
	; to store the actual palette data, even compressed,
	; we will instead synthesize it at runtime.
	; How? By running the NTSC modem once for each color.

	mov al, 16
	call PoutBegin
	xor ax, ax
.PaletteLoop:
	; Generate >=12 samples of NTSC signal
	push ax
	 xor bx, bx
	 call NTSC_synthesize_with_offset
	pop ax
	push ax
	 call NTSC_synthesize_with_offset

	 ; Decode the signal
	 lea bp, [sincos + 4*4]
	 mov bx, NTSCline
	 lea dx, [bx + 12*4]

	 call NTSCdecodeIntoYUV

	 call NTSCdecodeMakeR
	 call Pcalc2
	 call NTSCdecodeMakeG
	 call Pcalc2
	 call NTSCdecodeMakeB
	 call Pcalc2

	pop ax
	inc ax
	cmp al, 64
	jb .PaletteLoop
	ret

InitNTSCmode:
	; Attempt setting 32-bit 320x240 mode.
	; If that doesn't work, fall back to Mode-X, palette & dithering.
	mov di, VESA_Info
.NextMode:
	mov ax, 0x4F01
	inc word [VESA_mode]
	mov cx, [VESA_mode]
	cmp cx, 0x41FF
	jae .No32bitMode
	int 0x10
	cmp ah, 0
	jnz .NextMode
	cmp dword [di+0x12], 320 + (240<<16)
	jne .NextMode
	cmp byte [di+0x19], 32
	jne .NextMode

	xor eax, eax
	mov ax, [di+4]
	;mov [VESA_Granularity_kB], ax
	shl eax, 10
	mov [VESA_Granularity_bytes], eax
	
	mov ax, 0x4F02
	mov bx, 0x4013
    VESA_mode EQU $-2
	int 0x10
	cmp ah, 0
	jnz .No32bitMode

	mov ax, 0x9090
	; Change NTSCdecodeMakeLinear so it won't do un-gammacorrection
	mov word [NTSC_DECODE_POWER2], ax
	mov word [MODEX_RENDERING_ENABLE], ax
	mov word [MODEX_DECODE_ENABLE], ax
	mov word [MODEX_DECODE_ENABLE+2], ax
	ret

.No32bitMode:
	call InitModeX

	; Set 4*7*9 regular LINEAR palette (252 colors)
	; So that dithering will do proper gamma correction.

	mov al, 4
	call PoutBegin
	xor bp, bp
	xor si, si
	xor di, di
	; R = 63*sqrt(r/4)
	; G = 63*sqrt(r/7)
	; B = 63*sqrt(r/9)
.Ploop:
	mov ax, bp
	call Pcalc
	dd 4.0
	
	mov ax, si
	call Pcalc
	dd 7.0

	mov ax, di
	call Pcalc
	dd 9.0
	
	inc di
	cmp di, 9
	jb .Ploop
	xor di, di
	inc si
	cmp si, 7
	jb .Ploop
	xor si, si
	inc bp
	cmp bp, 4
	jb .Ploop
	ret

PoutBegin:
	mov dl, 0xC8 ; 3C8
	out dx, al
	ret
Pcalc:
	pop bx
	; ax = 63*sqrt(ax / [bx])
	mov [.temp], ax
	fild word[.temp]
	fdiv dword[bx]
	fsqrt
	fmul dword [const63]
	fistp word [.temp]
	add bx, 4
	push bx
	fwait
	mov ax, 0
	.temp EQU $-2
	;jmp PalOutput
PalOutput:
	mov dx, 0x3C9
	out dx, al
	ret

Pcalc2:
	call FloatToPositiveIntWithClamp
const63:
	dd 63.49
	dw 63
	jmp short PalOutput


Terminate:
	call APU_cleanup
	call JOY_cleanup

	; Wait for an input key
	xor ax, ax
WaitKeyLocation:
	int 16h
	mov ax, 3
	int 10h
	ret

;%include "maketestsl.inc"


section .bss

RAM:	        resb 0x800
OAM:	        resb 0x100
PALETTE:        resb 32
CIRAM:          resb 0x400*2

OAM2_x:   	resb 8
OAM2_index:	resb 8
OAM2_attr:	resb 8
OAM2_y:		resb 8
OAM2_sprindex:	resb 8
OAM3_x:		resb 8
OAM3_attr:	resb 8
OAM3_sprindex:	resb 8
OAM3_pattern:	resw 8 ; words

xbegins:	resd 320 ; Table of NTSCline offsets for each X coordinate
sincos:		resd 24  ; sin(pi/6 * x)
NTSCline:	resd 8*282

C_VROMPAGE	resw 1

VESA_Info	EQU NTSCline	; Buffer for holding VESA mode information

; BSS size so far:       0x3A12 bytes.
; With code size around  0x1F3C bytes.
; With org at:           0xA110 (PSP 0x100, HDR 0x10, PRG 0x4000, CHR 3*0x2000)
; This puts the cap at:  0xFA5E.

; Therefore there is no room for VRAM.
VRAM:		;resb 0x2000
; Nor for WRAM, either.
