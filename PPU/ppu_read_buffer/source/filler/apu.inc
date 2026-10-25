ADLIB_PORT EQU 0x388

section .const
LengthCounters:
	db 10,254,20, 2,40, 4,80, 6,  160, 8,60,10,14,12,26,14
        db 12, 16,24,18,48,20,96,22,  192,24,72,26,16,28,32,30
NoisePeriods:
	dw 2,4,8,16,32,48,64,80,101,127,190,254,381,508,1017,2034
section .text

APUframeDelay  dw 0
APUframePeriod equ 7458

APU_read:
	mov al, 0 ; APU not implemented
	ret
APU_write:
	ret

APU_tick:
	mov bx, APUframeDelay
	cmp word [bx], 0
	jnz .DecDelay
	add word [bx], APUframePeriod
	mov al, 0
    APUframe EQU $-1
	cmp al, 1
	je .Frame1
	cmp al, 3
	je .Frame3
	cmp al, 2
	je .Frame2
.Frame0:
	;
.Frame2:
	call APU_120hz
	jmp .FramePrefixDone
.Frame1:
	sub word [bx], 2
	jmp .FramePrefixDone
.Frame3:
	mov byte [APUframe],0xFF
	; if fivecycledivider,
	;    add word [bx], APUframePeriod-6
.FramePrefixDone:
	inc byte [APUframe]
	call APU_240hz
	



	ret
.DecDelay:
	dec word [bx]
	ret


APU_120hz:
	ret
APU_240hz:
	ret

APU_init:
	call .res
	in al, dx
	mov ax, 0xFF02
	call WriteR
	mov ax, 0x2104
	call WriteR
	;TODO: delay 10ms. At 200 MHz, this translates to 2 million instructions.
	in al,dx
    .res:
	mov ax, 0x6004
	call WriteR
	mov ax, 0x8004
	jmp WriteR

APU_cleanup:
	mov ax,1
.loop:
	call WriteR
	inc ax
	cmp ax, 244
	jbe .loop
	ret

WriteR:
	; al=index
	; ah=data
	; overwrites cx,dx
	push ax
	 mov dx, ADLIB_PORT
	 out dx, al
	 mov cx, 6
.w1:	 in al, dx
	 loop .w1
	 inc dx
	 mov al, ah
	 out dx, al
	 dec dx
	 mov cx, 35
.w2:	 in al,dx
	 loop .w2
	pop ax
	ret
NoteOff:
	; al=channel
	mov ah,0
	add al,0xB0
	jmp WriteR
