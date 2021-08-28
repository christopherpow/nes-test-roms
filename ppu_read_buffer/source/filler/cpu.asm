section .bss
tsc_diff	resd 2
mycount		resd 1
irq0counter	resw 1

section .data
irq8addr	dw 8*4, 0

section .text

pitvalue EQU 11
pitrate  EQU 108471
; pitvalue * pitrate = 1234DDh

; _fastcall double CPUinfo(void);
;                  Measures and returns CPU speed in Hz
_CPUinfo:
	push es
	 cli
	  mov cx, pitvalue
	  call ProgramPIT
	  les di, [irq8addr]
	  es mov eax, [di]
	  mov dword [oldi08], eax

	  es mov word [di+0], TempIrq0
	  es mov word [di+2], cs

	  xor eax, eax
	  mov dword [mycount], eax
	  
	  rdtsc
	  ; ^ OLD TSC -- produces EDX:EAX, but move to ESI:EDI

	  xchg eax, edi
	  xchg edx, esi
.needsmoreloops:
	 sti
	 mov cx, 20000
.loop1: dec cx
	 jnz .loop1 ; some looping.
	 cli ; stop counting, and read TSC as soon as possible
	  mov eax, [mycount]
	  cmp eax, 120
	  jb .needsmoreloops ; If we did not get enough precision, loop more
	  ; We are aiming to catch at least 120/pitrate seconds (1.1 milliseconds)

	  rdtsc
	  ; ^ NEW TSC -- produces EDX:EAX

	  sub eax, edi ; sub low parts
	  sbb edx, esi ; sub high parts
	  xchg eax, esi
	   ; ^ EDX:ESI is now TSC difference.
	   xor cx, cx
	   call ProgramPIT
	   ;les di, dword [irq8addr]
	   mov di, word [irq8addr]
	   mov eax, [oldi08]
	   es mov dword [di], eax
	 sti
	 xchg eax, esi
	 ; ^ EDX:EAX is now TSC difference.
	 ; Now CPU speed = TSCdifference * (pitrate / mycount)
	 imul edi, edx, pitrate
	 mov ecx, pitrate
	 mul ecx
	 add edx, edi
	 mov dword [tsc_diff+0], eax
	 mov dword [tsc_diff+4], edx
	 fild qword [tsc_diff]
	 fild dword [mycount]
	 fdivp st1 ;st(1), st
	pop es
	ret

TempIrq0:
	inc dword [mycount]
	add word  [irq0counter], pitvalue
	jc .callold
	push ax
	 mov al, 20h
	 out 20h, al
	pop ax
	iret
.callold:
	;jmp dword [oldi08]
	db 0EAh
	oldi08 dd 0

ProgramPIT:
	mov al, 34h
	out 43h, al
	mov ax, cx
	out 40h, al
	mov al, ah
	out 40h, al
	ret
