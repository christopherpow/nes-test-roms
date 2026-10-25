JOY_write:
	cmp dl, 0
	je .NoStrobe
	mov byte [J_SHIFT], 0
.NoStrobe:
	ret

JOY_read:
	; Note: This will return the keys correctly only
	;       if the game polls each joypad in sequence
	;       (not in parallel), and only for the first
	;       joypad polled.
	mov ah, [C_OPENBUS]
	and ah, 0xC0

	mov al, 0x00
    J_DATA  EQU $-1
	shr al, 5
    J_SHIFT EQU $-1
	and al, 1
	or al, ah
	inc byte [J_SHIFT]
	ret




JOY_init:
	mov [OldI09+2], cs
	;passthru
JOY_cleanup:
	push ds
	 push word 0
	 pop ds
	 ; Install or restore IRQ 1 handler (interrupt vector 9)
	 cli
	 mov eax, IRQ01
	OldI09 EQU $-4
	 ; On first invokation (init):
	 ; Fetch old interrupt handler
	 ; Put our own in place
	 xchg [9*4], eax
	 ; Save old interrupt handler, so that it can be restored later
	pop ds
	mov [OldI09], eax
	sti
	; On second invokation (cleanup),
	; Fetch own interrupt handler address
	; Put old interrupt handler in place
	; Save own interrupt handler.
	ret

IRQ01:
	pusha
	 in al, 60h

	 mov ah, al
	 and al, 0x7F ; which key
	 cmp al, 1
	 je .HitEsc

	 mov si, 15
.CheckKeyLoop:
	 cs cmp al, [JOY_keyboard_scancodes+si]
	 je .FoundKey
	 dec si
	 jns .CheckKeyLoop
	 ; Acknowledge IRQ
.DoneKey:
	 mov al, 20h
	 out 20h, al
	popa
	iret ; do not chain into old IRQ handler
.FoundKey:
	 ; If AH = 0x00, should do J_DATA = J_DATA |  (1 << si)
	 ;                  else   J_DATA = J_DATA & ~(1 << si)
	 mov cx, si
	 and cl, 7
	 mov al, 1
	 shl al, cl
	 mov si, J_DATA
	 test ah, 0x80
	 jnz .Clear
	 cs or byte [si], al
	 jmp .DoneKey
	.Clear:
	 not al
	 cs and byte [si], al
	 jmp .DoneKey

.HitEsc:
	; Modify program code to break out of the emulation loop
	mov ax, 0x9090 ; nop
	cs mov word [EscHitFlag],      ax
	; Also disable the part that waits for a key to be pressed at end
	cs mov word [WaitKeyLocation], ax
	jmp .DoneKey

section .const
JOY_keyboard_scancodes:
	; PC keyboard scancodes corresponding to the following
	; keys: A, B, select, start, up, down, left, right
	; Alternative set 1:
	db 0x20 ;'D'
	db 0x1E ;'A'
	db 0x23 ;'H'
	db 0x19 ;'P'
	db 'H' ; up
	db 'P' ; down
	db 'K' ; left
	db 'M' ; right
	; Alternative set 2:
	db 0x38 ; alt
	db 0x1D ; ctrl
	db 0x0F ; tab
	db 0x1C ; enter
	db 0x17 ; 'I'
	db 0x25 ; 'K'
	db 0x24 ; 'J'
	db 0x26 ; 'L'
section .text
