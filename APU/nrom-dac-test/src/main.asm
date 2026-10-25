.include "defines.inc"
.include "global.inc"

.segment "BANK0"
.proc main
	LDA #$00
	TAX
	TAY
@mainloop:
	JSR waitframe
	JMP @mainloop
.endproc

.proc waitframe
	LDA framecounter
:
	CMP framecounter    ; NMI will change framecounter and then it wont be equal anymore
	BEQ :-
	RTS
.endproc
