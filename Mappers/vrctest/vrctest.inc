; --------------------------------------------------------------- CHARMAP
.charmap '0',$00
.charmap '1',$01
.charmap '2',$02
.charmap '3',$03
.charmap '4',$04
.charmap '5',$05
.charmap '6',$06
.charmap '7',$07
.charmap '8',$08
.charmap '9',$09
.charmap 'A',$0a
.charmap 'B',$0b
.charmap 'C',$0c
.charmap 'D',$0d
.charmap 'E',$0e
.charmap 'F',$0f
.charmap 'G',$10
.charmap 'H',$11
.charmap 'I',$12
.charmap 'J',$13
.charmap 'K',$14
.charmap 'L',$15
.charmap 'M',$16
.charmap 'N',$17
.charmap 'O',$18
.charmap 'P',$19
.charmap 'Q',$1a
.charmap 'R',$1b
.charmap 'S',$1c
.charmap 'T',$1d
.charmap 'U',$1e
.charmap 'V',$1f
.charmap 'W',$20
.charmap 'X',$21
.charmap 'Y',$22
.charmap 'Z',$23
.charmap '-',$24
.charmap '!',$25
.charmap '?',$26
.charmap '.',$27
.charmap ',',$28
.charmap ';',$29
.charmap ':',$2a
.charmap '(',$2b
.charmap ')',$2c
.charmap '/',$2d
.charmap '=',$2e
.charmap ' ',$2f
; -------------------------------------------------- CONSTANTS AND MACROS
.enum Color
    white = 0
    red   = $55
    amber = $aa
    green = $ff
.endenum

.enum BGColor
    good = $11
    bad  = $16
.endenum

.enum Result
    no
    yes
    unknown
    early
    nc
    late
    redno
    redyes
    vrc2
    vrc4
    board
.endenum

.macro textyx row, col
.byte row, col
.endmacro

.macro headingtext txt
.byte .strlen(txt), txt, 0, .strlen(txt)
.byte $30
.repeat .strlen(txt) - 2
.byte $31
.endrep
.byte $32 + (.strat(txt, .strlen(txt)-1) = ':'), 0
.endmacro

.macro resulttext txt, var
.byte .strlen(txt), txt, var
.endmacro

.macro textend
.byte 0
.endmacro

.macro colortext clr, txt
.byte Color::clr, .strlen(txt), txt
.endmacro

screenvpos = 4 * (30 - 24)
