.macro set_vrom_page n
	lda #(n) ;VROM page n
	sta bus_conflict_antidote+(n)
.endmacro

seg_data "RODATA", { bus_conflict_antidote: .byte 0,1,2,3 }
