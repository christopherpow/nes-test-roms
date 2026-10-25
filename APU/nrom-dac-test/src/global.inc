.ifndef GLOBAL_INC
.define GLOBAL_INC
; init.asm
.global RESET_HANDLER

; nmi.asm
.global NMI_HANDLER

; irq.asm
.global IRQ_HANDLER

; control_read.asm
.global CONTROL_READ

; graphics.asm
.global load_palette, write_palette, load_nametable, clear_nametable, display_registers, update_scrolling

; sprites.asm
.global load_sprite, Stars2, Stars1

; main.asm
.global main

.globalzp framecounter
.globalzp system_state

.globalzp controller_1
.globalzp controller_2

.globalzp temp_8_0
.globalzp temp_8_1
.globalzp temp_8_2
.globalzp temp_8_3
.globalzp temp_16_0
.globalzp temp_16_1
.globalzp temp_16_2
.globalzp temp_16_3

.globalzp s_PPUCTRL
.globalzp s_PPUMASK

.globalzp ppu_scroll_x
.globalzp ppu_scroll_y

.globalzp palette_state
.globalzp palette_table
.globalzp nametable_state
.globalzp nametable_location

.globalzp DAC_LEVEL_PU1
.globalzp DAC_LEVEL_PU2
.globalzp DAC_SEQ_TRI
.globalzp DAC_LEVEL_NOI
.globalzp DAC_LEVEL_DMC

.globalzp star_layer_1_x
.globalzp star_layer_1_y
.globalzp star_layer_2_x
.globalzp star_layer_2_y
.globalzp OAM_RAM_start

.endif
