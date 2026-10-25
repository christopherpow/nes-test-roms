OAM = $0200

.globalzp sig, retraces, test_in_progress, test_result
.global sigvalue

; 32-element arrays used by the interbank fetch
out80 = $0100
outC0 = $0120
out20 = $0140
out24 = $0160
out28 = $0180
out2C = $01A0

regsel = $5000

; Bank footer bytes
; the known $00 and $FF are useful before we establish that
; bus conflicts are successfully avoided
known00 = $FFF6
knownFF = $FFF7
last_bank_number = $FFF8


; main.s
LF = 10
.global puts_16, puts_multiline_16, hexdump8
.globalzp reg80_value, reg81_value, reg00_value, reg01_value

; testseq.s
.global sanity_self_test, setup_oam_for_test, detect_chr_ram_size
.global calc_prg_bank, verify_bank80c0
.global load_mirror_sentinels, verify_mirror_ignores_bit3210
.global get_mirror_type, mirror_type_expect, get_mirror_type_once
.global run_auto_test_1, run_auto_test_2, run_auto_test_3
.globalzp failure_details, chr_ram_last_bank

ATFAIL_PRG2MIRROR = 1
ATFAIL_WRONG_MIRROR = 2
ATFAIL_MIRROR2PRG = 3
ATFAIL_WRONG_PRG = 4
ATFAIL_PRG_CHANGED = 5
ATFAIL_BAD_CHR_SIZE = 6
ATFAIL_KNOWN_00 = 7
ATFAIL_BUS_CONFLICT_AND = 8
ATFAIL_KNOWN_FF = 9
ATFAIL_BUS_CONFLICT_OR = 10
ATFAIL_1SM_MODE = 11
ATFAIL_CHR_D4 = 12
ATFAIL_INNER_CHANGED = 13

; ppuclear.s
.global ppu_clear_nt, ppu_clear_oam, ppu_screen_on

; pads.s
.global read_pads, autorepeat
.globalzp cur_keys, new_keys

; interbank.s
.global __LOWCODE_LOAD__, __LOWCODE_RUN__, __LOWCODE_SIZE__
.global try_returning, freeze_in_bank_0
.global safe_set_a_to_x, set_chr_bank_x
.global inner_outer_mode, read_mode_x_bank_y, verify_bus_conflict
; needs its own reimplementation of puts_multiline_16 because
; if the mapper is broke, it can't get back to puts_16
; space for four (ASCII) digit failure code
.global failed_to_return_testid

