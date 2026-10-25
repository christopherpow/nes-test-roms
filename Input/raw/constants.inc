; -*- asm -*-

; This list of constants (for use with XA65) is released to the public
; domain (as it should be!), or, failing that, under the terms of the WTFPL

#define COM(t) (255^t)
	
#define NameTable(x,y) ((x&31)|((y&31)<<5)|$2000|((x&32)<<5)|((y&32)<<6))
#define NamePalette(x,y) (((x&28)>>2)|((y&28)<<1)|$23C0|((x&32)<<5)|((y&32)<<6))

PPU_CTRL = $2000
#define PPU_CTRL_NT0 0
#define PPU_CTRL_NT1 1
#define PPU_CTRL_NT2 2
#define PPU_CTRL_NT3 3
#define PPU_CTRL_INC1 0
#define PPU_CTRL_INC32 4
#define PPU_CTRL_SPR_LEFT 0
#define PPU_CTRL_SPR_RIGHT 8
#define PPU_CTRL_BKGD_LEFT 0
#define PPU_CTRL_BKGD_RIGHT 16
#define PPU_CTRL_SPR_8X8 0
#define PPU_CTRL_SPR_8X16 32
#define PPU_CTRL_NMI_DIS 0
#define PPU_CTRL_NMI_ENA 128

PPU_MASK = $2001
#define PPU_MASK_COLOR 0
#define PPU_MASK_GREY 1
#define PPU_MASK_LEFT_BKGD_CLIP 0
#define PPU_MASK_LEFT_BKGD_SHOW 2
#define PPU_MASK_LEFT_SPR_CLIP 0
#define PPU_MASK_LEFT_SPR_SHOW 4
#define PPU_MASK_BKGD_DIS 0
#define PPU_MASK_BKGD_ENA 8
#define PPU_MASK_SPR_DIS 0
#define PPU_MASK_SPR_ENA 16
#define PPU_MASK_EMPH_RED 32
#define PPU_MASK_EMPH_GRN 64
#define PPU_MASK_EMPH_BLU 128

PPU_STATUS = $2002
#define PPU_STATUS_VBLANK 128
#define PPU_STATUS_SPR0_HIT 64
#define PPU_STATUS_SPR_OVER 32

PPU_OAM_ADDR = $2003
PPU_OAM_DATA = $2004

#define OAM_Y 0
#define OAM_T 1
#define OAM_A 2
#define OAM_A_P0 0
#define OAM_A_P1 1
#define OAM_A_P2 2
#define OAM_A_P3 3
#define OAM_A_FG 0
#define OAM_A_BG 32
#define OAM_A_FLIP_H 64
#define OAM_A_FLIP_V 128
#define OAM_X 3
	
PPU_SCROLL = $2005
PPU_ADDR = $2006
PPU_DATA = $2007

APU_ATTRIB_SQ1 = $4000
APU_ATTRIB_SQ2 = $4004
APU_ATTRIB_NSE = $400C

#define APU_ATTRIB_DUTY_12 0
#define APU_ATTRIB_DUTY_25 64
#define APU_ATTRIB_DUTY_50 128
#define APU_ATTRIB_DUTY_75 192
#define APU_ATTRIB_ENV_LOOP 32
#define APU_ATTRIB_ENV_DIS 16
#define APU_ATTRIB_ENV_ENA 0
#define APU_ATTRIB_VOL(n) (n&15)
#define APU_ATTRIB_ENV_PERIOD(n) (n&15)

APU_SWEEP_SQ1 = $4001
APU_SWEEP_SQ2 = $4005

#define APU_SWEEP_FULLY_DISABLED 8

APU_PERIOD_LO_SQ1 = $4002
APU_PERIOD_LO_SQ2 = $4006
APU_PERIOD_LO_TRI = $400A
APU_PERIOD_HI_SQ1 = $4003
APU_PERIOD_HI_SQ2 = $4007
APU_PERIOD_HI_TRI = $400B

#define APU_PERIOD_MAXIMUM_DURATION 8

APU_PERIOD_NSE = $400E
; ranges from 0-15, 128-143
#define APU_NSE_TONAL 128

APU_PERIOD_DMC = $4010
; ranges from 0-15
#define APU_DMC_IRQ_DIS 0
#define APU_DMC_IRQ_ENA 128
#define APU_DMC_LOOP 64

APU_DURATION_TRI = $4008
APU_DURATION_NSE = $400F
APU_DURATION_DMC = $4013

APU_DMC_OFFSET = $4012
APU_DAC = $4011

PPU_OAM_DMA = $4014

APU_STATUS = $4015
; read-only
#define APU_STATUS_DMC_RUNNING 16
; read & write
#define APU_STATUS_SQ1_ENA 1
#define APU_STATUS_SQ2_ENA 2
#define APU_STATUS_TRI_ENA 4
#define APU_STATUS_NSE_ENA 8
; write-only
#define APU_STATUS_DMC_MAKE_RUN 16
#define APU_STATUS_60HZ_IRQ 64
#define APU_STATUS_DMC_IRQ 128

APU_MODE = $4017
#define APU_MODE_60HZ 0
#define APU_MODE_48HZ 128
#define APU_MODE_IRQ_ENA 0
#define APU_MODE_IRQ_DIS 64

JOY_1 = $4016
JOY_2 = $4017
#define JOY_1_LATCH 1
#define JOY_NO_BUTTON 1
#define JOY_1_FAMICOM_MICROPHONE 4
#define JOY_ZAPPER_TRIGGER 16
#define JOY_ZAPPER_NO_LIGHT 8

#define PAL_GRAY33 $00
#define PAL_MIDNIGHT_BLUEGREEN $01
#define PAL_BLUE4 $02
#define PAL_INDIGO $03
#define PAL_ROYAL_PURPLE $04
#define PAL_BURGUNDY $05
#define PAL_DRIED_BLOOD $06
#define PAL_CHOCOLATE $07
#define PAL_DARK_OLIVE $08
#define PAL_HUNTER_GREEN $09
#define PAL_RACING_GREEN $0A
#define PAL_DARK_GREEN $0B
#define PAL_DARK_TEAL $0C
#define PAL_BAD $0D

#define PAL_GRAY59 $10
#define PAL_DODGER_BLUE3 $11
#define PAL_LIGHT_ROYAL_BLUE $12
#define PAL_BLUE_VIOLET $13
#define PAL_BARNEY_PURPLE $14
#define PAL_DARK_FUCHSIA $15
#define PAL_BROWNISH_RED $16
#define PAL_CHESTNUT $17
#define PAL_GREEN_BROWN $18
#define PAL_DARK_GRASS_GREEN $19
#define PAL_GREEN4 $1A
#define PAL_JUNGLE_GREEN $1B
#define PAL_DEEP_TURQUOISE $1C
#define PAL_BLACK $1D

#define PAL_WHITE_DUPLICATE $20
#define PAL_DARK_SKY_BLUE $21
#define PAL_LAVENDER_BLUE $22
#define PAL_MEDIUM_ORCHID $23
#define PAL_HELIOTROPE $24
#define PAL_HOT_PINK $25
#define PAL_INDIAN_RED2 $26
#define PAL_BROWNISH_ORANGE $27
#define PAL_PUKE $28
#define PAL_CHARTRUESE3 $29
#define PAL_GREEN_APPLE $2A
#define PAL_SEA_GREEN3 $2B
#define PAL_STEEL_BLUE2 $2C
#define PAL_GRAY23 $2D

#define PAL_WHITE $30
#define PAL_LIGHT_SKY_BLUE2 $31
#define PAL_LIGHT_PERIWINKLE $32
#define PAL_LIGHT_VIOLET $33
#define PAL_PLUM2 $34
#define PAL_POWDER_PINK $35
#define PAL_ROSY_BROWN2 $36
#define PAL_BURLYWOOD2 $37
#define PAL_GREENISH_BEIGE $38
#define PAL_PALE_OLIVE_GREEN $39
#define PAL_PASTEL_GREEN $3A
#define PAL_HOSPITAL_GREEN $3B
#define PAL_LIGHT_BLUE $3C
#define PAL_GREY62 $3D

	
#define JOY_A 1
#define JOY_B 2
#define JOY_SL 4
#define JOY_ST 8
#define JOY_UP 16
#define JOY_DN 32
#define JOY_LT 64
#define JOY_RT 128

;;; more contentious- preferred assembly mnemonics
;;; some 'conventional' synonyms
#define blt bcc
#define bge bcs
	
;;; some others I'd prefer to use
#define bnz bne
#define bz beq
#define bc bcs
#define bv bvs
#define b7s bmi
#define b7c bpl
#define xor eor
#define call jsr
