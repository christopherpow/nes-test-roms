; -*- asm -*-

; This list of constants (for use with CA65) is released to the public
; domain (as it should be!), or, failing that, under the terms of the WTFPL

;; #define COM(t) (255^t)
	
;; #define NameTable(x,y) ((x&31)|((y&31)<<5)|$2000|((x&32)<<5)|((y&32)<<6))
;; #define NamePalette(x,y) (((x&28)>>2)|((y&28)<<1)|$23C0|((x&32)<<5)|((y&32)<<6))

PPU_CTRL = $2000
.enum PPUCTRL
	NT0 = 0
	NT1 = 1
	NT2 = 2
	NT3 = 3
	INC1 = 0
	INC32 = 4
	SPR_LEFT = 0
	SPR_RIGHT = 8
	BKGD_LEFT = 0
	BKGD_RIGHT = 16
	SPR_8X8 = 0
	SPR_8X16 = 32
	NMI_DIS = 0
	NMI_ENA = 128
.endenum

PPU_MASK = $2001
.enum PPUMASK
	COLOR = 0
	GREY = 1
	LEFT_BKGD_CLIP = 0
	LEFT_BKGD_SHOW = 2
	LEFT_SPR_CLIP = 0
	LEFT_SPR_SHOW = 4
	BKGD_DIS = 0
	BKGD_ENA = 8
	SPR_DIS = 0
	SPR_ENA = 16
	EMPH_RED = 32
	EMPH_GRN = 64
	EMPH_BLU = 128
.endenum
	
PPU_STATUS = $2002
.enum PPUSTATUS
	VBLANK = 128
	SPR0_HIT = 64
	SPR_OVER = 32
.endenum

PPU_OAM_ADDR = $2003
PPU_OAM_DATA = $2004
.enum OAM
	Yoff = 0
	Tile = 1
	Attr = 2
	Xoff = 3
.endenum
.enum OAMATTR
	P0 = 0
	P1 = 1
	P2 = 2
	P3 = 3
	FG = 0
	BG = 32
	FLIP_H = 64
	FLIP_V = 128
.endenum
	
PPU_SCROLL = $2005
PPU_ADDR = $2006
PPU_DATA = $2007

APU_ATTRIB_SQ1 = $4000
APU_ATTRIB_SQ2 = $4004
APU_ATTRIB_NSE = $400C
.enum APUATTRIB
	DUTY_12 = 0
	DUTY_25 = 64
	DUTY_50 = 128
	DUTY_75 = 192
	ENV_LOOP = 32
	ENV_DIS = 16
	ENV_ENA = 0
.endenum
;; #define APU_ATTRIB_VOL(n) (n&15)
;; #define APU_ATTRIB_ENV_PERIOD(n) (n&15)

APU_SWEEP_SQ1 = $4001
APU_SWEEP_SQ2 = $4005
.enum APUSWEEP
	FULLY_DISABLED = 8
.endenum

	APU_PERIOD_LO_SQ1 = $4002
APU_PERIOD_LO_SQ2 = $4006
APU_PERIOD_LO_TRI = $400A
APU_PERIOD_HI_SQ1 = $4003
APU_PERIOD_HI_SQ2 = $4007
APU_PERIOD_HI_TRI = $400B
.enum APUPERIODHI
	MAXIMUM_DURATION = 8
.endenum

APU_PERIOD_NSE = $400E
; ranges from 0-15, 128-143
.enum APUNSE
	TONAL = 128
.endenum

APU_PERIOD_DMC = $4010
; ranges from 0-15
.enum APUDMC
	IRQ_DIS = 0
	IRQ_ENA = 128
	LOOP = 64
.endenum

APU_DURATION_TRI = $4008
APU_DURATION_NSE = $400F
APU_DURATION_DMC = $4013

APU_DMC_OFFSET = $4012
APU_DAC = $4011

PPU_OAM_DMA = $4014

APU_STATUS = $4015
.enum APUSTATUS
; read-only
	DMC_RUNNING = 16
; read & write
	SQ1_ENA = 1
	SQ2_ENA = 2
	TRI_ENA = 4
	NSE_ENA = 8
; write-only
	DMC_MAKE_RUN = 16
	SIXTY_IRQ = 64
	DMC_IRQ = 128
.endenum

APU_MODE = $4017
.enum APUMODE
	SIXTYHZ = 0
	FIFTYHZ = 128
	IRQ_ENA = 0
	IRQ_DIS = 64
.endenum

JOY_1 = $4016
JOY_2 = $4017
;; #define JOY_1_LATCH 1
;; #define JOY_NO_BUTTON 1
;; #define JOY_1_FAMICOM_MICROPHONE 4
;; #define JOY_ZAPPER_TRIGGER 16
;; #define JOY_ZAPPER_NO_LIGHT 8
.enum JOYBUT
	ALLBUT = 15
	ABUT = 1
	BBUT = 2
	SELECT = 4
	START = 8
	ALLJOY = 240
	UP = 16
	DN = 32
	LT = 64
	RT = 128
.endenum
