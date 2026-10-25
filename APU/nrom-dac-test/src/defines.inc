; character abstractions
.ifndef DEFINES_INC
.define DEFINES_INC 1

KEY_R				= 1 << 0
KEY_L				= 1 << 1
KEY_D				= 1 << 2
KEY_U				= 1 << 3
KEY_STA				= 1 << 4
KEY_SEL				= 1 << 5
KEY_B				= 1 << 6
KEY_A				= 1 << 7

; status flags
STATUS_RENDER 		= 1 << 0
STATUS_PU1PU2		= 1 << 1
STATUS_TRINOIDMC	= 1 << 2

PPUCTRL				= $2000
PPUMASK				= $2001
PPUSTATUS			= $2002
OAMADDR				= $2003
OAMDATA				= $2004
PPUSCROLL			= $2005
PPUADDR				= $2006
PPUDATA				= $2007

OAMDMA				= $4014
JOYPAD1				= $4016
JOYPAD2				= $4017

TEST_DAC_PU1PU2		= $4018
TEST_DAC_TRINOI		= $4019
TEST_DAC_DPCM		= $401A
TEST_TRI_SET		= $401A
.endif
