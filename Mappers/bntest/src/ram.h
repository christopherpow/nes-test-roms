; Copyright 2010 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty
; provided the copyright notice and this notice are preserved.
; This file is offered as-is, without any warranty.

xferBuf = $0100
OAM = $0200

; Turn this on to replace the upper left tile of a threatened
; building with a !
SHOW_THREATENED = 0

; Turn this on to annoy Nova
CHECK_ILLEGAL_MISSILES = 1
ILLEGAL_MISSILES_TIP = 13

; main fields
STATE_INACTIVE = 0
STATE_NEW_LEVEL = 1
STATE_ACTIVE = 2
STATE_LEVEL_REWARD = 3
STATE_REBUILDING_SILO = 4
STATE_CUTSCENE = 5
STATE_REBUILDING_HOUSE = 6
STATE_GAMEOVER = 7
.globalzp nmis, oamIndex, numPlayers, tvSystem
.globalzp debugHex1, debugHex2
.globalzp gameState

; pads fields
.globalzp cur_keys, new_keys, das_timer, das_keys
; pads methods
.global read_pads, autorepeat

; title methods
.global titleScreen

; unpkb methods
.global PKB_unpackblk

; random fields
.globalzp rand0, rand1, rand2, rand3
; random methods
.global random, initRandomTarget, chooseRandomTarget
.global findRandomDestroyedHouse

; bg constants
NUM_BUILDINGS = 12
BG_DIRTY_HOUSES = $01
BG_DIRTY_STATUS = $02
BG_DIRTY_TIP = $04
BUILDING_DESTROYED = 0
BUILDING_OK = 1
BUILDING_THREATENED = 2
; bg fields
.global housesStanding, houseX, score1s, score100s
.global gameDay, gameHour, gameMinute, gameSecond, gameTenthSecond, gameSubTenth
.global bgDirty, curTip, tipTimeLeft
.global buildingsDestroyedThisLevel, firstDestroyedHouse
.global main_palette
; bg methods
.global setupGameBG, blitBGUpdate, buildBGUpdate, buildLevelRewardBar
.global buildHouseRebuiltBar
.global newGame, addScore, incGameClock, countHousesLeft
.global puthex

; missiles constants
; missiles 0 and 2 are player 1's
; missiles 1 and 3 are player 2's
; missiles 4 through NUM_MISSILES - 1 are the enemy missiles
NUM_MISSILES = 20
MISSILE_SPAWN_Y = 8
BALLOON_SPAWN_Y = 40  ; in range 0-63 plus this
SILO_Y = 192  ; player missiles launch from here
BUILDING_HIT_Y = 200
; missiles fields
.global crosshairXLo, crosshairXHi, crosshairDXLo, crosshairDXHi
.global crosshairYLo, crosshairYHi, crosshairDYLo, crosshairDYHi
.global missileXLo, missileXHi, missileYLo, missileYHi
.global missileDYHi, missileDYLo
.global missileType, missileTarget, missileTime, missileAngle
; and about 400 bytes of free space to use when missiles
; are not active
.global missilesOverlay

; missiles methods
.global clearAllMissiles, updateMissiles
.global moveCrosshairPlayerX, drawCrosshairPlayerX
.global makeMissile, makeBalloon, firePlayerMissile
.global siloMissilesLeft

; explosion constants
NUM_EXPLOSIONS = 8
; explosion methods
.global clearExplosions, makeExplosion, updateAllExplosions

; smoke methods
.global makeSmoke, clearAllSmoke, updateSmoke

; math fields
.global missileSine, missileCosine
; math methods
.global getAngle, getSlope1, mul8, measureFromSilo

; bcd methods
.global bcd8bit

; levels constants
.globalzp NUM_MADE_DAYS, SKIP_TO_DAY
; levels fields
.global levelMissileSpeed, enemyMissilesLeft, levelReleasePeriod
.global levelMissileTypes, levelSalvoSizes
; levels methods
.global loadLevel

; scurry constants
NUM_VILLAGERS = NUM_BUILDINGS
; scurry methods
.global initVillagers, updateVillagers, villagersGoHome
.global testMissileThreats

; paldetect constants
TV_SYSTEM_NTSC  = 0
TV_SYSTEM_PAL   = 1
TV_SYSTEM_DENDY = 2
TV_SYSTEM_OTHER = 3
; paldetect methods
.global getTVSystem

; sound methods
.global init_sound, start_sound, update_sound, init_music, stop_music

; cutscene fields
.global character_name_offset, character_name0
; cutscene methods
.global load_cutscene, cut_choose_villagers
