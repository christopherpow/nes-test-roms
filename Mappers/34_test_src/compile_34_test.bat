md temp
del temp\34_test*.*

REM build headers

cc65\bin\ca65 34_test_header.s -D PRG_BANKS=4 -D CHR_BANKS=8 -D SUBMAPPER=1 -D WRAM=7 -g -o temp\34_test_header_1.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 34_test_header.s -D PRG_BANKS=16 -D CHR_BANKS=0 -D SUBMAPPER=2 -D WRAM=7 -g -o temp\34_test_header_2.o

cc65\bin\ld65 -o temp\34_test_header_1.bin -C header.cfg temp\34_test_header_1.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\34_test_header_2.bin -C header.cfg temp\34_test_header_2.o

REM build PRG banks

cc65\bin\ca65 34_test.s -D BANK=$0 -g -o temp\34_test_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 34_test.s -D BANK=$1 -g -o temp\34_test_1.o
cc65\bin\ca65 34_test.s -D BANK=$2 -g -o temp\34_test_2.o
cc65\bin\ca65 34_test.s -D BANK=$3 -g -o temp\34_test_3.o
cc65\bin\ca65 34_test.s -D BANK=$4 -g -o temp\34_test_4.o
cc65\bin\ca65 34_test.s -D BANK=$5 -g -o temp\34_test_5.o
cc65\bin\ca65 34_test.s -D BANK=$6 -g -o temp\34_test_6.o
cc65\bin\ca65 34_test.s -D BANK=$7 -g -o temp\34_test_7.o

cc65\bin\ld65 -otemp\34_test_0.bin -C 34_test.cfg temp\34_test_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -otemp\34_test_1.bin -C 34_test.cfg temp\34_test_1.o
cc65\bin\ld65 -otemp\34_test_2.bin -C 34_test.cfg temp\34_test_2.o
cc65\bin\ld65 -otemp\34_test_3.bin -C 34_test.cfg temp\34_test_3.o
cc65\bin\ld65 -otemp\34_test_4.bin -C 34_test.cfg temp\34_test_4.o
cc65\bin\ld65 -otemp\34_test_5.bin -C 34_test.cfg temp\34_test_5.o
cc65\bin\ld65 -otemp\34_test_6.bin -C 34_test.cfg temp\34_test_6.o
cc65\bin\ld65 -otemp\34_test_7.bin -C 34_test.cfg temp\34_test_7.o

REM build CHR banks

cc65\bin\ca65 34_test_chr.s -D BANK=$0 -g -o temp\34_test_chr_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 34_test_chr.s -D BANK=$1 -g -o temp\34_test_chr_1.o
cc65\bin\ca65 34_test_chr.s -D BANK=$2 -g -o temp\34_test_chr_2.o
cc65\bin\ca65 34_test_chr.s -D BANK=$3 -g -o temp\34_test_chr_3.o
cc65\bin\ca65 34_test_chr.s -D BANK=$4 -g -o temp\34_test_chr_4.o
cc65\bin\ca65 34_test_chr.s -D BANK=$5 -g -o temp\34_test_chr_5.o
cc65\bin\ca65 34_test_chr.s -D BANK=$6 -g -o temp\34_test_chr_6.o
cc65\bin\ca65 34_test_chr.s -D BANK=$7 -g -o temp\34_test_chr_7.o
cc65\bin\ca65 34_test_chr.s -D BANK=$8 -g -o temp\34_test_chr_8.o
cc65\bin\ca65 34_test_chr.s -D BANK=$9 -g -o temp\34_test_chr_9.o
cc65\bin\ca65 34_test_chr.s -D BANK=$A -g -o temp\34_test_chr_A.o
cc65\bin\ca65 34_test_chr.s -D BANK=$B -g -o temp\34_test_chr_B.o
cc65\bin\ca65 34_test_chr.s -D BANK=$C -g -o temp\34_test_chr_C.o
cc65\bin\ca65 34_test_chr.s -D BANK=$D -g -o temp\34_test_chr_D.o
cc65\bin\ca65 34_test_chr.s -D BANK=$E -g -o temp\34_test_chr_E.o
cc65\bin\ca65 34_test_chr.s -D BANK=$F -g -o temp\34_test_chr_F.o

cc65\bin\ld65 -o temp\34_test_chr_0.chr -C chr4k.cfg temp\34_test_chr_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\34_test_chr_1.chr -C chr4k.cfg temp\34_test_chr_1.o
cc65\bin\ld65 -o temp\34_test_chr_2.chr -C chr4k.cfg temp\34_test_chr_2.o
cc65\bin\ld65 -o temp\34_test_chr_3.chr -C chr4k.cfg temp\34_test_chr_3.o
cc65\bin\ld65 -o temp\34_test_chr_4.chr -C chr4k.cfg temp\34_test_chr_4.o
cc65\bin\ld65 -o temp\34_test_chr_5.chr -C chr4k.cfg temp\34_test_chr_5.o
cc65\bin\ld65 -o temp\34_test_chr_6.chr -C chr4k.cfg temp\34_test_chr_6.o
cc65\bin\ld65 -o temp\34_test_chr_7.chr -C chr4k.cfg temp\34_test_chr_7.o
cc65\bin\ld65 -o temp\34_test_chr_8.chr -C chr4k.cfg temp\34_test_chr_8.o
cc65\bin\ld65 -o temp\34_test_chr_9.chr -C chr4k.cfg temp\34_test_chr_9.o
cc65\bin\ld65 -o temp\34_test_chr_A.chr -C chr4k.cfg temp\34_test_chr_A.o
cc65\bin\ld65 -o temp\34_test_chr_B.chr -C chr4k.cfg temp\34_test_chr_B.o
cc65\bin\ld65 -o temp\34_test_chr_C.chr -C chr4k.cfg temp\34_test_chr_C.o
cc65\bin\ld65 -o temp\34_test_chr_D.chr -C chr4k.cfg temp\34_test_chr_D.o
cc65\bin\ld65 -o temp\34_test_chr_E.chr -C chr4k.cfg temp\34_test_chr_E.o
cc65\bin\ld65 -o temp\34_test_chr_F.chr -C chr4k.cfg temp\34_test_chr_F.o

; REM assemble ROMs

copy /b temp\34_test_chr_0.chr  + temp\34_test_chr_1.chr  + temp\34_test_chr_2.chr  + temp\34_test_chr_3.chr    temp\34_test_chr_03.chr
copy /b temp\34_test_chr_4.chr  + temp\34_test_chr_5.chr  + temp\34_test_chr_6.chr  + temp\34_test_chr_7.chr    temp\34_test_chr_47.chr
copy /b temp\34_test_chr_8.chr  + temp\34_test_chr_9.chr  + temp\34_test_chr_A.chr  + temp\34_test_chr_B.chr    temp\34_test_chr_8B.chr
copy /b temp\34_test_chr_C.chr  + temp\34_test_chr_D.chr  + temp\34_test_chr_E.chr  + temp\34_test_chr_F.chr    temp\34_test_chr_CF.chr
copy /b temp\34_test_chr_03.chr + temp\34_test_chr_47.chr + temp\34_test_chr_8B.chr + temp\34_test_chr_CF.chr   temp\34_test_chr_0F.chr

copy /b temp\34_test_0.bin + temp\34_test_1.bin + temp\34_test_2.bin + temp\34_test_3.bin   temp\34_test_03.bin
copy /b temp\34_test_4.bin + temp\34_test_5.bin + temp\34_test_6.bin + temp\34_test_7.bin   temp\34_test_47.bin

copy /b temp\34_test_header_1.bin + temp\34_test_0.bin  + temp\34_test_1.bin + temp\34_test_chr_0F.chr   temp\34_test_1.nes
copy /b temp\34_test_header_2.bin + temp\34_test_03.bin + temp\34_test_47.bin                            temp\34_test_2.nes

; REM finish

@echo.
@echo.
@echo Build complete and successful!
@IF NOT "%1"=="" GOTO endbuild
@pause
@GOTO endbuild

:badbuild
@echo.
@echo.
@echo Build error!
@IF NOT "%1"=="" GOTO endbuild
@pause
:endbuild
