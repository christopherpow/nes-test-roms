md temp
del temp\3_test*.*

REM build headers

cc65\bin\ca65 3_test_header.s -D PRG_BANKS=2 -D CHR_BANKS=4 -D SUBMAPPER=0 -D WRAM=0 -g -o temp\3_test_header_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 3_test_header.s -D PRG_BANKS=2 -D CHR_BANKS=4 -D SUBMAPPER=1 -D WRAM=0 -g -o temp\3_test_header_1.o
cc65\bin\ca65 3_test_header.s -D PRG_BANKS=2 -D CHR_BANKS=4 -D SUBMAPPER=2 -D WRAM=0 -g -o temp\3_test_header_2.o

cc65\bin\ld65 -o temp\3_test_header_0.bin -C header.cfg temp\3_test_header_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\3_test_header_1.bin -C header.cfg temp\3_test_header_1.o
cc65\bin\ld65 -o temp\3_test_header_2.bin -C header.cfg temp\3_test_header_2.o

REM build PRG banks

cc65\bin\ca65 3_test.s -g -o temp\3_test.o
@IF ERRORLEVEL 1 GOTO badbuild

cc65\bin\ld65 -o temp\3_test.bin -C 3_test.cfg temp\3_test.o
@IF ERRORLEVEL 1 GOTO badbuild

REM build CHR banks

cc65\bin\ca65 3_test_chr.s -D BANK=$0 -g -o temp\3_test_chr_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 3_test_chr.s -D BANK=$1 -g -o temp\3_test_chr_1.o
cc65\bin\ca65 3_test_chr.s -D BANK=$2 -g -o temp\3_test_chr_2.o
cc65\bin\ca65 3_test_chr.s -D BANK=$3 -g -o temp\3_test_chr_3.o

cc65\bin\ld65 -o temp\3_test_chr_0.chr -C chr4k.cfg temp\3_test_chr_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\3_test_chr_1.chr -C chr4k.cfg temp\3_test_chr_1.o
cc65\bin\ld65 -o temp\3_test_chr_2.chr -C chr4k.cfg temp\3_test_chr_2.o
cc65\bin\ld65 -o temp\3_test_chr_3.chr -C chr4k.cfg temp\3_test_chr_3.o

; REM assemble ROMs

copy /b temp\3_test_chr_0.chr   + temp\3_test_chr_0.chr   + temp\3_test_chr_1.chr   + temp\3_test_chr_1.chr     temp\3_test_chr_01.chr
copy /b temp\3_test_chr_2.chr   + temp\3_test_chr_2.chr   + temp\3_test_chr_3.chr   + temp\3_test_chr_3.chr     temp\3_test_chr_23.chr

copy /b temp\3_test_header_0.bin + temp\3_test.bin + temp\3_test_chr_01.chr + temp\3_test_chr_23.chr   temp\3_test_0.nes
copy /b temp\3_test_header_1.bin + temp\3_test.bin + temp\3_test_chr_01.chr + temp\3_test_chr_23.chr   temp\3_test_1.nes
copy /b temp\3_test_header_2.bin + temp\3_test.bin + temp\3_test_chr_01.chr + temp\3_test_chr_23.chr   temp\3_test_2.nes

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
