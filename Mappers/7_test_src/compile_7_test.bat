md temp
del temp\7_test*.*

REM build headers

cc65\bin\ca65 7_test_header.s -D PRG_BANKS=16 -D CHR_BANKS=0 -D SUBMAPPER=0 -D WRAM=0 -g -o temp\7_test_header_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 7_test_header.s -D PRG_BANKS=16 -D CHR_BANKS=0 -D SUBMAPPER=1 -D WRAM=0 -g -o temp\7_test_header_1.o
cc65\bin\ca65 7_test_header.s -D PRG_BANKS=16 -D CHR_BANKS=0 -D SUBMAPPER=2 -D WRAM=0 -g -o temp\7_test_header_2.o

cc65\bin\ld65 -o temp\7_test_header_0.bin -C header.cfg temp\7_test_header_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\7_test_header_1.bin -C header.cfg temp\7_test_header_1.o
cc65\bin\ld65 -o temp\7_test_header_2.bin -C header.cfg temp\7_test_header_2.o

REM build PRG banks

cc65\bin\ca65 7_test.s -D BANK=$0 -g -o temp\7_test_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 7_test.s -D BANK=$1 -g -o temp\7_test_1.o
cc65\bin\ca65 7_test.s -D BANK=$2 -g -o temp\7_test_2.o
cc65\bin\ca65 7_test.s -D BANK=$3 -g -o temp\7_test_3.o
cc65\bin\ca65 7_test.s -D BANK=$4 -g -o temp\7_test_4.o
cc65\bin\ca65 7_test.s -D BANK=$5 -g -o temp\7_test_5.o
cc65\bin\ca65 7_test.s -D BANK=$6 -g -o temp\7_test_6.o
cc65\bin\ca65 7_test.s -D BANK=$7 -g -o temp\7_test_7.o

cc65\bin\ld65 -o temp\7_test_0.bin -C 7_test.cfg temp\7_test_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\7_test_1.bin -C 7_test.cfg temp\7_test_1.o
cc65\bin\ld65 -o temp\7_test_2.bin -C 7_test.cfg temp\7_test_2.o
cc65\bin\ld65 -o temp\7_test_3.bin -C 7_test.cfg temp\7_test_3.o
cc65\bin\ld65 -o temp\7_test_4.bin -C 7_test.cfg temp\7_test_4.o
cc65\bin\ld65 -o temp\7_test_5.bin -C 7_test.cfg temp\7_test_5.o
cc65\bin\ld65 -o temp\7_test_6.bin -C 7_test.cfg temp\7_test_6.o
cc65\bin\ld65 -o temp\7_test_7.bin -C 7_test.cfg temp\7_test_7.o

; REM assemble ROMs

copy /b temp\7_test_0.bin + temp\7_test_1.bin + temp\7_test_2.bin + temp\7_test_3.bin   temp\7_test_03.bin
copy /b temp\7_test_4.bin + temp\7_test_5.bin + temp\7_test_6.bin + temp\7_test_7.bin   temp\7_test_47.bin

copy /b temp\7_test_03.bin + temp\7_test_47.bin   temp\7_test_07.bin

copy /b temp\7_test_header_0.bin + temp\7_test_07.bin   temp\7_test_0.nes
copy /b temp\7_test_header_1.bin + temp\7_test_07.bin   temp\7_test_1.nes
copy /b temp\7_test_header_2.bin + temp\7_test_07.bin   temp\7_test_2.nes

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
