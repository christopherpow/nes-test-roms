md temp
del temp\2_test*.*

REM build headers

cc65\bin\ca65 2_test_header.s -D PRG_BANKS=16 -D CHR_BANKS=0 -D SUBMAPPER=0 -D WRAM=0 -g -o temp\2_test_header_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 2_test_header.s -D PRG_BANKS=16 -D CHR_BANKS=0 -D SUBMAPPER=1 -D WRAM=0 -g -o temp\2_test_header_1.o
cc65\bin\ca65 2_test_header.s -D PRG_BANKS=16 -D CHR_BANKS=0 -D SUBMAPPER=2 -D WRAM=0 -g -o temp\2_test_header_2.o

cc65\bin\ld65 -o temp\2_test_header_0.bin -C header.cfg temp\2_test_header_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\2_test_header_1.bin -C header.cfg temp\2_test_header_1.o
cc65\bin\ld65 -o temp\2_test_header_2.bin -C header.cfg temp\2_test_header_2.o

REM build PRG banks

cc65\bin\ca65 2_test.s -D BANK=$0 -g -o temp\2_test_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 2_test.s -D BANK=$1 -g -o temp\2_test_1.o
cc65\bin\ca65 2_test.s -D BANK=$2 -g -o temp\2_test_2.o
cc65\bin\ca65 2_test.s -D BANK=$3 -g -o temp\2_test_3.o
cc65\bin\ca65 2_test.s -D BANK=$4 -g -o temp\2_test_4.o
cc65\bin\ca65 2_test.s -D BANK=$5 -g -o temp\2_test_5.o
cc65\bin\ca65 2_test.s -D BANK=$6 -g -o temp\2_test_6.o
cc65\bin\ca65 2_test.s -D BANK=$7 -g -o temp\2_test_7.o
cc65\bin\ca65 2_test.s -D BANK=$8 -g -o temp\2_test_8.o
cc65\bin\ca65 2_test.s -D BANK=$9 -g -o temp\2_test_9.o
cc65\bin\ca65 2_test.s -D BANK=$A -g -o temp\2_test_A.o
cc65\bin\ca65 2_test.s -D BANK=$B -g -o temp\2_test_B.o
cc65\bin\ca65 2_test.s -D BANK=$C -g -o temp\2_test_C.o
cc65\bin\ca65 2_test.s -D BANK=$D -g -o temp\2_test_D.o
cc65\bin\ca65 2_test.s -D BANK=$E -g -o temp\2_test_E.o
cc65\bin\ca65 2_test.s -D BANK=$F -g -o temp\2_test_F.o

cc65\bin\ld65 -o temp\2_test_0.bin -C 2_test.cfg temp\2_test_0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\2_test_1.bin -C 2_test.cfg temp\2_test_1.o
cc65\bin\ld65 -o temp\2_test_2.bin -C 2_test.cfg temp\2_test_2.o
cc65\bin\ld65 -o temp\2_test_3.bin -C 2_test.cfg temp\2_test_3.o
cc65\bin\ld65 -o temp\2_test_4.bin -C 2_test.cfg temp\2_test_4.o
cc65\bin\ld65 -o temp\2_test_5.bin -C 2_test.cfg temp\2_test_5.o
cc65\bin\ld65 -o temp\2_test_6.bin -C 2_test.cfg temp\2_test_6.o
cc65\bin\ld65 -o temp\2_test_7.bin -C 2_test.cfg temp\2_test_7.o
cc65\bin\ld65 -o temp\2_test_8.bin -C 2_test.cfg temp\2_test_8.o
cc65\bin\ld65 -o temp\2_test_9.bin -C 2_test.cfg temp\2_test_9.o
cc65\bin\ld65 -o temp\2_test_A.bin -C 2_test.cfg temp\2_test_A.o
cc65\bin\ld65 -o temp\2_test_B.bin -C 2_test.cfg temp\2_test_B.o
cc65\bin\ld65 -o temp\2_test_C.bin -C 2_test.cfg temp\2_test_C.o
cc65\bin\ld65 -o temp\2_test_D.bin -C 2_test.cfg temp\2_test_D.o
cc65\bin\ld65 -o temp\2_test_E.bin -C 2_test.cfg temp\2_test_E.o
cc65\bin\ld65 -o temp\2_test_F.bin -C 2_test.cfg temp\2_test_F.o

; REM assemble ROMs

copy /b temp\2_test_0.bin + temp\2_test_1.bin + temp\2_test_2.bin + temp\2_test_3.bin   temp\2_test_03.bin
copy /b temp\2_test_4.bin + temp\2_test_5.bin + temp\2_test_6.bin + temp\2_test_7.bin   temp\2_test_47.bin
copy /b temp\2_test_8.bin + temp\2_test_9.bin + temp\2_test_A.bin + temp\2_test_B.bin   temp\2_test_8B.bin
copy /b temp\2_test_C.bin + temp\2_test_D.bin + temp\2_test_E.bin + temp\2_test_F.bin   temp\2_test_CF.bin

copy /b temp\2_test_03.bin + temp\2_test_47.bin + temp\2_test_8B.bin + temp\2_test_CF.bin   temp\2_test_0F.bin

copy /b temp\2_test_header_0.bin + temp\2_test_0F.bin   temp\2_test_0.nes
copy /b temp\2_test_header_1.bin + temp\2_test_0F.bin   temp\2_test_1.nes
copy /b temp\2_test_header_2.bin + temp\2_test_0F.bin   temp\2_test_2.nes

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
