md temp
del temp\bxrom_test*.o
del temp\bxrom_test.nes

cc65\bin\ca65 bxrom_test_header.s -g -o temp\bxrom_test_header.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$0 -g -o temp\bxrom_test0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$1 -g -o temp\bxrom_test1.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$2 -g -o temp\bxrom_test2.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$3 -g -o temp\bxrom_test3.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$4 -g -o temp\bxrom_test4.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$5 -g -o temp\bxrom_test5.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$6 -g -o temp\bxrom_test6.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$7 -g -o temp\bxrom_test7.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$8 -g -o temp\bxrom_test8.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$9 -g -o temp\bxrom_test9.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$A -g -o temp\bxrom_testA.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$B -g -o temp\bxrom_testB.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$C -g -o temp\bxrom_testC.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$D -g -o temp\bxrom_testD.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$E -g -o temp\bxrom_testE.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ca65 bxrom_test.s -D BANK_NUMBER=$F -g -o temp\bxrom_testF.o
@IF ERRORLEVEL 1 GOTO badbuild

cc65\bin\ld65 -o temp\bxrom_test_header.bin -C header.cfg temp\bxrom_test_header.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_test0.bin -C bank32k.cfg temp\bxrom_test0.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_test1.bin -C bank32k.cfg temp\bxrom_test1.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_test2.bin -C bank32k.cfg temp\bxrom_test2.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_test3.bin -C bank32k.cfg temp\bxrom_test3.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_test4.bin -C bank32k.cfg temp\bxrom_test4.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_test5.bin -C bank32k.cfg temp\bxrom_test5.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_test6.bin -C bank32k.cfg temp\bxrom_test6.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_test7.bin -C bank32k.cfg temp\bxrom_test7.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_test8.bin -C bank32k.cfg temp\bxrom_test8.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_test9.bin -C bank32k.cfg temp\bxrom_test9.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_testA.bin -C bank32k.cfg temp\bxrom_testA.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_testB.bin -C bank32k.cfg temp\bxrom_testB.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_testC.bin -C bank32k.cfg temp\bxrom_testC.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_testD.bin -C bank32k.cfg temp\bxrom_testD.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_testE.bin -C bank32k.cfg temp\bxrom_testE.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o temp\bxrom_testF.bin -C bank32k.cfg temp\bxrom_testF.o
@IF ERRORLEVEL 1 GOTO badbuild

copy /b temp\bxrom_test_header.bin+temp\bxrom_test0.bin+temp\bxrom_test1.bin+temp\bxrom_test2.bin+temp\bxrom_test3.bin+temp\bxrom_test4.bin+temp\bxrom_test5.bin+temp\bxrom_test6.bin+temp\bxrom_test7.bin+temp\bxrom_test8.bin+temp\bxrom_test9.bin+temp\bxrom_testA.bin+temp\bxrom_testB.bin+temp\bxrom_testC.bin+temp\bxrom_testD.bin+temp\bxrom_testE.bin+temp\bxrom_testF.bin temp\bxrom_test.nes


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
