del mmc5ramsize*.o
del mmc5ramsize*.nes

cc65\bin\ca65 mmc5ramsize.s -g -o mmc5ramsize.o
@IF ERRORLEVEL 1 GOTO badbuild

cc65\bin\ld65 -o mmc5ramsize_u_ines1.nes     -D batt=0 -D ines2=0 -D ramsize=-6 -C mmc5ramsize.cfg mmc5ramsize.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o mmc5ramsize_s_ines1.nes     -D batt=1 -D ines2=0 -D ramsize=-6 -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_u_ines2_0.nes   -D batt=0 -D ines2=1 -D ramsize=-6 -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_u_ines2_1.nes   -D batt=0 -D ines2=1 -D ramsize=1  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_u_ines2_2.nes   -D batt=0 -D ines2=1 -D ramsize=2  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_u_ines2_4.nes   -D batt=0 -D ines2=1 -D ramsize=3  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_u_ines2_8.nes   -D batt=0 -D ines2=1 -D ramsize=4  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_u_ines2_16.nes  -D batt=0 -D ines2=1 -D ramsize=5  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_u_ines2_32.nes  -D batt=0 -D ines2=1 -D ramsize=6  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_u_ines2_64.nes  -D batt=0 -D ines2=1 -D ramsize=7  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_u_ines2_128.nes -D batt=0 -D ines2=1 -D ramsize=8  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_s_ines2_0.nes   -D batt=1 -D ines2=1 -D ramsize=-6 -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_s_ines2_1.nes   -D batt=1 -D ines2=1 -D ramsize=1  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_s_ines2_2.nes   -D batt=1 -D ines2=1 -D ramsize=2  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_s_ines2_4.nes   -D batt=1 -D ines2=1 -D ramsize=3  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_s_ines2_8.nes   -D batt=1 -D ines2=1 -D ramsize=4  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_s_ines2_16.nes  -D batt=1 -D ines2=1 -D ramsize=5  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_s_ines2_32.nes  -D batt=1 -D ines2=1 -D ramsize=6  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_s_ines2_64.nes  -D batt=1 -D ines2=1 -D ramsize=7  -C mmc5ramsize.cfg mmc5ramsize.o
cc65\bin\ld65 -o mmc5ramsize_s_ines2_128.nes -D batt=1 -D ines2=1 -D ramsize=8  -C mmc5ramsize.cfg mmc5ramsize.o
@IF ERRORLEVEL 1 GOTO badbuild
cc65\bin\ld65 -o mmc5ramsize_s_ines2_2u1.nes -D batt=1 -D ines2=1 -D ramsize=1 -D splitram=7 -C mmc5ramsize.cfg mmc5ramsize.o
@IF ERRORLEVEL 1 GOTO badbuild

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
