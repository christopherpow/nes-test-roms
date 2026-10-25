@del temp\*.s
@del temp\*.o
@del temp\d34test.nes
@del temp\d34test.dbg
@del temp\d34test.map

cc65\bin\cc65 -o temp\d34test.c.s -O -T -g d34test.c
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\d34test.c.o -g temp\d34test.c.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\d34test4016.o -g d34test.s -D PORT=0
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\d34test4017.o -g d34test.s -D PORT=1
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o temp\d34test4016.nes -m temp\d34test4016.map --dbgfile temp\d34test4016.dbg -C d34test.cfg temp\d34test4016.o temp\d34test.c.o temp\runtime.lib
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o temp\d34test4017.nes -m temp\d34test4017.map --dbgfile temp\d34test4017.dbg -C d34test.cfg temp\d34test4017.o temp\d34test.c.o temp\runtime.lib
@IF ERRORLEVEL 1 GOTO error

@echo.
@echo.
@echo Build successful!
@pause
@GOTO end
:error
@echo.
@echo.
@echo Build error!
@pause
:end