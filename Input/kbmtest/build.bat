@del temp\*.s
@del temp\*.o
@del temp\kbmtest.nes
@del temp\kbmtest.dbg
@del temp\kbmtest.map

cc65\bin\cc65 -o temp\kbmtest.c.s -O -T -g kbmtest.c
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\kbmtest.c.o -g temp\kbmtest.c.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\kbmtest.o -g kbmtest.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o temp\kbmtest.nes -m temp\kbmtest.map --dbgfile temp\kbmtest.dbg -C kbmtest.cfg temp\kbmtest.o temp\kbmtest.c.o temp\runtime.lib
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