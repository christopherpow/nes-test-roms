@del temp\*.s
@del temp\*.o
@del temp\mset.nes
@del temp\mset.dbg
@del temp\mset.map

cc65\bin\cc65 -o temp\mset.c.s -O -T -g mset.c
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\mset.c.o -g temp\mset.c.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\mset.o -g mset.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o temp\mset.nes -m temp\mset.map --dbgfile temp\mset.dbg -C mset.cfg temp\mset.o temp\mset.c.o temp\runtime.lib
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