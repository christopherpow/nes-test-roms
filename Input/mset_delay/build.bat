@del temp\*.s
@del temp\*.o
@del temp\mset_delay.nes
@del temp\mset_delay.dbg
@del temp\mset_delay.map

cc65\bin\cc65 -o temp\mset_delay.c.s -O -T -g mset_delay.c
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\mset_delay.c.o -g temp\mset_delay.c.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\mset_delay.o -g mset_delay.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o temp\mset_delay.nes -m temp\mset_delay.map --dbgfile temp\mset_delay.dbg -C mset_delay.cfg temp\mset_delay.o temp\mset_delay.c.o temp\runtime.lib
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