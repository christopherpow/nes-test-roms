@del temp\*.s
@del temp\*.o
@del temp\mict.nes
@del temp\mict.dbg
@del temp\mict.map

cc65\bin\cc65 -o temp\mict.c.s -O -T -g mict.c
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\mict.c.o -g temp\mict.c.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\mict.o -g mict.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o temp\mict.nes -m temp\mict.map --dbgfile temp\mict.dbg -C mict.cfg temp\mict.o temp\mict.c.o temp\runtime.lib
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