@del temp\*.s
@del temp\*.o
@del temp\ctrltest.nes
@del temp\ctrltest.dbg
@del temp\ctrltest.map

cc65\bin\cc65 -o temp\ctrltest.c.s -O -T -g ctrltest.c
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\ctrltest.c.o -g temp\ctrltest.c.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\ctrltest4016.o -g ctrltest.s -D PORT=0
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o temp\ctrltest4017.o -g ctrltest.s -D PORT=1
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o temp\ctrltest4016.nes -m temp\ctrltest4016.map --dbgfile temp\ctrltest4016.dbg -C ctrltest.cfg temp\ctrltest4016.o temp\ctrltest.c.o temp\runtime.lib
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o temp\ctrltest4017.nes -m temp\ctrltest4017.map --dbgfile temp\ctrltest4017.dbg -C ctrltest.cfg temp\ctrltest4017.o temp\ctrltest.c.o temp\runtime.lib
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