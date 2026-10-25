@REM Generates temp\runtime.lib
@REM
@REM cc65\ should contain latest cc65 build (cc65\bin)
@REM librsc\ should contain latest cc65 libsrc folder
@REM
@REM contents:
@REM   libsrc\runtime
@REM     -condes.s (don't need constructor/destructor feature)
@REM     -callirq.s (IRQ management not needed, uses .constructor)
@REM     -callmain.s (main() argument generation not needed for NES)
@REM     -stkchk.s (not needed, uses .constructor)
@REM   libsrc\common\copydata.s (used by crt0.s for copying DATA segment to RAM)
@REM
md temp\runtime
del temp\runtime\*.o
del temp\runtime.lib
for %%X in (libsrc\runtime\*.s) do cc65\bin\ca65.exe %%X -g -o temp\runtime\%%~nX.o
cc65\bin\ca65.exe libsrc\common\copydata.s -g -o temp\runtime\copydata.o
del temp\runtime\condes.o
del temp\runtime\callirq.o
del temp\runtime\callmain.o
del temp\runtime\stkchk.o
for %%X in (temp\runtime\*.o) do cc65\bin\ar65.exe a temp\runtime.lib %%X
pause