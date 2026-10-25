@del temp\mset.c.pre.txt

cc65\bin\cc65 -o temp\mset.c.pre.txt -E -O -T -g mset.c
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