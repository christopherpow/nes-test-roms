@del temp\*.o
@del temp\*.txt
@del temp\*.nes
@echo.
@echo Compiling...
cc65\bin\ca65 zapper.s -D ZAPPER_LIGHT -g -o temp\zapper_light.o
@IF ERRORLEVEL 1 GOTO failure
cc65\bin\ca65 zapper.s -D ZAPPER_TRIGGER -g -o temp\zapper_trigger.o
@IF ERRORLEVEL 1 GOTO failure
cc65\bin\ca65 zapper.s -D ZAPPER_FLIP -g -o temp\zapper_flip.o
@IF ERRORLEVEL 1 GOTO failure
cc65\bin\ca65 zapper_stream.s -g -o temp\zapper_stream.o
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Linking...
cc65\bin\ld65 -o temp\zapper_light.nes -C example.cfg temp\zapper_light.o -m temp\zapper_light.map.txt
@IF ERRORLEVEL 1 GOTO failure
cc65\bin\ld65 -o temp\zapper_flip.nes -C example.cfg temp\zapper_flip.o -m temp\zapper_flip.map.txt
@IF ERRORLEVEL 1 GOTO failure
cc65\bin\ld65 -o temp\zapper_trigger.nes -C example.cfg temp\zapper_trigger.o -m temp\zapper_trigger.map.txt
@IF ERRORLEVEL 1 GOTO failure
cc65\bin\ld65 -o temp\zapper_stream.nes -C example.cfg temp\zapper_stream.o -m temp\zapper_stream.map.txt
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Success!
@pause
@GOTO endbuild
:failure
@echo.
@echo Build error!
@pause
:endbuild
