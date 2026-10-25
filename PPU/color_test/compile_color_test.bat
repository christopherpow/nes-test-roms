del temp\color_test.o
del temp\color_test.nes
cc65\bin\ca65 color_test.s -g -o temp\color_test.o
cc65\bin\ld65 -C nrom.cfg -o temp\color_test.nes temp\color_test.o -m temp\color_test_map.txt -Ln temp\color_test_labels.txt
@pause