all:	../test_cpu_flag_concurrency.nes

#####
../test_cpu_flag_concurrency.nes: test_cpu_flag_concurrency.o
	ld65  -o $@ $^ -v -C nes.cfg

test_cpu_flag_concurrency.o: test_cpu_flag_concurrency.s
	ca65 --smart -o $@ $^ -t nes -v -I common -DCUSTOM_IRQ -DCUSTOM_NMI

#####
clean:
	rm -f test_cpu_flag_concurrency.o ../test_cpu_flag_concurrency.nes
