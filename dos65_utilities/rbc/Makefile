
all:  	rbclib


rbclib: crt0.s rbc.s
	ca65  crt0.s --cpu 6502
	ca65 rbc.s --cpu 6502
	ar65 a rbc.lib crt0.o rbc.o

clean:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.com
	@rm -f *.co6
	@rm -f *.map
	@rm -f *.lst
	@rm -f *.hex
	@rm -f *.s19

cleandust:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.map
	@rm -f *.lst
