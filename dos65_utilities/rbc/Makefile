
all:  	rbclib rbc816


rbclib: crt0.s rbc.s
	ca65  crt0.s --cpu 6502
	ca65 rbc.s --cpu 6502
	ar65 a rbc.lib crt0.o rbc.o

rbc816: crt0816.s rbc816.s
	ca65  crt0816.s --cpu 6502
	ca65 rbc816.s --cpu 6502
	ar65 a rbc816.lib crt0816.o rbc816.o



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
