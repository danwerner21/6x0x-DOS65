
all:  	dos65.s19 dos65.hex

dos65.out: dos65.asm ccm215.asm pem215.asm simrbc.asm dosdefn.asm macro.asm
	ca65 dos65.asm -l dos65.lst
	ld65 dos65.o -m dos65.map -C dos65.cfg -o dos65.out

dos65.s19: dos65.out
	srec_cat  dos65.out -binary -offset 0x0100 -exclude 0x0000 0xB800 -o  dos65.s19  -data-only
	sed -i -e '$$aS9' dos65.s19
	cp dos65.s19 ../bin/

dos65.hex: dos65.out
	srec_cat  DOS65.out -binary -offset 0x0100 -exclude 0x0000 0xB800 -o  dos65.s -data-only
	srec_cat  dos65.s  -offset -0x8000  -o  dos651.s  -data-only
	srec_cat  dos651.s -exclude 0x6000 0x7EE0  -o dos65.hex  -intel -address-length=2
	@rm -f dos65.s
	@rm -f dos651.s
	cp dos65.hex ../bin/


pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/dos65.asm dos65.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/ccm215.asm ccm215.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/pem215.asm pem215.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/simrbc.asm simrbc.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dosdefn.asm dosdefn.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/macro.asm macro.asm


clean:
	@rm -f *.o
	@rm -f *.bin
	@rm -f *.lis
	@rm -f *.err
	@rm -f *.out
	@rm -f *.com
	@rm -f *.co6
	@rm -f *.map
	@rm -f *.lst
	@rm -f *.hex
	@rm -f *.s19
	@rm -f *.sys
	@rm -f *.s

cleandust:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.map
	@rm -f *.lst
