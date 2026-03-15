all: spsc.com

spsc.out: spsc.asm macro.asm io.asm screen.asm defines.asm print.asm
	ca65 spsc.asm -l spsc.lst
	ld65 spsc.o -vm -v -m spsc.map -C dos65.cfg -o spsc.out


spsc.com: spsc.out
	srec_cat  spsc.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  spsc.s19  -data-only
	srec_cat  spsc.s19  -offset -0x0800  -o  spscA.s  -data-only
	srec_cat  spscA.s -o spsc.com  -BINARY
	@rm -f spscA.s
	sed -i -e '$$aS9' spsc.s19
	@cpmrm -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG 0:spsc.com
	cpmcp -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG  spsc.com 0:spsc.com




pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/spsc.asm spsc.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/io.asm io.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/screen.asm screen.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/print.asm print.asm


clean:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.com
	@rm -f *.co6
	@rm -f *.map
	@rm -f *.lst
	@rm -f *.hex
	@rm -f *.s19
	@rm -f *.s

cleandust:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.map
	@rm -f *.lst
