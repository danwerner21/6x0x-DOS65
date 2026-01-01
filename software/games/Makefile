
all:  	microchess.com



microchess.out: microchess.asm
	ca65 --cpu 6502 microchess.asm -l microchess.lst
	ld65 microchess.o  -m alloc.map -C dos65.cfg -o microchess.out


microchess.com: microchess.out
	srec_cat  microchess.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  microchess.s19  -data-only
	srec_cat  microchess.s19  -offset -0x0800  -o  microchessA.s  -data-only
	srec_cat  microchessA.s -o microchess.com  -BINARY
	@rm -f microchessA.s
	sed -i -e '$$aS9' microchess.s19
	cp microchess.com ../../bin/common/
	cp microchess.s19 ../../bin/common/
	@cpmrm -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img microchess.co6
	cpmcp -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img  microchess.com 0:microchess.co6
	@cpmrm -f wbw_hd512 ../../bin/duodyne02/hd512_duodyne.img microchess.co6
	cpmcp -f wbw_hd512 ../../bin/duodyne02/hd512_duodyne.img  microchess.com 0:microchess.co6
	@cpmrm -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG 0:microchess.com
	cpmcp -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG  microchess.com 0:microchess.com



pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/microchess.asm microchess.asm

clean:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.com
	@rm -f *.com
	@rm -f *.map
	@rm -f *.lst
	@rm -f *.hex
	@rm -f *.s19
	@rm -f assign.s
	@rm -f nvset.s
	@rm -f xmr.s
	@rm -f xms.s

cleandust:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.map
	@rm -f *.lst
