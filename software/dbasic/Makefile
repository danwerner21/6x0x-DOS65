
all:  	dbasic.com dbasic816.com dbasicmap.com

dbasic.out: dbasic.asm zeropage.asm tokens.asm definitions.asm messages.asm numconst.asm iovect.asm diskcmds.asm screencmds.asm ay38910.asm rtc.asm
	ca65 --cpu 6502 dbasic.asm -l dbasic.lst
	ld65 dbasic.o  -m dbasic.map -C dos65.cfg -o dbasic.out

dbasicmap.out: dbasic.asm zeropage.asm tokens.asm definitions.asm messages.asm numconst.asm iovect.asm diskcmds.asm screencmds.asm ay38910.asm rtc.asm
	ca65 --cpu 6502 -D MEMORYMAPPEDSCREEN dbasic.asm -l dbasicmap.lst -o dbasicmap.o
	ld65 dbasicmap.o  -m dbasicmap.map -C dos65.cfg -o dbasicmap.out

dbasic816.out: dbasic.asm macro816.asm zeropage.asm tokens.asm definitions.asm messages.asm numconst.asm iovect.asm diskcmds.asm screencmds.asm ay38910.asm rtc.asm
	ca65 --cpu 65816 -D DUODYNE dbasic.asm -l dbasic816.lst -o dbasic816.o
	ld65 dbasic816.o  -m dbasic816.map -C dos65.cfg -o dbasic816.out


dbasicmap.com: dbasicmap.out
	srec_cat  dbasicmap.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  dbasicmap.s19  -data-only
	srec_cat  dbasicmap.s19  -offset -0x0800  -o  dbasicA.s  -data-only
	srec_cat  dbasicA.s -o dbasicmap.com  -BINARY
	@rm -f dbasicA.s
	sed -i -e '$$aS9' dbasicmap.s19
	cp dbasicmap.com ../../bin/6502PC/
	cp dbasicmap.s19 ../../bin/6502PC/
	@cpmrm -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG 0:dbasicmp.com
	cpmcp -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG  dbasicmap.com 0:dbasicmp.com


dbasic.com: dbasic.out
	srec_cat  dbasic.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  dbasic.s19  -data-only
	srec_cat  dbasic.s19  -offset -0x0800  -o  dbasicA.s  -data-only
	srec_cat  dbasicA.s -o dbasic.com  -BINARY
	@rm -f dbasicA.s
	sed -i -e '$$aS9' dbasic.s19
	cp dbasic.com ../../bin/6x0x/
	cp dbasic.s19 ../../bin/6x0x/
	cp dbasic.com ../../bin/nhyodyne/
	cp dbasic.s19 ../../bin/nhyodyne/
	cp dbasic.com ../../bin/duodyne02/
	cp dbasic.s19 ../../bin/duodyne02/
	@cpmrm -f wbw_hd512 ../../bin/duodyne02/hd512_duodyne.img dbasic.co6
	cpmcp -f wbw_hd512 ../../bin/duodyne02/hd512_duodyne.img  dbasic.com 0:dbasic.co6
	@cpmrm -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG 0:dbasic.com
	cpmcp -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG  dbasic.com 0:dbasic.com

dbasic816.com: dbasic816.out
	srec_cat  dbasic816.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  dbasic816.s19  -data-only
	srec_cat  dbasic816.s19  -offset -0x0800  -o  dbasic816A.s  -data-only
	srec_cat  dbasic816A.s -o dbasic816.com  -BINARY
	@rm -f dbasic816A.s
	sed -i -e '$$aS9' dbasic816.s19
	cp dbasic816.com ../../bin/duodyne/
	cp dbasic816.s19 ../../bin/duodyne/
	@cpmrm -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img dbasic.co6
	cpmcp -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img  dbasic816.com 0:dbasic.co6



pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/dbasic.asm dbasic.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/zeropage.asm zeropage.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/tokens.asm tokens.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/definitions.asm definitions.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/messages.asm messages.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/numconst.asm numconst.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/iovect.asm iovect.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/diskcmds.asm diskcmds.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/screencmds.asm screencmds.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/rtc.asm rtc.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/ay38910.asm ay38910.asm


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
