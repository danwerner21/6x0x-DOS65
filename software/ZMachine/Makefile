
all:  	zip816.com zip.com


zip816.out: zip.asm bugger.asm cold.asm disk.asm dispatch.asm eq.asm hardeq.asm \
         io.asm machine.asm main.asm objects.asm ops0.asm ops1.asm ops2.asm \
	 opsx.asm paging.asm read.asm subs.asm warm.asm zdos.asm zstring.asm
	ca65 --cpu 65816 -D DUODYNE zip.asm -l zip816.lst -o zip816.o
	ld65 zip816.o  -m zip816.map -C dos65.cfg -o zip816.out

zip.out: zip.asm bugger.asm cold.asm disk.asm dispatch.asm eq.asm hardeq.asm \
         io.asm machine.asm main.asm objects.asm ops0.asm ops1.asm ops2.asm \
	 opsx.asm paging.asm read.asm subs.asm warm.asm zdos.asm zstring.asm
	ca65 --cpu 6502 zip.asm  -l zip.lst -o zip.o
	ld65 zip.o  -m zip.map -C dos65.cfg -o zip.out

zip.com: zip.out
	srec_cat  zip.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  zip.s19  -data-only
	srec_cat  zip.s19  -offset -0x0800  -o  zipA.s  -data-only
	srec_cat  zipA.s -o zip.com  -BINARY
	@rm -f zipA.s
	sed -i -e '$$aS9' zip.s19
	cp zip.com ../../bin/common/
	cp zip.s19 ../../bin/common/
	cp ziptest.z3 ../../bin/common/
	cp zork1.z1 ../../bin/common/
	cp zork1.z3 ../../bin/common/
	cp planet.z3 ../../bin/common/

zip816.com: zip816.out
	srec_cat  zip816.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  zip816.s19  -data-only
	srec_cat  zip816.s19  -offset -0x0800  -o  zipA.s  -data-only
	srec_cat  zipA.s -o zip816.com  -BINARY
	@rm -f zipA.s
	sed -i -e '$$aS9' zip816.s19
	cp zip816.com ../../bin/common/
	cp zip816.s19 ../../bin/common/
	@cpmrm -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img zip.co6
	cpmcp -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img  zip816.com 0:zip.co6
	@cpmrm -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img ziptest.z3
	cpmcp -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img  ziptest.z3 0:ziptest.z3
	@cpmrm -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img zork1.z1
	cpmcp -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img  zork1.z1 0:zork1.z1
	@cpmrm -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img zork1.z3
	cpmcp -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img  zork1.z3 0:zork1.z3
	@cpmrm -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img planet.z3
	cpmcp -f wbw_hd512 ../../bin/duodyne/hd512_duodyne.img  planet.z3 0:planet.z3

pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/zip.asm zip.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bugger.asm bugger.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/cold.asm cold.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/disk.asm disk.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dispatch.asm dispatch.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/hardeq.asm hardeq.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/io.asm io.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/machine.asm machine.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/main.asm main.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/objects.asm objects.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/ops0.asm ops0.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/ops1.asm ops1.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/ops2.asm ops2.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/opsx.asm opsx.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/paging.asm paging.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/read.asm read.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/subs.asm subs.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/warm.asm warm.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/zdos.asm zdos.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/zstring.asm zstring.asm

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
