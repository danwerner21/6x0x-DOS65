all:  	rom.hex testch375.s19


6502PCbios.out: 6502PCbios.asm bios_serial.asm ../supermon/supermon.asm \
			  bios_pager.asm
	ca65 6502PCbios.asm -l 6502PCbios.lst
	ld65 6502PCbios.o -vm -v -m 6502PCbios.map -C dos65.cfg -o 6502PCbios.out

bios_ch375.out: bios_ch375.asm
	ca65 bios_ch375.asm -l bios_ch375.lst
	ld65 bios_ch375.o -vm -v -m bios_ch375.map -C dos65.cfg -o bios_ch375.out


rom.hex: 6502PCbios.out
	srec_cat  6502PCbios.out -binary -exclude 0x0000 0xEF00 -offset -0x8F00 -o rom.hex  -intel -address-length=2
	cp rom.hex ../bin/6502PC/

testch375.s19: bios_ch375.out
	srec_cat  bios_ch375.out  -binary -exclude 0x0000 0x700 -offset 0x900 -o testch375.s19 --Data-Only

pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/6502PCbios.asm 6502PCbios.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_serial.asm bios_serial.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_pager.asm bios_pager.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_ch375.out bios_ch375.out


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