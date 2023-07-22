
all:  	dos65_b800.out dos65_d000.out dos65_b800.s19 dos65_d000.s19

dos65_b800.out: dos65.asm ccm215.asm pemrbc.asm simrbc.asm dosdefn.asm macro.asm
	ca65 -D DOSBEGIN=47104  -D RBC6X0X  dos65.asm -l dos65_b800.lst -o dos65_b800.o
	ld65 dos65_b800.o -m dos65_b800.map -C dos65_b800.cfg -o dos65_b800.out

dos65_d000.out: dos65.asm ccm215.asm pemrbc.asm simrbc.asm dosdefn.asm macro.asm
	ca65 -D DOSBEGIN=53248 -D NHYODYNE  dos65.asm -l dos65_d000.lst -o dos65_d000.o
	ld65 dos65_d000.o -m dos65_d000.map -C dos65_d000.cfg -o dos65_d000.out

dos65_b800.s19: dos65_b800.out
	srec_cat  dos65_b800.out -binary -offset 0x0100 -exclude 0x0000 0xB800 -o  dos65_b800.s19  -data-only
	sed -i -e '$$aS9' dos65_b800.s19
	cp dos65_b800.s19 ../bin/6x0x/

dos65_d000.s19: dos65_d000.out
	srec_cat  dos65_d000.out -binary -offset 0x0100 -exclude 0x0000 0xD000 -o  dos65_d000.s19  -data-only
	sed -i -e '$$aS9' dos65_d000.s19
	cp dos65_d000.s19 ../bin/nhyodyne/

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
