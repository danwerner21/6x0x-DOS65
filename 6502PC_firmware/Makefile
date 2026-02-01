all:  	 testmulti.s19 dos65drv.s19 rom.hex pcdos65.s19

6502PCbios.out: 6502PCbios.asm bios_serial.asm ../supermon/supermon.asm \
			  bios_pager.asm bios_ide.asm bios_video.asm
	ca65 6502PCbios.asm -l 6502PCbios.lst
	ca65 6502PCbios.asm -D ESP -l 6502PCbiosesp.lst -o 6502PCbiosesp.o
	ca65 6502PCbios.asm -D VIDEO -l 6502PCbiosvideo.lst -o 6502PCbiosvideo.o
	ld65 6502PCbios.o -vm -v -m 6502PCbios.map -C dos65.cfg -o 6502PCbios.out
	ld65 6502PCbiosesp.o -vm -v -m 6502PCbiosesp.map -C dos65.cfg -o 6502PCbiosesp.out
	ld65 6502PCbiosvideo.o -vm -v -m 6502PCbiosvideo.map -C dos65.cfg -o 6502PCbiosvideo.out

dos65drv.out: dos65drv.asm bios_serial.asm  bios_rtc.asm \
			  bios_pager.asm bios_ide.asm bios_ch375.asm bios_esp.asm bios_multi.asm bios_video.asm
	ca65 dos65drv.asm -l dos65drv.lst
	ld65 dos65drv.o -vm -v -m dos65drv.map -C dos65.cfg -o dos65drv.out

bios_ch375.out: bios_ch375.asm
	ca65 bios_ch375.asm -l bios_ch375.lst
	ld65 bios_ch375.o -vm -v -m bios_ch375.map -C dos65.cfg -o bios_ch375.out

testmulti.out: testmulti.asm bios_multi.asm
	ca65 testmulti.asm -l testmulti.lst
	ld65 testmulti.o -vm -v -m testmulti.map -C dos65.cfg -o testmulti.out


rom.hex: 6502PCbios.out bios_ch375.asm bios_ide.asm
	srec_cat  6502PCbios.out -binary -exclude 0x0000 0xEF00 -offset -0x8F00 -o rom.h1  -intel -address-length=2 --Data_Only
#	srec_cat  6502PCbiosesp.out -binary -exclude 0x0000 0xEF00 -offset -0x7F00 -o rom.h2  -intel -address-length=2
	srec_cat  6502PCbiosvideo.out -binary -exclude 0x0000 0xEF00 -offset -0x7F00 -o rom.h2  -intel -address-length=2
	cat 	rom.h1 rom.h2 > rom.hex
	@rm	rom.h1
	@rm	rom.h2
	cp rom.hex ../bin/6502PC/

dos65drv.s19: dos65drv.out
	srec_cat  dos65drv.out  -binary -offset 0x100 -exclude 0x0000 0xC000 -o dos65drv.s19 --Data-Only

testmulti.s19: testmulti.out
	srec_cat  testmulti.out  -binary -offset 0x900 -exclude 0x0000 0x1000  -o testmulti.s19 --Data-Only
	sed -i -e '$$aS9' testmulti.s19

pcdos65.s19: loader.asm ../dos65_os/dos65_pc6502.s19 dos65drv.s19
	ca65 loader.asm -l loader.lst
	ld65 loader.o -vm -v -m loader.map -C dos65.cfg -o loader.out
	srec_cat  loader.out -binary -offset 0x100 -exclude 0x0000 0x0800 -o dos65_loader.1  -Data_Only
	srec_cat  ../dos65_os/dos65_pc6502.s19  -offset -0xA800  -o dos65_loader.2 --Data-Only
	srec_cat  dos65drv.s19  -offset -0x8000  -o dos65_loader.3 --Data-Only
	srec_cat  dos65_loader.1 dos65_loader.2 dos65_loader.3 -o pcdos65.s19 --Data-Only
	@rm	dos65_loader.1
	@rm	dos65_loader.2
	@rm	dos65_loader.3
	@rm	loader.out
	sed -i -e '$$aS9' pcdos65.s19
	@cp pcdos65.s19 ../bin/6502PC

pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/6502PCbios.asm 6502PCbios.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_serial.asm bios_serial.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_pager.asm bios_pager.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_ch375.asm bios_ch375.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_ide.asm bios_ide.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_rtc.asm bios_rtc.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_esp.asm bios_esp.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_video.asm bios_video.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_multi.asm bios_multi.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/loader.asm loader.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dos65drv.asm dos65drv.asm


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