all:  	rom.hex


6x0xbios.out: 6x0xbios.asm bios_ppp_common.asm bios_serial.asm bios_ppp_console.asm ../supermon/supermon.asm bios_ppp_hd.asm \
              bios_diov3_ide.asm bios_diov3_flp.asm bios_rtc.asm bios_dskyng.asm \
			  bios_pager.asm bios_options.asm bios_dispatch.asm bios_console.asm

	ca65 6x0xbios.asm -l 6x0xbios.lst
	ld65 6x0xbios.o -vm -v -m 6x0xbios.map -C dos65.cfg -o 6x0xbios.out

rom.hex: 6x0xbios.out
	srec_cat  6x0xbios.out -binary -exclude 0x0000 0xBF00 -offset 0x0100 -o rom.hex  -intel -address-length=2
	cp rom.hex ../bin/6x0x/


pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/6x0xbios.asm 6x0xbios.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/supermon.asm supermon.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_serial.asm bios_serial.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_ppp_common.asm bios_ppp_common.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_ppp_console.asm bios_ppp_console.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_ppp_hd.asm bios_ppp_hd.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_diov3_ide.asm bios_diov3_ide.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_diov3_flp.asm bios_diov3_flp.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_rtc.asm bios_rtc.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_dskyng.asm bios_dskyng.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_options.asm bios_options.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_pager.asm bios_pager.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_dispatch.asm bios_dispatch.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_console.asm bios_console.asm


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