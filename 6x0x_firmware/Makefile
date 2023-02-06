all:  	rom.hex sd_boot.s19 sd_write_os.s19


6x0xbios.out: 6x0xbios.asm bios_ppp_common.asm bios_serial.asm bios_ppp_console.asm miniassembler.asm bios_ppp_hd.asm bios_diov3_ide.asm bios_diov3_flp.asm bios_clrdir.asm
	ca65 6x0xbios.asm -l 6x0xbios.lst
	ld65 6x0xbios.o -vm -v -m 6x0xbios.map -C dos65.cfg -o 6x0xbios.out

rom.hex: 6x0xbios.out
	srec_cat  6x0xbios.out -binary -exclude 0x0000 0xBF00 -offset 0x0100 -o rom.hex  -intel -address-length=2
	cp rom.hex ../bin/

sd_boot.out: sd_boot.asm
	ca65 sd_boot.asm  -l sd_boot.lst
	ld65 sd_boot.o -vm -v -m sd_boot.map -C dos65.cfg -o sd_boot.out

sd_boot.s19: sd_boot.out
	srec_cat  sd_boot.out -binary -exclude 0x0000 0x0100 -offset 0x0100 -o sd_boot.s19
	cp sd_boot.s19 ../bin/

sd_write_os.out: sd_write_os.asm
	ca65 sd_write_os.asm  -l sd_write_os.lst
	ld65 sd_write_os.o -vm -v -m sd_write_os.map -C dos65.cfg -o sd_write_os.out

sd_write_os.s19: sd_write_os.out
	srec_cat  sd_write_os.out -binary -exclude 0x0000 0x0700 -offset 0x0100 -o sd_write_os.s19
	cp sd_write_os.s19 ../bin/

pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/6x0xbios.asm 6x0xbios.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/miniassembler.asm miniassembler.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_serial.asm bios_serial.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_ppp_common.asm bios_ppp_common.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_ppp_console.asm bios_ppp_console.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_ppp_hd.asm bios_ppp_hd.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_diov3_ide.asm bios_diov3_ide.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_diov3_flp.asm bios_diov3_flp.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/bios_clrdir.asm bios_clrdir.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/sd_boot.asm sd_boot.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/sd_write_os.asm sd_write_os.asm

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