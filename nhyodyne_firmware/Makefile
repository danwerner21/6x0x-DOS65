
all:  	zloader.bin dos65.com dos65drv.sys dos65drv.rom

zloader.bin: zloader.asm
	z80asm -b -l -m zloader.asm  2> zloader.err
	cat zloader_addr0100.bin zloader_addr8000.bin > zloader.bin
	@rm zloader_addr0100.bin
	@rm zloader_addr8000.bin
	@rm zloader.o


com.s19: com.asm dos65drv.rom  ../supermon/supermon.asm dospager.asm ../dos65_os/dosdefn.asm
	ca65 -D USEROM=0 com.asm -l com.lst
	ld65 com.o -m com.map -C mbc.cfg -o com.out
	srec_cat  com.out -binary -exclude 0x0000 0xF200 -o com.s19  -data-only

dos65drv.out: dos65drv.asm dosser.asm ../dos65_os/dosdefn.asm doside.asm dosmd.asm dosdskyn.asm drvmacro.asm dosflp.asm
	ca65 -D USEROM=0 dos65drv.asm -l dos65drv.lst
	ld65 dos65drv.o -m dos65drv.map -C dos65.cfg -o dos65drv.out

dos65drvr.out: dos65drv.asm dosser.asm ../dos65_os/dosdefn.asm doside.asm dosmd.asm dosdskyn.asm drvmacro.asm dosflp.asm
	ca65 -D USEROM=1 dos65drv.asm -l dos65drvr.lst -o dos65drvr.o
	ld65 dos65drvr.o -m dos65drvr.map -C dos65.cfg -o dos65drvr.out

dos65.com: ../dos65_os/dos65_d000.out zloader.bin com.s19
	srec_cat  ../dos65_os/dos65_d000.out -binary -offset -0x7F00  -o  dos65.s -data-only
	srec_cat  dos65.s -crop 0x5000 0x71FF -o  dos65.s1 -data-only
	srec_cat  com.s19 -offset -0x8000 -o  com.s -data-only
	cat 	dos65.s1 com.s > dos651.s
	srec_cat  dos651.s -offset -0x5000 -o dos65.bin  -BINARY
	cat 	zloader.bin dos65.bin> dos65.com
	@rm -f dos651.s
	@rm -f dos651.s1
	@rm -f com.s
	@rm -f dos65.bin
	cp dos65.com ../bin/

dos65drv.sys: dos65drv.out
	srec_cat  dos65drv.out -binary -exclude 0x0000 0x8800 -o  dos65drv.s19  -data-only
	srec_cat  dos65drv.s19  -offset -0x8800  -o  dos65drvA.s  -data-only
	srec_cat  dos65drvA.s -o dos65drv.sys  -BINARY
	@rm -f dos65drvA.s
	cp dos65drv.sys ../bin/

dos65drv.rom: dos65drvr.out
	srec_cat  dos65drvr.out -binary -exclude 0x0000 0x8800 -o  dos65drvr.s19  -data-only
	srec_cat  dos65drvr.s19   -offset 0x060000 -o dos65drv.rom   -data-only

rom.bin: rom.asm dos65drv.rom  ../supermon/supermon.asm dospager.asm ../dos65_os/dosdefn.asm
	ca65 -D USEROM=1 rom.asm -l rom.lst
	ld65 rom.o -m rom.map -C mbc.cfg -o rom.out
	srec_cat  rom.out -binary -offset -0x8000 -o  rom.s  -data-only
	srec_cat  rom.s  -crop 0x0000 0x8000  -o  rom.s1  -data-only
	@cat rom.s1 dos65drv.rom > romB.s
	srec_cat  romB.s -o rom.bin -binary
	@rm rom.s
	@rm rom.s1
	@rm rom.o
	@rm romB.s
	cp rom.bin ../bin/mbcrom.bin


pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/rom.asm rom.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/com.asm com.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dos65drv.asm dos65drv.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dosser.asm dosser.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dosmd.asm dosmd.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dosdskyn.asm dosdskyn.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dosflp.asm dosflp.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/doside.asm doside.asm

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
	@rm -f *.rom

cleandust:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.map
	@rm -f *.lst
