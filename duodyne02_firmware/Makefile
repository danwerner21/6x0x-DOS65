
all:  	  rom02.hex duodos65.com emudos.bin

dos65.bin: dos65.asm ../dos65_os/ccm215.asm ../dos65_os/pemrbc.asm ../dos65_os/simrbc.asm dosdefn.asm ../dos65_os/macro.asm dos65drv.asm dosser.asm doside.asm dosrtc.asm
	ca65 -D DOSBEGIN=53248 -D DUODYNE02  dos65.asm -l dos65_d000.lst -o dos65_d000.o
	ld65 dos65_d000.o -m dos65_d000.map -C dos65_d000.cfg
	cat dosldr.out dos65.out dosdvr.out > dos65.bin
	srec_cat  dos65.bin -binary -offset 0x0400  -o  dos65_d000.s19  -data-only -address-length=3
	srec_cat  dos65.bin -binary -offset 0x0300  -o  dos65_d000.s19a  -data-only -address-length=3
	sed -i -e '$$aS8' dos65_d000.s19
	cp dos65_d000.s19 ../bin/duodyne02/


rom02.out: rom.asm
	ca65   rom.asm -l rom02.lst -o rom02.o
	ld65 rom02.o -m rom02.map -C rom02.cfg -o rom02.out

rom02.hex: rom02.out
	srec_cat  rom02.out -binary -offset 0xE000 -o  rom02.s19  -data-only -address-length=3
	srec_cat  rom02.out -binary -offset 0xE000 -o  rom02.hex  -intel -address-length=2
	srec_cat  rom02.s19  -o rom02.bin  -BINARY
	sed -i -e '$$aS8' rom02.s19

duodos65.com: loader.asm dos65.bin
	z80asm -b -l -m loader.asm  2> loader.err
	srec_cat  dos65.bin -binary -offset 0x0300  -o  dos65_d000.s19a  -data-only -address-length=3
	srec_cat  loader_addr0100.bin -binary  -o  dos65_loader.1  -Data_Only
	srec_cat  dos65_loader.1 dos65_d000.s19a -o duodos65.com  -binary
	@rm	dos65_loader.1
	@rm	loader_addr0100.bin
	@cp duodos65.com ../bin/duodyne02
	@rm	dos65_d000.s19a



emudos.bin: rom02.out
	srec_cat  dos65.bin -binary -offset 0x8400  -o  dos65_d000.s19a  -data-only -address-length=3
	srec_cat  rom02.out -binary -offset 0xE000 -o  rom02.s19a  -data-only -address-length=3
	srec_cat  dos65_d000.s19a rom02.s19a  -o emudos.bin  -BINARY
	@rm	dos65_d000.s19a
	@rm	rom02.s19a

pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/rom.asm rom.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/supermon.asm supermon.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dos65.asm dos65.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dos65drv.asm dos65drv.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dosser.asm dosser.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/doside.asm doside.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dosflp.asm dosflp.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dospager.asm dospager.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dosrtc.asm dosrtc.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/loader.asm loader.asm

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
	@rm -f *.s19a
	@rm -f *.sys
	@rm -f *.s

cleandust:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.map
	@rm -f *.lst
