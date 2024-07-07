
all:  	 scrm816.hex rom816.hex dos65_d000.s19 duodos65.com emudos.bin

dos65_d000.out: dos65.asm ../dos65_os/ccm215.asm ../dos65_os/pemrbc.asm ../dos65_os/simrbc.asm dosdefn.asm ../dos65_os/macro.asm dos65drv.asm dosser.asm doside.asm dosrtc.asm
	ca65 -D DOSBEGIN=53248 -D DUODYNE  dos65.asm -l dos65_d000.lst -o dos65_d000.o
	ld65 dos65_d000.o -m dos65_d000.map -C dos65_d000.cfg -o dos65_d000.out

rom816.out: rom816.asm
	ca65   rom816.asm -l rom816.lst -o rom816.o
	ld65 rom816.o -m rom816.map -C rom816.cfg -o rom816.out

scrm816.out: scrm816.asm
	ca65   scrm816.asm -l scrm816.lst -o scrm816.o
	ld65 scrm816.o -m scrm816.map -C rom816.cfg -o scrm816.out

testfp.out: testfp.asm
	ca65   testfp.asm -l testfp.lst -o testfp.o
	ld65 testfp.o -m testfp.map -C rom816.cfg -o testfp.out

testbus.out: testbus.asm
	ca65   testbus.asm -l testbus.lst -o testbus.o
	ld65 testbus.o -m testbus.map -C rom816.cfg -o testbus.out

rom816.hex: rom816.out
	srec_cat  rom816.out -binary -offset 0x0200 -exclude 0x0000 0xE000 -o  rom816.s19  -data-only -address-length=3
	srec_cat  rom816.out -binary -offset 0x0200 -exclude 0x0000 0xE000 -o  rom816.hex  -intel -address-length=2
	srec_cat  rom816.s19  -o rom816.bin  -BINARY
	srec_cat  dos65_d000.s19 rom816.s19  -o emudos.bin  -BINARY
	sed -i -e '$$aS8' rom816.s19

scrm816.hex: scrm816.out
	srec_cat  scrm816.out -binary -offset 0x0200 -exclude 0x0000 0xE000 -o  scrm816.s19  -data-only -address-length=3
	srec_cat  scrm816.out -binary -offset 0x0200 -exclude 0x0000 0xE000 -o  scrm816.hex  -intel -address-length=2
	sed -i -e '$$aS8' scrm816.s19

dos65_d000.s19: dos65_d000.out
	@rm -f allocA.s
	srec_cat  dos65_d000.out -binary -offset 0x8800  -o  dos65_d000.s19  -data-only -address-length=3
	srec_cat  dos65_d000.out -binary -offset 0x0800  -o  dos65_d000.s19a  -data-only -address-length=3
	sed -i -e '$$aS8' dos65_d000.s19
	cp dos65_d000.s19 ../bin/duodyne/

duodos65.com: loader.asm dos65_d000.s19a
	z80asm -b -l -m loader.asm  2> loader.err
	srec_cat  loader_addr0100.bin -binary  -o  dos65_loader.1  -Data_Only
	srec_cat  dos65_d000.s19a -offset -0x0100 -o  dos65_loader.2  -Data_Only
	srec_cat  dos65_loader.1 dos65_loader.2 -o duodos65.com  -binary
	@rm	dos65_loader.1
	@rm	dos65_loader.2
	@rm	loader_addr0100.bin
	@cp duodos65.com ../bin/duodyne


emudos.bin: rom816.out dos65_d000.out
	srec_cat  dos65_d000.out -binary -offset 0x8800  -o  dos65_d000.s19  -data-only -address-length=3
	srec_cat  rom816.out -binary -offset 0x0200 -exclude 0x0000 0xE000 -o  rom816.s19  -data-only -address-length=3
	srec_cat  rom816.out -binary -offset 0x0200 -exclude 0x0000 0xE000 -o  rom816.hex  -intel -address-length=2
	srec_cat  rom816.s19  -o rom816.bin  -BINARY
	srec_cat  dos65_d000.s19 rom816.s19  -o emudos.bin  -BINARY
	sed -i -e '$$aS8' rom816.s19


pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/testfp.asm testfp.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/testbus.asm testbus.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/scrm816.asm scrm816.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/rom816.asm rom816.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/supermon816.asm supermon816.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dos65.asm dos65.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dos65drv.asm dos65drv.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dosser.asm dosser.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/doside.asm doside.asm
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
