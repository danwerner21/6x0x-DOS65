.PHONY: all validate clean cleandust pretty

PYTHON ?= python3

all: wyrmhold.com

validate:
	$(PYTHON) tools/validate.py

wyrmhold.out: wyrmhold.asm macro.asm defines.asm sound.asm video.asm world.asm rng.asm ui.asm entity.asm combat.asm player.asm town.asm castle.asm shrine.asm tiles.asm metatiles.asm title.asm
	ca65 wyrmhold.asm -l wyrmhold.lst
	ld65 wyrmhold.o -vm -v -m wyrmhold.map -C dos65.cfg -o wyrmhold.out


wyrmhold.com: wyrmhold.out
	srec_cat  wyrmhold.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  wyrmhold.s19  -data-only
	srec_cat  wyrmhold.s19  -offset -0x0800  -o  wyrmholdA.s  -data-only
	srec_cat  wyrmholdA.s -o wyrmhold.com  -BINARY
	@rm -f wyrmholdA.s
	sed -i -e '$$aS9' wyrmhold.s19
	@cpmrm -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG 0:WYRMHOLD.COM
	cpmcp -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG  wyrmhold.com 0:WYRMHOLD.COM


clean:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.com
	@rm -f *.COM
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


pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/wyrmhold.asm wyrmhold.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/macro.asm macro.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/defines.asm defines.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/sound.asm sound.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/video.asm video.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/world.asm world.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/rng.asm rng.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/ui.asm ui.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/entity.asm entity.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/combat.asm combat.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/player.asm player.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/town.asm town.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/castle.asm castle.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/shrine.asm shrine.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/tiles.asm tiles.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/metatiles.asm metatiles.asm
