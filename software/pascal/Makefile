
all: pascal.com prun.com

# --- Compiler (PASCAL.COM) ---

pascal.out: pascal.asm lexer.asm parser.asm symtab.asm codegen.asm \
            zeropage.asm definitions.asm messages.asm iolib.asm
	ca65 --cpu 6502 pascal.asm -l pascal.lst
	ld65 pascal.o -m pascal.map -C dos65.cfg -o pascal.out

pascal.com: pascal.out
	srec_cat  pascal.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  pascal.s19  -data-only
	srec_cat  pascal.s19  -offset -0x0800  -o  pascalA.s  -data-only
	srec_cat  pascalA.s -o pascal.com  -BINARY
	@rm -f pascalA.s
	sed -i -e '$$aS9' pascal.s19
	cp pascal.com ../../bin/6502PC/
	cp pascal.s19 ../../bin/6502PC/
	cp pascal.com ../../bin/6x0x/
	cp pascal.s19 ../../bin/6x0x/
	@cpmrm -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG 0:pascal.com 2>/dev/null || true
	cpmcp -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG  pascal.com 0:pascal.com

# --- Runtime (PRUN.COM) ---

prun.out: prun.asm zeropage.asm definitions.asm iolib.asm
	ca65 --cpu 6502 prun.asm -l prun.lst
	ld65 prun.o -m prun.map -C dos65.cfg -o prun.out

prun.com: prun.out
	srec_cat  prun.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  prun.s19  -data-only
	srec_cat  prun.s19  -offset -0x0800  -o  prunA.s  -data-only
	srec_cat  prunA.s -o prun.com  -BINARY
	@rm -f prunA.s
	sed -i -e '$$aS9' prun.s19
	cp prun.com ../../bin/6502PC/
	cp prun.s19 ../../bin/6502PC/
	cp prun.com ../../bin/6x0x/
	cp prun.s19 ../../bin/6x0x/
	@cpmrm -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG 0:prun.com 2>/dev/null || true
	cpmcp -f wbw_hd512 ../../bin/6502PC/DOS65_6502PC.IMG  prun.com 0:prun.com

pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/pascal.asm pascal.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/prun.asm   prun.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/lexer.asm  lexer.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/parser.asm parser.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/symtab.asm symtab.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/codegen.asm codegen.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/iolib.asm  iolib.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/codegen.asm codegen.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/zeropage.asm zeropage.asm

clean:
	@rm -f *.o *.out *.com *.map *.lst *.s19
