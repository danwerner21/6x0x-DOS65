
all:  	assign.com alloc.com asm.com \
	bcompile.com bcomp816.com compare.com copy.com debug.com edit.com run.com sedit.com cpm.co6 \
	dbasic.com dbasic816.com s19.com writeos.com rtc.com format.com nvset.com xmr.com xms.com clrdir.s19


assign.com: assign.c
	cc65  -t none -O --cpu 6502  assign.c
	ca65 --cpu 6502  assign.s
	ld65 assign.o  ./rbc/rbc.lib -m assign.map -C dos65c.cfg -o assign.com
	ld65 assign.o  ./rbc/rbcduo02.lib -m assignduo02.map -C dos65c.cfg -o assignduo02.com
	srec_cat  assign.com -binary -offset 0x0800 -exclude 0x0000 0x0800 -o  assign.s19  -data-only
	cp assign.com ../bin/common/
	cp assignduo02.com ../bin/duodyne02/assign.co6
	sed -i -e '$$aS9' assign.s19
	cp assign.s19 ../bin/common/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img 0:assign.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  assign.com 0:assign.co6
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img 0:assign.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  assignduo02.com 0:assign.co6


alloc.out: alloc203.asm
	ca65 --cpu 6502 alloc203.asm -l alloc.lst
	ld65 alloc203.o  -m alloc.map -C dos65.cfg -o alloc.out

bcompile.out: compl203.asm
	ca65 --cpu 6502 compl203.asm -l bcompile.lst
	ld65 compl203.o  -m bcompile.map -C dos65.cfg -o bcompile.out

bcomp816.out: compl203.asm
	ca65 --cpu 65816 -D DUODYNE compl203.asm -l bcomp816.lst -o bcomp816.o
	ld65 bcomp816.o  -m bcomp816.map -C dos65.cfg -o bcomp816.out

asm.out: asm211.asm
	ca65 --cpu 6502 asm211.asm -l asm.lst
	ld65 asm211.o  -m asm.map -C dos65.cfg -o asm.out

clrdir.out: clrdir.asm
	ca65 clrdir.asm -l clrdir.lst
	ld65 clrdir.o -vm -v -m clrdir.map -C dos65.cfg -o clrdir.out

compare.out: compr202.asm
	ca65 --cpu 6502 compr202.asm -l compare.lst
	ld65 compr202.o  -m compare.map -C dos65.cfg -o compare.out

copy.out: copy201.asm
	ca65 --cpu 6502 copy201.asm -l copy.lst
	ld65 copy201.o  -m copy.map -C dos65.cfg -o copy.out

cpm.out: cpm.asm
	ca65 --cpu 6502 cpm.asm -l cpm.lst
	ld65 cpm.o  -m cpm.map -C dos65.cfg -o cpm.out

dbasic.out: dbasic.asm
	ca65 --cpu 6502 dbasic.asm -l dbasic.lst
	ld65 dbasic.o  -m dbasic.map -C dos65.cfg -o dbasic.out

dbasic816.out: dbasic.asm
	ca65 --cpu 65816 -D DUODYNE dbasic.asm -l dbasic816.lst -o dbasic816.o
	ld65 dbasic816.o  -m dbasic816.map -C dos65.cfg -o dbasic816.out

debug.out: debug202.asm
	ca65 --cpu 6502 debug202.asm -l debug.lst
	ld65 debug202.o  -m debug.map -C dos65.cfg -o debug.out

edit.out: edit203.asm
	ca65 --cpu 6502 edit203.asm -l edit.lst
	ld65 edit203.o  -m edit.map -C dos65.cfg -o edit.out

format.out: format.asm
	ca65 --cpu 6502 format.asm -l format.lst
	ld65 format.o  -m format.map -C dos65.cfg -o format.out

nvset.com: nvset.c
	cc65  -t none -O --cpu 6502  nvset.c
	ca65 --cpu 6502  nvset.s
	ld65 nvset.o  ./rbc/rbc.lib -m nvset.map -C dos65c.cfg -o nvset.com
	srec_cat  nvset.com -binary -offset 0x0800 -exclude 0x0000 0x0800 -o  nvset.s19  -data-only
	cp nvset.com ../bin/common/
	sed -i -e '$$aS9' nvset.s19
	cp nvset.s19 ../bin/common/

run.out: run205.asm
	ca65 --cpu 6502 run205.asm -l run.lst
	ld65 run205.o  -m run.map -C dos65.cfg -o run.out

sedit.out: sedit.asm
	ca65 --cpu 6502 sedit.asm -lsedit.lst
	ld65 sedit.o  -m sedit.map -C dos65.cfg -o sedit.out

s19.out: s19.asm
	ca65 --cpu 6502 s19.asm -ls19.lst
	ld65 s19.o  -m s19.map -C dos65.cfg -o s19.out

writeos.out: writeos.asm
	ca65 --cpu 6502 writeos.asm -l writeos.lst
	ld65 writeos.o  -m writeos.map -C dos65.cfg -o writeos.out

rtc.out: rtc.asm
	ca65 --cpu 6502 rtc.asm -l rtc.lst
	ld65 rtc.o  -m rtc.map -C dos65.cfg -o rtc.out

alloc.com: alloc.out
	srec_cat  alloc.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  alloc.s19  -data-only
	srec_cat  alloc.s19  -offset -0x0800  -o  allocA.s  -data-only
	srec_cat  allocA.s -o alloc.com  -BINARY
	@rm -f allocA.s
	sed -i -e '$$aS9' alloc.s19
	cp alloc.com ../bin/common/
	cp alloc.s19 ../bin/common/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img alloc.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  alloc.com 0:alloc.co6
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img alloc.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  alloc.com 0:alloc.co6

bcompile.com: bcompile.out
	srec_cat  bcompile.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  bcompile.s19  -data-only
	srec_cat  bcompile.s19  -offset -0x0800  -o  bcompileA.s  -data-only
	srec_cat  bcompileA.s -o bcompile.com  -BINARY
	@rm -f bcompileA.s
	sed -i -e '$$aS9' bcompile.s19
	cp bcompile.com ../bin/6x0x/
	cp bcompile.s19 ../bin/6x0x/
	cp bcompile.com ../bin/nhyodyne/
	cp bcompile.s19 ../bin/nhyodyne/
	cp bcompile.com ../bin/duodyne02/
	cp bcompile.s19 ../bin/duodyne02/
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img bcompile.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  bcompile.com 0:bcompile.co6

bcomp816.com: bcomp816.out
	srec_cat  bcomp816.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  bcomp816.s19  -data-only
	srec_cat  bcomp816.s19  -offset -0x0800  -o  bcomp816A.s  -data-only
	srec_cat  bcomp816A.s -o bcomp816.com  -BINARY
	@rm -f bcomp816A.s
	sed -i -e '$$aS9' bcomp816.s19
	cp bcomp816.s19 ../bin/duodyne/
	cp bcomp816.com ../bin/duodyne/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img bcompile.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  bcomp816.com 0:bcompile.co6


asm.com: asm.out
	srec_cat  asm.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  asm.s19  -data-only
	srec_cat  asm.s19  -offset -0x0800  -o  asmA.s  -data-only
	srec_cat  asmA.s -o asm.com  -BINARY
	@rm -f asmA.s
	sed -i -e '$$aS9' asm.s19
	cp asm.com ../bin/common/
	cp asm.s19 ../bin/common/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img asm.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  asm.com 0:asm.co6
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img asm.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  asm.com 0:asm.co6


clrdir.s19: clrdir.out
	srec_cat  clrdir.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  clrdir.s19  -data-only
	sed -i -e '$$aS9' clrdir.s19
	cp clrdir.s19 ../bin/common/

compare.com: compare.out
	srec_cat  compare.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  compare.s19  -data-only
	srec_cat  compare.s19  -offset -0x0800  -o  compareA.s  -data-only
	srec_cat  compareA.s -o compare.com  -BINARY
	@rm -f compareA.s
	sed -i -e '$$aS9' compare.s19
	cp compare.com ../bin/common/
	cp compare.s19 ../bin/common/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img compare.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  compare.com 0:compare.co6
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img compare.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  compare.com 0:compare.co6


copy.com: copy.out
	srec_cat  copy.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  copy.s19  -data-only
	srec_cat  copy.s19  -offset -0x0800  -o  copyA.s  -data-only
	srec_cat  copyA.s -o copy.com  -BINARY
	@rm -f copyA.s
	sed -i -e '$$aS9' copy.s19
	cp copy.com ../bin/common/
	cp copy.s19 ../bin/common/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img copy.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  copy.com 0:copy.co6
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img copy.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  copy.com 0:copy.co6


cpm.co6: cpm.out
	srec_cat  cpm.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  cpm.s19  -data-only
	srec_cat  cpm.s19  -offset -0x0800  -o  cpmA.s  -data-only
	srec_cat  cpmA.s -o cpm.co6  -BINARY
	@rm -f cpmA.s
	sed -i -e '$$aS9' cpm.s19
	cp cpm.co6 ../bin/nhyodyne/
	cp cpm.s19 ../bin/nhyodyne/

debug.com: debug.out
	srec_cat  debug.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  debug.s19  -data-only
	srec_cat  debug.s19  -offset -0x0800  -o  debugA.s  -data-only
	srec_cat  debugA.s -o debug.com  -BINARY
	@rm -f debugA.s
	sed -i -e '$$aS9' debug.s19
	cp debug.com ../bin/6x0x/
	cp debug.s19 ../bin/6x0x/
	cp debug.com ../bin/nhyodyne/
	cp debug.s19 ../bin/nhyodyne/
	cp debug.com ../bin/duodyne02/
	cp debug.s19 ../bin/duodyne02/
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img debug.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  debug.com 0:debug.co6


dbasic.com: dbasic.out
	srec_cat  dbasic.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  dbasic.s19  -data-only
	srec_cat  dbasic.s19  -offset -0x0800  -o  dbasicA.s  -data-only
	srec_cat  dbasicA.s -o dbasic.com  -BINARY
	@rm -f dbasicA.s
	sed -i -e '$$aS9' dbasic.s19
	cp dbasic.com ../bin/6x0x/
	cp dbasic.s19 ../bin/6x0x/
	cp dbasic.com ../bin/nhyodyne/
	cp dbasic.s19 ../bin/nhyodyne/
	cp dbasic.com ../bin/duodyne02/
	cp dbasic.s19 ../bin/duodyne02/
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img dbasic.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  dbasic.com 0:dbasic.co6

dbasic816.com: dbasic816.out
	srec_cat  dbasic816.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  dbasic816.s19  -data-only
	srec_cat  dbasic816.s19  -offset -0x0800  -o  dbasic816A.s  -data-only
	srec_cat  dbasic816A.s -o dbasic816.com  -BINARY
	@rm -f dbasic816A.s
	sed -i -e '$$aS9' dbasic816.s19
	cp dbasic816.com ../bin/duodyne/
	cp dbasic816.s19 ../bin/duodyne/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img dbasic.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  dbasic816.com 0:dbasic.co6


edit.com: edit.out
	srec_cat  edit.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  edit.s19  -data-only
	srec_cat  edit.s19  -offset -0x0800  -o  editA.s  -data-only
	srec_cat  editA.s -o edit.com  -BINARY
	@rm -f editA.s
	sed -i -e '$$aS9' edit.s19
	cp edit.com ../bin/common/
	cp edit.s19 ../bin/common/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img edit.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  edit.com 0:edit.co6
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img edit.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  edit.com 0:edit.co6


format.com: format.out
	srec_cat  format.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  format.s19  -data-only
	srec_cat  format.s19  -offset -0x0800  -o  formatA.s  -data-only
	srec_cat  formatA.s -o format.com  -BINARY
	@rm -f formatA.s
	sed -i -e '$$aS9' format.s19
	cp format.com ../bin/common/
	cp format.s19 ../bin/common/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img format.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  format.com 0:format.co6
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img format.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  format.com 0:format.co6


run.com: run.out
	srec_cat  run.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  run.s19  -data-only
	srec_cat  run.s19  -offset -0x0800  -o  runA.s  -data-only
	srec_cat  runA.s -o run.com  -BINARY
	@rm -f runA.s
	sed -i -e '$$aS9' run.s19
	cp run.com ../bin/common/
	cp run.s19 ../bin/common/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img run.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  run.com 0:run.co6
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img run.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  run.com 0:run.co6


sedit.com: sedit.out
	srec_cat  sedit.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  sedit.s19  -data-only
	srec_cat  sedit.s19  -offset -0x0800  -o  seditA.s  -data-only
	srec_cat  seditA.s -o sedit.com  -BINARY
	@rm -f seditA.s
	sed -i -e '$$aS9' sedit.s19
	cp sedit.com ../bin/common/
	cp sedit.s19 ../bin/common/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img sedit.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  sedit.com 0:sedit.co6
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img sedit.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  sedit.com 0:sedit.co6

s19.com: s19.out
	srec_cat  s19.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  s19.s19  -data-only
	srec_cat  s19.s19  -offset -0x0800  -o  s19A.s  -data-only
	srec_cat  s19A.s -o s19.com  -BINARY
	@rm -f s19A.s
	sed -i -e '$$aS9' s19.s19
	cp s19.com ../bin/common/
	cp s19.s19 ../bin/common/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img s19.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  s19.com 0:s19.co6
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img s19.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  s19.com 0:s19.co6


writeos.com: writeos.out
	srec_cat  writeos.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  writeos.s19  -data-only
	srec_cat  writeos.s19  -offset -0x0800  -o  writeosA.s  -data-only
	srec_cat  writeosA.s -o writeos.com  -BINARY
	@rm -f writeosA.s
	sed -i -e '$$aS9' writeos.s19
	cp writeos.com ../bin/common/
	cp writeos.s19 ../bin/common/

rtc.com: rtc.out
	srec_cat  rtc.out -binary -offset 0x0100 -exclude 0x0000 0x0800 -o  rtc.s19  -data-only
	srec_cat  rtc.s19  -offset -0x0800  -o  rtcA.s  -data-only
	srec_cat  rtcA.s -o rtc.com  -BINARY
	@rm -f rtcA.s
	sed -i -e '$$aS9' rtc.s19
	cp rtc.com ../bin/6x0x/
	cp rtc.s19 ../bin/6x0x/

xmr.com: xmr.c
	cc65  -t none -O --cpu 6502  xmr.c
	ca65 --cpu 6502  xmr.s
	ld65 xmr.o  ./rbc/rbc.lib -m xmr.map -C dos65c.cfg -o xmr.com
	ld65 xmr.o  ./rbc/rbc816.lib -m xmr816.map -C dos65c.cfg -o xmr816.com
	srec_cat  xmr.com -binary -offset 0x0800 -exclude 0x0000 0x0800 -o  xmr.s19  -data-only
	cp xmr.com ../bin/common/
	cp xmr816.com ../bin/common/
	sed -i -e '$$aS9' xmr.s19
	cp xmr.s19 ../bin/common/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img xmr.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  xmr816.com 0:xmr.co6
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img xmr.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  xmr.com 0:xmr.co6


xms.com: xms.c
	cc65  -t none -O --cpu 6502  xms.c
	ca65 --cpu 6502  xms.s
	ld65 xms.o  ./rbc/rbc.lib -m xms.map -C dos65c.cfg -o xms.com
	ld65 xms.o  ./rbc/rbc816.lib -m xms816.map -C dos65c.cfg -o xms816.com
	srec_cat  xms.com -binary -offset 0x0800 -exclude 0x0000 0x0800 -o  xms.s19  -data-only
	cp xms.com ../bin/common/
	cp xms816.com ../bin/common/
	sed -i -e '$$aS9' xms.s19
	cp xms.s19 ../bin/common/
	@cpmrm -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img xms.co6
	cpmcp -f wbw_hd512 ../bin/duodyne/hd512_duodyne.img  xms816.com 0:xms.co6
	@cpmrm -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img xms.co6
	cpmcp -f wbw_hd512 ../bin/duodyne02/hd512_duodyne.img  xms.com 0:xms.co6


pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -l ./backup/alloc203.asm alloc203.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/compl203.asm compl203.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/asm211.asm asm211.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/compr202.asm compr202.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/copy201.asm copy201.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/dbasic.asm dbasic.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/debug202.asm debug202.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/edit203.asm edit203.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/format.asm format.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/run205.asm run205.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/sedit.asm sedit.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/s19.asm s19.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/writeos.asm writeos.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/rtc.asm rtc.asm
	pretty6502 -p1 -a0 -du -mu -l ./backup/clrdir.asm clrdir.asm

clean:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.com
	@rm -f *.com
	@rm -f *.map
	@rm -f *.lst
	@rm -f *.hex
	@rm -f *.s19
	@rm -f assign.s
	@rm -f nvset.s
	@rm -f xmr.s
	@rm -f xms.s

cleandust:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.map
	@rm -f *.lst
