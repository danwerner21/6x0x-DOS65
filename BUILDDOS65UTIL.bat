CD DOS65UTIL
cls
ECHO BUILDING DRIVE FORMAT UTILITY
cc65 -t none -O  FORMAT.c
cc65 -t none -O  DOS65.c
CA65 DOS65asm.s
CA65 DOS65.s
ca65 FORMAT.s
erase a.out
ld65 -C N8VEM65.cfg -m FORMAT.map FORMAT.O DOS65.o DOS65asm.o N8VEM65.lib
copy A.OUT FORMAT.COM
erase a.out
PAUSE
CLS
ECHO BUILDING SYSGEN UTILITY
cc65 -t none -O  SYSGEN.c
cc65 -t none -O  DOS65.c
CA65 DOS65asm.s
CA65 DOS65.s
ca65 SYSGEN.s
erase a.out
ld65 -C N8VEM65.cfg -m SYSGEN.map SYSGEN.O DOS65.o DOS65asm.o N8VEM65.lib
copy A.OUT SYSGEN.COM
erase a.out
PAUSE
