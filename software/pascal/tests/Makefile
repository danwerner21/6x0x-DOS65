
all: hello.pcd arith.pcd bool.pcd

%.o: %.asm
	ca65 $< -o $@

%.pcd: %.o
	ld65 $< -C pcd.cfg -o $@

clean:
	@rm -f *.o *.pcd *.lst
