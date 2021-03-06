SHELL=/bin/bash

# a special make target indicating all the other targets whose recipe do not 
# create files and should be executed every time they are invoked
.PHONY: clean

# Use racket to initiate the main method of the compile-driver passing the
# source file name ($<) as the filename argument to the main method and redirecting
# the target the output to the target file name ($@)
%.s: %.rkt compiler.rkt
	racket -t compile-driver.rkt  -m $< > $@

# Use nasm to create a linkable object format of the source
%.o: %.s
	nasm -f elf64 -o $@ $<

# Use gcc to link the program's .o file with the runtime system. -lgmp is for 
# the gmp library
%.run: %.o runtime.o gc.o mem.o
	gcc mem.o gc.o runtime.o $< -o $@ -lgmp

# Use gcc to creat a linkable object format of the runtime system
runtime.o: runtime.c runtime.h mem.h
	gcc -c -g runtime.c -o runtime.o

# Use gcc to create a linkable object format of the garbage collector
gc.o: gc.c mem.h runtime.h
	gcc -c -g gc.c -o gc.o

# Use gcc to create a linkable object format of the memory manager
mem.o: mem.c mem.h
	gcc -c -g mem.c -o mem.o
 
# The - instructs make to continue even though a particular command fails. 
clean:
	rm -f *.s
	rm -f *.o
	rm -f *.run
	rm -f *.output
