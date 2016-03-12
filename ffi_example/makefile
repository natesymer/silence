all: exampledylib

exampledylib: example.o
	gcc -dynamiclib -current_version 1.0 example.o -o example.dylib

example.o: example.c
	gcc -c -fpic example.c

clean:
	rm *o example.dylib