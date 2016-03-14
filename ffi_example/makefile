all: exampledylib

exampledylib: expression.o example.o
	gcc -dynamiclib -current_version 1.0 expression.o example.o -o example.dylib

example.o: example.c
	gcc -c -fpic example.c

expression.o: ../cbits/Expression.c
	gcc -c -fpic ../cbits/Expression.c

clean:
	rm *o example.dylib