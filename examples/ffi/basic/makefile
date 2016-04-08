all: basic

basic: expression.o basic.o
	gcc -dynamiclib -current_version 1.0 expression.o basic.o -o basic.dylib

basic.o: basic.c
	gcc -c -fpic basic.c

expression.o: ../../../cbits/Expression.c
	gcc -c -fpic ../../../cbits/Expression.c

clean:
	rm *o *.dylib