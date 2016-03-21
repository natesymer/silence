all: exampledylib postgres-sl

postgres-sl: expression.o pg_example.o
	gcc -L$(shell pg_config --libdir) -lpq -dynamiclib -current_version 1.0 expression.o pg_example.o -o postgres-sl.dylib

exampledylib: expression.o example.o
	gcc -dynamiclib -current_version 1.0 expression.o example.o -o example.dylib

pg_example.o: pg_example.c
	gcc -c -I$(shell pg_config --includedir) -fpic pg_example.c

example.o: example.c
	gcc -c -fpic example.c

expression.o: ../cbits/Expression.c
	gcc -c -fpic ../cbits/Expression.c

clean:
	rm *o example.dylib