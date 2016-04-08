all: postgres

postgres: expression.o postgres.o
	gcc -L$(shell pg_config --libdir) -lpq -dynamiclib -current_version 1.0 expression.o postgres.o -o postgres.dylib

postgres.o: postgres.c
	gcc -c -I$(shell pg_config --includedir) -fpic postgres.c

expression.o: ../../../cbits/Expression.c
	gcc -c -fpic ../../../cbits/Expression.c

clean:
	rm *o *.dylib