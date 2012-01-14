CC = clang
CFLAGS = -Wall -O2 -Werror


problem-307: problem-307.o solver.o
	$(CC) $(CFLAGS) problem-307/obj/problem-307.o problem-307/obj/solver.o -o problem-307/bin/solution
	ln -sf problem-307/bin/solution solve-307

problem-307.o: 
	$(CC) $(CFLAGS) -c problem-307/src/problem-307.c problem-307/src/problem-307.h 
	mv problem-307.o problem-307/obj/

solver.o: problem-307.o
	$(CC) $(CFLAGS) -c problem-307/src/solver.c -o problem-307/obj/solver.o



clean:
	rm -f *.o 
	rm -f solve-* 
	rm -f test-*
	rm -f problem-*/{obj,bin}/*

test: problem-307.o
	$(CC) $(CFLAGS) -c problem-307/tests/tests.c -o problem-307/obj/test.o
	$(CC) $(CFLAGS) problem-307/obj/problem-307.o problem-307/obj/test.o -o problem-307/bin/tests
	ln -sf problem-307/bin/tests run-test-307
