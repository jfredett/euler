CC = clang
CFLAGS = -Wall -O2 -Werror

build: setup problem-307 problem-205

setup:
	mkdir -p bin

problem-307: problem-307.o solver-307.o
	$(CC) $(CFLAGS) problem-307/obj/problem-307.o problem-307/obj/solver.o -o problem-307/bin/solution
	ln -sf problem-307/bin/solution bin/solve-307

problem-307.o: 
	$(CC) $(CFLAGS) -c problem-307/src/problem-307.c problem-307/src/problem-307.h 
	mv problem-307.o problem-307/obj/

solver-307.o: problem-307.o
	$(CC) $(CFLAGS) -c problem-307/src/solver.c -o problem-307/obj/solver.o

problem-205: problem-205.o solver-205.o
	$(CC) $(CFLAGS) problem-205/obj/problem-205.o problem-205/obj/solver.o -o problem-205/bin/solution
	ln -sf problem-205/bin/solution bin/solve-205

problem-205.o: 
	$(CC) $(CFLAGS) -c problem-205/src/problem-205.c problem-205/src/problem-205.h 
	mv problem-205.o problem-205/obj/

solver-205.o: problem-205.o
	$(CC) $(CFLAGS) -c problem-205/src/solver.c -o problem-205/obj/solver.o


clean:
	rm -f *.o 
	rm -f solve-* 
	rm -f test-*
	rm -f problem-*/{obj,bin}/*
	rm -rf bin/

test-307: problem-307.o
	$(CC) $(CFLAGS) -c problem-307/tests/tests.c -o problem-307/obj/test.o
	$(CC) $(CFLAGS) problem-307/obj/problem-307.o problem-307/obj/test.o -o problem-307/bin/tests
	ln -sf problem-307/bin/tests bin/test-307

test-205: problem-205.o
	$(CC) $(CFLAGS) -c problem-205/tests/tests.c -o problem-205/obj/test.o
	$(CC) $(CFLAGS) problem-205/obj/problem-205.o problem-205/obj/test.o -o problem-205/bin/tests
	ln -sf problem-205/bin/tests bin/test-205
