all: pcv2.o stack.o
	gcc pcv2.o stack.o -o main -Wall -std=c99

pcv2.o:
	gcc -c pcv2.c -o pcv2.o -Wall -std=c99

stack.o:
	gcc -c stack.c -o stack.o -Wall -std=c99

run: main
	./main

clean:
	rm -f *.o main
