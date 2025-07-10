all: item.o ab.o main.o
	gcc item.o ab.o main.o -o binaria -std=c99 -Wall
	rm *.o

item.o:
	gcc -c item.c -o item.o

ab.o:
	gcc -c ab.c -o ab.o

main.o:
	gcc -c main.c -o main.o
	 
clean:
	rm *.o binaria

run:
	./binaria
