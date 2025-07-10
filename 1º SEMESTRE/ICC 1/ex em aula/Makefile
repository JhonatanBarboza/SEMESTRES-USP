all: item.o listaencadeada.o main.o
	gcc item.o listaencadeada.o main.o -o listaencadeada -std=c99 -Wall
	
item.o:
	gcc -c ../TAD\ Item/item.c -o item.o

listaencadeada.o:
	gcc -c listaencadeada.c -I ../TAD\ Item/ -o listaencadeada.o
	 
main.o:
	gcc -c main_encadeada.c -I ../TAD\ Item/ -o main.o
	 
clean:
	rm *.o listaencadeada
