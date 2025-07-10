# Compila todos os arquivos e gera o executável 'cliente'
all: todos

todos: clean cliente run

item.o: item.c item.h
	gcc -c item.c -o item.o -std=c99 -Wall

circulo.o: Pilha.c Pilha.h item.h
	gcc -c Pilha.c -o Pilha.o -std=c99 -Wall

balanceada.o: balanceada.c Pilha.h item.h
	gcc -c balanceada.c -o balanceda.o -std=c99 -Wall

main.o: main.c Pilha.h item.h
	gcc -c main.c -o main.o -std=c99 -Wall

# Gera o executável 'cliente' a partir dos arquivos objetos
cliente: item.o Pilha.o main.o
	gcc item.o Pilha.o main.o -o cliente -std=c99 -Wall -lm

# Regra para executar o programa 'cliente'
run: cliente
	./cliente

# Remove todos os arquivos objetos e o executável gerado
clean:
	rm -f *.o cliente
