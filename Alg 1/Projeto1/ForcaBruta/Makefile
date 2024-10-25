# Compila todos os arquivos e gera o executável 'cliente'
all: cliente

PCV.o: PCV.c PCV.h 
	gcc -c PCV.c -o PCV.o -std=c99 -Wall

fila.o: fila.c fila.h
	gcc -c fila.c -o fila.o -std=c99 -Wall

lista.o: lista.c lista.h
	gcc -c lista.c -o lista.o -std=c99 -Wall

main.o: main.c fila.h PCV.h
	gcc -c main.c -o main.o -std=c99 -Wall

# Gera o executável 'cliente' a partir dos arquivos objetos
cliente: main.o fila.o lista.o PCV.o
	gcc main.o fila.o lista.o PCV.o -o cliente -std=c99 -Wall -lm

# Regra para executar o programa 'cliente'
run: cliente
	./cliente

# Remove todos os arquivos objetos e o executável gerado
clean:
	rm -f *.o cliente
