# Compila todos os arquivos, gera o executável 'cliente', e roda o programa
all: cliente run

# Gera o executável 'cliente' a partir dos arquivos objetos
cliente: ex1.o
	gcc ex1.o -o cliente -std=c99 -Wall

# Compila o arquivo 'ex1.c' em um arquivo objeto 'ex1.o'
ex1.o: ex1.c
	gcc -c ex1.c -o ex1.o -std=c99 -Wall

# Regra para executar o programa 'cliente'
run: cliente
	./cliente

# Remove todos os arquivos objetos e o executável gerado
clean:
	rm -f *.o cliente
