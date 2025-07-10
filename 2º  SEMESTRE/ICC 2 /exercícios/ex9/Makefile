# Compila todos os arquivos, gera o executável 'cliente', e roda o programa
all: cliente run

# Gera o executável 'cliente' a partir dos arquivos objetos
cliente: ex2.o
	gcc ex2.o -o cliente -std=c99 -Wall

# Compila o arquivo 'ex1.c' em um arquivo objeto 'ex1.o'
ex1.o: ex2.c
	gcc -c ex2.c -o ex2.o -std=c99 -Wall

# Regra para executar o programa 'cliente'
run: cliente
	./cliente

# Remove todos os arquivos objetos e o executável gerado
clean:
	rm -f *.o cliente
