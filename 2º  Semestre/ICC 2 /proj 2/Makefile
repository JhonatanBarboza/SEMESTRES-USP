# Compila todos os arquivos e gera o executável 'palavra'
all: todos

# Alvo 'todos' que primeiro limpa os arquivos antigos, depois compila e executa
todos: clean palavra run

# Compila main.o a partir de main.c
palavra.o: palavra.c 
	gcc -c palavra.c -o palavra.o -std=c99 -Wall

# Gera o executável 'palavra' a partir dos arquivos objetos
palavra: palavra.o
	gcc palavra.o -o palavra -std=c99 -Wall -lm

# Regra para executar o programa 'palavra'
run: palavra
	./palavra

# Remove todos os arquivos objetos e o executável gerado
clean:
	rm -f *.o palavra
