# Compila todos os arquivos e gera o executável 'cliente'
all: todos

# Alvo 'todos' que primeiro limpa os arquivos antigos, depois compila e executa
todos: clean cliente run

# Compila item.o a partir de item.c e item.h
item.o: item.c item.h
	gcc -c item.c -o item.o -std=c99 -Wall

# Compila lista.o a partir de lista.c e lista.h
lista.o: lista.c lista.h item.h
	gcc -c lista.c -o lista.o -std=c99 -Wall

# Compila inserir.o a partir de inserir.c
inserir.o: inserir.c lista.h item.h
	gcc -c inserir.c -o inserir.o -std=c99 -Wall

# Gera o executável 'cliente' a partir dos arquivos objetos
cliente: item.o lista.o inserir.o
	gcc item.o lista.o inserir.o -o cliente -std=c99 -Wall -lm

# Regra para executar o programa 'cliente'
run: cliente
	./cliente

# Remove todos os arquivos objetos e o executável gerado
clean:
	rm -f *.o cliente
