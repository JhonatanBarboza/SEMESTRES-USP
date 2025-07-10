# Compila todos os arquivos e gera o executável 'arvore'
all: todos

# Alvo 'todos' que primeiro limpa os arquivos antigos, depois compila e executa
todos: clean arvore run

# Compila item.o a partir de item.c e item.h
item.o: item.c item.h
	gcc -c item.c -o item.o -std=c99 -Wall

# Compila ab.o a partir de ab.c e ab.h
ab.o: ab.c ab.h item.h
	gcc -c ab.c -o ab.o -std=c99 -Wall

# Compila main.o a partir de main.c
main.o: main.c ab.h item.h
	gcc -c main.c -o main.o -std=c99 -Wall

# Gera o executável 'arvore' a partir dos arquivos objetos
arvore: item.o ab.o main.o
	gcc item.o ab.o main.o -o arvore -std=c99 -Wall -lm

# Regra para executar o programa 'arvore'
run: arvore
	./arvore

# Remove todos os arquivos objetos e o executável gerado
clean:
	rm -f *.o arvore
