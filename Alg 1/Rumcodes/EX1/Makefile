# Compila todos os arquivos e gera o executável 'cliente'
all: cliente

# Compila o arquivo 'ponto.c' e gera 'ponto.o'
ponto.o: ponto.c ponto.h
	gcc -c ponto.c -o ponto.o -std=c99 -Wall

# Compila o arquivo 'circulo.c' e gera 'circulo.o'
circulo.o: circulo.c circulo.h ponto.h
	gcc -c circulo.c -o circulo.o -std=c99 -Wall

# Compila o arquivo 'main.c' e gera 'main.o'
main.o: main.c ponto.h circulo.h
	gcc -c main.c -o main.o -std=c99 -Wall

# Gera o executável 'cliente' a partir dos arquivos objetos
cliente: ponto.o circulo.o main.o
	gcc ponto.o circulo.o main.o -o cliente -std=c99 -Wall

# Regra para executar o programa 'cliente'
run: cliente
	./cliente

# Remove todos os arquivos objetos e o executável gerado
clean:
	rm -f *.o cliente
