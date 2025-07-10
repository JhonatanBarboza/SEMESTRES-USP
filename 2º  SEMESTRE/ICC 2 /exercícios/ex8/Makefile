# Compila todos os arquivos, gera o executável 'busca_ternaria', e roda o programa
all: clean busca_ternaria run

# Gera o executável 'busca_ternaria' a partir dos arquivos objetos
busca_ternaria: busca_ternaria.o
	gcc busca_ternaria.o -o busca_ternaria -std=c99 -Wall

# Compila o arquivo 'busca_ternaria.c' em um arquivo objeto 'busca_ternaria.o'
busca_ternaria.o: busca_ternaria.c
	gcc -c busca_ternaria.c -o busca_ternaria.o -std=c99 -Wall

# Regra para executar o programa 'busca_ternaria'
run: busca_ternaria
	./busca_ternaria

# Remove todos os arquivos objetos e o executável gerado
clean:
	rm -f *.o busca_ternaria
