#include <stdio.h>
#include "PCV.h"

int main(){
    int num_cidades, cidade_inicial, arestas;

    LISTA* distancia = PCV_ler_dados(&num_cidades, &cidade_inicial, &arestas);
    PERMUTACAO* melhor_caminho = PCV_Melhor_Caminho(num_cidades, cidade_inicial, arestas, distancia);
    PCV_Apagar(distancia, melhor_caminho);
    
    return 0;
}