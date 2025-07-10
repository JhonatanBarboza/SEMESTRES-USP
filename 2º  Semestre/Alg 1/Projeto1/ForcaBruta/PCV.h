#ifndef PCV_H
	#define PCV_H

    #include "fila.h"
    #include "lista.h"

    typedef struct Permutacao_ PERMUTACAO;

    void PCV_Apagar(LISTA* distancia, PERMUTACAO* melhor_caminho);
    LISTA* PCV_ler_dados(int *num_cidades, int *inicial, int *arestas);
    PERMUTACAO* PCV_Melhor_Caminho(int num_cidades, int cidade_inicial, int arestas, LISTA* lista);

#endif
