3 1 3
1 2 10
1 3 10
2 3 10

4 1 6
1 2 1
1 3 12
1 4 1
2 3 1
2 4 20
3 4 1


4 1 6
1 2 1
1 3 12
1 4 1
2 3 1
2 4 20
3 4 1


5 1 8
1 2 1
1 3 9
1 4 14
1 5 1
2 3 1
2 4 8
3 4 1
4 5 1


6 1 12
1 2 1
2 3 1
3 4 1
4 5 1
5 6 1
1 6 1
1 3 8
1 4 12
1 5 10
2 4 9
3 5 11
4 6 14

10 1 45
1 2 1
1 3 2
1 4 3
1 5 4
1 6 5
1 7 6
1 8 7
1 9 8
1 10 1
2 3 1
2 4 2
2 5 3
2 6 4
2 7 5
2 8 6
2 9 7
2 10 8
3 4 1
3 5 2
3 6 3
3 7 4
3 8 5
3 9 6
3 10 7
4 5 1
4 6 2
4 7 3
4 8 4
4 9 5
4 10 6
5 6 1
5 7 2
5 8 3
5 9 4
5 10 5
6 7 1
6 8 2
6 9 3
6 10 4
7 8 1
7 9 2
7 10 3
8 9 1
8 10 2
9 10 1


11 1 11
1 2 1
2 3 1
3 4 1
4 5 1
5 6 1
6 7 1
7 8 1
8 9 1
9 10 1
10 11 1
11 1 1



12 1 20
1 2 1
1 3 15
1 4 20
2 5 25
2 6 30
3 7 18
3 8 24
4 9 22
4 10 27
5 11 33
6 12 35
7 9 12
8 11 40
9 12 38
10 11 28
10 12 32
5 6 12
7 8 11
9 10 14
11 12 17


12 1 12
1 2 1
2 3 1 
3 4 1
4 5 1
5 6 1
6 7 1 
7 8 1
8 9 1 
9 10 1
10 11 1 
11 12 1 
12 1 1


12 1 20
1 2 1
2 3 1
3 4 1
4 5 1
5 6 1
6 7 1
7 8 1
8 9 1
9 10 1
10 11 1
11 12 1
12 1 1
1 3 2
2 5 2
3 6 2
4 7 2
5 8 2
6 9 2
7 10 2
8 11 2





12 1 30
1 2 1
2 3 1
3 4 1
4 5 1
5 6 1
6 7 1
7 8 1
8 9 1
9 10 1
10 11 1
11 12 1
12 1 1
1 3 2
2 5 2
3 6 2
4 7 2
5 8 2
6 9 2
7 10 2
8 11 2
9 12 2
10 1 2
11 3 2
12 5 2
1 4 3
2 6 3
3 7 3
4 8 3
5 9 3
6 10 3


12 1 40
1 2 1
2 3 1
3 4 1
4 5 1
5 6 1
6 7 1
7 8 1
8 9 1
9 10 1
10 11 1
11 12 1
12 1 1
1 3 2
2 5 2
3 6 2
4 7 2
5 8 2
6 9 2
7 10 2
8 11 2
9 12 2
10 1 2
11 3 2
12 5 2
1 4 3
2 6 3
3 7 3
4 8 3
5 9 3
6 10 3
7 11 3
8 12 3
9 1 3
10 2 3
11 4 3
12 6 3
1 5 4
2 7 4
3 8 4
4 9 4




#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fila.h"
#include "PCV.h"


// Estrutura para armazenar uma permutação
typedef struct PERMUTACAO_{
    int *elementos; 
    int tamanho;    
    int custo;
} PERMUTACAO;

// Dados fornecidos pelo usuário
typedef struct INDICE_ {  
    int cidadeA;
    int cidadeB;
    int distancia;
} INDICE;


// Prototipos das funções
PERMUTACAO* Permutacao_Alocar(int n);
void PCV_Apagar(INDICE* distancia, PERMUTACAO* melhor_caminho );
bool PCV_Elemento_Presente(int *elementos, int tamanho, int valor);
INDICE* PCV_ler_dados(int *num_cidades, int *inicial, int *arestas);
PERMUTACAO* PCV_Melhor_Caminho(int num_cidades, int cidade_inicial, int arestas, INDICE* distancia);
PERMUTACAO* PCV_Gerar_Permutacoes(int *vetor, int num_cidades, INDICE* distancia, int arestas, int cidade_inicial);
void PCV_Avalia_Caminho(PERMUTACAO* caminho, INDICE* distancia, int num_cidades, int arestas, PERMUTACAO* melhor_caminho);


//função para receber os dados do usuario
INDICE* PCV_ler_dados(int *num_cidades, int *inicial, int *arestas) {
    scanf(" %d", num_cidades);  //numero de cidades
    scanf(" %d", inicial);      //cidade inicial
    scanf(" %d", arestas);      //numero de erestas

    INDICE *distancia = (INDICE*)malloc(*arestas * sizeof(INDICE)); 
    if (distancia == NULL){
        printf("Erro ao alocar memória.\n");
        exit(1);
    }

    for (int i = 0; i < *arestas; i++) {
        scanf(" %d %d %d", &distancia[i].cidadeA, &distancia[i].cidadeB, &distancia[i].distancia); //cidade A, B e distancia 
    }
    return distancia;
}

// função cria todas as pemutações possiveis com os caminhos
PERMUTACAO* PCV_Melhor_Caminho(int num_cidades, int cidade_inicial, int arestas, INDICE* distancia) {
    int x = num_cidades - 1;  // Número de cidades a serem permutadas
    int vet[x];               // Vetor para armazenar cidades
    int k = 0;                // Índice para preencher o vetor

    // Preenche o vetor com todas as cidades excluindo a cidade inicial
    for (int i = 0; i < arestas; i++) {
        if (!PCV_Elemento_Presente(vet, k, distancia[i].cidadeA) && cidade_inicial != distancia[i].cidadeA)
            vet[k++] = distancia[i].cidadeA;
        if (!PCV_Elemento_Presente(vet, k, distancia[i].cidadeB) && cidade_inicial != distancia[i].cidadeB)
            vet[k++] = distancia[i].cidadeB;
        if (k >= x)         // Verifica se já preencheu todas as cidades necessárias
            break;
    }

    // Gera todas as permutações e às avalia 
    PERMUTACAO *melhor_caminho = PCV_Gerar_Permutacoes(vet, num_cidades, distancia, arestas, cidade_inicial); //gera as pemutações e atribuia a caminhos

    printf("Cidade inicial: %d", cidade_inicial);
    if (melhor_caminho->custo != -1) {
        printf("\nMelhor Caminho: ");
        for (int i = 0 ; i < num_cidades+1 ; i++) {
            printf("%d ", melhor_caminho->elementos[i]);
        }
        printf("\nCusto Total: %d\n", melhor_caminho->custo);
    } else {
        printf("Não foi possível encontrar um caminho válido.\n");
    }

    return (melhor_caminho);
}

// Função para liberar memória
void PCV_Apagar(INDICE* distancia, PERMUTACAO* melhor_caminho ) {
    free(melhor_caminho->elementos);  
    free(melhor_caminho);
    free(distancia);
}

//**************************************************************************************************************

// Verifica se um elemento já está presente na permutação
bool PCV_Elemento_Presente(int *elementos, int tamanho, int valor) {
    for (int i = 0; i < tamanho; i++) {
        if (elementos[i] == valor) 
            return (true);
    }
    return (false);
}

// Função para alocar uma nova permutação
PERMUTACAO* Permutacao_Alocar(int n) {
    PERMUTACAO* caminhos = (PERMUTACAO*)malloc(sizeof(PERMUTACAO));
    if (caminhos == NULL) {
        printf("Erro ao alocar memória.\n");
        exit(1);
    }

    caminhos->elementos = (int*)malloc(n * sizeof(int));
    if (caminhos->elementos == NULL) {
        printf("Erro ao alocar memória.\n");
        free(caminhos);
        exit(1);
    }

    caminhos->tamanho = 0;
    caminhos->custo = -1;
    return caminhos;
}


PERMUTACAO* PCV_Gerar_Permutacoes(int *vetor, int num_cidades, INDICE* distancia, int arestas, int cidade_inicial) {
    
    PERMUTACAO* melhor_caminho = Permutacao_Alocar(num_cidades + 1);
    PERMUTACAO* PermutacaoInicial = Permutacao_Alocar(num_cidades - 1); // Cria a permutação inicial (vazia)
    PERMUTACAO* caminho_completo = Permutacao_Alocar(num_cidades + 1);  // Cria o caminho completo incluindo a cidade inicial no começo e no fim
    PERMUTACAO* atual;

    Fila* fila = NULL; // Ponteiro para a estrutura de fila que armazenará permutações parciais
    Fila_Inicializar(&fila); 
    Fila_Enfileirar(fila, PermutacaoInicial);

    // Loop enquanto houver permutações parciais na fila a serem processadas
    while (!Fila_Vazia(fila)) {
        atual = Fila_Desenfileirar(fila);

        // Se a permutação atual tiver o tamanho máximo (ou seja, contém todas as cidades)
        if (atual->tamanho == num_cidades - 1) {
            
            caminho_completo->elementos[0] = cidade_inicial;
            for (int i=0 ; i<atual->tamanho ; i++){
                caminho_completo->elementos[i+1] = atual->elementos[i];
            }
            
            caminho_completo->elementos[num_cidades] = cidade_inicial;
            caminho_completo->tamanho = num_cidades + 1;

            // Avalia o caminho completo para ver se é o melhor caminho
            PCV_Avalia_Caminho(caminho_completo, distancia, num_cidades, arestas, melhor_caminho);

        } else {
            // Se a permutação ainda não está completa, tenta adicionar novos elementos
            for (int i = 0; i < num_cidades-1; i++) {
                // Verifica se o elemento vetor[i] já está presente na permutação atual
                if (!PCV_Elemento_Presente(atual->elementos, atual->tamanho, vetor[i])) {
                    PERMUTACAO* Nova_Pemutacao = Permutacao_Alocar(num_cidades-1);

                    // Copia os elementos da permutação atual para a nova permutação
                    for (int i=0 ; i<atual->tamanho ; i++){
                        Nova_Pemutacao->elementos[i] = atual->elementos[i];}
                    Nova_Pemutacao->tamanho = atual->tamanho;

                    // Adiciona o novo elemento (vetor[i]) na nova permutação
                    Nova_Pemutacao->elementos[Nova_Pemutacao->tamanho] = vetor[i];
                    Nova_Pemutacao->tamanho++;  // Incrementa o tamanho da nova permutação

                    Fila_Enfileirar(fila, Nova_Pemutacao);
                }
            }
        }
    }

    Fila_Apagar(fila);
    free(caminho_completo->elementos);
    free(caminho_completo);
    free(atual->elementos);  // Libera os elementos da permutação atual
    free(atual);             // Libera a permutação atual

    return melhor_caminho;  // Retorna o melhor caminho encontrado
}

void PCV_Avalia_Caminho(PERMUTACAO* caminho, INDICE* distancia, int num_cidades, int arestas, PERMUTACAO* melhor_caminho) {
    int custo = 0;
    int caminho_valido = 1;

    // Loop para calcular o custo do caminho
    for (int j = 0; j < num_cidades; j++) {  
        if (caminho_valido == 0)
            break;

        int origem = caminho->elementos[j];
        int destino = caminho->elementos[j + 1];
        int distancia_ij = -1;

        // Encontra a distância entre a origem e o destino
        for (int k = 0; k < arestas; k++) {
            if ((distancia[k].cidadeA == origem && distancia[k].cidadeB == destino) ||
                (distancia[k].cidadeA == destino && distancia[k].cidadeB == origem)) {
                distancia_ij = distancia[k].distancia;
                break;
            }
        }

        // Se não encontrou uma distância válida, desconsidera o caminho
        if (distancia_ij == -1) {
            caminho_valido = 0;
            break;
        }

        // Incrementa o custo do caminho
        custo += distancia_ij;
    }

    // Verifica se o caminho é válido e se é o melhor até agora
    if (caminho_valido && (melhor_caminho->custo == -1 || custo < melhor_caminho->custo)) {
        melhor_caminho->custo = custo;
        for (int i=0 ; i<num_cidades+1 ; i++){
            melhor_caminho->elementos[i] = caminho->elementos[i];
        }
    }
}







MAIS ATUAL 



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fila.h"
#include "PCV.h"


// Estrutura para armazenar uma permutação
typedef struct PERMUTACAO_{
    int *elementos; 
    int tamanho;    
    int custo;
} PERMUTACAO;

// Dados fornecidos pelo usuário
typedef struct INDICE_ {  
    int cidadeA;
    int cidadeB;
    int distancia;
} INDICE;


// Prototipos das funções
PERMUTACAO* Permutacao_Alocar(int n);
bool PCV_Elemento_Presente(int *elementos, int tamanho, int valor);
PERMUTACAO* PCV_Gerar_Permutacoes(int *vetor, int num_cidades, INDICE* distancia, int arestas, int cidade_inicial);
void PCV_Avalia_Caminho(PERMUTACAO* caminho, INDICE* distancia, int num_cidades, int arestas, PERMUTACAO* melhor_caminho);


//função para receber os dados do usuario
INDICE* PCV_ler_dados(int *num_cidades, int *inicial, int *arestas) {
    scanf(" %d %d %d", num_cidades, inicial, arestas);

    INDICE *distancia = (INDICE*)malloc(*arestas * sizeof(INDICE)); 
    if (distancia == NULL){
        printf("Erro ao alocar memória.\n");
        exit(1);
    }

    for (int i = 0; i < *arestas; i++) {
        scanf(" %d %d %d", &distancia[i].cidadeA, &distancia[i].cidadeB, &distancia[i].distancia);} //cidade A, B e distancia 
    
    return distancia;
}

// função cria todas as pemutações possiveis com os caminhos
PERMUTACAO* PCV_Melhor_Caminho(int num_cidades, int cidade_inicial, int arestas, INDICE* distancia) {
    int x = num_cidades - 1;  // Número de cidades a serem permutadas
    int vet[x];               // Vetor para armazenar cidades
    int k = 0;                // Índice para preencher o vetor

    // Preenche o vetor com todas as cidades excluindo a cidade inicial
    for (int i = 0; i < arestas; i++) {
        if (!PCV_Elemento_Presente(vet, k, distancia[i].cidadeA) && cidade_inicial != distancia[i].cidadeA)
            vet[k++] = distancia[i].cidadeA;
        if (!PCV_Elemento_Presente(vet, k, distancia[i].cidadeB) && cidade_inicial != distancia[i].cidadeB)
            vet[k++] = distancia[i].cidadeB;
        if (k >= x)         // Verifica se já preencheu todas as cidades necessárias
            break;
    }

    // Gera todas as permutações e às avalia 
    PERMUTACAO *melhor_caminho = PCV_Gerar_Permutacoes(vet, num_cidades, distancia, arestas, cidade_inicial); //gera as pemutações e atribuia a caminhos

    printf("Cidade inicial: %d", cidade_inicial);
    if (melhor_caminho->custo != -1) {
        printf("\nMelhor Caminho: ");
        for (int i = 0 ; i < num_cidades+1 ; i++) {
            printf("%d ", melhor_caminho->elementos[i]);
        }
        printf("\nCusto Total: %d\n", melhor_caminho->custo);
    } else {
        printf("\nNão foi possível encontrar um caminho válido.\n");
    }

    return (melhor_caminho);
}

// Função para liberar memória
void PCV_Apagar(INDICE* distancia, PERMUTACAO* melhor_caminho ) {
    free(melhor_caminho->elementos);  
    free(melhor_caminho);
    free(distancia);
}

//************************************************************************************************************

// Verifica se um elemento já está presente na permutação
bool PCV_Elemento_Presente(int *elementos, int tamanho, int valor) {
    for (int i = 0; i < tamanho; i++) {
        if (elementos[i] == valor) 
            return (true);
    }
    return (false);
}

// Função para alocar uma nova permutação
PERMUTACAO* Permutacao_Alocar(int n) {
    PERMUTACAO* caminhos = (PERMUTACAO*)malloc(sizeof(PERMUTACAO));
    if (caminhos == NULL) {
        printf("Erro ao alocar memória.\n");
        exit(1);
    }

    caminhos->elementos = (int*)malloc(n * sizeof(int));
    if (caminhos->elementos == NULL) {
        printf("Erro ao alocar memória.\n");
        free(caminhos);
        exit(1);
    }

    caminhos->tamanho = 0;
    caminhos->custo = -1;
    return caminhos;
}


PERMUTACAO* PCV_Gerar_Permutacoes(int *vetor, int num_cidades, INDICE* distancia, int arestas, int cidade_inicial) {
    
    PERMUTACAO* caminho_completo = Permutacao_Alocar(num_cidades + 1);  // Guarda cada caminho possivel 
    PERMUTACAO* melhor_caminho = Permutacao_Alocar(num_cidades + 1);    // Guarda o meu caminho 
    PERMUTACAO* atual;                                                   

    Fila* fila = NULL; // Ponteiro para a estrutura de fila que armazenará permutações parciais
    Fila_Inicializar(&fila); 
    Fila_Enfileirar(fila, caminho_completo);

    // Loop enquanto houver permutações parciais na fila a serem processadas
    while (!Fila_Vazia(fila)) {
        atual = Fila_Desenfileirar(fila);

        // Se a permutação atual tiver o tamanho máximo (ou seja, contém todas as cidades)
        if (atual->tamanho == num_cidades - 1) {
            
            caminho_completo->elementos[0] = cidade_inicial;
            for (int i=0 ; i<atual->tamanho ; i++){
                caminho_completo->elementos[i+1] = atual->elementos[i];}

            caminho_completo->elementos[num_cidades] = cidade_inicial;
            caminho_completo->tamanho = num_cidades + 1;

            // Avalia o caminho completo para ver se é o melhor caminho
            PCV_Avalia_Caminho(caminho_completo, distancia, num_cidades, arestas, melhor_caminho);

        } else {
            // Se a permutação ainda não está completa, tenta adicionar novos elementos
            for (int i = 0; i < num_cidades-1; i++) {
                // Verifica se o elemento vetor[i] já está presente na permutação atual
                if (!PCV_Elemento_Presente(atual->elementos, atual->tamanho, vetor[i])) {
                    PERMUTACAO* Nova_Pemutacao = Permutacao_Alocar(num_cidades-1);

                    // Copia os elementos da permutação atual para a nova permutação
                    for (int i=0 ; i<atual->tamanho ; i++){
                        Nova_Pemutacao->elementos[i] = atual->elementos[i];}
                    Nova_Pemutacao->tamanho = atual->tamanho;

                    // Adiciona o novo elemento (vetor[i]) na nova permutação
                    Nova_Pemutacao->elementos[Nova_Pemutacao->tamanho] = vetor[i];
                    Nova_Pemutacao->tamanho++; 

                    Fila_Enfileirar(fila, Nova_Pemutacao);
                }
            }
        }
    }

    Fila_Apagar(fila);
    free(caminho_completo->elementos);
    free(caminho_completo);
    free(atual->elementos); 
    free(atual);            

    return melhor_caminho;  // Retorna o melhor caminho encontrado
}

void PCV_Avalia_Caminho(PERMUTACAO* caminho, INDICE* distancia, int num_cidades, int arestas, PERMUTACAO* melhor_caminho) {
    int custo = 0;
    int caminho_valido = 1;

    // Loop para calcular o custo do caminho
    for (int j = 0; j < num_cidades; j++) {  
        if (caminho_valido == 0)
            break;

        int origem = caminho->elementos[j];
        int destino = caminho->elementos[j + 1];
        int distancia_ij = -1;

        // Encontra a distância entre a origem e o destino
        for (int k = 0; k < arestas; k++) {
            if ((distancia[k].cidadeA == origem && distancia[k].cidadeB == destino) ||
                (distancia[k].cidadeA == destino && distancia[k].cidadeB == origem)) {
                distancia_ij = distancia[k].distancia;
                break;
            }
        }

        // Se não encontrou uma distância válida, desconsidera o caminho
        if (distancia_ij == -1) {
            caminho_valido = 0;
            break;
        }

        // Incrementa o custo do caminho
        custo += distancia_ij;
    }

    // Verifica se o caminho é válido e se é o melhor até agora
    if (caminho_valido && (melhor_caminho->custo == -1 || custo < melhor_caminho->custo)) {
        melhor_caminho->custo = custo;
        for (int i=0 ; i<num_cidades+1 ; i++){
            melhor_caminho->elementos[i] = caminho->elementos[i];
        }
    }
}
