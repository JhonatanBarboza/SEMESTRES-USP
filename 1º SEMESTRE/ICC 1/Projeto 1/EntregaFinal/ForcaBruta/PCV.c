#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "fila.h"
#include "lista.h"
#include "PCV.h"

// Estrutura para armazenar uma permutação
typedef struct Permutacao_ {
    int *elementos; 
    int tamanho;    
    int custo;
} PERMUTACAO;

// Protótipos das funções
PERMUTACAO* Permutacao_Alocar(int n);
bool PCV_Elemento_Presente(int *elementos, int tamanho, int valor);
PERMUTACAO* PCV_Gerar_Permutacoes(int *vetor, int num_cidades, LISTA* lista, int arestas, int cidade_inicial);
void PCV_Avalia_Caminho(PERMUTACAO* caminho, LISTA* lista, int num_cidades, int arestas, PERMUTACAO* melhor_caminho);

// Função para receber os dados do usuário
LISTA* PCV_ler_dados(int *num_cidades, int *inicial, int *arestas) {
    int cidadeA, cidadeB, distancia;
    scanf("%d %d %d", num_cidades, inicial, arestas); 

    LISTA *lista = Lista_Criar(*arestas);

    for (int i = 0; i < *arestas; i++) {
        scanf("%d %d %d", &cidadeA, &cidadeB, &distancia);
        Lista_Inserir(lista, cidadeA, cidadeB, distancia);
    }

    return lista;
}

// Função cria todas as permutações possíveis com os caminhos
PERMUTACAO* PCV_Melhor_Caminho(int num_cidades, int cidade_inicial, int arestas, LISTA* lista) {
    int num_permutacoes = num_cidades - 1;
    int cidades[num_permutacoes]; // Vetor para armazenar cidades
    int k = 0;                    // Índice para preencher o vetor

    // Preenche o vetor com todas as cidades excluindo a cidade inicial
    for (int i = 0; i < arestas; i++) {
        int cidadeA = Lista_Consultar(lista, i, 'A');
        int cidadeB = Lista_Consultar(lista, i, 'B');

        if (!PCV_Elemento_Presente(cidades, k, cidadeA) && cidade_inicial != cidadeA)
            cidades[k++] = cidadeA;
        if (!PCV_Elemento_Presente(cidades, k, cidadeB) && cidade_inicial != cidadeB)
            cidades[k++] = cidadeB;
        if (k >= num_permutacoes)
            break;
    }

    // Gera todas as permutações e as avalia
    PERMUTACAO *melhor_caminho = PCV_Gerar_Permutacoes(cidades, num_cidades, lista, arestas, cidade_inicial);

    printf("%d ", cidade_inicial);
    if (melhor_caminho->custo != -1) {
        //printf("\nMelhor Caminho: ");
        for (int i = 0; i < num_cidades; i++) {
            printf("%d-", melhor_caminho->elementos[i]);
        }
        printf("%d", melhor_caminho->elementos[num_cidades]);
        printf(" %d\n", melhor_caminho->custo);
    } else {
        printf("\nNão foi possível encontrar um caminho válido.\n");
    }

    return melhor_caminho;
}

// Verifica se um elemento já está presente na permutação
bool PCV_Elemento_Presente(int *elementos, int tamanho, int valor) {
    for (int i = 0; i < tamanho; i++) {
        if (elementos[i] == valor)
            return true;
    }
    return false;
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

// Gera permutações de caminhos
PERMUTACAO* PCV_Gerar_Permutacoes(int *vetor, int num_cidades, LISTA* lista, int arestas, int cidade_inicial) {
    PERMUTACAO* caminho_completo = Permutacao_Alocar(num_cidades + 1);
    PERMUTACAO* melhor_caminho = Permutacao_Alocar(num_cidades + 1);    
    PERMUTACAO* atual;                                                   

    Fila* fila = NULL; // Será utilizada para gerar as permutações 
    Fila_Inicializar(&fila); 
    Fila_Enfileirar(fila, caminho_completo);

    // Loop enquanto houver permutações parciais na fila a serem processadas
    while (!Fila_Vazia(fila)) {
        atual = Fila_Desenfileirar(fila);

        // Se uma permutação estiver completa
        if (atual->tamanho == num_cidades - 1) {
            caminho_completo->elementos[0] = cidade_inicial;
            for (int i = 0; i < atual->tamanho; i++) {
                caminho_completo->elementos[i + 1] = atual->elementos[i];
            }

            caminho_completo->elementos[num_cidades] = cidade_inicial;
            caminho_completo->tamanho = num_cidades + 1;

            // Verifica se é o melhor caminho
            PCV_Avalia_Caminho(caminho_completo, lista, num_cidades, arestas, melhor_caminho);

        } else {
            // Se a permutação ainda não está completa, tenta adicionar novos elementos
            for (int i = 0; i < num_cidades - 1; i++) {
                // Verifica se o elemento vetor[i] já está presente na permutação atual
                if (!PCV_Elemento_Presente(atual->elementos, atual->tamanho, vetor[i])) {
                    PERMUTACAO* Nova_Permutacao = Permutacao_Alocar(num_cidades - 1);

                    // Copia os elementos da permutação atual para a nova permutação
                    for (int j = 0; j < atual->tamanho; j++) {
                        Nova_Permutacao->elementos[j] = atual->elementos[j];
                    }
                    Nova_Permutacao->tamanho = atual->tamanho;

                    // Adiciona o novo elemento (vetor[i]) na nova permutação
                    Nova_Permutacao->elementos[Nova_Permutacao->tamanho] = vetor[i];
                    Nova_Permutacao->tamanho++; 

                    Fila_Enfileirar(fila, Nova_Permutacao);
                }
            }
        }
    }

    Fila_Apagar(fila);
    free(caminho_completo->elementos);
    free(caminho_completo);
    free(atual->elementos); 
    free(atual);            

    return melhor_caminho;
}

// Avalia o caminho e calcula o custo
void PCV_Avalia_Caminho(PERMUTACAO* caminho, LISTA* lista, int num_cidades, int arestas, PERMUTACAO* melhor_caminho) {
    int custo = 0;
    int caminho_valido = 1;

    // Loop para calcular o custo do caminho
    for (int j = 0; j < num_cidades; j++) {
        if (!caminho_valido)
            break;

        int origem = caminho->elementos[j];
        int destino = caminho->elementos[j + 1];
        int distancia_ij = -1;

        // Encontra a distância entre a origem e o destino
        for (int k = 0; k < arestas; k++) {
            int cidadeA = Lista_Consultar(lista, k, 'A');
            int cidadeB = Lista_Consultar(lista, k, 'B');
            int distancia = Lista_Consultar(lista, k, 'D');

            if ((cidadeA == origem && cidadeB == destino) ||
                (cidadeA == destino && cidadeB == origem)) {
                distancia_ij = distancia;
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
        for (int i = 0; i < num_cidades + 1; i++) {
            melhor_caminho->elementos[i] = caminho->elementos[i];
        }
    }
}

// Função para liberar memória
void PCV_Apagar(LISTA* lista, PERMUTACAO* melhor_caminho) {
    free(melhor_caminho->elementos);  
    free(melhor_caminho);
    Lista_Apagar(lista);
}