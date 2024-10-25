#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "lista.h"

// Estrutura lista sequencial
typedef struct Lista_ {
    INDICE *dados;
    int tamanho;
    int capacidade;
} LISTA;

// Estrutura dados de entrada
typedef struct INDICE_ {
    int cidadeA;
    int cidadeB;
    int distancia;
} INDICE;

// Função para inicializar a lista com uma capacidade inicial
LISTA* Lista_Criar(int tamanho_dados) {
    LISTA *lista = (LISTA *)malloc(sizeof(LISTA));
    if (lista == NULL) {
        printf("Erro ao alocar memória \n");
        exit(1);
    }

    INDICE *cidades = (INDICE *)malloc(tamanho_dados * sizeof(INDICE));
    if (cidades == NULL) {
        printf("Erro ao alocar memória \n");
        exit(1);
    }

    lista->dados = cidades;
    lista->tamanho = 0;
    lista->capacidade = tamanho_dados;
    return lista;
}

// Verifica se a lista está vazia
bool Lista_Vazia(LISTA *lista) {
    return lista->tamanho == 0;
}

// Verifica se a lista está cheia
bool Lista_Cheia(LISTA *lista) {
    return lista->tamanho == lista->capacidade;
}

// Função para inserir um elemento do tipo INDICE na lista
void Lista_Inserir(LISTA *lista, int cidadeA, int cidadeB, int distancia) {
    if (Lista_Cheia(lista)) {
        printf("Erro: a lista está cheia!\n");
        return;
    }

    INDICE cidade;
    cidade.cidadeA = cidadeA;
    cidade.cidadeB = cidadeB;
    cidade.distancia = distancia;

    lista->dados[lista->tamanho] = cidade;
    lista->tamanho++;
}

// Função para remover o último elemento da lista
void Lista_Remover(LISTA *lista) {
    if (!Lista_Vazia(lista)) {
        lista->tamanho--;
    } else {
        printf("A lista já está vazia!\n");
    }
}

// Função para consultar um dado específico
int Lista_Consultar(LISTA *lista, int indice, char campo) {
    if (indice >= 0 && indice < lista->tamanho) {
        INDICE *elemento = &lista->dados[indice];

        if (campo == 'A')
            return elemento->cidadeA; // Retorna cidadeA
        else if (campo == 'B')
            return elemento->cidadeB; // Retorna cidadeB
        else if (campo == 'D')
            return elemento->distancia; // Retorna distancia
        else {
            printf("Erro: campo inválido.\n");
            return -1;
        }
    } else {
        printf("Erro: índice fora dos limites.\n");
        return -1;
    }
}

// Função para liberar a memória usada pela lista
void Lista_Apagar(LISTA *lista) {
    if (lista->dados != NULL) {
        free(lista->dados);
    }
    free(lista);
}
