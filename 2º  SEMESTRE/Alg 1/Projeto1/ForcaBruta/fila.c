#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "fila.h"

// Estrutura para nó
typedef struct No {
    PERMUTACAO *permutacao;
    struct No *proximo;
} No;

// Estrutura fila encadeada
typedef struct Fila_ {
    No *inicio;
    No *fim;
} Fila;

// Inicializa a fila encadeada
void Fila_Inicializar(Fila **fila) {
    *fila = (Fila*) malloc(sizeof(Fila));
    if (*fila == NULL) {
        printf("Erro ao alocar memória para a fila.\n");
        exit(1);
    }
    (*fila)->inicio = NULL;
    (*fila)->fim = NULL;
}

// Verifica se a fila está vazia
bool Fila_Vazia(Fila *fila) {
    return fila->inicio == NULL;
}

// Insere uma permutação parcial na fila
void Fila_Enfileirar(Fila *fila, PERMUTACAO *p) {
    No *novoNo = (No*) malloc(sizeof(No));
    if (novoNo == NULL) {
        printf("Erro ao alocar memória para o nó da fila.\n");
        exit(1);
    }
    novoNo->permutacao = p;
    novoNo->proximo = NULL;

    if (Fila_Vazia(fila)) {
        fila->inicio = novoNo;
    } else {
        fila->fim->proximo = novoNo;
    }

    fila->fim = novoNo;
}

// Remove uma permutação parcial da fila
PERMUTACAO* Fila_Desenfileirar(Fila *fila) {
    if (Fila_Vazia(fila)) {
        printf("Erro: Fila vazia\n");
        exit(1);
    }

    No *temp = fila->inicio;
    PERMUTACAO *permutacao = temp->permutacao;
    fila->inicio = fila->inicio->proximo;

    if (fila->inicio == NULL) {
        fila->fim = NULL;
    }

    free(temp);
    return permutacao;
}

// Libera a memória da fila
void Fila_Apagar(Fila *fila) {
    while (!Fila_Vazia(fila)) {
        PERMUTACAO *p = Fila_Desenfileirar(fila);
        free(p);
    }
    free(fila);
}
