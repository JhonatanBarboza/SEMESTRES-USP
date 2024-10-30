// lista.c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "lista.h"

struct Lista_ {
    int *dados;
    int tamanho;
    int capacidade;
};

// Função auxiliar para busca binária
static int busca_binaria(LISTA *lista, int elemento) {
    int esquerda = 0, direita = lista->tamanho - 1;
    while (esquerda <= direita) {
        int meio = esquerda + (direita - esquerda) / 2;
        if (lista->dados[meio] == elemento) {
            return meio;
        } else if (lista->dados[meio] < elemento) {
            esquerda = meio + 1;
        } else {
            direita = meio - 1;
        }
    }
    return -1;
}

LISTA* Lista_Criar(int capacidade) {
    LISTA *lista = (LISTA*)malloc(sizeof(LISTA));
    lista->dados = (int*)malloc(capacidade * sizeof(int));
    lista->tamanho = 0;
    lista->capacidade = capacidade;
    return lista;
}

void Lista_Apagar(LISTA *lista) {
    free(lista->dados);
    free(lista);
}

void Lista_Inserir(LISTA *lista, int elemento) {
    if (lista->tamanho >= lista->capacidade) {
        lista->capacidade *= 2;
        lista->dados = (int*)realloc(lista->dados, lista->capacidade * sizeof(int));
    }

    // Encontra posição para inserção para manter a ordenação
    int i = lista->tamanho - 1;
    while (i >= 0 && lista->dados[i] > elemento) {
        lista->dados[i + 1] = lista->dados[i];
        i--;
    }
    lista->dados[i + 1] = elemento;
    lista->tamanho++;
}

bool Lista_Pertence(LISTA *lista, int elemento) {
    return busca_binaria(lista, elemento) != -1;
}

void Lista_Remover(LISTA *lista) {
    if (lista->tamanho > 0) {
        lista->tamanho--;
    }
}

void Lista_Imprimir(LISTA *lista) {
    for (int i = 0; i < lista->tamanho; i++) {
        printf("%d ", lista->dados[i]);
    }
    printf("\n");
}

// Funções de união e interseção
LISTA* Lista_Uniao(LISTA* lista_A, LISTA* lista_B) {
    LISTA *uniao = Lista_Criar(lista_A->capacidade + lista_B->capacidade);
    int i = 0, j = 0;

    while (i < lista_A->tamanho && j < lista_B->tamanho) {
        if (lista_A->dados[i] < lista_B->dados[j]) {
            Lista_Inserir(uniao, lista_A->dados[i++]);
        } else if (lista_A->dados[i] > lista_B->dados[j]) {
            Lista_Inserir(uniao, lista_B->dados[j++]);
        } else {
            Lista_Inserir(uniao, lista_A->dados[i]);
            i++;
            j++;
        }
    }
    while (i < lista_A->tamanho) Lista_Inserir(uniao, lista_A->dados[i++]);
    while (j < lista_B->tamanho) Lista_Inserir(uniao, lista_B->dados[j++]);

    return uniao;
}

LISTA* Lista_Interseccao(LISTA* lista_A, LISTA* lista_B) {
    LISTA *interseccao = Lista_Criar(lista_A->capacidade);

    for (int i = 0; i < lista_A->tamanho; i++) {
        if (Lista_Pertence(lista_B, lista_A->dados[i])) {
            Lista_Inserir(interseccao, lista_A->dados[i]);
        }
    }
    return interseccao;
}