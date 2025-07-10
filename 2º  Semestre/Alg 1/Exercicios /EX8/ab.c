#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "ab.h"
#include "item.h"

typedef struct No_ NO;

typedef struct No_ {
    ITEM *item;
    NO* esq;
    NO* dir;
}NO; 

typedef struct arv_bin {
    NO* raiz;
    int profundidade;
}AB;

// Cria uma árvore vazia retornando NULL
AB *ab_criar(void) {
    AB* ab = (AB*)malloc(sizeof (AB));
    if (ab != NULL){
        ab->profundidade = -1;
        ab->raiz = NULL;    
    }
    return (ab);
}

// Função auxiliar para criar um novo nó na árvore
NO *ab_criar_no(ITEM *item) {
    NO *novo = (NO *)malloc(sizeof(NO));
    if (novo) {
        novo->item = item;
        novo->esq = NULL;
        novo->dir = NULL;
    }
    return novo;
}

void ab_inserir_no(NO *raiz, NO *no, int lado, int chave) {
    if (raiz == NULL) return;

    if (chave == item_get_chave(raiz->item)) {
        if (lado == FILHO_DIR) {
            raiz->dir = no;
        } else if (lado == FILHO_ESQ) {
            raiz->esq = no;
        }
    } else {
        ab_inserir_no(raiz->esq, no, lado, chave);
        ab_inserir_no(raiz->dir, no, lado, chave);
    }
}


bool ab_inserir (AB *T, ITEM *item, int lado, int chave){
    if (T->raiz == NULL){
        return ((T->raiz = ab_criar_no (item)) != NULL);
    }
    else{
        NO* novo_no = ab_criar_no (item);
        if (novo_no != NULL){
            ab_inserir_no (T->raiz, novo_no, lado, chave);
            return true;
        }
    }
    return false;
}


void ab_imprimir_no(NO *no) {
    if (no == NULL) return;
    ab_imprimir_no(no->esq);
    int chave = item_get_chave (no->item);
    printf("%d, ", chave);
    ab_imprimir_no(no->dir);
}

void ab_imprimir(AB *T) {
    if (T == NULL) return;
    ab_imprimir_no(T->raiz);
    printf("\n");
}


// Libera recursivamente a memória da árvore
void ab_apagar_no(NO **no) {
    if (*no == NULL) return;
    
    ab_apagar_no(&(*no)->esq);   // Chame recursivamente para o filho esquerdo
    ab_apagar_no(&(*no)->dir);   // Chame recursivamente para o filho direito

    item_apagar(&(*no)->item);   // Libere o item associado ao nó
    free(*no);                   // Libere o nó em si
    *no = NULL;
}

// Libera recursivamente a memória da árvore
void ab_apagar_arvore(AB **T) {
    if (*T == NULL) return;
    ab_apagar_no(&(*T)->raiz);   // Libere todos os nós a partir da raiz
    free(*T);                    // Libere a estrutura da árvore em si
    *T = NULL;
}

int estritamente_binaria(NO *T, int *a) {
    if (T == NULL) return 0; // Caso base: nó vazio é considerado estritamente binário

    // Se o nó atual possui apenas um filho, a árvore não é estritamente binária
    if ((T->esq == NULL && T->dir != NULL) || (T->esq != NULL && T->dir == NULL)) {
        *a = 1;  // Marca que a árvore não é estritamente binária
        return 1; // Retorna 0 imediatamente
    }

    // Chama recursivamente para os filhos esquerdo e direito
    estritamente_binaria(T->esq, a);
    estritamente_binaria(T->dir, a);

    return *a == 1; // Retorna 0 se a árvore é estritamente binária, 1 caso contrário
}

int ab_estritamente_binaria(AB *T) {
    if (T == NULL) return 0; // Árvore vazia é considerada estritamente binária
    int a = 0;
    return estritamente_binaria(T->raiz, &a);
}


