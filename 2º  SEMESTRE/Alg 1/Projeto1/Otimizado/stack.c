#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "stack.h"

typedef struct no_ NO;
struct no_ {
    int dado;
    NO *anterior;
};

struct pilha_ {
    int tamanho;
    NO *topo;
};

PILHA *pilha_criar () {
    PILHA *newStack = malloc (sizeof(PILHA));
    if (newStack) {
        newStack->tamanho = 0;
        newStack->topo = NULL;
    }

    return newStack;
}

bool pilha_vazia (PILHA *pilha) {
    if (pilha){
        return (pilha->tamanho == 0);
    }
    return true;
        
}

bool pilha_cheia (PILHA *pilha) {
    NO *no_teste = malloc (sizeof(NO));
    if (no_teste==NULL) {
        return true;
    }
    return false;
}

void pilha_empilhar (PILHA *pilha, int dado) {
    if (pilha!=NULL) {
        NO *newNode = malloc (sizeof(NO));
        if (newNode) {
            newNode->dado = dado;
            newNode->anterior = pilha->topo;
            pilha->topo = newNode;
            pilha->tamanho++;
        }
    }
}

int pilha_desempilhar (PILHA *pilha) {
    if (pilha!=NULL && !pilha_vazia(pilha)) {
        int aux;
        NO *no_aux = pilha->topo;
        aux = no_aux->dado;
        pilha->topo = no_aux->anterior;
        free(no_aux);
        pilha->tamanho--;
        return aux;
    }
    return -1;
}

void pilha_apagar (PILHA **pilha) {
    if (pilha != NULL && *pilha != NULL) {
        while (!pilha_vazia(*pilha)) {
            pilha_desempilhar(*pilha);
        }
        free(*pilha);
        *pilha = NULL;  
    }
}

void pilha_inverter (PILHA *pilha) {
    NO *atual, *anterior, *proximo;
    anterior = NULL;
    atual = pilha->topo;

    while (atual!=NULL) {
        proximo = atual->anterior;
        atual->anterior = anterior;
        anterior = atual;
        atual = proximo;
    }
    pilha->topo = anterior;
}

void pilha_imprimir (PILHA *pilha) {
    if (pilha!= NULL && !pilha_vazia(pilha)) {
        NO *aux = pilha->topo;
        int i = 0;
        while(aux!=NULL && !pilha_vazia(pilha)) {
            if (i == 0) {
                printf("%d", aux->dado);
            } else {
                printf(" - %d", aux->dado);
            }
            i++;
            aux=aux->anterior;
        }
        printf("\n");
    }
}

void pilha_desempilha_tudo (PILHA *pilha) {
    if(pilha!=NULL && !pilha_vazia(pilha)){
        while(!pilha_vazia(pilha)) {
            pilha_desempilhar(pilha);
        }
    }
}

void pilha_desempilha_imprime (PILHA *pilha) {
    if (pilha != NULL) {
        int i = 0;
        while (!pilha_vazia(pilha)) {
            int aux = pilha_desempilhar(pilha);
            if (i == 0) {
                printf("%d", aux);
            } else {
                printf(" - %d", aux);
            }
            i++;
        }
        printf("\n");
        
    }
}