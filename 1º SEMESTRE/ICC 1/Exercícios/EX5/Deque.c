#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "item.h"
#include "Deque.h"

typedef struct deque_ {
    int inicio, final, tamanho;
    ITEM *item[MAX_TAMANHO];
} DEQUE;

// Cria o deque e inicializa as variáveis com 0
DEQUE* deque_criar(){ 
    DEQUE *d = (DEQUE*)malloc(sizeof(DEQUE));
    if (d != NULL){
        d->inicio = 0;
        d->final = 0;
        d->tamanho = 0;
        return d;
    }
    exit(1); // Sai do programa se a alocação falhar
}

// Libera a memória do deque e o desaloca
void deque_apagar(DEQUE** deque){
    free (*deque);
    deque = NULL; // Define o ponteiro para NULL após liberar a memória
}

// Insere um item no início do deque e ajusta o índice do início
bool deque_inserir_inicio(DEQUE* deque, ITEM* item) { 
    if (deque != NULL && !deque_cheia(deque)){
        deque->item[deque->inicio] = item;
        deque->tamanho++;
        // Ajusta o índice do início, movendo-o para a posição anterior circularmente
        deque->inicio = (deque->inicio - 1 + MAX_TAMANHO) % MAX_TAMANHO;
        return true;
    }
    return false;
}

// Insere um item no final do deque e ajusta o índice do final
bool deque_inserir_fim(DEQUE* deque, ITEM* item) { 
    if (deque != NULL && !deque_cheia(deque)){
        // Ajusta o índice do final, movendo-o para a próxima posição circularmente
        deque->final = (deque->final + 1 + MAX_TAMANHO) % MAX_TAMANHO;
        deque->item[deque->final] = item;
        deque->tamanho++;
        return true;
    }
    return false;
}

// Remove e retorna o item do início do deque, ajustando o índice do início
ITEM* deque_remover_inicio(DEQUE* deque) {
    if (deque != NULL && !deque_vazia(deque)){
        // Ajusta o índice do início para a próxima posição circularmente
        deque->inicio = (deque->inicio + 1 + MAX_TAMANHO) % MAX_TAMANHO;
        ITEM *i = deque->item[deque->inicio];
        deque->item[deque->inicio] = NULL; // Remove a referência ao item
        deque->tamanho--;
        return i;
    }
    return NULL;
}

// Remove e retorna o item do final do deque, ajustando o índice do final
ITEM* deque_remover_fim(DEQUE* deque) {
    if (deque != NULL && !deque_vazia(deque)){
        ITEM *i = deque->item[deque->final];
        deque->item[deque->final] = NULL; // Remove a referência ao item
        deque->tamanho--;
        // Ajusta o índice do final para a posição anterior circularmente
        deque->final = (deque->final - 1 + MAX_TAMANHO) % MAX_TAMANHO;
        return i;
    }
    return NULL;
}

// Retorna o primeiro item do deque (posição imediatamente anterior ao início)
ITEM* deque_primeiro(DEQUE* deque) {
    if (deque != NULL){
        return deque->item[deque->inicio - 1];
    }
    return NULL;
}

// Retorna o último item do deque
ITEM* deque_ultimo(DEQUE* deque) {
    if (deque != NULL){
        return deque->item[deque->final];
    }
    return NULL;
}

// Retorna o número de itens no deque
int deque_contar(DEQUE* deque) {
    if (deque != NULL){
        return deque->tamanho;
    }
    return -1; // Retorna -1 em caso de erro
}

// Verifica se o deque está vazio
bool deque_vazia(DEQUE* deque) {
    if (deque != NULL && deque->tamanho == 0){
        return true;
    }
    return false;
}

// Verifica se o deque está cheio
bool deque_cheia(DEQUE* deque) {
    if (deque != NULL && deque->tamanho == MAX_TAMANHO){
        return true;
    }
    return false;
}
