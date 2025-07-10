#include "item.h"
#include <stdbool.h>
#include <stdio.h>

typedef struct no_ {
    ITEM *item; //conteudo a ser quardado
    struct no_* proximo; //aponta para o proxomo no
}NO;

typedef struct lista_ {
    NO* inicio;
    NO* fim;
    int tamanho;
    bool ordenada;
}LISTA;

LISTA *lista_criar(bool ordenada){
    LISTA *lista = (LISTA*)malloc(1*sizeof(LISTA));
    if (lista != NULL){
        lista->inicio = NULL;
        lista->fim = NULL;
        lista->tamanho = 0;
        lista->ordenada = ordenada;
        return (lista);
    }
    return (NULL);
}

NO * no_criar (ITEM* item){
    NO* no;
    no = (NO*)malloc(1*sizeof(NO));
    if (no != NULL){
        no->item = item;
        no->proximo = NULL;
        return (no);
    }
    return (NULL);
}

bool lista_inserir(LISTA *lista, ITEM *item){
    if (lista == NULL){
        printf("Erro");
        return (false);
    }
    NO* no = no_criar(item);
    if (lista->ordenada){
        lista_inserir_ordena(lista, no);} //lista ordenada
    else{
        lista_inserir_nao_ordena(lista, no);} //lista não ordenada
    lista->tamanho++;
    return (true);
}

void lista_inserir_ordena(LISTA* lista, NO* no){
    //incirir se a lista esta fazia
    if (lista->tamanho == 0){
        lista->inicio = no;
        lista->fim = no;
    }
    //incerir no inicio
    else { if(lista->inicio < (item_get_chave (no->item))){
        no->proximo = lista->inicio;
        lista->inicio = no;
    }
    //incerir no fim
    else { if(lista->fim < (item_get_chave (no->item))){
        lista_inserir_nao_ordena(lista, no);
    }
    //incerir no meio
    else {
        NO* aux = lista->inicio;
        while( aux->proximo < (item_get_chave (no->item))){
            aux = aux->proximo;
        }
        no->proximo = aux->proximo;
        aux->proximo = no;
    }}}
}


void lista_inserir_nao_ordena(LISTA* lista, NO* no){
    NO* aux = lista->fim;    
    lista->fim = no;
    if(lista->tamanho == 0)
        lista->inicio = no;
    else
        aux->proximo = no;
}


ITEM *lista_remover(LISTA *lista, int chave){
    if(lista == NULL){
        printf("Erro");
        return (NULL);
    }
    //remover do inicio
    //retirar do fim
    //retirar do meio
}










bool lista_apagar(LISTA **lista);
ITEM *lista_busca(LISTA *lista, int chave);
int lista_tamanho(LISTA *lista);
bool lista_vazia(LISTA *lista);
bool lista_cheia(LISTA *lista);
void lista_imprimir(LISTA *lista);


//Funções adicionais
int lista_inverter(LISTA **lista);
bool lista_comparar(LISTA *l1, LISTA *l2);

