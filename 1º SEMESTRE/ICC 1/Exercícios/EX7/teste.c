#include "item.h"
#include "lista.h"
#include <stdbool.h>
#include <stdio.h>

#ifndef STRUCTS_
    #define STRUCTS_
        struct lista_{ 
            ITEM *lista[TAM_MAX];
            int inicio; // pos do primeiro item
            int fim; // pos do próximo espaço vazio
            int tamanho;
        };
#endif

bool lista_inserir(LISTA *lista, ITEM *item){

    if (lista==NULL || lista_cheia(lista)){
        printf("Erro!");
        return (false);
    }
    if (lista->tamanho == 0){
        lista->lista[0] = item;
        lista->inicio = 0;
        lista->fim = 0;
    }else{
        int final = item_get_chave (lista->lista[lista->fim]); //estou assumindo que o numero esta sendo guardado na chave
        int num = item_get_chave (item);
        if( num > final ){
            lista->lista[(lista->fim + 1) % TAM_MAX] = item; //insere no fim da lista circular
        }
        else{
            int possicao = Busca (lista, item); // faz a busca 
            int num = lista->fim;
            while(num == possicao){
                lista->lista[((num + 1 + TAM_MAX) % TAM_MAX)+1] = lista->lista[((num + 1 + TAM_MAX) % TAM_MAX)];
            }
            lista->lista[num] = item;
        }
    }
    lista->fim = (lista->fim + 1 + TAM_MAX) % TAM_MAX; //ajusta o fim
    lista->tamanho++;
    return (true);
}

int Busca (LISTA *lista, ITEM *item){
    int a = item_get_chave (lista->lista[lista->inicio]);
    int b = lista->inicio;
    int num = item_get_chave (item);

    while(num < a){
        a = ((a + 1 + TAM_MAX) % TAM_MAX);
        b++;
    }

    return b;
}