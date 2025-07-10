#include "item.h"
#include "lista.h"
#include <stdbool.h>
#include <stdio.h>

#ifndef STRUCTS_
    #define STRUCTS_
        struct lista_{ 
            ITEM *lista[TAM_MAX];
            int inicio;  // pos do primeiro item
            int fim;     // pos do próximo espaço vazio
            int tamanho;
        };
#endif

int Busca(LISTA *lista, ITEM *item);

bool lista_inserir(LISTA *lista, ITEM *item){

    if (lista == NULL || lista_cheia(lista)){
        printf("Erro!");
        return false;1111
    }

    if (lista->tamanho == 0){
        // Caso lista vazia, insere o item na primeira posição
        lista->lista[lista->inicio] = item;
    } else {
        int final = item_get_chave(lista->lista[(lista->fim)-1]); // Pega a chave do último elemento
        int num = item_get_chave(item); // Chave do novo item

        if (num > final) {
            // Caso a chave do novo item seja maior que a chave do último, insere no fim
            lista->lista[(lista->fim) % TAM_MAX] = item;
        } else {
            // Busca a posição correta para inserção
            int posicao = Busca(lista, item);

            // Desloca os elementos para a direita (da posição fim até a posição encontrada)
            for (int i = lista->fim; i >= posicao; i--) {
                lista->lista[(i + 1) % TAM_MAX] = lista->lista[i % TAM_MAX];
            }

            // Insere o novo item na posição encontrada
            lista->lista[posicao] = item;
        }
    }

    // Ajusta o fim e aumenta o tamanho da lista
    lista->fim = (lista->fim + 1) % TAM_MAX;
    lista->tamanho++;
    return true;
}

int Busca(LISTA *lista, ITEM *item){
    int chaveItem = item_get_chave(item);
    int i = lista->inicio;

    // Percorre a lista circular para encontrar a posição correta
    while (i != (lista->fim + 1) % TAM_MAX) {
        int chaveAtual = item_get_chave(lista->lista[i]);
        if (chaveItem < chaveAtual) {
            return i; // Retorna a posição onde o item deve ser inserido
        }
        i = (i + 1) % TAM_MAX; // Move para o próximo índice, respeitando a circularidade
    }

    return i; // Retorna o fim da lista se a posição não foi encontrada
}
