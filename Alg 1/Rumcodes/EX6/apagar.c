#include <stdio.h>
#include <stdlib.h>
#include "fila.h"

#ifndef _STRUCTS_
    #define _STRUCTS_

    typedef struct no_ {
    ITEM* item;
    struct no_* proximo;
    }noObj;

    typedef noObj NO;

    typedef struct fila_ {
    NO* inicio;
    NO* fim;
    int tamanho;
    }filaObj;

    typedef filaObj FILA;
#endif

void fila_apagar(FILA **f);

void fila_apagar(FILA **f) {

    // Verifica se a fila existe e se não está vazia
    if (f != NULL && *f != NULL && (*f)->inicio != NULL) {  
        item_apagar((&(*f)->inicio->item));         // Libera o item associado ao nó
        free (fila_remover(*f));
        fila_apagar(f);                       // Chama recursivamente para apagar o próximo nó
    } else {
        free(*f);                             // Libera a estrutura da fila quando estiver vazia
        *f = NULL;                            // Evita ponteiros soltos
        return;
    }
}

   

