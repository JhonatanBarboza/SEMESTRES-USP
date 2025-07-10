#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "item.h"
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

void fila_apagar(FILA **fila);
void apagar_no(NO *no);

void fila_apagar(FILA **fila){
    apagar_no((*fila)->fim);
    
    free(*fila);
    *fila = NULL;
    return;
}

void apagar_no(NO *no){
    if(no->proximo != NULL){
        apagar_no(no->proximo);
        no->proximo = NULL;
    }

    if(no->proximo == NULL){
        item_apagar(&(no->item));
        no->item = NULL;
        
        free(no);
        //no = NULL;

        return;
    }
}