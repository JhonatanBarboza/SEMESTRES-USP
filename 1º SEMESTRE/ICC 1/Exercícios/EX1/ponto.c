#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "ponto.h"
#include "circulo.h"

    struct ponto_{
        float x;
        float y;
    };
    
    PONTO *ponto_criar (float x, float y){ //Faz a alocação do ponteiro para o ponto
        PONTO* pon;

        if ((pon = (PONTO* )malloc(sizeof(PONTO))) == NULL){
            exit(1);}
        
        return pon;
    }
    
    bool ponto_set (PONTO *p, float x, float y){//atualiza os valores de x e y
        if (p == NULL){
            return false;
        }

        p->x=x;
        p->y=y;

        return true;
    }

    float ponto_get_x (PONTO *p){//retorna x
        if (p != NULL){
            return (p->x);
        }
        exit(1);
    }
    
    float ponto_get_y (PONTO *p){//retorna y
        if (p != NULL){
            return (p->y);
        }
        exit(1);
    }

    void ponto_print (PONTO *p){//imprime o ponto 

        printf("Ponto: (%.1f, %.1f)\n", p->x, p->y);
    
    }

    void ponto_apagar (PONTO **p){//Apaga a memoria 
        if (*p != NULL)
            free(*p);
        *p = NULL;
    }