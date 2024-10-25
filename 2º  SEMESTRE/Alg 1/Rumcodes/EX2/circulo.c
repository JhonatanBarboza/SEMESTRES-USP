#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "ponto.h"
#include "circulo.h"


struct circulo_{//define o tipo circulo
    PONTO* p;
    float raio;
};

CIRCULO *circulo_criar(PONTO *p, float raio) {//aloca memoria para a variavel do tipo circulo
    CIRCULO* cir;

    cir = (CIRCULO*)malloc(sizeof(CIRCULO));

    if (cir == NULL) {//se não for possivel alocar o programa se encera 
        exit(1);
    }

    cir->p = p;
    cir->raio = raio;

    return cir;   
}

bool circulo_set_ponto(CIRCULO *c, PONTO *p) {//atualiza a variavel p
    if (p == NULL || c == NULL) {
        return false;
    }
    
    c->p = p;

    return true;
}

bool circulo_set_raio(CIRCULO *c, float raio) {//atualiza a variavel raio
    if (c == NULL) {
        return false;
    }
    
    c->raio = raio;
    return true;
}

PONTO *circulo_get_ponto(CIRCULO *c) {//retorna a variavel p
    if (c != NULL) {
        return c->p;
    }
    exit(1);
}

float circulo_get_raio(CIRCULO *c) {//retorna a variavel raio
    if (c != NULL) {
        return c->raio;
    }
    exit(1);
}

void circulo_apagar(CIRCULO **c) {//desaloca o ponteiro c e faz ele apontar para NULL
    if (*c != NULL) {
        free(*c);
    }
    *c = NULL;
}
