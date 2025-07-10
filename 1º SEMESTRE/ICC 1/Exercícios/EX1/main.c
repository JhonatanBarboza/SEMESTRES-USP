#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "ponto.h"
#include "circulo.h"

int main() {

    PONTO *p;
    CIRCULO *c;
    float x, y, X, Y, raio;

    // Lê os valores de x e y
    scanf("%f %f", &x, &y);

    // Lê os valores de X, Y e raio
    scanf("%f %f %f", &X, &Y, &raio);

    // Cria um ponto
    p = ponto_criar(x, y);

    // Define o ponto com x e y
    ponto_set(p, x, y);

    // Obtém os valores de x e y do ponto e imprime
    ponto_get_x(p);
    ponto_get_y(p);
    ponto_print(p);

    // Redefine o ponto com novos valores de X e Y
    ponto_set(p, X, Y);

    // Cria um círculo com o ponto p e o raio
    c = circulo_criar(p, raio);
    
    // Define o raio e o ponto do círculo
    circulo_set_raio(c, raio);
    circulo_set_ponto(c, p);

    // Imprime as informações do círculo
    printf("Circulo: Centro (%.1f, %.1f), Raio = %.1f\n", X, Y, raio);
    
    // Apaga o ponto e o círculo da memória
    ponto_apagar(&p);   // Libera a memória do ponto
    circulo_apagar(&c); // Libera a memória do círculo

    return 0;
}
