#include "item.h"
#include "Pilha.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main (){
    int n;
    char* sequencia = (char*)malloc(100 * sizeof(char));
    if (sequencia == NULL) {
        printf("Erro ao alocar memória.\n");
        return -1;
    }

    printf("Quantas sequências: ");
    if (scanf(" %d", &n) != 1) {
        printf("Erro na leitura do número de sequências.\n");
        free(sequencia);
        return -1;
    }

    for (int i = 0; i < n; i++) {
        printf("Digite a sequência: ");
        if (scanf("%99s", sequencia) != 1) {
            printf("Erro na leitura da sequência.\n");
            free(sequencia);
            return -1;
        }

        float res = rpn(sequencia);
        printf("O resultado é: %f\n\n", res);
    }

    free(sequencia);
    return 0;
}
