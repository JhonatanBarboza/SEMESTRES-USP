#include "item.h"
#include "Pilha.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main (){
    int n;
    char* sequencia = NULL; 
    sequencia = (char*)malloc(1001 * sizeof(char));

    scanf(" %d", &n);

    int tam = strlen (sequencia);
    sequencia [tam] = '\0';

    for (int i=0; i<n ; i++){
        scanf(" %s", sequencia);
        if (balanceada(sequencia))
            printf("BALANCEADA\n");
        else 
            printf("não balanceada\n");
    }

    return 0;
}
