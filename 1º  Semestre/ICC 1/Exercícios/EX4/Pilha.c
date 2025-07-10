#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "item.h"
#include "Pilha.h"

typedef struct pilha {
    ITEM *item[TAM];
    int topo;
} PILHA;

// Cria uma pilha 
PILHA* pilha_criar(void) {
    PILHA *P = (PILHA*)malloc(1* sizeof(PILHA));
    if (P == NULL) exit(1);
    P->topo = 0;
    return P;
}

// Desaloca a pilha
void pilha_apagar(PILHA** pilha) {
    if (pilha == NULL) exit(1);

    free(*pilha);
    *pilha = NULL;
}

// Verifica se a pilha está vazia
bool pilha_vazia(PILHA* pilha) {
    return (pilha->topo == 0);
}

// Verifica se a pilha está cheia 
bool pilha_cheia(PILHA* pilha) {
    return (pilha->topo == TAM);
}

// Retorna o tamanho da pilha 
int pilha_tamanho(PILHA* pilha) {
    if (pilha == NULL) exit(1);

    return TAM;
}

// Retorna o topo da pilha
ITEM* pilha_topo(PILHA* pilha) {
    if (pilha == NULL) exit(1);

    int chave = pilha->topo;
    int *b=malloc(1*sizeof(char*));
    *b = pilha->topo;

    ITEM *a = item_criar(chave, b);
    return a;
}

// Acrescenta um item no fim da pilha 
bool pilha_empilhar(PILHA* pilha, ITEM* item) {
    if (pilha == NULL) exit(1);
    
    if (pilha_cheia(pilha)) return false;

    pilha->item[pilha->topo] = item;
    pilha->topo++;
    return true;
}

// Decrementa um item da pilha 
ITEM* pilha_desempilhar(PILHA* pilha) {
    if ((pilha != NULL) && (!pilha_vazia(pilha))) {
        ITEM *i = pilha_topo(pilha);
        pilha->item[pilha->topo - 1] = NULL;
        pilha->topo--;
        return i;
    }
    return NULL;
}

//************************************************************************************************************** */
#include "Pilha.h"
#include <stdio.h>
#include <stdlib.h>

float rpn(char *sequencia) {
    int i = 0;
    float res;
    float *temp = (float*)malloc(sizeof(float));
    ITEM *num1 = NULL, *num2 = NULL, *resultado = NULL, *item = NULL;
    
    PILHA *pilha = pilha_criar();
    
    while (sequencia[i] != '\0') {
        if (sequencia[i] >= '0' && sequencia[i] <= '9') {
            *temp = sequencia[i] - '0';
            item = item_criar(i, temp);
            pilha_empilhar(pilha, item);
        } else {

            num1 = pilha_desempilhar(pilha);
            float *chave1 = item_get_dados(num1);
            printf("chave1 %.2f", *chave1);

            num2 = pilha_desempilhar(pilha);
            float *chave2 = item_get_dados(num2);
            printf("chave2 %.2f", *chave2);

            switch (sequencia[i]) {
                case '+':
                    //printf("chave1 %f e chave 2 %f", *chave1, *chave2);
                    res = *chave2 + *chave1;
                    //printf("res %f", res);
                    break;
                case '-':
                    res = *chave2 - *chave1;
                    break;
                case '*':
                    res = *chave2 * *chave1;
                    break;
                case '/':
                    if (*chave1 != 0) {
                        res = *chave2 / *chave1;
                    } else {
                        printf("Erro: divisão por zero\n");
                        pilha_apagar(&pilha);
                        item_apagar(&num1);
                        item_apagar(&num2);
                        return -1;
                    }
                    break;
                default:
                    printf("Erro: operador desconhecido '%c'\n", sequencia[i]);
                    pilha_apagar(&pilha);
                    item_apagar(&num1);
                    item_apagar(&num2);
                    return -1;
            }
            *temp = res;
            resultado = item_criar(i, temp);
            pilha_empilhar(pilha, resultado);
            item_apagar(&num1);
            item_apagar(&num2);
        }
        i++;
    }

    resultado = pilha_desempilhar(pilha);
    if (pilha_vazia(pilha) && resultado != NULL) {
        temp = item_get_dados(resultado);
        res = *temp;
    } else {
        printf("Erro: expressão inválida\n");
        res = -1;
    }

    item_apagar(&resultado);
    pilha_apagar(&pilha);
    free (temp);
    return res;
}
