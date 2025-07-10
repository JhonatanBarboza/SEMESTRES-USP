#include "Pilha.h"
#include <stdio.h>
#include <stdlib.h>

float rpn(char *sequencia) {
    int i = 0; //declaração de variaveis
    float res;
    float *chave1, *chave2, *temp;
    ITEM *num1 = NULL, *num2 = NULL, *resultado = NULL, *item = NULL;
    
    PILHA *pilha = pilha_criar(); //cria a pilha 
    
    while (sequencia[i] != '\0') {
        if (sequencia[i] >= '0' && sequencia[i] <= '9') {
            temp = (float*)malloc(sizeof(float)); //toda interação ele é alocado para compor o proximo item
            *temp = sequencia[i] - '0';
            item = item_criar(i, temp);
            pilha_empilhar(pilha, item);
            
        } else {

            num1 = pilha_topo(pilha); //desempilha dois numeros para fazer a operação
            float *chave1 = item_get_dados(num1);
            pilha_desempilhar(pilha);

            num2 = pilha_topo(pilha);
            float *chave2 = item_get_dados(num2);
            pilha_desempilhar(pilha);

            switch (sequencia[i]) { //calcula e atribui a res 
                case '+':
                    res = *chave2 + *chave1;
                    break;
                case '-':
                    res = *chave2 - *chave1;
                    break;
                case '*':
                    res = *chave2 * *chave1;
                    break;
                case '/':
                    if (*chave1 != 0) {//veriafica divisão por zero
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

    resultado = pilha_desempilhar(pilha); //desempilha pela ultima vez
    if (pilha_vazia(pilha) && resultado != NULL) {
        temp = item_get_dados(resultado);
        res = *temp;
    } else {
        printf("Erro: expressão inválida\n");
        res = -1;
    }

    item_apagar(&resultado); 
    pilha_apagar(&pilha); //apaga apilha 
    free (temp);
    return res;
}
