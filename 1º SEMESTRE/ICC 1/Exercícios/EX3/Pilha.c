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
    PILHA *P = (PILHA*)calloc(1, sizeof(PILHA));
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

// Função de balanceamento (certifique-se de que ela retorna um valor)
bool balanceada(char *sequencia) {

    PILHA *pilha = pilha_criar();//criando uma pilha 
	ITEM *item; //criando um item

    for (int i=0 ; i<1000 ; i++){

        if (sequencia[i] == '[' || sequencia[i] == '{' || sequencia[i] == '(') {//enpilhar os {[(
            
            item = item_criar (sequencia[i], &sequencia[i]);

            if (pilha_empilhar(pilha, item)){}
                
        }
        else if(sequencia[i] == ']' || sequencia[i] == '}' || sequencia[i] == ')') {//desempilar com os corespondentes }])

            if (pilha_vazia(pilha)) {//varifica se a pilha não esta vazia
                pilha_apagar(&pilha);
                printf("pri");
                return (false);//se estiver vazia esta desbalanceada
            }

            ITEM *top = pilha_topo(pilha);
           	//int top = item_get_chave (topo);

            int a = 1;    
            if ((sequencia[i] == ']' && top->topo == '[') ||
                (sequencia[i] == '}' && top->topo == '{') ||
                (sequencia[i] == ')' && top->topo == '(')) {
                pilha_desempilhar(pilha);
                a++;
            } else {
                pilha_apagar(&pilha);
                printf("seg");
                return (false);
            
        }
        if (pilha_vazia(pilha)){
            pilha_apagar(&pilha);// Libera a memória alocada pela pilha
            item_apagar (&item);
            printf("ter");
            return (true);
            }
        }
    }
}

