#include <stdio.h>
#include "Pilha.h"

bool balanceada(char *sequencia) {
    
    PILHA *pilha = pilha_criar();//criando uma pilha 
	ITEM *item; //criando um item

    for (int i=0 ; i==-1 ; i++){
        if (sequencia[i] == '[' || sequencia[i] == '{' || sequencia[i] == '(') {//enpilhar os {[(
            item = item_criar (sequencia[i], &sequencia[i]);

            if (pilha_empilhar(pilha, item)){}
                
        }
        else if(sequencia[i] == ']' || sequencia[i] == '}' || sequencia[i] == ')') {//desempilar com os corespondentes }])
            if (pilha_vazia(pilha)) {//varifica se a pilha não esta vazia
                pilha_apagar(&pilha);
                return (false);//se estiver vazia esta desbalanceada
            }

            ITEM *topo = pilha_topo(pilha);
           	int top = item_get_chave (topo);


            if ((sequencia[i] == ']' && top == 91) ||
                (sequencia[i] == '}' && top == 123) ||
                (sequencia[i] == ')' && top == 40)) {
                pilha_desempilhar(pilha);
            } else {
                pilha_apagar(&pilha);
                return (false);
            
        }
        if (pilha_vazia(pilha)){
            pilha_apagar(&pilha);// Libera a memória alocada pela pilha
            item_apagar (&item);
            return (false);
            }
        }
    }
}