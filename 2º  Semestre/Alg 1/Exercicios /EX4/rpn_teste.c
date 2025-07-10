#include "Pilha.h"

float rpn(char *sequencia){
    int i=0;
    float res=0, *pont=0; 
    ITEM *num1, *num2, *resultado;

    PILHA*pilha = pilha_criar();

    while(1){

        if (sequencia[i] == '\0'){//verifica se é o final da string 
            resultado = pilha_desempilhar(pilha); //desmpilha o resultado
            pont = item_get_dados (resultado);//recuperando a resposta final
            res = *pont;
            return (res); //retornando o resultado            
        }
        //se for um numero irei empilhar 
        if (sequencia[i] == '0' || sequencia[i] == '1' ||  sequencia[i] == '2' || sequencia[i] == '3' || sequencia[i] == '4' || sequencia[i] == '5' || sequencia[i] == '6' || sequencia[i] == '7' || sequencia[i] == '8' || sequencia[i] == '9'){
            float temp = sequencia[i] - '0'; //convertendo de char para int 
            float *p = &temp;
            ITEM *item = item_criar (temp, p); //criando um item para empilhar 
            pilha_empilhar(pilha, item); //empilhando o numero
        }
        //se for uma operação desmpilho os dois ultimos numeros faz a operação e empilha o resultado
        else {
            num1 = pilha_desempilhar(pilha);//desempilhando os dois ultimos numeros para fazer a operação
            num2 = pilha_desempilhar(pilha);

           	float *chave1 = item_get_dados (num1);//recuperando os numeros 
           	float *chave2 = item_get_dados (num2);

            switch (sequencia[i]) {
                case '+':
                    res = *chave2+*chave1; //fazendo a operação
                    pont = &res;
                    resultado = item_criar (res, pont);//criar um item para empilhar o resultado 
                    pilha_empilhar(pilha, resultado);
                    break;

                case '-':
                    res = *chave2-*chave1; //fazendo a operação
                    pont = &res;
                    resultado = item_criar (res, pont);//criar um item para empilhar o resultado 
                    pilha_empilhar(pilha, resultado);
                    break;

                case '*':
                    res = *chave2**chave1; //fazendo a operação
                    pont = &res;
                    resultado = item_criar (res, pont);//criar um item para empilhar o resultado 
                    pilha_empilhar(pilha, resultado);
                    break;

                case '/':
                    res = *chave2 / *chave1; //fazendo a operação
                    pont = &res;
                    resultado = item_criar (res, pont);//criar um item para empilhar o resultado 
                    pilha_empilhar(pilha, resultado);
                    break;

                default:
                    return -1;

            }
        }
    i++;
    }
}


