#include <stdio.h>
#include <stdlib.h>

typedef struct{
    int *vet;
    int tam;
}BIN;

void quicksort(int *vet, int esq, int dir) { //vetor a ser ordenado, esquerda (começo) do vet, direita (fim) do vet
    int i = esq, j = dir, x = vet[(esq + dir) / 2], y; 

    while(i <= j){ 
        while(vet[i] < x && i < dir) i++;
        while(vet[j] > x && j > esq) j--;
        if(i <= j){
            y = vet[i];
            vet[i] = vet[j];
            vet[j] = y;
            i++;
            j--;
        }
    }
     
    if(j > esq) quicksort(vet, esq, j);
    if(i < dir) quicksort(vet, i, dir);
    
    return;
}

BIN decimalprabin(int num){
    int tam = 32; //tamanho maximo de inteiro, 32 bits
    BIN bin;
    bin.vet = calloc(sizeof(int), tam); //vetor binario começa tudo 0
    if(bin.vet == NULL) exit (1);

    for(int i = 0; i < tam && num > 0; i++){ 
        bin.vet[i] = num % 2; //pega o resto da divisao por 2 do nosso numero e guarda no bin
        num /= 2; 
        if(!num) tam = i + 1; //quando o numero for 0 retorno o tamanho novo do bagulho
    }

    bin.vet = realloc(bin.vet, sizeof(int) * tam);
    bin.tam = tam;
    return bin;
}

int contauns(BIN bin, int tam){
    int num = 0;
    for(int i = 0; i < tam; i++)
        if(bin.vet[i]) num++; //vou pegando a quantidade de 1's que tem no meu binario

    return num;
}

void printa(int *vet, int tam){
    for(int i = 0; i < tam; i++) printf("%d ", vet[i]);
    printf("\n");
    return;
}

void contaeprinta(int *vet, int n, int uns){ //funçao que vai contar o numero de 1's e printar
    if(uns == 32){
        quicksort(vet, 0, n - 1);
        printa(vet, n);
        return; //quando bater 32 (numero de bits maximo, entao numero maximo possivel de numeros 1) retorna
    }
    int *vet2 = malloc(sizeof(int) * n);
    int j = 0;
    for(int i = 0; i < n; i++){
        BIN bin = decimalprabin(vet[i]);
        if(contauns(bin, bin.tam) == uns){
            vet2[j] = vet[i];
            j++;
        }
    }

    if(j) quicksort(vet2, 0, j - 1); //ordena o bucket (caso o j nao seja 0, ou seja, tem o que ordenar)
    if(j) printa(vet2, j); //printa aquele balde ordenado
    contaeprinta(vet, n, uns + 1); //chamo pro proximo balde 
    return;
}

int main(void){
    int quant; //numero de entradas
    scanf("%d", &quant);

    int *vet = malloc(sizeof(int) * quant); //guarda os numeros de entrada
    for(int i = 0; i < quant; i++) scanf(" %d", &vet[i]);
    contaeprinta(vet, quant, 1);
    
    return 0;
}