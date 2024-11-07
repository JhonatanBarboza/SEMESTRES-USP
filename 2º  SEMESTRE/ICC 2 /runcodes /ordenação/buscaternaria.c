#include <stdio.h>
#include <stdlib.h>

void quicksort(int *vet, int esq, int dir) { //vetor a ser ordenado, esquerda (começo) do vet, direita (fim) do vet
    int i = esq, j = dir, x = vet[(esq + dir) / 2], y; //auxiliares

    while(i <= j){ //paia de explicar, fica a cargo do leitor
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

int buscaternaria(int *vet, int inicio, int fim, int chave){
    if(inicio <= fim){
        int meio1 = inicio + (fim - inicio) / 3;
        int meio2 = fim - (fim - inicio) / 3;
        
        if (vet[meio1] == chave)
            return meio1;  //encontrou a chave no primeiro pivô
        if (vet[meio2] == chave)
            return meio2;  //encontrou a chave no segundo pivô

        //se a chave for menor que o primeiro pivô, busca na primeira parte
        if (chave < vet[meio1])
            return buscaternaria(vet, inicio, meio1 - 1, chave);

        //se a chave estiver entre os dois pivôs, busca na segunda parte
        if(chave > vet[meio1] && chave < vet[meio2])
            return buscaternaria(vet, meio1 + 1, meio2 - 1, chave);

        //se a chave for maior que o segundo pivô, busca na terceira parte
        return buscaternaria(vet, meio2 + 1, fim, chave);
    }
    return -1;  //chave não encontrada
}

int main(void){
    int n, chave;
    scanf("%d", &n);
    int *vet = malloc(sizeof(int) * n);
    for(int i = 0; i < n; i++)
        scanf("%d ", &vet[i]);

    scanf("%d", &chave);
    quicksort(vet, 0, n - 1);
    printf("%d\n", buscaternaria(vet, 0, n - 1, chave));

    return 0;
}