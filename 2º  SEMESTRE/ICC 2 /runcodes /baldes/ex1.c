#include <stdio.h>

#define tam_bucket 100  // Número máximo de elementos por balde

// Estrutura para representar um balde (bucket)
typedef struct {
    int topo;               
    int balde[tam_bucket]; 
} bucket;

// Função para contar o número de bits iguais a 1 na representação binária de um número
int contar_bits_1(int num) {
    int cont = 0;
    while (num > 0) {
        cont += num % 2; 
        num = num / 2;
    }
    return cont;    
}

// Função para imprimir um vetor de tamanho 'tam'
void imprimir_vetor(int b[], int tam) {
    for (int i = 0; i < tam; i++) {
        printf("%d ", b[i]);
    }
    printf("\n");
}

// Função para realizar o bubble sort
void bubble_sort(int v[], int tam) {
    int i, j, temp, flag;
    for (j = 0; j < tam - 1; j++) {
        flag = 0;
        for (i = 0; i < tam - 1; i++) {
            if (v[i + 1] < v[i]) {  // Troca os elementos se estiverem fora de ordem
                temp = v[i];
                v[i] = v[i + 1];
                v[i + 1] = temp;
                flag = 1;
            }
        }
        if (!flag)  // Se não houve trocas, encerra o loop
            break;
    }
}

// Função para realizar o bucket sort
void bucket_sort(int v[], int tam) {
    bucket b[tam];  // Número máximo de baldes baseado no número de elementos
    int i, j, k;

    // Inicializa todos os baldes
    for (i = 0; i < tam; i++)
        b[i].topo = 0;

    // Coloca os elementos nos baldes com base no número de bits 1
    for (i = 0; i < tam; i++) {
        int bit_count = contar_bits_1(v[i]);  // Conta os bits 1 do elemento
        b[bit_count].balde[b[bit_count].topo] = v[i];  // Coloca no balde correspondente
        b[bit_count].topo++;
    }

    // Ordena cada balde e imprime os baldes
    for (i = 0; i < tam; i++) {
        if (b[i].topo > 0) {  // Apenas imprime baldes não vazios
            bubble_sort(b[i].balde, b[i].topo);  // Ordena os elementos do balde
            imprimir_vetor(b[i].balde, b[i].topo);  // Imprime o balde
        }
    }

    // Reorganiza os elementos dos baldes de volta no vetor original, em ordem crescente
    i = 0;
    for (j = 0; j < tam; j++) {
        // Percorre cada balde na ordem correta (balde 0, balde 1, balde 2, etc.)
        for (k = 0; k < b[j].topo; k++) {
            // Copia os elementos ordenados dos baldes para o vetor final
            v[i] = b[j].balde[k];  
            i++;
        }
    }
    bubble_sort(v, tam);  // Ordena os elementos do balde
}

int main() {
    int N;
    
    // Lê o número de elementos
    scanf("%d", &N);
    int vet[N];  // Inicializa o vetor com N elementos

    // Lê os elementos do vetor
    for (int i = 0; i < N; i++) {
        scanf("%d", &vet[i]);
    }

    // Chama a função de ordenação bucket sort
    bucket_sort(vet, N);

    // Imprime o vetor final ordenado
    imprimir_vetor(vet, N);

    return 0;
}
