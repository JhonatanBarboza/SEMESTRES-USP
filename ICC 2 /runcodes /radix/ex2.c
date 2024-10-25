#include <stdio.h>
#include <stdlib.h>

// Função auxiliar para encontrar o maior valor absoluto no vetor
int maior_valor_absoluto(int v[], int tam) {
    int max = abs(v[0]);
    for (int i = 1; i < tam; i++) {
        if (abs(v[i]) > max)
            max = abs(v[i]);
    }
    return max;
}

// Função de contagem para o Radix Sort (por dígitos)
void counting_sort(int v[], int tam, int exp) {
    int output[tam];
    int count[10] = {0};

    for (int i = 0; i < tam; i++) {
        count[(abs(v[i]) / exp) % 10]++;
    }

    for (int i = 1; i < 10; i++)
        count[i] += count[i - 1];

    for (int i = tam - 1; i >= 0; i--) {
        output[count[(abs(v[i]) / exp) % 10] - 1] = v[i];
        count[(abs(v[i]) / exp) % 10]--;
    }

    for (int i = 0; i < tam; i++)
        v[i] = output[i];
}

// Função de Radix Sort para inteiros
void radix_sort(int v[], int tam) {
    int max = maior_valor_absoluto(v, tam);
    for (int exp = 1; max / exp > 0; exp *= 10)
        counting_sort(v, tam, exp);
}

// Função principal para lidar com números negativos e não-negativos
void radix_sort_negativos(int v[], int tam) {
    int negativos[tam], positivos[tam];
    int neg_count = 0, pos_count = 0;

    // Separar números negativos e positivos
    for (int i = 0; i < tam; i++) {
        if (v[i] < 0)
            negativos[neg_count++] = v[i];
        else
            positivos[pos_count++] = v[i];
    }

    // Ordena os números negativos e positivos separadamente
    radix_sort(positivos, pos_count);

    // Inverte a ordem dos números negativos e aplica radix sort nos valores absolutos
    for (int i = 0; i < neg_count; i++)
        negativos[i] = -negativos[i];  // Faz o valor negativo positivo para ordenação
    radix_sort(negativos, neg_count);
    for (int i = 0; i < neg_count; i++)
        negativos[i] = -negativos[i];  // Volta a ser negativo

    // Combina negativos e positivos no vetor original
    for (int i = 0; i < neg_count; i++)
        v[i] = negativos[neg_count - i - 1];  // Coloca negativos em ordem decrescente
    for (int i = 0; i < pos_count; i++)
        v[neg_count + i] = positivos[i];  // Coloca positivos em ordem crescente
}

int main() {
    int N;
    
    // Lê o número de elementos
    scanf("%d", &N);
    int vet[N];

    // Lê os elementos do vetor
    for (int i = 0; i < N; i++) {
        scanf("%d", &vet[i]);
    }

    // Aplica o Radix Sort modificado
    radix_sort_negativos(vet, N);

    // Imprime o vetor ordenado
    for (int i = 0; i < N; i++) {
        printf("%d ", vet[i]);
    }
    printf("\n");

    return 0;
}
