#include "conjunto.h"

int main() {
    int TAD, tamanho_A, tamanho_B, operacao, elemento;
    
    // Leitura do tipo de TAD, tamanho do conjunto A e tamanho do conjunto B
    scanf("%d %d %d", &TAD, &tamanho_A, &tamanho_B);
    int conjunto_A[tamanho_A], conjunto_B[tamanho_B];

    // Leitura dos elementos do conjunto A
    for (int i = 0; i < tamanho_A; i++) {
        scanf("%d", &conjunto_A[i]);   
    }
    
    // Leitura dos elementos do conjunto B
    for (int i = 0; i < tamanho_B; i++) { 
        scanf("%d", &conjunto_B[i]);
    }

    // Leitura da operação a ser realizada (1: Pertencimento, 2: União, 3: Interseção)
    scanf("%d", &operacao);
    if (operacao == 1) {  // Se a operação for de pertencimento, lê o elemento
        scanf("%d", &elemento);
    }

    // Seleção do tipo de TAD para a operação: 0 para lista, 1 para árvore
    if (TAD == 0) {
        switch (operacao) {
            case 1:
                Conjunto_Pertence_0(conjunto_A, tamanho_A, elemento);  // Verifica pertencimento usando lista
                break;
            case 2:
                Conjunto_Uniao_0(conjunto_A, tamanho_A, conjunto_B, tamanho_B);  // União de conjuntos usando lista
                break;
            case 3:
                Conjunto_Intersecao_0(conjunto_A, tamanho_A, conjunto_B, tamanho_B);  // Interseção de conjuntos usando lista
                break;
            default:
                printf("Comando inválido!\n");  // Tratamento de erro para operação inválida
                return 1;
        }

    } else if (TAD == 1) {
        switch (operacao) {
            case 1:
                Conjunto_Pertence_1(conjunto_A, tamanho_A, elemento);  // Verifica pertencimento usando árvore
                break;
            case 2:
                Conjunto_Uniao_1(conjunto_A, tamanho_A, conjunto_B, tamanho_B);  // União de conjuntos usando árvore
                break;
            case 3:
                Conjunto_Intersecao_1(conjunto_A, tamanho_A, conjunto_B, tamanho_B);  // Interseção de conjuntos usando árvore
                break;
            default:
                printf("Comando inválido!\n");  // Tratamento de erro para operação inválida
                return 1;
        }
    }

    return 0;
}
