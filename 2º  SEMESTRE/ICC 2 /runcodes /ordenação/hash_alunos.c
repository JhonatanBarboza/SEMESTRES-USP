#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TAMANHO_TABELA 1031  // Tamanho da tabela hash

// Estrutura para armazenar cada entrada na lista encadeada
typedef struct No {
    int posicao;
    char nome[26];
    struct No *proximo;
} No;

// Tabela hash
No *tabela[TAMANHO_TABELA];

// Função hash para calcular o índice com base na posição
int funcao_hash(int posicao) {
    return posicao % TAMANHO_TABELA;
}

// Função para inserir um par (posição, nome) na tabela hash
void inserir(int posicao, const char *nome) {
    int indice = funcao_hash(posicao);
    No *novo_no = (No *)malloc(sizeof(No));
    novo_no->posicao = posicao;
    strncpy(novo_no->nome, nome, 25);
    novo_no->nome[25] = '\0';  // Garantir terminação nula
    novo_no->proximo = tabela[indice];
    tabela[indice] = novo_no;
}

// Função para buscar o nome de um aluno pela posição
const char *buscar(int posicao) {
    int indice = funcao_hash(posicao);
    No *atual = tabela[indice];
    while (atual != NULL) {
        if (atual->posicao == posicao) {
            return atual->nome;
        }
        atual = atual->proximo;
    }
    return NULL;  // Retorna NULL se não encontrar a posição
}

int main() {
    int n_alunos;
    scanf("%d", &n_alunos);

    // Inicializar a tabela hash
    for (int i = 0; i < TAMANHO_TABELA; i++) {
        tabela[i] = NULL;
    }

    // Inserir cada aluno na tabela hash
    for (int i = 0; i < n_alunos; i++) {
        int posicao;
        char nome[26];
        scanf("%d %s", &posicao, nome);
        inserir(posicao, nome);
    }

    // Consultas
    int n_consultas;
    scanf("%d", &n_consultas);

    for (int i = 0; i < n_consultas; i++) {
        int posicao;
        scanf("%d", &posicao);
        const char *resultado = buscar(posicao);
        if (resultado != NULL) {
            printf("%s\n", resultado);
        } else {
            printf("Posição %d não encontrada.\n", posicao);
        }
    }

    // Liberar memória
    for (int i = 0; i < TAMANHO_TABELA; i++) {
        No *atual = tabela[i];
        while (atual != NULL) {
            No *temp = atual;
            atual = atual->proximo;
            free(temp);
        }
    }

    return 0;
}


