#include "conjunto.h"
#include "lista.h"
#include "arvore.h"

// Verifica se um elemento pertence ao conjunto A, implementado com listas
void Conjunto_Pertence_0(int* conjunto_A, int tamanho_A, int elemento) {
    LISTA* conjunto = Lista_Criar(tamanho_A); // Cria a lista para o conjunto A

    for (int i = 0; i < tamanho_A; i++) {
        Lista_Inserir(conjunto, conjunto_A[i]); // Insere cada elemento de conjunto_A na lista
    }

    // Verifica se o elemento está na lista e imprime o resultado
    if (Lista_Pertence(conjunto, elemento)) {
        printf("Pertence\n");
    } else {
        printf("Não pertence\n");
    }

    Lista_Apagar(conjunto); // Libera a memória da lista
}

// Realiza a união entre dois conjuntos A e B, implementados com listas
void Conjunto_Uniao_0(int* conjunto_A, int tamanho_A, int* conjunto_B, int tamanho_B) {
    LISTA* lista_A = Lista_Criar(tamanho_A);
    LISTA* lista_B = Lista_Criar(tamanho_B);

    // Preenche lista_A e lista_B com os elementos dos conjuntos A e B
    for (int i = 0; i < tamanho_A; i++) {
        Lista_Inserir(lista_A, conjunto_A[i]);
    }
    for (int i = 0; i < tamanho_B; i++) {
        Lista_Inserir(lista_B, conjunto_B[i]);
    }

    // Cria e imprime a união das listas
    LISTA* uniao = Lista_Uniao(lista_A, lista_B);
    Lista_Imprimir(uniao);

    // Libera a memória das listas
    Lista_Apagar(lista_A);
    Lista_Apagar(lista_B);
    Lista_Apagar(uniao);
}

// Realiza a interseção entre dois conjuntos A e B, implementados com listas
void Conjunto_Intersecao_0(int* conjunto_A, int tamanho_A, int* conjunto_B, int tamanho_B) {
    LISTA* lista_A = Lista_Criar(tamanho_A);
    LISTA* lista_B = Lista_Criar(tamanho_B);

    // Preenche lista_A e lista_B com os elementos dos conjuntos A e B
    for (int i = 0; i < tamanho_A; i++) {
        Lista_Inserir(lista_A, conjunto_A[i]);
    }
    for (int i = 0; i < tamanho_B; i++) {
        Lista_Inserir(lista_B, conjunto_B[i]);
    }

    // Cria e imprime a interseção das listas
    LISTA* intersecao = Lista_Interseccao(lista_A, lista_B);
    Lista_Imprimir(intersecao);

    // Libera a memória das listas
    Lista_Apagar(lista_A);
    Lista_Apagar(lista_B);
    Lista_Apagar(intersecao);
}

// Verifica se um elemento pertence ao conjunto A, implementado com árvores
void Conjunto_Pertence_1(int* conjunto_A, int tamanho_A, int elemento) {
    ARVORE* conjunto = Arvore_Criar(tamanho_A); // Cria a árvore para o conjunto A

    for (int i = 0; i < tamanho_A; i++) {
        Arvore_Inserir(conjunto, conjunto_A[i]); // Insere cada elemento de conjunto_A na árvore
    }

    // Verifica se o elemento está na árvore e imprime o resultado
    if (Arvore_Pertence(conjunto, elemento)) {
        printf("Pertence\n");
    } else {
        printf("Não pertence\n");
    }

    Arvore_Apagar(conjunto); // Libera a memória da árvore
}

// Realiza a união entre dois conjuntos A e B, implementados com árvores
void Conjunto_Uniao_1(int* conjunto_A, int tamanho_A, int* conjunto_B, int tamanho_B) {
    ARVORE* Arvore_A = Arvore_Criar(tamanho_A);
    ARVORE* Arvore_B = Arvore_Criar(tamanho_B);

    // Preenche Arvore_A e Arvore_B com os elementos dos conjuntos A e B
    for (int i = 0; i < tamanho_A; i++) {
        Arvore_Inserir(Arvore_A, conjunto_A[i]);
    }
    for (int i = 0; i < tamanho_B; i++) {
        Arvore_Inserir(Arvore_B, conjunto_B[i]);
    }

    // Cria e imprime a união das árvores
    ARVORE* uniao = Arvore_Uniao(Arvore_A, Arvore_B);
    Arvore_Imprimir(uniao);

    // Libera a memória das árvores
    Arvore_Apagar(Arvore_A);
    Arvore_Apagar(Arvore_B);
    Arvore_Apagar(uniao);
}

// Realiza a interseção entre dois conjuntos A e B, implementados com árvores
void Conjunto_Intersecao_1(int* conjunto_A, int tamanho_A, int* conjunto_B, int tamanho_B) {
    ARVORE* Arvore_A = Arvore_Criar(tamanho_A);
    ARVORE* Arvore_B = Arvore_Criar(tamanho_B);

    // Preenche Arvore_A e Arvore_B com os elementos dos conjuntos A e B
    for (int i = 0; i < tamanho_A; i++) {
        Arvore_Inserir(Arvore_A, conjunto_A[i]);
    }
    for (int i = 0; i < tamanho_B; i++) {
        Arvore_Inserir(Arvore_B, conjunto_B[i]);
    }

    // Cria e imprime a interseção das árvores
    ARVORE* intersecao = Arvore_Interseccao(Arvore_A, Arvore_B);
    Arvore_Imprimir(intersecao);

    // Libera a memória das árvores
    Arvore_Apagar(Arvore_A);
    Arvore_Apagar(Arvore_B);
    Arvore_Apagar(intersecao);
}