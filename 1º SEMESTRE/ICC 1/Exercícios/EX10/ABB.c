#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "ABB.h"
#include "item.h"

typedef struct No_ {
    ITEM *item;
    struct No_ *esq;
    struct No_ *dir;
} NO;

typedef struct abb {
    NO *raiz;
    int profundidade;
} ABB;

// Cria uma árvore vazia retornando NULL
ABB *abb_criar(void) {
    ABB *abb = (ABB *)malloc(sizeof(ABB));
    if (abb != NULL) {
        abb->profundidade = -1;
        abb->raiz = NULL;
    }
    return abb;
}

// Função auxiliar para criar um novo nó na árvore
NO *abb_criar_no(ITEM *item) {
    NO *novo = (NO *)malloc(sizeof(NO));
    if (novo) {
        novo->item = item;
        novo->esq = NULL;
        novo->dir = NULL;
    }
    return novo;
}

// Inserir nó recursivamente na árvore
NO *abb_inserir_no(NO *raiz, NO *no) {
    //Chama recurcivamente até achar um NULL para inserir
    if (raiz == NULL) {
        return no;
    }

    int chave_raiz = item_get_chave(raiz->item);
    int chave_no = item_get_chave(no->item);

    // Verifica para incerir a direita ou a esqueda
    if (chave_no < chave_raiz) {
        raiz->esq = abb_inserir_no(raiz->esq, no);
    } else if (chave_no > chave_raiz) {
        raiz->dir = abb_inserir_no(raiz->dir, no);
    }
    return raiz;
}

bool abb_inserir(ABB *T, ITEM *item) {
    if (T == NULL) {
        return false;
    }

    NO *novo_no = abb_criar_no(item);
    if (novo_no != NULL) {
        T->raiz = abb_inserir_no(T->raiz, novo_no);
        return true;
    }
    return false;
}

void abb_imprimir_no(NO *no) {
    if (no == NULL) return;
    abb_imprimir_no(no->esq);
    printf("%d, ", item_get_chave(no->item));
    abb_imprimir_no(no->dir);
}

void abb_imprimir(ABB *T) {
    if (T == NULL) return;
    abb_imprimir_no(T->raiz);
    printf("\n");
}

// Função auxiliar para calcular a altura de um nó
int calcular_altura(NO *no) {
    if (no == NULL) return -1;

    int altura_esq = calcular_altura(no->esq);
    int altura_dir = calcular_altura(no->dir);

    return 1 + (altura_esq > altura_dir ? altura_esq : altura_dir);
}

// Verifica se a árvore é balanceada recursivamente
bool verificar_balanceamento(NO *no) {
    if (no == NULL) return true;

    int altura_esq = calcular_altura(no->esq);
    int altura_dir = calcular_altura(no->dir);

    if (abs(altura_esq - altura_dir) > 1) {
        return false;
    }

    return verificar_balanceamento(no->esq) && verificar_balanceamento(no->dir);
}

// Implementa a função pedida
bool abb_perfeitamente_balanceada(ABB *T) {
    if (T == NULL || T->raiz == NULL) {
        return true;
    }
    return verificar_balanceamento(T->raiz);
}

// Libera recursivamente a memória da árvore
void abb_apagar_no(NO **no) {
    if (*no == NULL) return;

    abb_apagar_no(&(*no)->esq);
    abb_apagar_no(&(*no)->dir);

    item_apagar(&(*no)->item);
    free(*no);
    *no = NULL;
}

void abb_apagar(ABB **T) {
    if (*T == NULL) return;
    abb_apagar_no(&(*T)->raiz);
    free(*T);
    *T = NULL;
}

// Função auxiliar para encontrar e substituir o maior elemento na subárvore esquerda
void troca_max_esq(NO *sub_arvore, NO *pai, NO *no) {
    if (sub_arvore->dir != NULL) {
        troca_max_esq(sub_arvore->dir, sub_arvore, no);
    } else {
        // Substitui o item do nó a ser removido pelo maior item da subárvore esquerda
        ITEM *temp = no->item;
        no->item = sub_arvore->item;
        sub_arvore->item = temp;

        // Remove o nó substituído
        if (pai->dir == sub_arvore) {
            pai->dir = sub_arvore->esq;
        } else {
            pai->esq = sub_arvore->esq;
        }

        free(sub_arvore);
    }
}

// Função auxiliar para remoção recursiva
bool abb_remover_aux(NO **raiz, int chave) {
    NO *p;
    if (*raiz == NULL) {
        return false;
    }

    if (chave == item_get_chave((*raiz)->item)) {
        // Caso 1 e 2: nó com zero ou um filho
        if ((*raiz)->esq == NULL || (*raiz)->dir == NULL) {
            p = *raiz;
            if ((*raiz)->esq == NULL) {
                *raiz = (*raiz)->dir;
            } else {
                *raiz = (*raiz)->esq;
            }
            item_apagar(&p->item);
            free(p);
        } else {
            // Caso 3: nó com dois filhos
            troca_max_esq((*raiz)->esq, *raiz, *raiz);
        }
        return true;
    } else if (chave < item_get_chave((*raiz)->item)) {
        return abb_remover_aux(&(*raiz)->esq, chave);
    } else {
        return abb_remover_aux(&(*raiz)->dir, chave);
    }
}

// Função principal para remoção de um nó na árvore
bool abb_remover(ABB *T, int chave) {
    if (T != NULL) {
        return abb_remover_aux(&T->raiz, chave);
    }
    return false;
}

ITEM *abb_busca(ABB *T, int chave) {
    NO *atual = T->raiz;
    while (atual != NULL) {
        int chave_atual = item_get_chave(atual->item);
        if (chave == chave_atual) {
            return atual->item;
        } else if (chave < chave_atual) {
            atual = atual->esq;
        } else {
            atual = atual->dir;
        }
    }
    return NULL;
}

bool abb_vazia(ABB *T) {
    return T == NULL || T->raiz == NULL;
}
