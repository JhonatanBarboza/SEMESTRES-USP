#ifndef STACK_H
    #define STACK_H

    typedef struct pilha_ PILHA;
    typedef struct no_ NO;

    PILHA *pilha_criar();
    void pilha_empilhar(PILHA *pilha, int dado);
    int pilha_desempilhar(PILHA *pilha);
    void pilha_desempilha_tudo(PILHA *pilha);
    bool pilha_vazia (PILHA *pilha);
    bool pilha_cheia (PILHA *pilha);
    void pilha_apagar(PILHA **pilha);
    void pilha_imprimir(PILHA *pilha);
    void pilha_desempilha_imprime(PILHA *pilha);
    void pilha_inverter(PILHA *pilha);


#endif