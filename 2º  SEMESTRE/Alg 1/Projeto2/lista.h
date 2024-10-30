#ifndef LISTA_H
    #define LISTA_H
    #include <stdbool.h>

    typedef struct Lista_ LISTA;

    void Lista_Apagar (LISTA *lista); 
    void Lista_Remover (LISTA *lista);  
    void Lista_Imprimir (LISTA *lista);
    LISTA* Lista_Criar (int tamanho_dados);
    void Lista_Inserir (LISTA* lista, int elemento);

    bool Lista_Pertence (LISTA *lista, int elemento);
    LISTA* Lista_Uniao (LISTA* lista_A,LISTA* lista_B); 
    LISTA* Lista_Interseccao (LISTA* lista_A,LISTA* lista_B);

#endif 
