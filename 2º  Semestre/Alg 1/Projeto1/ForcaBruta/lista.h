#ifndef LISTA_H
    #define LISTA_H

    typedef struct Lista_ LISTA;
    typedef struct INDICE_ INDICE;

    void Lista_Apagar(LISTA *lista); 
    void Lista_Remover(LISTA *lista);  
    LISTA* Lista_Criar(int tamanho_dados);
    int Lista_Consultar(LISTA *lista, int indice, char campo);
    void Lista_Inserir(LISTA* lista, int cidadeA, int cidadeB, int distancia);

#endif 
