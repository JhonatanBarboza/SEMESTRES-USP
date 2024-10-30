#ifndef ARVORE_H
    #define ARVORE_H
    #include <stdbool.h>

    typedef struct Arvore_ ARVORE;

    void Arvore_Apagar (ARVORE* arvore); 
    void Arvore_Remover (ARVORE* arvore);  
    void Arvore_Imprimir (ARVORE* arvore);
    ARVORE* Arvore_Criar (int tamanho_dados);
    void Arvore_Inserir (ARVORE* arvore, int elemento);

    bool Arvore_Pertence (ARVORE* arvore, int elemento);
    ARVORE* Arvore_Uniao (ARVORE* arvore_A,ARVORE* arvore_B); 
    ARVORE* Arvore_Interseccao (ARVORE* arvore_A,ARVORE* arvore_B);

#endif 
