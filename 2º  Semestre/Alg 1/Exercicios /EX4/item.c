#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>


	typedef struct item_ {
        int chave;
		void *dados;
    }ITEM;

//Cria um item, coloca a chave passada nele e aponta os dados para NULL 
	ITEM *item_criar(char chave){

		ITEM *item = (ITEM*)calloc( 1 , sizeof(ITEM));

		if (item == NULL)
			return (NULL);

		item -> chave = chave;
		item -> dados = NULL;

		return (item);
	}

//Desloca o **item 
	bool item_apagar(ITEM **item){

		if (*item == NULL)
			return (false);

		free (*item);
		item = NULL;
		return (true);
	}

//imprime o *item
	void item_imprimir(ITEM *item){

		if (item == NULL)
			exit(1);

		printf("%d", item -> chave);
	}

//retorna a chave do item
	char item_get_chave(ITEM *item){

		if (item == NULL)
			exit(1);

		return (item -> chave);
	}

	//atualiza a chave em item
	bool item_set_chave(ITEM *item, char chave){
		if (item == NULL)
			return (false);

		item -> chave = chave;
		return (true);
	}

//retorna a chave do item
	void *item_get_dados (ITEM *item){

		if (item == NULL)
			exit(1);

		void *a = item -> dados;
		return (a);
	}