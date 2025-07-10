#ifndef fila_H
	#define fila_H

	#include <stdbool.h>

    typedef struct Fila_ Fila;
	typedef struct Permutacao_ PERMUTACAO;

	bool Fila_Vazia(Fila *fila);
	void Fila_Apagar(Fila *fila);
	void Fila_Inicializar(Fila **fila);
	PERMUTACAO* Fila_Desenfileirar(Fila *fila);
	void Fila_Enfileirar(Fila *fila, PERMUTACAO *p);
	  
#endif
