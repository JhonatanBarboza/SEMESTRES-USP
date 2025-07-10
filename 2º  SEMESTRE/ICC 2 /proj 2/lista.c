#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "lista.h"

typedef struct lista_ {
  char *vet;
  int tamanho;
  int capacidade;
}LISTA;

int buscaBinariaLista(int v[], int inicio, int fim, int chave);

LISTA *lista_criar(int tam){
  LISTA *lista = (LISTA *) malloc(sizeof(LISTA));
  if(lista == NULL) return NULL;
  
  lista->vet = (char *) malloc(tam * sizeof(char));
  if(lista->vet == NULL) return NULL;

  lista->tamanho = 0;
  lista->capacidade = tam;
  return lista;
}

void lista_apagar(LISTA **lista){
  if(*lista == NULL) return;

  free(*lista);
  *lista = NULL;
  return;
}

bool lista_inserir(LISTA *lista, int elemento){ 
  if(lista == NULL) return false;
  if(lista->tamanho > TAM_MAX) return false;

  /*Percorremos o vetor até encontrarmos o primeiro
  elemento maior que o elemento a ser inserido*/
  int i = 0;
  while((i < lista->tamanho) && (lista->vet[i] < elemento)) i++;
  /*Se o elemento já existe na lista, ele não é inserido*/
  if(lista_busca(lista, elemento) == elemento) return false;

  /*Deslocamos parte do vetor para a direita (a partir da
  posição correta), abrindo espaço para o elemento ser inserido*/
  for(int j = lista->tamanho; j > i; j--){
    lista->vet[j] = lista->vet[j-1];
  }
  lista->vet[i] = elemento;
  lista->tamanho++;

  return true;
}

int lista_remover(LISTA *lista, int elemento) { 
    // Retorna ERRO se a lista for NULL ou vazia
    if (lista == NULL || lista->tamanho == 0) return ERRO;

    // Busca a posição do elemento usando busca binária
    int i = buscaBinariaLista(lista->vet, 0, lista->tamanho - 1, elemento);

    // Se o elemento não for encontrado, retorna ERRO
    if (i == ERRO) return ERRO;

    // Desloca elementos para a esquerda para preencher o espaço
    for (int j = i; j < lista->tamanho - 1; j++) {
        lista->vet[j] = lista->vet[j + 1];
    }

    // Diminui o tamanho da lista e marca a última posição como ERRO
    lista->vet[lista->tamanho - 1] = ERRO;
    lista->tamanho--;

    return elemento; // Retorna o elemento removido
}


void lista_imprimir(LISTA *lista){
  if(lista == NULL) return;

  for(int i = 0; i < lista->tamanho; i++){
    /*Devido à forma como removemos, checamos o elemento
    para não imprimirmos itens removidos*/
    if(lista->vet[i] != ERRO) printf("%d ", lista->vet[i]);
  }
  printf("\n");

  return;
}

/*Retorna a chave se ela foi encontrada, ERRO senão*/
int lista_busca(LISTA *lista, int chave){
  if(lista == NULL) return ERRO;
  if(lista->tamanho == 0) return ERRO;

  if(buscaBinariaLista(lista->vet, 0, lista->tamanho - 1, chave) != ERRO) return chave;
  return ERRO;
}

int lista_consultar(LISTA *lista, int indice){  
  return lista->vet[indice];
}

/*Retorna a posição do elemento em que ele foi encontrado (ERRO se não foi)*/
int buscaBinariaLista(int v[], int inicio, int fim, int chave){
  if(inicio > fim) return ERRO;

  int meio = (inicio + fim) / 2;

  if(v[meio] == chave) return meio;
  else if(chave < v[meio]) return buscaBinariaLista(v, inicio, meio - 1, chave);
  else{
    return buscaBinariaLista(v, meio + 1, fim, chave);
  }
}