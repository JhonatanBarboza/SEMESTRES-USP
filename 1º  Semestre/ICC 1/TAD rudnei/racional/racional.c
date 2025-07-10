#include "racional.h"

#include <stdlib.h>
#include <stdio.h>

struct racional{
      int num;
      int den;
  };

RACIONAL* criar(int num, int den) {
  if(den == 0) {
    return NULL;
  }
  RACIONAL* rac;
  if ((rac = (RACIONAL* )calloc(1,sizeof(RACIONAL))) == NULL)
      exit(1);
  rac->num = num;
  rac->den = den;
  return rac;
}

void apagar(RACIONAL* rac) {
  if(rac != NULL) {  
   free(rac);
  }
}

RACIONAL* adicao(RACIONAL* a, RACIONAL* b) {
  RACIONAL* res;
  
  res = criar(0,1);

  (*res).den = (*a).den * (*b).den;
  res->num = a->den * b->num + b->den * a->num;

  return res;
}

RACIONAL* multiplicacao(RACIONAL* a, RACIONAL* b) {
  RACIONAL* res;
  
  res = criar(0,1);

  res->den = a->den * b->den;
  res->num = a->num * b->num;

  return res;
}

void imprimir(RACIONAL* rac) {
  printf("novo racional: %d/%d\n", rac->num, rac->den);
}
