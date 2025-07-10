#include "racional.h"
#include<stdio.h>

int main() {
   //RACIONAL racional1;

   RACIONAL *r1 = criar(1,2);
   RACIONAL *r2 = criar(1,3);
   imprimir(r1);
   imprimir(r2);

   RACIONAL *r3 = adicao(r1, r2);
   imprimir(r3);
   //printf("%d \n",r3->num); /*Erro: não pode acessar valores do TAD diretamente*/

   RACIONAL *r4 = multiplicacao(r1, r2);
   imprimir(r4);

   apagar(r1);
   apagar(r2);
   apagar(r3);
   apagar(r4);
   
   return 0;
}
