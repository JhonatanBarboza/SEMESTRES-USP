#include <stdio.h>
#include <stdbool.h>
#include "ponto.h"
#include "circulo.h"

int main (void){

  float a, b, r;

  scanf(" %f %f %f", &a, &b, &r);  //leitura do circulo
  /*é interessante deixar um espaço antes da primeira leitura pois limpa o buffer*/

  PONTO *centro_circulo = ponto_criar(a,b);
  CIRCULO *c = circulo_criar(centro_circulo, r);

  unsigned int n;  //numero de pontos
  scanf(" %d", &n);

  PONTO *ponto_aux = ponto_criar(0,0);

  while (n--){
      scanf(" %f %f", &a, &b);
      ponto_set(ponto_aux, a, b);

      //diferença da (distancia do ponto com o centro do circulo) com o tamanho do raio
      float resultado = distancia(circulo_get_ponto(c), ponto_aux) - circulo_get_raio(c);

      if (resultado < 0)
          printf("dentro\n");
      else if (resultado > 0)
          printf("fora\n");
      else
          printf("no raio\n");
  }

  //sempre lembrar de desalocar a memoria que foi alocada
  ponto_apagar(&centro_circulo);
  ponto_apagar(&ponto_aux);
  circulo_apagar(&c);

	return(0);
}