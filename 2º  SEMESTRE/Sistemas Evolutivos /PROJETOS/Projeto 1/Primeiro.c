#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define TamPop 10    // Tamanho da população
#define maxx 100  // Valor máximo para inicialização dos indivíduos
#define TaxMut 0.01   // Taxa de mutação

int ind[TamPop + 1];    // Indivíduos
int ind2[TamPop + 1];   // Indivíduos temporários para o torneio
float fit[TamPop + 1];  // Aptidão dos indivíduos
int gen = 1;            // Geração
float maxfit;           // Melhor aptidão
int maxi;               // Índice do melhor indivíduo
int numger = 10;        // Numero de gerações

//***********inicia a população*************
void iniciapop(int tampop, int ind[]) {
    srand(time(NULL));
    for (int i = 1; i <= tampop; i++) {
        ind[i] = rand() % maxx;
    }
}

//***********inicia a avaliação*************
void avalia(int tampop) {
    printf("Geração %d\n", gen);
    for (int i = 1; i <= tampop; i++) {
        float x = ind[i];
        float y = -0.2 * (x - 10) * (x - 10) + 10;  // Função de aptidão
        fit[i] = y;
        printf("\tIndivíduo %d (%d) = %f\n", i, ind[i], fit[i]);
    }
}

//***********inicia seleção por elitismo*************
void elitismo(int tampop) {
    // Busca pelo melhor indivíduo
    maxfit = fit[1];
    maxi = 1;
    for (int i = 2; i <= tampop; i++) {
        if (fit[i] > maxfit) {
            maxfit = fit[i];
            maxi = i;
        }
    }

    // Criação de nova geração por elitismo
    for (int i = 1; i <= tampop; i++) {
        if (i == maxi) {
            continue; // Protege o melhor indivíduo
        }
        // Crossover: o melhor indivíduo "transfere" parte de seus genes para os demais
        ind[i] = (ind[i] + ind[maxi]) / 2;

        // Mutação
        ind[i] = ind[i] + (int)(((rand() % maxx - (maxx / 2.0)) / 100.0) * TaxMut * maxx);
    }
}

int main() {
    srand(time(NULL));  // Inicializa o gerador de números aleatórios

    // Inicializa a população
    iniciapop(TamPop, ind);

    for (int i=0; i<numger; i++){
    // Avalia a primeira geração
    avalia(TamPop);

    // Realiza o elitismo para gerar uma nova geração
    elitismo(TamPop);

    // Avalia a nova geração
    gen++;}
    

    return 0;
}
