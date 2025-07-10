#include <stdio.h>
#include <stdlib.h>
#include <cmath>
#include <time.h>

// Estrutura do indivíduo
typedef struct _individuo{
    float x;
    float y;
    float z;
} individuo;

#define TamPop 10    // Tamanho da população
#define TaxMut 0.01   // Taxa de mutação

individuo ind[TamPop + 1];    // Indivíduos
int gen = 1;            // Geração
float maxfit;           // Melhor aptidão
int maxi;               // Índice do melhor indivíduo
int numger = 10;        // Numero de gerações

//***********inicia a população*************
void iniciapop(int tampop, individuo ind[]) {
    srand(time(NULL));
    for (int i = 1; i <= tampop; i++) {
        ind[i].x = rand() % 100;
        ind[i].y = rand() % 100;
    }
}

//***********inicia a avaliação*************
void avalia(int tampop) {
    printf("Geração %d\n", gen);
    for (int i = 1; i <= tampop; i++) {
        float x = ind[i].x;
        float y = ind[i].y;
        float z = ind[i].z = 50.0f - sqrt(x * x + y * y);  // Função de aptidão
        ind[i].z = z;
        printf("\tIndivíduo %d= %f\n", i, ind[i].z);
    }
}

//***********inicia seleção por elitismo*************
void elitismo(int tampop) {
    // Busca pelo melhor indivíduo
    maxfit = ind[1].z;
    maxi = 1;
    for (int i = 2; i <= tampop; i++) {
        if (ind[i].z > maxfit) {
            maxfit = ind[i].z;
            maxi = i;
        }
    }

    // Criação de nova geração por elitismo
    for (int i = 1; i <= tampop; i++) {
        if (i == maxi) {
            continue; // Protege o melhor indivíduo
        }
        // Mantém o melhor indivíduo e realiza o crossover e mutação nos outros
    for (int i = 1; i <= tampop; i++) {
        if (i == maxi) continue;

        ind[i].x = (ind[i].x + ind[maxi].x) / 2.0f;
        ind[i].y = (ind[i].y + ind[maxi].y) / 2.0f;

        ind[i].z = 50.0f - sqrt(ind[i].x * ind[i].x + ind[i].y * ind[i].y);
    }
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


