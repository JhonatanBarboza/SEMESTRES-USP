#include <GL/glut.h>
#include <iostream>
#include <cstdlib>
#include <ctime>
#include <algorithm>
#include <vector>
#include <cmath>
#include <iostream>
using namespace std;

// Estruturas para Predador e Presa
typedef struct Presa_ {
    float x, y;
    float dx, dy; 
    float velocidade;
    int alcance_visao;
    float energia;
    bool viva;
}Presa;

typedef struct Predador_ {
    float x, y;
    float dx, dy;  
    float velocidade;
    int alcance_visao;
    float energia;
    bool vivo;
}Predador;

typedef struct Arbusto_{
    float x, y;
    float energia;
    bool vivo;
}Arbusto;



//*************************************** Variaveis Globais ******************************************************




// Variáveis Globais fixas
int geracao = 1; 
const int NUM_PREDADORES = 10;
const int NUM_PRESAS = 100;
const int NUM_ARBUSTO = 1000;

//variaveis ajustaveis
int NUM_PREDADORES_INICIAL = 5;
int NUM_PRESAS_INICIAL = 25;
int NUM_ARBUSTOS_INICIAL = 20;
int bordas = 500;

//controle de energia 
float enrgia_inicio_predadores = 30.0f;
float enrgia_inicio_presas = 30.0f;
float enrgia_inicio_arbustos = 30.0f;

float ganhar_energia_predadores = 20.0f;
float ganhar_energia_presas = 2.0f;
float ganhar_energia_arbustos = 2.0f;

float retirar_energia_predadores = 0.05f;
float retirar_energia_presas = 0.05f;
float retirar_energia_arbusto = 3.0f;

float reproduzir_predadores = 40.0f;
float reproduzir_presas = 40.0f;
float reproduzir_arbusto = 100.0f;

float retirar_reproducao_predadores = 20.0f;
float retirar_reproducao_presas = 20.0f;
float retirar_reproducao_arbusto = 70.0f;

//controle de atributos 
float velocidade_predadores = 0.6f;
float velocidade_presas = 0.0f;
float velo_randomica_predadores = 1.0f;
float velo_randomica_presas = 0.8f;

int visao_predadores = 200;
int visao_presas = 0;
int visao_randomica_predadores = 300; // 1
int visao_randomica_presas = 100; // 1





//*************************************** Inicializar a Simulação ******************************************************




//variaveis armazenamento
Presa presas[NUM_PRESAS];
Predador predadores[NUM_PREDADORES];
Arbusto arbustos[NUM_ARBUSTO];
int cont_predadores = NUM_PREDADORES_INICIAL-1;
int cont_presas = NUM_PRESAS_INICIAL-1;
int cont_arbustos = NUM_ARBUSTOS_INICIAL-1;


void inicializar_simulacao() {
    //inicializar presas, predadores e arbustos
    for (int i = 0; i < NUM_PREDADORES; i++) {
        predadores[i].x = 1;
        predadores[i].y = 1;
        predadores[i].velocidade = 0.0f;   
        predadores[i].alcance_visao = 0; 
        predadores[i].energia = enrgia_inicio_predadores; 
        predadores[i].vivo = false;
    }
    for (int i = 0; i < NUM_PRESAS; i++) {
        presas[i].x = 1;
        presas[i].y = 1;
        presas[i].velocidade = 0.0f;
        presas[i].alcance_visao = 0;
        presas[i].energia = enrgia_inicio_presas;
        presas[i].viva = false;
    }
    for (int i = 0; i < NUM_ARBUSTO; i++){
        arbustos[i].x = 1;
        arbustos[i].y = 1;
        arbustos[i].energia = enrgia_inicio_arbustos;
        arbustos[i].vivo = false;
    }

    // Fuções para inicializar as primeiras presas e predadores 
    for (int i = 0; i < NUM_PREDADORES_INICIAL; i++) {
        predadores[i].x = rand() % bordas;
        predadores[i].y = rand() % bordas;
        predadores[i].velocidade = velocidade_predadores + static_cast<float>(rand()) / (static_cast<float>(RAND_MAX / velo_randomica_predadores));  // Aleatoriza a velocidade inicial
        predadores[i].alcance_visao = visao_predadores + rand() % visao_randomica_predadores;  // Aleatoriza o alcance de visão
        predadores[i].vivo = true;
    }
    for (int i = 0; i < NUM_PRESAS_INICIAL; i++) {
        presas[i].x = rand() % bordas;
        presas[i].y = rand() % bordas;
        presas[i].velocidade = velocidade_presas + static_cast<float>(rand()) / (static_cast<float>(RAND_MAX / velo_randomica_presas));  // Aleatoriza a velocidade inicial
        presas[i].alcance_visao = visao_presas + rand() % visao_randomica_presas;  // Aleatoriza o alcance de visão
        presas[i].viva = true;
    }

    for (int i = 0; i < NUM_ARBUSTOS_INICIAL; i++){
        arbustos[i].x = rand() % bordas;
        arbustos[i].y = rand() % bordas;
        arbustos[i].vivo = true;
    }
}




//*************************************** Funções Alxiliares ******************************************************




// Função que calcula a distancia entre dois pontos
float distancia(float x1, float y1, float x2, float y2) {
    return sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
}

// Função para delimitar as bordas
void rebote_borda(float &x, float &y, float &dx, float &dy, float limite_min, float limite_max, float velocidade) {
    if (x <= limite_min) {
        x = limite_min;
        dx = fabs(dx);  // Inverte para a direita
    } else if (x >= limite_max) {
        x = limite_max;
        dx = -fabs(dx);  // Inverte para a esquerda
    }

    if (y <= limite_min) {
        y = limite_min;
        dy = fabs(dy);  // Inverte para cima
    } else if (y >= limite_max) {
        y = limite_max;
        dy = -fabs(dy);  // Inverte para baixo
    }

    // Atualiza a posição
    x += (dx * velocidade);
    y += (dy * velocidade);
}

/*
// Função para teletransportar o indivíduo quando ultrapassa as bordas
void rebote_borda(float &x, float &y, float &dx, float &dy, float limite_min, float limite_max, float velocidade) {
    // Verifica se ultrapassou a borda horizontal
    if (x < limite_min) {
        x = limite_max;  // Teletransporta para o lado direito
    } else if (x > limite_max) {
        x = limite_min;  // Teletransporta para o lado esquerdo
    }

    // Verifica se ultrapassou a borda vertical
    if (y < limite_min) {
        y = limite_max;  // Teletransporta para o lado de baixo
    } else if (y > limite_max) {
        y = limite_min;  // Teletransporta para o lado de cima
    }

    // Atualiza a posição
    x += (velocidade);
    y += (velocidade);
}*/

// Função para gerar um ângulo aleatório entre 0 e 360 graus
float gerar_angulo_aleatorio() {
    float angulo = (rand() % 60) * M_PI / 180.0;  // Gera entre 0 e 360 graus em radianos
    return angulo;
}

// Função para remover predador morto e compactar o vetor
void remover_predador_morto(int i) {
    if (cont_predadores > -1) {
        for (int j = i; j < cont_predadores - 1; j++) {
            predadores[j] = predadores[j + 1];
        }
        cont_predadores--;  // Decrementa o número de predadores
    }
}

// Função para remover presa morta e compactar o vetor
void remover_presas_mortas(int i) {
    if (cont_presas > -1) {
        for (int j = i; j < cont_presas - 1; j++) {
            presas[j] = presas[j + 1];
        }
        cont_presas--;  // Decrementa o número de presas
    }
}

// Função para remover arbusto morto e compactar o vetor
void remover_arbustos_mortos(int i) {
    if (cont_arbustos > -1) {
        for (int j = i; j < cont_arbustos - 1; j++) {
            arbustos[j] = arbustos[j + 1];
        }
        cont_arbustos--;  // Decrementa o número de arbustos
    }
}

// Função para gerar um valor aleatório entre -1 e 1
float gerar_valor_aleatorio_direcao() {
    return ((rand() % 2001) - 1000) / 1000.0f;  // Gera um valor entre -1 e 1
}




//************************************** Funções Controle de movimento ********************************************




// Função para difir como a presa ira fugir 
void fugir(int i, int j) {
    //printf("fugir\n");
    float dx = presas[i].x - predadores[j].x;
    float dy = presas[i].y - predadores[j].y;
    float dist = distancia(presas[i].x, presas[i].y, predadores[j].x, predadores[j].y);

    if (dist > 0.001f) {
        // Calcular a direção normalizada na direção oposta ao predador
        float direcao_x = dx / dist;
        float direcao_y = dy / dist;

        // Gerar um ângulo aleatório para variar a direção de fuga
        float angulo = gerar_angulo_aleatorio();

        // Aplicar o ângulo à direção de fuga
        float novo_dx = direcao_x * cos(angulo) - direcao_y * sin(angulo);
        float novo_dy = direcao_x * sin(angulo) + direcao_y * cos(angulo);

        // Mover a presa na nova direção com a variação angular
        float mov_x = novo_dx * presas[i].velocidade;
        float mov_y = novo_dy * presas[i].velocidade;

        presas[i].x += mov_x;
        presas[i].y += mov_y;

        // Consumo de energia proporcional à distância percorrida (energia diminui)
        presas[i].energia -= retirar_energia_presas; 

        if (presas[i].energia <= 0.0f) {
        remover_presas_mortas(i);  
        return;
        }

        // Atualizar direção de movimento da presa
        presas[i].dx = novo_dx;
        presas[i].dy = novo_dy;

        // Verificar bordas e rebater se necessário
        rebote_borda(presas[i].x, presas[i].y, presas[i].dx, presas[i].dy, 1.0f, bordas - 1, presas[i].velocidade);
    }
}

void perseguir(int i, int j) {
    //printf("perceguir\n");
    float distanciaX = presas[j].x - predadores[i].x;
    float distanciaY = presas[j].y - predadores[i].y;
    float dist = distancia(predadores[i].x, predadores[i].y, presas[j].x, presas[j].y);
    if (dist > 0.001f) {
        // Verificar bordas antes de mover
        float limite_min = 1.0f, limite_max = bordas - 1;
        rebote_borda(predadores[i].x, predadores[i].y, predadores[i].dx, predadores[i].dy, limite_min, limite_max, predadores[i].velocidade);

        // Mover em direção à presa
        predadores[i].x += (distanciaX / dist) * (predadores[i].velocidade + 0.02f);
        predadores[i].y += (distanciaY / dist) * (predadores[i].velocidade + 0.2f);

        // Reduzir energia do predador com base na distância percorrida
        predadores[i].energia -= retirar_energia_predadores; 

        // Verificar se o predador morreu por falta de energia
        if (predadores[i].energia <= 0.0f) {
            remover_predador_morto(i);
            return;  // Interromper a perseguição se o predador estiver morto
        }
    }
}


// Movimento aleatório da presa
void mover_aleatorio_presa(int i) {
    //printf("mover presa\n");
    // Atualizar direções de forma suave com pequenos ajustes aleatórios
    presas[i].dx += gerar_valor_aleatorio_direcao() * 0.05f;  // Pequena variação na direção x (reduzida)
    presas[i].dy += gerar_valor_aleatorio_direcao() * 0.05f;  // Pequena variação na direção y (reduzida)

    // Normalizar as direções para evitar aceleração excessiva
    float magnitude = sqrt(presas[i].dx * presas[i].dx + presas[i].dy * presas[i].dy);
    if (magnitude > 0.001f) {
        presas[i].dx /= magnitude;
        presas[i].dy /= magnitude;
    }

    // Atualizar posição da presa com base na direção e velocidade reduzida
    float fator_velocidade_reduzido = presas[i].velocidade;  // Reduz a velocidade pela metade
    presas[i].x += presas[i].dx * fator_velocidade_reduzido;
    presas[i].y += presas[i].dy * fator_velocidade_reduzido;


    presas[i].energia -= retirar_energia_presas;

    if (presas[i].energia <= 0.0f) {
        remover_presas_mortas(i);  
        return;
    }

    // Verificar bordas e ajustar direção se necessário
    rebote_borda(presas[i].x, presas[i].y, presas[i].dx, presas[i].dy, 1.0f, bordas-1, presas[i].velocidade);
}

// Movimento aleatório// Movimento aleatório do predador
void mover_aleatorio_predador(int i) {
    //printf("mover predador \n");
    // Atualizar direções de forma suave com pequenos ajustes aleatórios
    predadores[i].dx += gerar_valor_aleatorio_direcao() * 0.05f;  // Pequena variação na direção x
    predadores[i].dy += gerar_valor_aleatorio_direcao() * 0.05f;  // Pequena variação na direção y

    // Normalizar as direções para evitar aceleração excessiva
    float magnitude = sqrt(predadores[i].dx * predadores[i].dx + predadores[i].dy * predadores[i].dy);
    if (magnitude > 0.001f) {
        predadores[i].dx /= magnitude;
        predadores[i].dy /= magnitude;
    }
    // Atualizar posição da presa com base na direção e velocidade reduzida
    float fator_velocidade_reduzido = predadores[i].velocidade * 0.01f;  // Reduz a velocidade pela metade
    presas[i].x += predadores[i].dx * fator_velocidade_reduzido;
    presas[i].y += predadores[i].dy * fator_velocidade_reduzido;

    predadores[i].energia -= retirar_energia_predadores; 

    if (predadores[i].energia <= 0.0){
        remover_predador_morto(i);
        return;
    }
    // Verificar bordas e ajustar direção se necessário
    rebote_borda(predadores[i].x, predadores[i].y, predadores[i].dx, predadores[i].dy, 1.0f, bordas+1, predadores[i].velocidade);
}

void mover_presa_arbusto(int i, int j) {
    //printf("mover arbusto\n");
    
    float distanciaX = arbustos[j].x - presas[i].x;
    float distanciaY = arbustos[j].y - presas[i].y;
    float dist = distancia(presas[i].x, presas[i].y, arbustos[j].x, arbustos[j].y);

    if (dist > 0.001f) {
        // Mover em direção ao arbusto
        presas[i].x += (distanciaX / dist) * (presas[i].velocidade + 0.2f);
        presas[i].y += (distanciaY / dist) * (presas[i].velocidade + 0.2f);

        // Verificar bordas após o movimento
        float limite_min = 1.0f, limite_max = bordas - 1;
        rebote_borda(presas[i].x, presas[i].y, presas[i].dx, presas[i].dy, limite_min, limite_max, presas[i].velocidade);

        // Reduzir energia da presa com base na distância percorrida
        presas[i].energia -= retirar_energia_presas;

        // Verificar se a presa morreu por falta de energia
        if (presas[i].energia <= 0.0f) {
            remover_presas_mortas(i);
            return;  // Interromper o movimento se a presa estiver morta
        }
    }
}




//************************************** Funções de Reprodução ****************************************************



// Função para gerar uma nova geração de predadores
void gerar_filho_predador(int pai, int mae, int i) {
    // Verificar se o índice 'i' está dentro dos limites
    if (i >= NUM_PREDADORES || i < 0 || pai >= NUM_PREDADORES || pai < 0 || mae >= NUM_PREDADORES || mae < 0|| !predadores[pai].vivo || !predadores[mae].vivo) {
        return;
    }

    // Crossover: combina genes do melhor predador (pai) com a melhor predadora (mãe)
    predadores[i].velocidade = (predadores[pai].velocidade + predadores[mae].velocidade) / 2.0f;
    predadores[i].alcance_visao = (predadores[pai].alcance_visao + predadores[mae].alcance_visao) / 2;
        
    //mutação
    predadores[i].velocidade += ((rand() % 100) / 100.0f) * 0.2f;
    predadores[i].alcance_visao += rand () % 20;
            
    predadores[i].energia = enrgia_inicio_predadores;
    predadores[i].vivo = true; 

    predadores[i].x = predadores[mae].x;
    predadores[i].y = predadores[mae].y;

    predadores[pai].energia -= retirar_reproducao_predadores;
    predadores[mae].energia -= retirar_reproducao_predadores;
}


// Função para gerar uma nova geração de predadores
void gerar_filho_presa(int pai, int mae, int i) {
    // Verificar se o índice 'i' está dentro dos limites
    if (i >= NUM_PRESAS || i < 0 || pai >= NUM_PRESAS || pai < 0 || mae >= NUM_PRESAS || mae < 0|| !presas[pai].viva || !presas[mae].viva) {
        return;
    }

    // Crossover: combina genes do melhor predador (pai) com a melhor predadora (mãe)
    presas[i].velocidade = (presas[pai].velocidade + presas[mae].velocidade) / 2.0f;
    presas[i].alcance_visao = (presas[pai].alcance_visao + presas[mae].alcance_visao) / 2;
        
    //mutação
    //presas[i].velocidade += ((rand() % 100) / 100.0f) * 0.1f;
    //presas[i].alcance_visao += rand() % 2;
            
    presas[i].energia = enrgia_inicio_presas;
    presas[i].viva = true; 

    presas[i].x = presas[mae].x;
    presas[i].y = presas[mae].y;

    presas[pai].energia = retirar_reproducao_presas;
    presas[mae].energia = retirar_reproducao_presas;
}

void gerar_arbustinho(int pai, int i) {
    // Verificar se o índice 'i' está dentro dos limites
    if (i >= NUM_ARBUSTO || i < 0 || pai >= NUM_ARBUSTO || pai < 0 || !arbustos[pai].vivo) {
        return;
    }

    float novo_x, novo_y;

    // 10% de chance de o arbustinho nascer em uma posição aleatória no mapa
    if (rand() % 200 < 7) {
        novo_x = rand() % bordas; 
        novo_y = rand() % bordas;
    } else {
        // Gerar novo arbustinho com base nas coordenadas do pai (90% das vezes)
        novo_x = arbustos[pai].x + ((rand() % 100) / 100.0f) * 8.0f - 4.0f;
        novo_y = arbustos[pai].y + ((rand() % 100) / 100.0f) * 8.0f - 4.0f;
        
        // Garantir que o novo arbustinho está dentro das bordas
        novo_x = fmax(0.0f, fmin(novo_x, bordas - 1.0f));
        novo_y = fmax(0.0f, fmin(novo_y, bordas - 1.0f));
    }

    // Atribuir os valores ao novo arbustinho
    arbustos[i].x = novo_x;
    arbustos[i].y = novo_y;
    arbustos[i].energia = enrgia_inicio_arbustos;
    arbustos[i].vivo = true;

    // Reduzir a energia do arbusto pai após a reprodução
    arbustos[pai].energia -= retirar_reproducao_arbusto;
}





//************************************** Funções Controle da Simulação ********************************************




void atualizar_simulacao() {

    // Movimentar predadores
    for (int i = 0; i < NUM_PREDADORES; i++) {
        if (predadores[i].vivo) {
            int presa_mais_proxima = -1;
            float menor_distancia = 9999.0f;
            float dist = -1;

            for (int j = 0; j < NUM_PRESAS; j++) {
                if (presas[j].viva) {
                    dist = distancia(predadores[i].x, predadores[i].y, presas[j].x, presas[j].y);
                    if (dist < menor_distancia) {
                        menor_distancia = dist;
                        presa_mais_proxima = j;
                    }
                }
            }

            if (menor_distancia <= predadores[i].alcance_visao && presa_mais_proxima != -1) {
                if (menor_distancia < 3.0f) {
                    presas[presa_mais_proxima].viva = false;
                    predadores[i].energia += ganhar_energia_predadores;
                    remover_presas_mortas(presa_mais_proxima);
                } else {
                    perseguir(i, presa_mais_proxima);
                }
            } else {
                mover_aleatorio_predador(i);
            }
        }
    }

    // Movimentar presas
    for (int i = 0; i < NUM_PRESAS; i++) {
        if (presas[i].viva) {
            int predador_mais_proximo = -1;
            float menor_distancia = 9999.0f;
            float dist = -1;

            for (int j = 0; j < NUM_PREDADORES; j++) {
                if (predadores[j].vivo) {
                    dist = distancia(presas[i].x, presas[i].y, predadores[j].x, predadores[j].y);
                    if (dist < menor_distancia) {
                        menor_distancia = dist;
                        predador_mais_proximo = j;
                    }
                }
            }

            if (menor_distancia <= presas[i].alcance_visao && predador_mais_proximo != -1) {
                fugir(i, predador_mais_proximo);
            } else {
                int arbusto_mais_proximo = -1;
                menor_distancia = 9999.0f;
                dist = -1;

                for (int j = 0; j < NUM_ARBUSTO; j++) {
                    if (arbustos[j].vivo) {
                        dist = distancia(presas[i].x, presas[i].y, arbustos[j].x, arbustos[j].y);
                        if (dist < menor_distancia) {
                            menor_distancia = dist;
                            arbusto_mais_proximo = j;
                        }
                    }
                }

                if (menor_distancia <= presas[i].alcance_visao && arbusto_mais_proximo != -1) {
                    if (menor_distancia < 3.0f) {
                        arbustos[arbusto_mais_proximo].energia -= retirar_energia_arbusto;
                        if (arbustos[arbusto_mais_proximo].energia <= 0) {
                            arbustos[arbusto_mais_proximo].vivo = false;
                            remover_arbustos_mortos(arbusto_mais_proximo);
                            cont_arbustos--;
                        }
                        presas[i].energia += ganhar_energia_presas;
                    } else {
                        mover_presa_arbusto(i, arbusto_mais_proximo);
                    }
                } else {
                    mover_aleatorio_presa(i);
                }
            }
        }
    }

    // Atualizar os arbustos
    for (int i = 0; i < NUM_ARBUSTO; i++) {
        if (cont_arbustos >= NUM_ARBUSTO) {
            break;
        }
        if (arbustos[i].vivo) {
            if (arbustos[i].energia >= reproduzir_arbusto) {
                if (cont_arbustos < NUM_ARBUSTO) {
                    gerar_arbustinho(i, cont_arbustos);
                    cont_arbustos++;
                }

            }
            if (arbustos[i].energia < 120.0f){
                arbustos[i].energia += ganhar_energia_arbustos;
            }
        }
    }

    int pai = -1, mae = -1;
    geracao++;

    // Gerar novas presas
    for (int i = 0; i <= cont_presas; i++) {
        if (cont_presas >= NUM_PRESAS) {
            break;
        }
        if (presas[i].viva && presas[i].energia >= reproduzir_presas) {
            if (pai == -1) {
                pai = i;
            } else if (mae == -1) {
                mae = i;
            }

            if (pai != -1 && mae != -1) {
                gerar_filho_presa(pai, mae, cont_presas);
                cont_presas++;
                pai = -1;
                mae = -1;
            }
        }
    }

    pai = -1;
    mae = -1;

    // Gerar novos predadores
    for (int i = 0; i <= cont_predadores; i++) {
        if (cont_predadores >= NUM_PREDADORES) {
            break;
        }
        if (predadores[i].vivo && predadores[i].energia >= reproduzir_predadores) {
            if (pai == -1) {
                pai = i;
            } else if (mae == -1) {
                mae = i;
            }

            if (pai != -1 && mae != -1) {
                gerar_filho_predador(pai, mae, cont_predadores);
                cont_predadores++;
                pai = -1;
                mae = -1;
            }
        }
    }

    // Condição de término
    if (cont_predadores < 0 && cont_presas < 0 && cont_arbustos < 0) {
        exit(1);
    }
}




//**************************************** OpenGL *****************************************************************



// Desenho na Tela
void exibir() {
    glClear(GL_COLOR_BUFFER_BIT);

    // Desenhar os arbustos
    glColor3f(0.0f, 0.5f, 0.0f);  
    for (int i = 0; i < NUM_ARBUSTO; i++) {
        if (arbustos[i].vivo) {
            glBegin(GL_QUADS);
            glVertex2f(arbustos[i].x - 2.5f, arbustos[i].y - 2.5f);
            glVertex2f(arbustos[i].x + 2.5f, arbustos[i].y - 2.5f);
            glVertex2f(arbustos[i].x + 2.5f, arbustos[i].y + 2.5f);
            glVertex2f(arbustos[i].x - 2.5f, arbustos[i].y + 2.5f);
            glEnd();
        }
    }

    // Desenhar as presas
    glColor3f(1.0f, 1.0f, 1.0f);  
    for (int i = 0; i < NUM_PRESAS; i++) {
        if (presas[i].viva) {
            glBegin(GL_QUADS);
            glVertex2f(presas[i].x - 1, presas[i].y - 1);
            glVertex2f(presas[i].x + 1, presas[i].y - 1);
            glVertex2f(presas[i].x + 1, presas[i].y + 1);
            glVertex2f(presas[i].x - 1, presas[i].y + 1);
            glEnd();
        }
    }

    // Desenhar os predadores
    glColor3f(1.0f, 0.5f, 0.0f);  
    for (int i = 0; i < NUM_PREDADORES; i++) {
        if (predadores[i].vivo) {
            glBegin(GL_QUADS);
            glVertex2f(predadores[i].x - 1.5f, predadores[i].y - 1.5f);
            glVertex2f(predadores[i].x + 1.5f, predadores[i].y - 1.5f);
            glVertex2f(predadores[i].x + 1.5f, predadores[i].y + 1.5f);
            glVertex2f(predadores[i].x - 1.5f, predadores[i].y + 1.5f);
            glEnd();
        }
    }


    glutSwapBuffers();
}

// Função Timer
void timer(int valor) {
    atualizar_simulacao();
    glutPostRedisplay();
    glutTimerFunc(1000 / 30, timer, 0);  // 30 FPS
}

// Configurações do OpenGL
void inicializarOpenGL() {
    glClearColor(0.0, 0.0, 0.0, 1.0);  // Fundo preto
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluOrtho2D(0, bordas, 0, bordas);  // Ambiente 2D 
}



//**************************************** Main *****************************************************************



int main(int argc, char** argv) {
    srand(time(0));  // Inicializar o gerador de números aleatórios

    // Inicializar GLUT
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
    glutInitWindowSize(800, 800);
    glutCreateWindow("Simulacao Predador e Presa");

    // Inicializar simulação e OpenGL
    if (geracao == 1)
        inicializar_simulacao();
    inicializarOpenGL();

    // Registrar callbacks
    glutDisplayFunc(exibir);
    glutTimerFunc(0, timer, 0);

    // Iniciar loop principal
    glutMainLoop();
    return 0;
}
