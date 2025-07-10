#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <GL/glut.h>
#include <cmath>     
#include <algorithm> 
#include <vector>

#define TamPop 20    
#define maxx 48   
#define minx -51       
#define maxy 56   
#define miny -50       

float TaxMut = 0.1;   
float ind[TamPop + 1];    
float fit[TamPop + 1];    
float indtemp[TamPop + 1]; 
int gen = 1;            
int numger = 500;        
int selectionMethod = 0; // 0 para elitismo, 1 para torneio, 2 para roleta

float fitnessFunction(float x);
void iniciapop(int tampop, float ind[]);
void avalia(int tampop);
void elitismo(int tampop);
void torneio(int tampop);
void rouletteWheel(int tampop);
void ajustaTaxaMutacao(int tampop);
void display();
void initOpenGL();
void runAlgorithm();
void keyboard(unsigned char key, int x, int y);

int main(int argc, char** argv) {
    srand(time(NULL));  // Inicializa a semente do gerador de números aleatórios
    iniciapop(TamPop, ind);  // Inicializa a população

    // Inicializa o GLUT e o OpenGL
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
    glutInitWindowSize(1850, 500);
    glutInitWindowPosition(90, 90);
    glutCreateWindow("Algoritmo Genético: Elitismo e Torneio");
    initOpenGL();
    glutDisplayFunc(display);   // Define a função de exibição
    glutKeyboardFunc(keyboard); // Define a função para lidar com teclado

    glutMainLoop();  // Entra no loop principal do GLUT

    return 0;
}

// Função de aptidão com vários máximos e mínimos locais e um máximo global
float fitnessFunction(float x) {
    //return sin(x)*((x*x)/(45));
    return sin(x)*x+cos(x*9.3)*9.3;

}

// Função para iniciar a população
void iniciapop(int tampop, float ind[]) {
    for (int i = 1; i <= tampop; i++) {
        ind[i] = minx + rand() % (maxx - minx + 1);
    }
}

// Função para avaliar a aptidão da população
void avalia(int tampop) {
    printf("Geração %d (Método: %s)\n", gen, 
           selectionMethod == 0 ? "Elitismo" : "Torneio de 2");
    for (int i = 1; i <= tampop; i++) {
        float x = ind[i];
        float y = fitnessFunction(x);  // Função de aptidão
        fit[i] = y;
        printf("\tIndivíduo %d (%f) = %f\n", i, ind[i], fit[i]);
    }
}



// Função de elitismo para gerar nova geração
void elitismo(int tampop) {
    int maxi = 1;
    float maxfit = fit[1];
    
    // Busca pelo melhor indivíduo
    for (int i = 2; i <= tampop; i++) {
        if (fit[i] > maxfit) {
            maxfit = fit[i];
            maxi = i;
        }
    }

    // Criação de nova geração por elitismo
    for (int i = 1; i <= tampop; i++) {
        if (i == maxi) continue;  // Protege o melhor indivíduo

        // Crossover: o melhor indivíduo "transfere" parte de seus genes para os demais
        ind[i] = (ind[i] + ind[maxi]) / 2;

        // Mutação: muda a posição do indivíduo de forma controlada dentro do domínio
        int mutacao = (int)(((rand() % maxx - (maxx / 2.0)) / 100.0) * TaxMut * maxx);
        ind[i] = std::max(static_cast<float>(minx), std::min(static_cast<float>(maxx), static_cast<float>(ind[i] + mutacao)));  // Garantir que permaneça no domínio
    }
}

// Função de torneio de 2
void torneio(int tampop) {
    int a, b, pai1, pai2;
    int maxi = 1;
    float maxfit = fit[1];
    
    // Busca pelo melhor indivíduo para protegê-lo
    for (int i = 2; i <= tampop; i++) {
        if (fit[i] > maxfit) {
            maxfit = fit[i];
            maxi = i;
        }
    }

    // Backup dos indivíduos
    for (int i = 1; i <= tampop; i++)
        indtemp[i] = ind[i];
    
    // Torneio
    for (int i = 1; i <= tampop; i++) {
        if (i == maxi)    // Protege o melhor indivíduo
            continue;

        // Primeiro torneio para selecionar pai1
        a = (rand() % tampop) + 1;
        b = (rand() % tampop) + 1;
        pai1 = (fit[a] > fit[b]) ? a : b;

        // Segundo torneio para selecionar pai2
        a = (rand() % tampop) + 1;
        b = (rand() % tampop) + 1;
        pai2 = (fit[a] > fit[b]) ? a : b;

        // Crossover
        ind[i] = (indtemp[pai1] + indtemp[pai2]) / 2.0;  

        // Mutação
        int mutacao = (int)(((rand() % maxx - (maxx / 2.0)) / 100.0) * TaxMut * maxx);
        ind[i] = std::max(static_cast<float>(minx), std::min(static_cast<float>(maxx), static_cast<float>(ind[i] + mutacao)));
    }
}


// Função auxiliar para seleção por roleta
float calcularSomaAptidao(int tampop) {
    float soma = 0;
    for (int i = 1; i <= tampop; i++) {
        // Garantir que a aptidão seja não-negativa
        soma += (fit[i] >= 0) ? fit[i] : 0;
    }
    return soma;
}

// Seleção por Roleta
void roleta(int tampop) {
    float somaAptidao = calcularSomaAptidao(tampop);
    std::vector<float> probabilidades(tampop + 1);
    std::vector<float> limitesRoleta(tampop + 1);
    
    // Calcular probabilidades de seleção
    for (int i = 1; i <= tampop; i++) {
        probabilidades[i] = (fit[i] >= 0 ? fit[i] : 0) / somaAptidao;
        
        // Calcular limites acumulados da roleta
        limitesRoleta[i] = (i > 1 ? limitesRoleta[i-1] : 0) + probabilidades[i];
    }
    
    // Backup dos indivíduos
    for (int i = 1; i <= tampop; i++)
        indtemp[i] = ind[i];
    
    // Encontrar o melhor indivíduo para elitismo
    int maxi = 1;
    float maxfit = fit[1];
    for (int i = 2; i <= tampop; i++) {
        if (fit[i] > maxfit) {
            maxfit = fit[i];
            maxi = i;
        }
    }
    
    // Gerar nova população
    for (int i = 1; i <= tampop; i++) {
        if (i == maxi) continue;  // Protege o melhor indivíduo
        
        // Girar a roleta
        float roleta = (float)rand() / RAND_MAX;
        
        // Selecionar indivíduo
        int pai = 1;
        for (int j = 1; j <= tampop; j++) {
            if (roleta <= limitesRoleta[j]) {
                pai = j;
                break;
            }
        }
        
        // Crossover
        ind[i] = (ind[i] + indtemp[pai]) / 2.0;
        
        // Mutação
        int mutacao = (int)(((rand() % maxx - (maxx / 2.0)) / 100.0) * TaxMut * maxx);
        ind[i] = std::max(static_cast<float>(minx), std::min(static_cast<float>(maxx), static_cast<float>(ind[i] + mutacao)));
    }
}





// Função para ajustar a taxa de mutação ao longo das gerações
void ajustaTaxaMutacao(int tampop) {
    int cont = 0;

    for (int i = 1; i <= tampop; i++) {
        for (int j = 1; j <= tampop; j++) {
            // Arredondar para duas casas decimais
            float fit_i_rounded = std::round(fit[i] * 100.0) / 100.0;
            float fit_j_rounded = std::round(fit[j] * 100.0) / 100.0;

            if (fit_i_rounded == fit_j_rounded)
                cont++;
        }
    }
    
    // Se 60% ou mais da população for igual, aumentar a taxa de mutação
    if (cont >= (tampop * (tampop - 1) / 2) * 0.6) {
        TaxMut = 1.0; // Aumenta a taxa de mutação
    } else {    
        TaxMut = 0.01; // Mantém a taxa de mutação baixa
    }
}

// Funções de OpenGL (sem alterações)
void display() {
    glClear(GL_COLOR_BUFFER_BIT);  // Limpa a tela

    // Desenha a curva da função de aptidão
    glColor3f(0.0, 0.0, 0.0); // Cor preta para a linha da função
    glBegin(GL_LINE_STRIP);
    for (float x = minx; x <= maxx; x += 0.1) {
        float y = fitnessFunction(x);
        glVertex2f(x, y);
    }
    glEnd();

    // Plota os pontos da população atual sobre a curva
    glPointSize(5.0);
    glBegin(GL_POINTS);
    for (int i = 1; i <= TamPop; i++) {
        float y = fitnessFunction(ind[i]);  // Calcula y para garantir que o ponto esteja na curva
        glVertex2f(ind[i], y);
    }
    glEnd();

    glFlush();  // Atualiza o buffer para exibir o desenho
}

void initOpenGL() {
    glClearColor(1.0, 1.0, 1.0, 1.0);  // Define a cor de fundo (branco)
    glMatrixMode(GL_PROJECTION);       // Muda para a matriz de projeção
    glLoadIdentity();                  // Reseta a matriz de projeção
    gluOrtho2D(minx, maxx, miny, maxy);  // Define o sistema de coordenadas
}

void runAlgorithm() { 
    avalia(TamPop);  
    display();       
    glutPostRedisplay(); 
    
    // Seleciona o método de seleção
    switch(selectionMethod) {
        case 0: 
            ajustaTaxaMutacao(TamPop); 
            elitismo(TamPop);
            break;
        case 1: 
            TaxMut = 0.1;   
            torneio(TamPop);
            break;
        case 2: 
            TaxMut = 0.1;
            roleta(TamPop);
            break;
    }
    
    gen++;
}

// E modificar a função keyboard():
void keyboard(unsigned char key, int x, int y) {
    if (key == 13) {  // Tecla Enter
        if (gen <= numger) {
            runAlgorithm();
        } else {
            printf("Número máximo de gerações atingido.\n");
        }
    } else if (key == 'm' || key == 'M') {  // Tecla 'm' para alternar entre métodos
        selectionMethod = (selectionMethod + 1) % 3;  // Cicla entre 0, 1 e 2
        
        // Mensagem descritiva do método
        const char* metodos[] = {"Elitismo", "Torneio de 2", "Roleta"};
        printf("Método de seleção alterado para: %s\n", metodos[selectionMethod]);
        
        gen = 1;  // Reinicia a contagem de gerações
        iniciapop(TamPop, ind);  // Reinicia a população
    }
}