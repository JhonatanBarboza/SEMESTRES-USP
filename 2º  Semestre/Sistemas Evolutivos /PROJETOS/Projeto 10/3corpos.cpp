#include <GL/glut.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <deque>

#define G 6.67430  // Constante gravitacional (NÃO MEXER)
#define NUM_ESTRELAS 5
#define bordas 80000
#define mb 5                           // Mutiplica as bordas
#define almentar_raio 5                // Este valor é multiplicado ao raio (deixar os corpos visiveis em grandes escalas)
#define colisao_estrelas true          // Ativa acolisão das estrelas (true ativado, false desativado) 
#define MAX_TRAJETORIA 1000

// Estrutura para armazenar as propriedades de cada estrela
typedef struct Estrela_ {
    float x, y;           // Posição
    float vx, vy;         // Velocidade
    double massa;         // Massa
    double raio;          // Raio
    int ativo;            // Ativo ou não
    std::deque<std::pair<float, float>> trajetoria;  // Histórico da trajetória
} Estrela;

Estrela estrelas [NUM_ESTRELAS];

// Função para calcular a força gravitacional entre duas estrelas
void calcular_gravidade_estrela(Estrela* a, Estrela* b) {
    float dx = b->x - a->x;  // Distância no eixo X entre estrela a e estrela b
    float dy = b->y - a->y;  // Distância no eixo Y entre estrela a e estrela b
    float distancia = sqrt(dx * dx + dy * dy);

    if (distancia > 0.01f) {  // Evitar divisão por zero
        // Força gravitacional
        float forca = G * a->massa * b->massa / (distancia * distancia);

        // Aceleração no eixo X e Y para a (direção oposta à estrela b)
        float ax_a = forca * dx / distancia / a->massa;
        float ay_a = forca * dy / distancia / a->massa;

        // Aceleração no eixo X e Y para b (direção oposta à estrela a)
        float ax_b = -forca * dx / distancia / b->massa;
        float ay_b = -forca * dy / distancia / b->massa;

        // Atualizar as velocidades das estrelas
        a->vx += ax_a;
        a->vy += ay_a;

        b->vx += ax_b;
        b->vy += ay_b;
    }
}

// Função para inicializar os planetas com posições, velocidades e massas
void inicializar_estrelas() {
    for (int i = 0; i < NUM_ESTRELAS; i++) {
        estrelas[i].massa = 1000000000;// Massa do estrela (não pode ser zero)
        estrelas[i].raio = pow(estrelas[i].massa, 1.0f / 3.0f);  // Raio inicial do estrela
        estrelas[i].x = rand()%(bordas*2+1)-(bordas);  // Posição no eixo X
        estrelas[i].y = rand()%(bordas*2+1)-(bordas);  // Posição no eixo Y
        estrelas[i].vx = (rand() % 800) - 400;  // Velocidade inicial em X
        estrelas[i].vy = (rand() % 800) - 400;  // Velocidade inicial em Y
        estrelas[i].ativo = 1;  // Planeta começa ativo
    }
}

void limpar_todas_trajetorias() {
    for (int i = 0; i < NUM_ESTRELAS; i++) {
        estrelas[i].trajetoria.clear();  // Limpar o histórico de todas as estrelas
    }
}

// Função para verificar colisões entre estrelas
void verificar_colisoes_estrelas() {
    for (int i = 0; i < NUM_ESTRELAS; i++) {
        if (!estrelas[i].ativo) continue;

        for (int j = i + 1; j < NUM_ESTRELAS; j++) {
            if (!estrelas[j].ativo) continue;

            // Calcular a distância entre as duas estrelas
            float dx = estrelas[j].x - estrelas[i].x;
            float dy = estrelas[j].y - estrelas[i].y;
            float distancia = sqrt(dx * dx + dy * dy);

            // Verificar se há colisão (distância menor que a soma dos raios)
            if (distancia <= (estrelas[i].raio*almentar_raio + estrelas[j].raio*almentar_raio)) {
                // Colisão detectada, combinar as duas estrelas

            }
        }
    }
}

float distancia(float x1, float y1, float x2, float y2) {
    return sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
}

// Função para atualizar a posição das estrelas
void atualizar_estrelas() {
    for (int i = 0; i < NUM_ESTRELAS; i++) {
        if (estrelas[i].x > bordas*mb || estrelas[i].y > bordas*mb || estrelas[i].x < -bordas*mb || estrelas[i].y < -bordas*mb){
                inicializar_estrelas();
                limpar_todas_trajetorias();
        }
        if (!estrelas[i].ativo) continue;

        // Calcular a interação gravitacional com as outras estrelas
        for (int j = i + 1; j < NUM_ESTRELAS; j++) {
            if (estrelas[j].ativo) {
                calcular_gravidade_estrela(&estrelas[i], &estrelas[j]);
            }
        }

        // Atualizar a posição das estrelas com base na velocidade
        estrelas[i].x += estrelas[i].vx;
        estrelas[i].y += estrelas[i].vy;

        // Adicionar posição atual ao histórico
        estrelas[i].trajetoria.push_back({estrelas[i].x, estrelas[i].y});
        
        // Limitar o número de pontos no histórico
        if (estrelas[i].trajetoria.size() > MAX_TRAJETORIA) {
            estrelas[i].trajetoria.pop_front();
        }
    }
    if (colisao_estrelas == 1){
        verificar_colisoes_estrelas();
    }
}

// Função para desenhar um círculo (estrela/planetas)
void desenhar_circulo(float cx, float cy, float raio) {
    glBegin(GL_POLYGON);
    for (int i = 0; i < 100; i++) {
        float theta = 2.0f * M_PI * i / 100;
        float x = raio * cosf(theta);
        float y = raio * sinf(theta);
        glVertex2f(cx + x, cy + y);
    }
    glEnd();
}

void desenhar_trajetoria(const std::deque<std::pair<float, float>>& trajetoria, float r, float g, float b) {
    glBegin(GL_LINE_STRIP);  // Início de linhas conectadas
    glColor3f(r, g, b);      // Define a cor da linha
    for (const auto& ponto : trajetoria) {
        glVertex2f(ponto.first, ponto.second);
    }
    glEnd();
}

// Desenho na tela
void exibir() {
    glClear(GL_COLOR_BUFFER_BIT);

    // Desenhar trajetórias
    for (int i = 0; i < NUM_ESTRELAS; i++) {
        if (estrelas[i].ativo) {
            // Definir a cor da trajetória com base na estrela
            if (i == 0) desenhar_trajetoria(estrelas[i].trajetoria, 1.0f, 1.0f, 0.0f);  // Amarelo
            if (i == 1) desenhar_trajetoria(estrelas[i].trajetoria, 0.0f, 0.5f, 1.0f);  // Azul claro
            if (i == 2) desenhar_trajetoria(estrelas[i].trajetoria, 1.0f, 1.0f, 1.0f);  // Branco
        }
    }

    // Desenhar estrelas
    for (int i = 0; i < NUM_ESTRELAS; i++) {
        if (estrelas[i].ativo) {
            if (i==0){
                glColor3f(1.0f, 1.0f, 0.0f);  // cor das estrelas
            }
            if (i==1){
                glColor3f(0.0f, 0.5f, 1.0f);  // cor das estrelas
            }
            if (i==2){
                glColor3f(1.0f, 1.0f, 1.0f);  // cor das estrelas
            }
            desenhar_circulo(estrelas[i].x, estrelas[i].y, estrelas[i].raio*almentar_raio);
        }
    }
    glutSwapBuffers();
}

// Função Timer para atualização
void timer(int valor) {
    atualizar_estrelas();
    glutPostRedisplay();
    glutTimerFunc(1000 / 60, timer, 0);  // 60 FPS
}

// Configurações do OpenGL
void inicializarOpenGL() {
    glClearColor(0.0, 0.0, 0.0, 1.0);  // Fundo preto
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluOrtho2D(-bordas*mb, bordas*mb, -bordas*mb, bordas*mb);  // Ambiente 2D
}

// Função principal
int main(int argc, char** argv) {
    srand(time(0));  // Inicializar o gerador de números aleatórios

    // Inicializar GLUT
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
    glutInitWindowSize(1000, 1000);
    glutCreateWindow("Simulacao de orbita com Gravidade");

    // Inicializar planetas e OpenGL
    inicializar_estrelas();
    inicializarOpenGL();

    // Registrar callbacks
    glutDisplayFunc(exibir);
    glutTimerFunc(0, timer, 0);

    // Iniciar loop principal
    glutMainLoop();
    return 0;
}
