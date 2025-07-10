#include <GL/glut.h>
#include <cmath>

// Função para desenhar os eixos
void drawAxes() {
    glBegin(GL_LINES);

    // Eixo X em vermelho
    glColor3f(1.0f, 0.0f, 0.0f);
    glVertex3f(-25.0f, 0.0f, 0.0f);
    glVertex3f(25.0f, 0.0f, 0.0f);

    // Eixo Y em verde
    glColor3f(0.0f, 1.0f, 0.0f);
    glVertex3f(0.0f, -25.0f, 0.0f);
    glVertex3f(0.0f, 25.0f, 0.0f);

    // Eixo Z em azul
    glColor3f(0.0f, 0.0f, 1.0f);
    glVertex3f(0.0f, 0.0f, -10.0f);
    glVertex3f(0.0f, 0.0f, 15.0f);

    glEnd();
}

// Função para desenhar uma grade no plano XY
void drawGrid() {
    glColor3f(0.5f, 0.5f, 0.5f); // Cor da grade (cinza)
    glBegin(GL_LINES);
    
    for (float i = -20.0f; i <= 20.0f; i += 1.0f) {
        // Linhas verticais no plano YZ
        glVertex3f(i, -20.0f, 0.0f);
        glVertex3f(i, 20.0f, 0.0f);

        // Linhas horizontais no plano XZ
        glVertex3f(-20.0f, i, 0.0f);
        glVertex3f(20.0f, i, 0.0f);
    }
    
    glEnd();
}

void display() {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();
    
    // Ajuste da câmera para melhor visualização
    gluLookAt(30.0, 30.0, 30.0,  // Posição da câmera
              0.0, 0.0, 0.0,     // Ponto de foco
              0.0, 0.0, 1.0);    // Vetor "up" (orientação)

    // Desenha os eixos e a grade
    drawAxes();
    drawGrid();

    // Desenha a superfície do cone
    float x, y, z;
    float step = 0.5;
    float c = 10.0f; // Altura máxima do cone

    glColor3f(0.0f, 0.7f, 0.7f); // Cor da superfície do cone (ciano)
    glBegin(GL_QUADS);
    for (x = -20.0; x <= 20.0; x += step) {
        for (y = -20.0; y <= 20.0; y += step) {
            // Calcula z para os quatro pontos do quad
            z = c - sqrt(x * x + y * y);
            if (z < 0) z = 0; // Evita valores negativos
            glVertex3f(x, y, z);

            z = c - sqrt((x + step) * (x + step) + y * y);
            if (z < 0) z = 0;
            glVertex3f(x + step, y, z);

            z = c - sqrt((x + step) * (x + step) + (y + step) * (y + step));
            if (z < 0) z = 0;
            glVertex3f(x + step, y + step, z);

            z = c - sqrt(x * x + (y + step) * (y + step));
            if (z < 0) z = 0;
            glVertex3f(x, y + step, z);
        }
    }
    glEnd();

    glutSwapBuffers();
}

void init() {
    glEnable(GL_DEPTH_TEST); // Ativa o teste de profundidade para renderizar em 3D
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f); // Fundo preto
    glMatrixMode(GL_PROJECTION); // Matriz de projeção
    gluPerspective(45.0, 1.0, 0.1, 100.0); // Define a perspectiva 3D
    glMatrixMode(GL_MODELVIEW); // Matriz de visualização
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH); // Buffers duplos com profundidade
    glutInitWindowSize(800, 600); // Tamanho da janela
    glutCreateWindow("3D Cone Surface with Grid and Axes");

    init(); // Inicializa configurações
    glutDisplayFunc(display); // Registra a função de renderização
    glutMainLoop(); // Loop de renderização

    return 0;
}
