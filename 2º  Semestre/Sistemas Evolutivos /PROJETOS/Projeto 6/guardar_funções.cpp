
/*
// Função para desenhar a superfície do cone opaca
void drawConeSurface() {
    float step = 0.5;
    glColor3f(0.0f, 0.7f, 0.7f); // Cor do cone (ciano)
    glBegin(GL_QUADS);
    for (float x = -50.0; x <= 50.0; x += step) {
        for (float y = -50.0; y <= 50.0; y += step) {
            float z = c - sqrt(x * x + y * y);
            if (z < 0) z = 0;
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
}*/


/*
// Função para desenhar os eixos
void drawAxes() {
    glLineWidth(2.0f); // Aumenta a espessura das linhas

    glBegin(GL_LINES);

    // Eixo X em vermelho
    glColor3f(0.0f, 0.0f, 1.5f);
    glVertex3f(-60.0f, 0.0f, 0.0f);
    glVertex3f(60.0f, 0.0f, 0.0f);

    // Eixo Y em verde
    glColor3f(0.0f, 0.0f, 1.5f);
    glVertex3f(0.0f, -60.0f, 0.0f);
    glVertex3f(0.0f, 60.0f, 0.0f);

    // Eixo Z em azul
    glColor3f(0.0f, 0.0f, 1.5f);
    glVertex3f(0.0f, 0.0f, -10.0f);
    glVertex3f(0.0f, 0.0f, 60.0f);

    glEnd();
}*/
/*
// Função para desenhar uma grade no plano XY
void drawGrid() {
    glColor3f(0.5f, 0.5f, 0.5f); // Cor da grade (cinza)
    glLineWidth(1.0f); // Aumenta a espessura da linha da grade
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
}*/
