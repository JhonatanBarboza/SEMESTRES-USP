#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_PALAVRAS 244770  // Limite para o número máximo de palavras válidas
#define MAX_TAMANHO_PALAVRA 50 // Tamanho máximo de cada palavra

// Estrutura para armazenar as palavras válidas
char palavras_validas[MAX_PALAVRAS][MAX_TAMANHO_PALAVRA]; // Grarda as palavras que contenhão as letra obrigatoria e as iniciais 
int total_palavras_validas = 0;                           // Quantidade de palavras carregadas 
int palavras_encontradas[MAX_PALAVRAS] = {0};             // Flag para palavras encontradas (0 não encotrada 1 encotrada)

char obrigatoria;
char letras[7];
int total_palavras[5] = {0};      // Total de palavras com 4, 5, 6, 7 e 8 letras
int progresso_palavras[5] = {0};  // Total de palavras corretas com 4, 5, 6, 7 e 8 letras (quanto o usuario já acertou)


// Função para verificar se a palavra contém a letra obrigatória
bool verifica_letra_obrigatoria(const char *palavra) {
    for (int i = 0; palavra[i] != '\0'; i++) {
        if (palavra[i] == obrigatoria) { // Verifica se a letra obrigatória está na palavra
            return true;
        }
    }
    return false;
}

// Função para verificar se uma palavra pode ser formada com as letras iniciais
bool verifica_letras(const char *palavra) {
    for (int i = 0; palavra[i] != '\0'; i++) {
        if (strchr(letras, palavra[i]) == NULL) { // Se a letra não estiver em `letras`
            return false;
        }
    }
    return true;
}

// Função para carregar palavras válidas a partir do arquivo, considerando apenas palavras com letras permitidas
int carregar_palavras(const char *nome_arquivo) {
    FILE *file = fopen(nome_arquivo, "r");
    if (!file) {
        printf("Erro ao abrir o arquivo %s\n", nome_arquivo);
        return 0;
    }

    char palavra[MAX_TAMANHO_PALAVRA];
    total_palavras_validas = 0;

    while (fscanf(file, "%s", palavra) != EOF) {
        if (verifica_letra_obrigatoria(palavra) && verifica_letras(palavra)) {  // Verifica se a palavra é composta apenas pelas letras permitidas
            int len = strlen(palavra);
            if (len >= 4 && len <= 8) {  // Considera palavras de 4 a 8 letras
                strncpy(palavras_validas[total_palavras_validas], palavra, MAX_TAMANHO_PALAVRA);
                total_palavras_validas++;
                total_palavras[len - 4]++;  // Incrementa o contador correspondente (4 letras => índice 0)
            }
        }
    }

    fclose(file);
    return 1;
}

// Função para inicializar o jogo com a letra obrigatória e as letras
void inicio(char obrig, char* letras_iniciais) {
    obrigatoria = obrig;
    letras[0] = obrigatoria;  // Coloca a letra obrigatória no início
    strncpy(letras+1, letras_iniciais, 6);  // Copia as 7 letras a partir da posição 1
    carregar_palavras("valid_words.txt");
}

// Função para verificar se uma palavra é válida
int palavra(char* entrada) {
    // Verifica se a palavra contém a letra obrigatória
    if (strchr(entrada, obrigatoria) == NULL) {
        printf("palavra invalida\n");
        return 0;
    }
    // Verifica se todas as letras da palavra estão na lista permitida
    for (int i = 0; i < strlen(entrada); i++) {
        if (strchr(letras, entrada[i]) == NULL) {
            printf("palavra invalida\n");
            return 0;
        }
    }

    // Busca a palavra na lista de palavras válidas
    for (int i = 0; i < total_palavras_validas; i++) {
        if (strcmp(entrada, palavras_validas[i]) == 0) {
            if (palavras_encontradas[i] == 0) { // Verifica se já não foi encontrada
                printf("sucesso + 1\n");
                palavras_encontradas[i] = 1;
                progresso_palavras[strlen(entrada) - 4]++;
                return 1;
            } else {
                printf("palavra ja encontrada\n");
                return 1;
            }
        }
    }
    printf("palavra invalida\n");
    return 0;
}

// Função para mostrar o progresso
void progresso() {
    printf("progresso atual:\n");
    for (int i = 0; i < 5; i++) {
        int faltando = total_palavras[i] - progresso_palavras[i];
        printf("(%d letras) %d palavra(s) encontrada(s) / %d palavra(s) faltando\n", i + 4, progresso_palavras[i], faltando);
    }
}

// Função para mostrar a solução final com as palavras restantes
void solucao() {
    printf("para encerrar o jogo estavam faltando as palavras: \n");

    // Exibir palavras de acordo com o número de letras
    for (int len = 4; len <= 8; len++) {
        printf("(%d letras) ", len);

        // Percorrer as palavras válidas e imprimir as que têm o tamanho adequado
        int encontrou_palavra = 0;
        for (int i = 0; i < total_palavras_validas; i++) {
            if (strlen(palavras_validas[i]) == len) {
                if (encontrou_palavra) printf(", ");
                printf("%s", palavras_validas[i]);
                encontrou_palavra = 1;
            }
        }
        printf("\n");
    }

    printf("fim!\n");
}

// Função principal com entrada interativa do usuário
int main() {
    char comando[50];
    char entrada[50];
    
    while (1) {
        scanf("%s", comando);

        if (strcmp(comando, "inicio") == 0) {  // Lê a letra obrigatória e as outras letras
            char letras_ini[7];
            // Lê a letra obrigatória
            scanf(" %c", &obrigatoria);
            
            // Lê as outras letras uma por uma
            for(int i = 0; i < 6; i++) {  // Lê 6 letras adicionais
                scanf(" %c", &letras_ini[i]);
            }
            letras_ini[6] = '\0';  // Adiciona o terminador de string
            
            inicio(obrigatoria, letras_ini);
        } 
        else if (strcmp(comando, "palavra") == 0) { // Lê a palavra a ser verificada
            scanf(" %s", entrada);
            palavra(entrada);
        } 
        else if (strcmp(comando, "progresso") == 0) {
            progresso();
        } 
        else if (strcmp(comando, "solucao") == 0) {
            solucao();
            break;
        } 
        else {
            printf("Comando desconhecido: %s\n", comando);
        }

        int contA, contB;
        for (int j=0 ; j<5 ; j++){
            contA += total_palavras[j];
            contB += progresso_palavras[j]; 
        }

        if (contA == contB){
            printf("parabens! voce encontrou todas as palavras");
            return 0;
        }
        else {
            contA = 0;
            contB = 0;
        }
    }
    
    return 0;
}