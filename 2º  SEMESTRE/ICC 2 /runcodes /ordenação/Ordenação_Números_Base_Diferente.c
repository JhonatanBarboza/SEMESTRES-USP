#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// Função para converter um número de uma base qualquer para decimal
int converter_para_decimal(const char *numero, int base) {
    int decimal = 0;
    int tamanho = strlen(numero);
    for (int i = 0; i < tamanho; i++) {
        char digito = numero[i];
        int valor;
        
        if (digito >= '0' && digito <= '9') {
            valor = digito - '0';
        } else if (digito >= 'A' && digito <= 'F') {
            valor = digito - 'A' + 10;
        } else if (digito >= 'a' && digito <= 'f') {
            valor = digito - 'a' + 10;
        } else {
            printf("Número inválido na base %d\n", base);
            exit(1);
        }
        
        decimal = decimal * base + valor;
    }
    return decimal;
}

// Função para converter um número decimal para uma base qualquer
void converter_de_decimal(int numero, int base, char *resultado) {
    char temp[5];
    int indice = 0;
    
    do {
        int resto = numero % base;
        if (resto < 10) {
            temp[indice++] = '0' + resto;
        } else {
            temp[indice++] = 'A' + (resto - 10);
        }
        numero /= base;
    } while (numero > 0);
    
    // Reverter a string temporária para obter o número na base correta
    for (int i = 0; i < indice; i++) {
        resultado[i] = temp[indice - i - 1];
    }
    resultado[indice] = '\0';
}

// Função de comparação para `qsort`
int comparar(const void *a, const void *b, void *ponteiro_base) {
    int base = *(int *)ponteiro_base;
    int decimal_a = converter_para_decimal(*(const char **)a, base);
    int decimal_b = converter_para_decimal(*(const char **)b, base);
    return decimal_a - decimal_b;
}

int main() {
    int base, quantidade_elementos;
    scanf("%d", &base);
    
    scanf("%d", &quantidade_elementos);
    
    char numeros[quantidade_elementos][5];  // Máximo de 4 dígitos, mais o caractere nulo
    char *ponteiros_numeros[quantidade_elementos];
    
    for (int i = 0; i < quantidade_elementos; i++) {
        scanf("%s", numeros[i]);
        ponteiros_numeros[i] = numeros[i];
    }
    
    // Ordenar usando qsort com a função de comparação personalizada
    qsort_r(ponteiros_numeros, quantidade_elementos, sizeof(char *), comparar, &base);
    
    // Imprimir os números ordenados na base original
    for (int i = 0; i < quantidade_elementos; i++) {
        printf("%s ", ponteiros_numeros[i]);
    }
    printf("\n");

    return 0;
}
