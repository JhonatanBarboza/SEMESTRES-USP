/*
Torre
Dada uma matriz quadrada 𝑀 de números naturais, o índice 𝑖 de uma certa linha e o índice 𝑗 de uma certa coluna, vamos definir o peso do cruzamento da linha  𝑖 com a coluna  𝑗, como sendo a soma de todos os elementos que estejam na linha  𝑖 ou na coluna  𝑗, mas não nas duas. Quer dizer, excluindo o elemento que está exatamente no cruzamento! Neste problema, você deve descobrir qual é o peso máximo entre todos os possíveis cruzamentos da matriz!

No jogo de xadrez, a torre é uma peça que pode se mover para qualquer outra posição do tabuleiro na linha ou na coluna da posição que ela ocupa. O professor Paulo está tentando inventar um novo tipo de jogo de xadrez onde todas as peças são torres, o tabuleiro também é quadrado mas pode ter qualquer dimensão e cada posição do tabuleiro é anotada com um número inteiro positivo, como na figura abaixo.

Ele definiu o peso de uma posição (i,j) como sendo a soma de todos os números que estejam na linha i com todos os números da coluna j, mas sem somar o número que está exatamente na posição (i,j). Quer dizer, se uma torre estiver na posição (i,j), o peso da posição é a soma de todas as posições que essa torre poderia atacar. O professor Paulo está solicitando a sua ajuda para implementar um programa que determine qual é o peso máximo entre todas as posições do tabuleiro.

No exemplo da figura acima, com um tabuleiro de dimensão seis (ou seja, seis linhas por seis colunas), o peso máximo é 67, referente à posição (4,4).
*/

#include <iostream>
#include <vector>
using namespace std;

int main(){

    int n, somaL=0, somaC=0, max=0;
    cin>>n;
    vector <vector<int>> tabuleiro (n, vector<int>(n, 0));
    int linhas[n];
    int colunas[n];

    for (int i=0;i<n;i++){ // lendo os valores do tabuleiro 
        for (int j=0;j<n;j++){
            cin>>tabuleiro[i][j];
        }
    }

    for (int i=0;i<n;i++){ // Achando a soma das linhas e colunas
        for (int j=0;j<n;j++){
            somaL += tabuleiro [i][j];
            somaC += tabuleiro [j][i];
        }
        linhas[i] = somaL;
        colunas[i] = somaC; 
        somaL=0;
        somaC=0;
    }

    for (int i=0;i<n;i++){ 
        for (int j=0;j<n;j++){
            if(max<(linhas[i]+colunas[j])-(tabuleiro[i][j]*2)){
                max = (linhas[i]+colunas[j])-(tabuleiro[i][j]*2);
            }
        }
    }

    cout<<max;


    return 0;
}