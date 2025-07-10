/*
Quadrado Mágico (OBI 2007)
Chama-se de quadrado mágico um arranjo, na forma de um quadrado, de 𝑁 × 𝑁 números inteiros tal que todas as linhas, colunas e diagonais têm a mesma soma. é um quadrado mágico de soma 15, pois todas as linhas (2+7+6 = 15, 9+5+1 = 15 e 4+3+8 = 15), colunas (2 + 9 + 4 = 15, 7 + 5 + 3 = 15 e 6 + 1 + 8 = 15) e diagonais (2 + 5 + 8 = 15 e 6 + 5 + 4 = 15) têm a mesma soma (15).
Escreva um programa que, dado um quadrado, determine se ele é magico ou não e qual a soma dele (caso seja mágico).

Entrada
A entrada contém um único conjunto de testes, que deve ser lido do dispositivo de entrada padrão. A primeira linha da entrada de cada caso de teste contém um inteiro N. As N linhas seguintes contêm N inteiros cada, separados por exatamente um espaço em branco. Os inteiros dentro do quadrado são todos maiores que 0 (zero) e menores que 1.000.

Saída
Seu programa deve imprimir, na saída padrão, uma única linha com um inteiro representando a soma do quadrado mágico ou -1 caso o quadrado não seja mágico.

*/

#include <iostream>
#include <vector>
using namespace std;

int main(){

    int n;
    long int x=0, y=0, a=0, b=0, c=0, d=0;
    cin>>n;

    vector<vector<int>> Q(n, vector<int>(n));//matriz do quadrado

    for (int i=0;i<n;i++){ //lendo o cubo
        for (int j=0;j<n;j++){
            cin>>Q[i][j];
            y+=Q[i][j];
        } 
        if(i==0) x=y; //verifica se as linhas tem a mesma soma
        else if(x!=y) {
            cout << "-1";
            return 0;
        }
        y=0;
    }
    //verificar se a soma das colunas são iguais
    for (int i=0;i<n;i++){ 

        c+=Q[i][i];    //verifica se a soma da diagonal são iguais
        d+=Q[i][n-1-i];

        for (int j=0;j<n;j++){
            b+=Q[j][i];
        }
        if(i==0) a=b; //verifica se as linhas tem a mesma soma
        else if(a!=b) {
            cout << "-1";
            return 0;
        }
        b=0;
    }

    if(c!=d) {
        cout << "-1";
        return 0;
    }
    else cout<<c; //se todos os valores somados forem iguais imprime o resultado

    return 0;
}
