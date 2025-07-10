/*
Fita Colorida
Roberto tem um conjunto de lápis com 10 tons diferentes de uma mesma cor, numerados de 0 a 9. Numa fita quadriculada, alguns quadrados foram coloridos inicialmente com o tom 0.

Roberto precisa determinar, para cada quadrado 
𝑄
Q não colorido, qual é a distância dele para o quadrado mais próximo de tom 0. A distância entre dois quadrados é definida com o número mínimo de movimentos para a esquerda, ou para a direita, para ir de um quadrado para o outro. O quadrado 
𝑄
Q, então, deve ser colorido com o tom cuja numeração corresponde à distância determinada. Se a distância for maior ou igual a 9, o quadrado deve ser colorido com o tom 9. Seu programa deve colorir e imprimir a fita quadriculada dada na entrada.

Entrada
A primeira linha da entrada contém apenas um inteiro 
𝑁
N, indicando o número de quadrados da fita. A segunda linha contém 
𝑁
N números inteiros: “-1” se o quadrado não está colorido, e “0” se está colorido com o tom 0.

Saída
Seu programa deve escrever na saída a fita totalmente colorida, de acordo com a regra definida acima.
*/


#include <iostream>
using namespace std;

int main(){

    int n, contdir=1, contesq=1;
    cin>>n;
    int m[n], fita[n];

    //le dados do usuario e atribui o valor maximo os quadrados da fita 
    for (int i=0;i<n;i++){
        cin>>m[i];
        fita[i]=9; //a cor não passara de 9
    }

    for(int i=0;i<n;i++){
        if (m[i]==0){ //encontra e atribui os zeros a fita 
            fita[i]=0;
            contdir=1;
            contesq=1;
            for (int j=i;j<n-1;j++){ //atribui numeros crecentes a direta do zero
                if (m[j+1]==-1 && fita[j+1]>contdir) {
                    fita[j+1]=contdir;
                    contdir++;
                }
                else break; // sai do laço se encontrar um zero ou um numero menor 
            }
            for (int j=i;j>0;j--){//atribui numeros crecentes a esquerda do zero
                if (m[j-1]==-1 && fita[j-1]>contesq){
                    fita[j-1]=contesq;
                    contesq++;
                }
                else break; // sai do laço se encontrar um zero ou um numero menor 
            }
        }
    }

    for (int i=0;i<n;i++){ //imprime a fita
        cout<<fita[i];
        if(i+1<n) cout<<" ";
    }

    return 0;
}



