/*
Faça um programa para ler um inteiro N, depois N inteiros positivos. Imprima a quantidade de cada algarismo que apareceu nos N números lidos.

Entrada

A entrada consiste de múltiplas linhas. A primeira linha contém um inteiro N representando a quantidade de número que serão fornecidos. Então seguirão N números inteiros.

Saída
A saída consiste de 10 linhas, cada linha contém um algarismo e o número de ocorrências desse algarismo.
*/

#include <bits/stdc++.h>

using namespace std;

int main(){

    int n, tam=0, cont=1, temp=0;
    cin>>n;
    int m[n], saida[10]={0};

    for (int i=0;i<n;i++){
        cin>>m[i];
    }

    for (int i=0;i<n;i++){ 
        cont=1;
        tam=0;
        for(int j=0;true;j++){
            if((m[i]/cont)!=0){
                cont*=10;
                tam++;
            }
            else {
                break;
            }
        }
        for(int j=0;j<tam;j++){
            temp = m[i]%10;
            m[i]=m[i]/10;
            for (int k=0;k<10;k++){
                if (temp==k) saida[k]+=1;
            }
        }
    }

    for (int i=0;i<10;i++){
        cout<<i<<" - "<<saida[i]<<endl;

    }

    return 0;
}
