/*
Fatorial
Faça um programa para ler um inteiro 
𝑁
N e imprima o valor de 
𝑁
!
N!. 
𝑁
!
N! significa fatorial de 
𝑁
N.

Entrada
A entrada consiste de uma linha contendo o valor de 
𝑁
N.

Saída
A saída consiste de uma linha contendo o valor de 
𝑁
!
N!.

*/

#include <iostream>
using namespace std;

int main(){

    int n, alx, cont;
    cin>>n;
    alx=n;
    cont=n;

    if (n==0 || n==1){
        cout<<"1";
        return 0;
    }

    for (int i=2;i<n;i++){
        alx *= (cont-1);
        cont--;
    }

    cout<<alx;

    return 0;
}