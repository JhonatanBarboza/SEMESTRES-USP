/*
Raízes
Seu professor lhe passou um exercício onde você deve encontrar a raiz quadrada de vários números, como você não quer perder tempo com essa tarefa tosca e sem sentido você resolveu fazer um programa que dados 
𝑁
N números ele retorna a raiz quadrada de cada um desses números

Entrada
A primeira linha de entrada contém um número inteiro 
𝑁
N representando a quantidade de números dos quais você terá que responder qual a raiz quadrada. A segunda linha da entrada contém os 
𝑁
N números separados por um espaço em branco.

Saída
Seu programa deve imprimir 
𝑁
N linhas, cada uma contendo a raiz do número na ordem, cada raiz com precisão de 4 casas decimais.

*/

#include <iostream>
#include <iomanip>
#include <math.h>
using namespace std;

int main(){

    int n;
    float num;
    cin>>n; //recebendo o numero 

    for (int i=0;i<n;i++){
        cin>>num;
        num=sqrt(num);
        printf("%.4f\n", num); //imprimir a raiz
    }

    return 0;
}
