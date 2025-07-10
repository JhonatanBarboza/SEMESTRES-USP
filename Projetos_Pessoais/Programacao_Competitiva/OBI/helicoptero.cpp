/*
Um fugitivo, um helicóptero e um policial estão em posições distintas numa pista circular, exatamente como a mostrada na figura ao lado, com dezesseis posições numeradas de 0 a 15 em direção anti-horária.

O helicóptero e o policial ficam sempre parados. O objetivo do fugitivo é chegar no helicóptero sem passar pelo policial antes, claro. Ele pode decidir correr na direção horária, ou na direção anti-horária.

Neste problema, dadas as posições do helicóptero, do policial e do fugitivo, e a direção em que o fugitivo decide correr, seu programa deve dizer se ele vai ou não conseguir fugir! Na figura, se o fugitivo decidir correr na direção horária, ele consegue fugir; se decidir correr na direção anti-horária, ele vai ser preso antes de chegar no helicóptero!

Entrada
A entrada consiste de uma linha com quatro inteiros: H, P, F e D, representando, respectivamente, as posições do helicóptero, do policial e do fugitivo, e a direção em que o fugitivo corre, -1 para horário e 1 para anti-horário.

Saída
Seu programa deve imprimir uma linha contendo o caractere "S" se o fugitivo consegue fugir, ou "N" caso contrário
*/

#include <bits/stdc++.h>
using namespace std;

int main(){

    int h, p, f, d;
    cin>>h>>p>>f>>d;

    deque<int> deque;

    if (d==1){
        for (int i=0;i<15;i++){
            deque.push_front(f);
            f++;
            if (f==16) f=0;
        }
    }
    if (d==-1){
        for (int i=0;i<15;i++){
            deque.push_front(f);
            f--;
            if (f==-1) f=15;
        }
    }

    while (true){
        if (deque.back()==h){
            cout<<"S";
            break;
        }
        else if (deque.back()==p){
            cout<<"N";
            break;
        }
        else{
            deque.pop_back();
        }
    }
    
    return 0;
}


