/*
Descrição do Problema:
Bino obteve média 
𝑁
1
N 
1
​
  na primeira etapa do semestre e média 
𝑁
2
N 
2
​
  na segunda etapa do semestre. Ele não sabe sua situação e quer sua ajuda para determinar se está aprovado, reprovado ou se precisa fazer a prova final.

A nota final de Bino é calculada utilizando uma média ponderada, onde o peso de 
𝑁
1
N 
1
​
  é 2 e o peso de 
𝑁
2
N 
2
​
  é 3. As regras são as seguintes:

Se a média final for maior ou igual a 7, Bino está aprovado.

Se a média final for menor que 3, Bino está reprovado.

Caso contrário, Bino terá que fazer a prova final.

Entrada:
A entrada consiste de duas linhas:

A primeira linha contém um número real representando a nota 
𝑁
1
N 
1
​
 .

A segunda linha contém um número real representando a nota 
𝑁
2
N 
2
​
 .

Saída:
A saída consiste de uma linha:

Caso Bino esteja aprovado, imprima "Aprovado".

Caso Bino esteja reprovado, imprima "Reprovado".

Caso Bino não esteja nem aprovado nem reprovado, imprima "Final".


*/

#include <iostream>
using namespace std;

int main (){

    int n, m;
    cin>>n>>m;

    n = (n*2+m*3)/5;

    if (n>=7) cout<<"Aprovado";
    else if (n<3) cout<<"Reprovado";
    else cout<<"Final";

    return 0;
}