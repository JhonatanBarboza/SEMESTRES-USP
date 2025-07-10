#include <iostream>
#include <string>
using namespace std;

int main(){

    int n, cont=0;
    cin>>n;

    string gabarito;
    string resposta;

    cin>>gabarito;
    cin>>resposta;

    for (int i=0;i<n;i++){
        if (gabarito[i]==resposta[i]) cont++;
    }

    cout<<cont<<endl;
    
    return 0;
}