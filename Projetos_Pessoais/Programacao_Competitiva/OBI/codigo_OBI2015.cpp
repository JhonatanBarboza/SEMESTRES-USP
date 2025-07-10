#include <iostream>
using namespace std;

int main (){
 
    int n, num=0;
    cin>>n;
    if (n<3) {
        cout<<"0"<<endl;
        return 0;
    }
    int *vetor = new int [n];
    for (int i=0;i<n;i++){
        cin>>vetor[i];
    }
    for (int i=0;i<n-2;i++){
        if (vetor[i]==1 && vetor[i+1]==0 && vetor[i+2]==0)
            num++;
    }
    
    cout<<num<<endl;

    delete[] vetor;

    return 0;
}