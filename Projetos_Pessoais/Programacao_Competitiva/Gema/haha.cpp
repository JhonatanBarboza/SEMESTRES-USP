#include <iostream>
#include <string>
using namespace std;

int main(){

    string rir, vog;
    cin>>rir;

    for (char letra : rir){
        if (letra =='a' || letra=='e' || letra=='i' || letra=='o' || letra=='u'){
            vog+=letra;
        }
    }
    int tamvog = vog.size();

    for (int i = 0; i < tamvog / 2; i++) { 
        if (vog[i] != vog[tamvog - i - 1]) {
            cout << "N" << endl;
            return 0;
        }
    }

    cout<<"S";

    return 0;
}