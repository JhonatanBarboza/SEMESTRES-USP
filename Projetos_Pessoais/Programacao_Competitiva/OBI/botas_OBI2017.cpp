#include <bits/stdc++.h>
using namespace std;

int main(){

    int n;
    cin>>n;
    
    int tam [n][3];
    char pe;
    
    for (int i=0;i<n;i++){
        cin >> tam [i][0];
        cin >> tam [i][1];
        tam [i][2]=1;
        cin >> pe;
        if (pe=='D') tam [i][2]=0;
        if (pe=='E') tam [i][2]=1;
    }

    sort (tam[][].begin(), tam[][].end());

    return 0;
}