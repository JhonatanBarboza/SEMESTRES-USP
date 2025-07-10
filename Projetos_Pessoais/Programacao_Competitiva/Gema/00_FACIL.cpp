#include <iostream>
using namespace std;

int main(){

    int a, b, c;
    cin>>a>>b>>c;

    if(a<b && a<c) {
        cout<<1<<endl;
        if (b<c) cout<<2<<endl<<3;
        else cout<<3<<endl<<2;
    }

    if(b<a && b<c) {
        cout<<2<<endl;
        if (a<c) cout<<1<<endl<<3;
        else cout<<3<<endl<<1;
    }

    if(c<a && c<b) {
        cout<<3<<endl;
        if (b<a) cout<<2<<endl<<1;
        else cout<<1<<endl<<2;
    }

    return 0;
}

/*
git add .
git commit -m "gema"
git push
*/