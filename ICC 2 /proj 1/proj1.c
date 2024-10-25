#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define MAX_DESCR 50

typedef struct horas
{
    int hh;
    int mm;
    int ss;
}horario;

typedef struct process
{
    int prior;
    horario chegada;
    char descricao[MAX_DESCR];
}celula;

int hcmp(horario h1, horario h2) 
{
    //Se h1<h2, return 1, se h2<h1 return 0
    if(h1.hh < h2.hh)
    {
        return 1;
    }
    else
    {
        if(h1.hh > h2.hh)
        {
            return 0;
        }
        else
        {
            if(h1.mm < h2.mm)
            {
                return 1;
            }
            else
            {
                if(h1.mm < h2.mm)
                {
                    return 0;
                }
                else
                {
                    if(h1.ss < h2.ss)
                    {
                        return 1;
                    }
                    else
                    {
                        return 0;
                    }
                }
            }
        }
    }
}

void intercala_p(celula* lista, int ini, int meio, int fim)
{
    int k=0,j=0;
    int n1 = meio-ini+1;
    int n2 = fim-meio;
    celula lvet[n1+1];
    celula rvet[n2+1];
    for(int i=0;i<n1;i++)
    {
        lvet[i] = lista[ini+i];
    }
    for(int i=0;i<n2;i++)
    {
        rvet[i] = lista[meio+1+i];
    }
    lvet[n1].prior = -999999;
    rvet[n2].prior = -999999;
    for(int i=ini;i<=fim;i++)
    {
        if(lvet[j].prior > rvet[k].prior)
        {
            lista[i] = lvet[j];
            j++;
        }
        else
        {
            lista[i] = rvet[k];
            k++;
        }
    }
}

void intercala_t(celula* lista, int ini, int meio, int fim)
{
    int k=0,j=0;
    int n1 = meio-ini+1;
    int n2 = fim-meio;
    celula lvet[n1+1];
    celula rvet[n2+1];
    for(int i=0;i<n1;i++)
    {
        lvet[i] = lista[ini+i];
    }
    for(int i=0;i<n2;i++)
    {
        rvet[i] = lista[meio+1+i];
    }
    lvet[n1].chegada.hh = 999999;
    rvet[n2].chegada.hh = 999999;
    for(int i=ini;i<=fim;i++)
    {
        if(hcmp(lvet[j].chegada, rvet[k].chegada))
        {
            lista[i] = lvet[j];
            j++;
        }
        else
        {
            lista[i] = rvet[k];
            k++;
        }
    }
}

void mergesort(celula* lista, int ini, int fim, char c)
{
    int meio;
    if(ini < fim)
    {
        meio = (fim+ini)/2;
        mergesort(lista, ini, meio, c);
        mergesort(lista, meio+1, fim, c);
        if(c == 'p')
        {
            intercala_p(lista, ini, meio, fim);
        }
        else
        {
            intercala_t(lista, ini, meio, fim);
        }
        
    }
}

int identifica(char* comando)
{
    //add - 0, exec - 1, next - 2, change - 3, print - 4
    char* str;
    int cont=0, flag=0;
    str = (char*) malloc(6*sizeof(char));
    for(int i=0;i<6 && flag==0;i++)
    {
        if(comando[i] != ' ')
        {
            str[i] = comando[i];
            cont++;
        }
        else
        {
            flag = 1;
        }
    }
    str = realloc(str, cont);
    if(strcmp(str, "add") == 0)
    {
        return 0;
    }
    if(strcmp(str, "next") == 0)
    {
        return 2;
    }
    if(strcmp(str, "exec") == 0)
    {
        return 1;
    }
    if(strcmp(str, "change") == 0)
    {
        return 3;
    }
    if(strcmp(str, "print") == 0)
    {
        return 4;
    }
}

void adiciona(celula *lista, char* comando, int cont)
{
    int k=0;
    int i=4;
    if(comando[i+1] != ' ')
    {
        lista[cont].prior = (comando[i]- 48)*10 + comando[i+1] - 48;
        i = i+3;
    }
    else
    {
        lista[cont].prior = comando[i] - 48;
        i = i+2;
    }
    //printf("%d\n", p->prior);
    for(int j=0;j<8;j = j + 3)
    {
        if(j == 0)
        {
            lista[cont].chegada.hh = (comando[i+j]-48)*10 + comando[i+j+1] - 48;
        }
        else
        {
            if(j == 3)
            {
                lista[cont].chegada.mm = (comando[i+j]-48)*10 + comando[i+j+1] - 48;
            }
            else
            {
                lista[cont].chegada.ss = (comando[i+j]-48)*10 + comando[i+j+1] - 48;
            }
        }
        
    }
    i = i + 9;
    while(comando[i+k] != '\0')
    {
        lista[cont].descricao[k] = comando[i+k];
        k++;
    }
    //printf("%s\n", p->descricao);
}

void printa_lista(celula *lista, int tam)
{
    for(int i=0;i<tam;i++)
    {
        printf("%02d %02d:%02d:%02d %s\n",lista[i].prior, lista[i].chegada.hh,
                                    lista[i].chegada.mm, lista[i].chegada.ss, 
                                    lista[i].descricao);
    }
    printf("\n");
}

void troca_p(celula* lista, int prior, int prior_, int tam)
{
    int achou = 0;
    for(int i=0;i<tam && achou == 0;i++)
    {
        if(lista[i].prior == prior)
        {
            lista[i].prior = prior_;
            achou = 1;
        }
    }
}

void troca_t(celula* lista, int h, int m, int s, int h_, int m_, int s_, int tam)
{
    int achou = 0;
    for(int i=0;i<tam && achou == 0;i++)
    {
        if(lista[i].chegada.hh == h && lista[i].chegada.mm == m && lista[i].chegada.ss == s)
        {
            lista[i].chegada.hh = h_;
            lista[i].chegada.mm = m_;
            lista[i].chegada.ss = s_;
            achou = 1;
        }
    }
}


int main()
{
    horario aux1, aux2;
    int prior1, prior2;
    char comando[67];
    celula *lista_p;
    int n, cont=0, cont1=0;
    lista_p = (celula*) malloc(sizeof(celula));
    do
    {
        scanf(" %[^\n]", comando);
        n = identifica(comando);
        switch(n)
        {
            case 0:
                if(cont == 0)
                {
                    adiciona(lista_p, comando, cont);
                    cont++;
                }
                else
                {
                    lista_p = realloc(lista_p, (cont + 1)*sizeof(celula));
                    adiciona(lista_p, comando, cont);
                    cont++;
                }
                break;
            case 1:
                cont1 = cont-1;
                if(comando[6] == 'p')
                {
                    mergesort(lista_p, 0, cont-1, 'p');
                }
                else
                {
                    mergesort(lista_p, 0, cont-1, 't');
                }
                for(int k=1;k<cont;k++)
                {
                    lista_p[k-1] = lista_p[k];
                }
                lista_p = realloc(lista_p, (cont-1)*sizeof(celula));
                cont--;
                break;
            case 2:
                if(comando[6] == 'p')
                {
                    mergesort(lista_p, 0, cont-1, 'p');
                    printf("%02d %02d:%02d:%02d %s\n",lista_p[0].prior, lista_p[0].chegada.hh,
                                    lista_p[0].chegada.mm, lista_p[0].chegada.ss, 
                                    lista_p[0].descricao);
                    printf("\n");
                }
                else
                {
                    mergesort(lista_p, 0, cont-1, 't');
                    printf("%02d %02d:%02d:%02d %s\n",lista_p[0].prior, lista_p[0].chegada.hh,
                                    lista_p[0].chegada.mm, lista_p[0].chegada.ss, 
                                    lista_p[0].descricao);
                    printf("\n");
                }
                break;
            case 3:
                if(comando[8] == 'p')
                {
                    if(comando[11] < 48 || comando[11] > 57)
                    {
                        prior1 = comando[10] - 48;
                        if(comando[13] < 48 || comando[13] > 57)
                        {
                            prior2 = comando[12] - 48;
                        }
                        else
                        {
                            prior2 = (comando[12] - 48)*10 + comando[13] - 48;
                        }
                    }
                    else
                    {
                        prior1 = (comando[10] - 48)*10 + comando[11] - 48;
                        if(comando[14] < 48 || comando[14] > 57)
                        {
                            prior2 = comando[13] - 48;
                        }
                        else
                        {
                            prior2 = (comando[13] - 48)*10 + comando[14] - 48;
                        }
                    }
                    troca_p(lista_p, prior1, prior2, cont);
                }
                else
                {
                    aux1.hh = (comando[10]-48)*10+comando[11]-48;
                    aux1.mm = (comando[13]-48)*10+comando[14]-48;
                    aux1.ss = (comando[16]-48)*10+comando[17]-48;
                    aux2.hh = (comando[19]-48)*10+comando[20]-48;
                    aux2.mm = (comando[22]-48)*10+comando[23]-48;
                    aux2.ss = (comando[25]-48)*10+comando[26]-48;
                    troca_t(lista_p, aux1.hh, aux1.mm, aux1.ss, aux2.hh, aux2.mm, aux2.ss, cont);
                }
                break;
            case 4:
                if(comando[7] == 'p')
                {
                    mergesort(lista_p, 0, cont-1, 'p');
                    printa_lista(lista_p, cont);
                }
                else
                {
                    mergesort(lista_p, 0, cont-1, 't');
                    printa_lista(lista_p, cont);
                }
                break;
        }
    }
    while(strcmp(comando, "quit") != 0);
    return 0;
}