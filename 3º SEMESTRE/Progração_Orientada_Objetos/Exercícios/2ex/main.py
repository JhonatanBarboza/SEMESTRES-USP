from tabuleiro import Tabuleiro

def main():
    try:
        # Le a primeira linha (posiçao inicial do tabuleiro)
        linhaTabuleiro = input().strip()
        partes = linhaTabuleiro.split()
        tamtab = len(partes)
        estadoInicial = [int(num) for num in partes]
        
        # Le a segunda linha (movimentos)
        movimentos = input().strip()
        
        # Cria o tabuleiro com o estado inicial
        tabuleiro = Tabuleiro(estadoInicial, tamtab)
        print(tabuleiro)
        
        # Executa os movimentos
        for movimento in movimentos:
            tabuleiro.mover(movimento)
            print(tabuleiro)  # imprime o tabuleiro após cada movimento
        
        # Verifica se o tabuleiro é a solução
        if tabuleiro.ehSolucao(tamtab):
            print("Posicao final: True")
        else:
            print("Posicao final: False")
            
    except Exception as e:
        pass

if __name__ == "__main__":
    main()