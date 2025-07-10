class Tabuleiro:

    # Construtor da classe Tabuleiro
    def __init__(self, estadoInicial, tamanho):
        self.dimensao = int(tamanho ** 0.5)
        self.tabuleiro = [[0 for _ in range(self.dimensao)] for _ in range(self.dimensao)]
        self.linhaVazia = 0
        self.colunaVazia = 0
        
        for i in range(tamanho):
            linha = i // self.dimensao
            coluna = i % self.dimensao
            self.tabuleiro[linha][coluna] = estadoInicial[i]
            
            if estadoInicial[i] == 0:
                self.linhaVazia = linha
                self.colunaVazia = coluna
    
    # Método para mover a peça vazia (0) na direção especificada
    # 'u' para cima, 'd' para baixo, 'l' para esquerda, 'r' para direita
    def mover(self, direcao):
        if direcao == 'u':
            return self.moverParaCima()
        elif direcao == 'd':
            return self.moverParaBaixo()
        elif direcao == 'l':
            return self.moverParaEsquerda()
        elif direcao == 'r':
            return self.moverParaDireita()
        else:
            return False
    
    # Método para mover a peça vazia para cima
    def moverParaCima(self):
        if self.linhaVazia >= self.dimensao - 1:
            return False
        self.tabuleiro[self.linhaVazia][self.colunaVazia] = self.tabuleiro[self.linhaVazia + 1][self.colunaVazia]
        self.tabuleiro[self.linhaVazia + 1][self.colunaVazia] = 0
        self.linhaVazia += 1
        return True
    
    # Método para mover a peça vazia para baixo
    def moverParaBaixo(self):
        if self.linhaVazia <= 0:
            return False
        self.tabuleiro[self.linhaVazia][self.colunaVazia] = self.tabuleiro[self.linhaVazia - 1][self.colunaVazia]
        self.tabuleiro[self.linhaVazia - 1][self.colunaVazia] = 0
        self.linhaVazia -= 1
        return True
    
    # Método para mover a peça vazia para a esquerda
    def moverParaEsquerda(self):
        if self.colunaVazia >= self.dimensao - 1:
            return False
        self.tabuleiro[self.linhaVazia][self.colunaVazia] = self.tabuleiro[self.linhaVazia][self.colunaVazia + 1]
        self.tabuleiro[self.linhaVazia][self.colunaVazia + 1] = 0
        self.colunaVazia += 1
        return True
    
    # Método para mover a peça vazia para a direita
    def moverParaDireita(self):
        if self.colunaVazia <= 0:
            return False
        self.tabuleiro[self.linhaVazia][self.colunaVazia] = self.tabuleiro[self.linhaVazia][self.colunaVazia - 1]
        self.tabuleiro[self.linhaVazia][self.colunaVazia - 1] = 0
        self.colunaVazia -= 1
        return True
    
    # Método para verificar se o tabuleiro é a solução
    def ehSolucao(self, tamanho):
        contador = 1
        for i in range(self.dimensao):
            for j in range(self.dimensao):
                if i == 0 and j == 0:
                    if self.tabuleiro[i][j] != 0:
                        return False
                else:
                    if self.tabuleiro[i][j] != contador:
                        return False
                    contador += 1
        return True
    
    # Método para imprimir o tabuleiro
    def __str__(self):
        sb = []
        linha_separadora = "+------" * self.dimensao + "+\n"
        sb.append(linha_separadora)
        
        for i in range(self.dimensao):
            sb.append("|")
            for j in range(self.dimensao):
                if self.tabuleiro[i][j] == 0:
                    sb.append("      |")
                else:
                    sb.append(f"  {self.tabuleiro[i][j]:2d}  |")
            sb.append("\n" + linha_separadora)
        
        return "".join(sb)