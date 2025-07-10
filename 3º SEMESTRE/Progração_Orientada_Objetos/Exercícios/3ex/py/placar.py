class Placar:
    def __init__(self):
        """
        Inicializa o placar do jogo Bozó com 10 posições.
        """
        self.POSICOES = 10
        self.placar = [0] * self.POSICOES
        self.taken = [False] * self.POSICOES

    def add(self, posicao, dados):
        """
        Adiciona uma sequência de dados em uma posição do placar.
        
        Args:
            posicao: Posição a ser preenchida (1-10)
            dados: Lista de 5 inteiros representando os valores dos dados
            
        Raises:
            ValueError: Se a posição estiver ocupada ou for inválida
        """
        if posicao < 1 or posicao > self.POSICOES:
            raise ValueError("Valor da posição ilegal")
        if self.taken[posicao-1]:
            raise ValueError("Posição ocupada")
        
        k = 0
        if 1 <= posicao <= 6:
            k = posicao * self.conta(posicao, dados)
        elif posicao == 7:  # full hand
            if self.check_full(dados):
                k = 15
        elif posicao == 8:  # sequência
            if self.check_seq_maior(dados):
                k = 20
        elif posicao == 9:  # quadra
            if self.check_quadra(dados):
                k = 30
        elif posicao == 10:  # quina
            if self.check_quina(dados):
                k = 40
        
        self.placar[posicao-1] = k
        self.taken[posicao-1] = True

    def getScore(self):
        """
        Calcula a pontuação total do placar.
        
        Returns:
            int: Soma dos valores das posições ocupadas
        """
        return sum(score for score, ocupado in zip(self.placar, self.taken) if ocupado)

    def conta(self, n, vet):
        """
        Conta quantas vezes um número aparece em um array.
        
        Args:
            n: Número a ser contado
            vet: Lista de valores
            
        Returns:
            int: Quantidade de ocorrências
        """
        return vet.count(n)

    def check_full(self, dados):
        """
        Verifica se os dados formam um full hand (trinca + par).
        
        Args:
            dados: Lista de 5 valores
            
        Returns:
            bool: True se for full hand
        """
        v = sorted(dados)
        return (v[0] == v[1] == v[2] and v[3] == v[4]) or \
               (v[0] == v[1] and v[2] == v[3] == v[4])

    def check_quadra(self, dados):
        """
        Verifica se os dados formam uma quadra.
        
        Args:
            dados: Lista de 5 valores
            
        Returns:
            bool: True se for quadra
        """
        v = sorted(dados)
        return (v[0] == v[1] == v[2] == v[3]) or \
               (v[1] == v[2] == v[3] == v[4])

    def check_quina(self, dados):
        """
        Verifica se os dados formam uma quina (todos iguais).
        
        Args:
            dados: Lista de 5 valores
            
        Returns:
            bool: True se for quina
        """
        return all(d == dados[0] for d in dados)

    def check_seq_maior(self, dados):
        """
        Verifica se os dados formam uma sequência maior (1-2-3-4-5 ou 2-3-4-5-6).
        
        Args:
            dados: Lista de 5 valores
            
        Returns:
            bool: True se for sequência maior
        """
        v = sorted(dados)
        return all(v[i] == v[i+1]-1 for i in range(4))

    def __str__(self):
        """
        Retorna uma representação visual do placar.
        
        Returns:
            str: String formatada mostrando o placar
        """
        s = ""
        for i in range(3):
            # Linha superior (posições 1-3, 7-9, 4-6)

            num = f" {self.placar[i]:<3}" if self.taken[i] else f"({i+1}) "
            s += num + "   |   "
            
            num = f" {self.placar[i+6]:<3}" if self.taken[i+6] else f"({i+7}) "
            s += num + "   |  "
            
            num = f" {self.placar[i+3]:<3}" if self.taken[i+3] else f"({i+4}) "
            s += num + "\n"
            s += "-------|----------|-------\n"
        
        # Linha inferior (posição 10)
        num = f" {self.placar[9]:<3}" if self.taken[9] else "(10)"
        s += f"       |   {num}   |\n       +----------+\n"
        return s