import random

class Dado:
    def __init__(self, n=6, seed=None):
        """
        Cria um dado com número de lados especificado.
        Se nenhum número for fornecido, cria um dado de 6 lados (cubo).
        
        Args:
            n: número de lados do dado (padrão: 6)
            seed: semente para o gerador aleatório (opcional)
        """
        self.lados = n
        self.atual = 1
        self.r = random.Random(seed) if seed is not None else random.Random()
        self.rolar()
        
        # Strings para representação gráfica do dado (6 lados)
        self.s010 = "|  *  |\n"
        self.s100 = "|*    |\n"
        self.s001 = "|    *|\n"
        self.s000 = "|     |\n"
        self.s101 = "|*   *|\n"
        self.s111 = "|* * *|\n"

    def rolar(self):
        """
        Simula a rolagem do dado usando um gerador aleatório.
        
        Returns:
            int: o número sorteado
        """
        self.atual = self.r.randint(1, self.lados)
        return self.atual

    def get_lado(self):
        """
        Retorna o último número sorteado no dado.
        
        Returns:
            int: o último lado selecionado
        """
        return self.atual

    def __str__(self):
        """
        Retorna uma representação gráfica do dado (apenas para 6 lados).
        
        Returns:
            str: representação ASCII do dado
        """
        if self.lados != 6:
            return "Não há representação para esse dado"
        
        s = "+-----+\n"
        lado = self.get_lado()
        
        if lado == 1:
            s += (self.s000 + self.s010 + self.s000)
        elif lado == 2:
            s += (self.s100 + self.s000 + self.s001)
        elif lado == 3:
            s += (self.s100 + self.s010 + self.s001)
        elif lado == 4:
            s += (self.s101 + self.s000 + self.s101)
        elif lado == 5:
            s += (self.s101 + self.s010 + self.s101)
        elif lado == 6:
            s += (self.s111 + self.s000 + self.s111)
        
        s += "+-----+\n"
        return s

    @staticmethod
    def main():
        """Método de teste para a classe Dado"""
        d = Dado(6, 1)
        for i in range(100):
            d.rolar()
            print(d.get_lado())
            print(f"{i}) ")
            print(d)
