import dado
from random import Random

class RolaDados:
    def __init__(self, quant = 5, seed = 0):
        """
        Construtor que cria e armazena vários objetos do tipo Dado.
        
        Args:
            n: Número de dados a serem criados
            seed: Semente para aleatoriedade (None para aleatório)
        """
        
        self.quant = quant # Quantidade de dados no conjunto
        
        if (seed != 0):
            rd = Random()
            rd.seed(seed)
            self.dados = [dado.Dado(6, rd.randint(1, 10000)) for _ in range(quant)] # Inicializando dados com a seed
        else:
            self.dados = [dado.Dado(6) for _ in range(quant)] 
    
    def rolar(self, quais=None):
        """
        Rola os dados conforme especificado.
        
        Args:
            quais: Pode ser:
                  - None: rola todos os dados
                  - String: contém números dos dados a rolar (ex: "1 3 5")
                  - Lista de booleanos: indica quais dados rolar
                  
        Returns:
            Lista com os valores atuais de todos os dados
        """
        if quais is None:
            return [d.rolar() for d in self.dados]
        elif isinstance(quais, str):
            return self._rolar_string(quais)
        elif isinstance(quais, (list, tuple)) and all(isinstance(x, bool) for x in quais):
            return self._rolar_booleanos(quais)
        else:
            raise ValueError("Argumento inválido para rolar()")

    def _rolar_booleanos(self, quais):
        """
        Rola os dados especificados por um array de booleanos.
        
        Args:
            quais: Lista de booleanos indicando quais dados rolar
            
        Returns:
            Lista com os valores atuais de todos os dados
        """
        resultados = []
        for i, deve_rolar in enumerate(quais):
            if i >= len(self.dados):
                break
            if deve_rolar:
                resultados.append(self.dados[i].rolar())
            else:
                resultados.append(self.dados[i].get_lado())
        return resultados

    def _rolar_string(self, s):
        """
        Rola os dados especificados por uma string de números.
        
        Args:
            s: String com números dos dados a rolar (ex: "1 3 5")
            
        Returns:
            Lista com os valores atuais de todos os dados
        """
        import re
        b = [False] * len(self.dados)
        numeros = re.findall(r'\d+', s)
        
        for num in numeros:
            i = int(num) - 1
            if 0 <= i < len(self.dados):
                b[i] = True
                
        return self._rolar_booleanos(b)

    def __str__(self):
        """
        Retorna representação visual dos dados, mostrados horizontalmente.
        
        Returns:
            String formatada com a representação dos dados
        """
        if not self.dados:
            return ""
            
        # Divide a representação de cada dado em linhas
        linhas_dados = [str(d).split('\n') for d in self.dados]
        
        # Constroi cada linha da saída combinando as linhas correspondentes de cada dado
        resultado = []
        for i in range(5):  # Cada dado tem 5 linhas na representação
            linha = ""
            for dado_linhas in linhas_dados:
                if i < len(dado_linhas):
                    linha += dado_linhas[i] + "    "
                else:
                    linha += "       "  # Espaço para dados com representação incompleta
            resultado.append(linha.rstrip())
        
        return '\n'.join(resultado)

    @staticmethod
    def main():
        """Método de teste para a classe RolaDados"""
        rd = RolaDados(5, None)
        rd.rolar()
        print("1          2          3          4          5")
        print(rd)
        
        while True:
            try:
                s = input("Digite os dados a rolar (ex: 1 3 5) ou ENTER para sair: ")
                if not s:
                    break
                rd.rolar(s)
                print(rd)
            except Exception as e:
                print(f"Erro: {e}")
