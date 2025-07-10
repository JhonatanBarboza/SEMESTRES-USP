import rolaDados
import placar
import dado

# Número de rodadas do jogo
NRODADAS = 10

class Bozo:
    @staticmethod
    def main():
        print("Digite a semente (zero para aleatório): ", end='')
        seed = int(input())
        pl = placar.Placar()
        print(pl)
        
        # Supondo que as classes RolaDados e Placar existam
        rd = rolaDados.RolaDados(5, seed)
        
        for rodada in range(NRODADAS):
            print(f"****** Rodada {rodada+1}")
            print("Pressione ENTER para lançar os dados")
            input()
            
            # Primeira tentativa
            rd.rolar()
            print("1          2          3          4          5")
            print(rd)
            
            # Segunda tentativa
            print("\nDigite os números dos dados que quiser TROCAR. Separados por espaços.")
            muda = input()
            rd.rolar(muda)
            print("1          2          3          4          5")
            print(rd)
            
            # Terceira tentativa
            print("\nDigite os números dos dados que quiser TROCAR. Separados por espaços.")
            muda = input()
            values = rd.rolar(muda)
            print("1          2          3          4          5")
            print(rd)
            
            print("\n\n\n")
            print(pl)
            
            pos = 0
            while pos <= 0:
                try:
                    print("Escolha a posição que quer ocupar com essa jogada ===> ", end='')
                    pos = int(input())
                    if pos > NRODADAS or pos <= 0:
                        pos = 0
                    pl.add(pos, values)
                except Exception:
                    pos = 0
                
                if pos == 0:
                    print("Valor inválido. Posição ocupada ou inexistente.")
            
            print("\n\n")
            print(pl)
            
        print("***********************************")
        print("***")
        print(f"*** Seu escore final foi: {pl.getScore()}")
        print("***")
        print("***********************************")
