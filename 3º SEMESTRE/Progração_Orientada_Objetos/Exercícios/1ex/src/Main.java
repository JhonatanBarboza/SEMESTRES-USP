import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        try {
            
            // Lê a linha de tamanho do tabuleiro
            String linhaTabuleiro = scanner.nextLine().trim(); 
            String[] partes = linhaTabuleiro.split("\\s+"); // Divide a linha em partes (ex: "1", "2", "3")
            int tamtab = partes.length;
            int[] estadoInicial = new int[tamtab];

            // Lê o tamanho do tabuleiro
            for (int i = 0; i < tamtab; i++) {
                estadoInicial[i] = Integer.parseInt(partes[i]); // Converte cada parte para inteiro
            }

            // Lê a linha de movimentos
            String movimentos = scanner.nextLine().trim();

            // Cria e processa o tabuleiro
            Tabuleiro tabuleiro = new Tabuleiro(estadoInicial, tamtab); // Cria o tabuleiro com o estado inicial
            System.out.println(tabuleiro);

            // Processa cada movimento
            for (int i = 0; i < movimentos.length(); i++) {
                char movimento = movimentos.charAt(i);
                
                tabuleiro.mover(movimento); // Move o tabuleiro
                System.out.println(tabuleiro); // Imprime o tabuleiro após o movimento
            }

            // Verifica se é a solução
            if (tabuleiro.ehSolucao(tamtab)) {
                System.out.println("Posicao final: true");
            } else {
                System.out.println("Posicao final: false");
            }

        } catch (Exception e) {}
        finally { scanner.close();}
    }
}