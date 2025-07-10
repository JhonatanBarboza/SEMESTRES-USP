public class Tabuleiro {
    private int[][] tabuleiro;
    private int linhaVazia, colunaVazia;

    // Construtor que inicializa o tabuleiro com o estado inicial e o tamanho
    public Tabuleiro(int[] estadoInicial, int tamanho) {
        int dimensao = (int) Math.sqrt(tamanho); // Calcula a dimensão do tabuleiro (ex.: 3 para 9 elementos)
        tabuleiro = new int[dimensao][dimensao]; // Cria um tabuleiro quadrado (ex.: 3x3)
    
        for (int i = 0; i < tamanho; i++) { // Preenche o tabuleiro com os valores do estado inicial
            int linha = i / dimensao;  
            int coluna = i % dimensao;   
            tabuleiro[linha][coluna] = estadoInicial[i];
    
            if (estadoInicial[i] == 0) { // Encontra a posição do espaço vazio (0)
                linhaVazia = linha;
                colunaVazia = coluna;
            }
        }
    }

    // Método para mover o espaço vazio (0) na direção especificada
    // 'u' para cima, 'd' para baixo, 'l' para esquerda, 'r' para direita
    public boolean mover(char direcao) {
        switch (direcao) {
            case 'u': return moverParaCima();
            case 'd': return moverParaBaixo();
            case 'l': return moverParaEsquerda();
            case 'r': return moverParaDireita();
            default: return false;
        }
    }

    // Métodos privados para mover o espaço vazio nas direções especificadas

    // Método para mover a peça para cima
    private boolean moverParaCima() {   
        if (linhaVazia >= tabuleiro.length-1) return false;
        tabuleiro[linhaVazia][colunaVazia] = tabuleiro[linhaVazia + 1][colunaVazia];
        tabuleiro[linhaVazia + 1][colunaVazia] = 0;
        linhaVazia++;
        return true;
    }

    // Método para mover a peça para baixo
    private boolean moverParaBaixo() {
        if (linhaVazia <= 0) return false;
        tabuleiro[linhaVazia][colunaVazia] = tabuleiro[linhaVazia - 1][colunaVazia];
        tabuleiro[linhaVazia - 1][colunaVazia] = 0;
        linhaVazia--;
        return true;
    }

    // Método para mover a peça para a esquerda
    private boolean moverParaEsquerda() {
        if (colunaVazia >= tabuleiro.length-1) return false;
        tabuleiro[linhaVazia][colunaVazia] = tabuleiro[linhaVazia][colunaVazia + 1];
        tabuleiro[linhaVazia][colunaVazia + 1] = 0;
        colunaVazia++;
        return true;
    }

    // Método para mover a peça para a direita  
    private boolean moverParaDireita() {
        if (colunaVazia <= 0) return false;
        tabuleiro[linhaVazia][colunaVazia] = tabuleiro[linhaVazia][colunaVazia - 1];
        tabuleiro[linhaVazia][colunaVazia - 1] = 0;
        colunaVazia--;
        return true;
    }

    // Método para verificar se o tabuleiro está na solução
    // O tabuleiro é considerado na solução se os números estão em ordem crescente
    public boolean ehSolucao(int tamanho) {
        int dimensao = (int) Math.sqrt(tamanho); 
        int contador = 1;
        for (int i = 0; i < dimensao; i++) {
            for (int j = 0; j < dimensao; j++) {
                if (i == 0 && j == 0) {
                    if (tabuleiro[i][j] != 0) return false;
                } else {
                    if (tabuleiro[i][j] != contador++) return false;
                }
            }
        }
        return true;
    }

    // Método para imprimir o tabuleiro
    @Override
    public String toString() {

        // Cria um StringBuilder para construir a representação do tabuleiro
        StringBuilder sb = new StringBuilder();
        int dimensao = tabuleiro.length;
        String linhaSeparadora = "+------".repeat(dimensao) + "+\n";
        sb.append(linhaSeparadora);

        // Adiciona os números do tabuleiro
        for (int i = 0; i < dimensao; i++) {
            sb.append("|");
            for (int j = 0; j < dimensao; j++) { // Preenche o tabuleiro com os valores
                if (tabuleiro[i][j] == 0) {
                    sb.append("   0  |");
                } else {
                    sb.append(String.format("  %2d  |", tabuleiro[i][j]));
                }
            }
            sb.append("\n").append(linhaSeparadora);
        }
        return sb.toString();
    }
}