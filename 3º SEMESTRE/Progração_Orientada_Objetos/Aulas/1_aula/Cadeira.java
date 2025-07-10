public class Cadeira {
    String posicao; // Posição da cadeira (ex: "frente", "trás", "esquerda", "direita")
    boolean ocupado; // Indica se a cadeira está ocupada ou não

    // Construtor para inicializar a cadeira
    public Cadeira(String posicaoInicial) {
        this.posicao = posicaoInicial; // Define a posição inicial da cadeira
        this.ocupado = false; // Inicialmente, a cadeira não está ocupada
    }

    // Método para sentar na cadeira
    void sentar() {
        if (!ocupado) { // Verifica se a cadeira não está ocupada
            ocupado = true; // Marca a cadeira como ocupada
            System.out.println("Você sentou na cadeira.");
        } else {
            System.out.println("A cadeira já está ocupada.");
        }
    }

    // Método para levantar da cadeira
    void levantar() {
        if (ocupado) { // Verifica se a cadeira está ocupada
            ocupado = false; // Marca a cadeira como desocupada
            System.out.println("Você se levantou da cadeira.");
        } else {
            System.out.println("A cadeira já está vazia.");
        }
    }

    // Método para virar a cadeira para uma nova posição
    void virar(String novaPosicao) {
        if (!ocupado) { // Só vira a cadeira se não estiver ocupada
            this.posicao = novaPosicao; // Atualiza a posição da cadeira
            System.out.println("A cadeira foi virada para: " + novaPosicao);
        } else {
            System.out.println("Não é possível virar a cadeira enquanto está ocupada.");
        }
    }

    // Método para obter a posição atual da cadeira
    String getPosicao() {
        return this.posicao; // Retorna a posição atual da cadeira
    }

    // Método principal para testar a classe
    public static void main(String[] args) {
        Cadeira cadeira = new Cadeira("frente"); // Cria uma cadeira na posição "frente"

        cadeira.sentar(); // Sentar na cadeira
        cadeira.virar("trás"); // Tentar virar a cadeira (não deve funcionar, pois está ocupada)
        cadeira.levantar(); // Levantar da cadeira
        cadeira.virar("trás"); // Virar a cadeira para "trás"
        System.out.println("Posição atual da cadeira: " + cadeira.getPosicao()); // Verificar a posição
    }
}