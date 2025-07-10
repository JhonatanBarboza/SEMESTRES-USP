public class ArvBal extends ArvBin {

    public ArvBal(int capacidade) {
        super(capacidade);
    }

    @Override
    public void inserir(String valor) {
        super.inserir(valor);
        balancearArvore();
    }

    @Override
    public boolean remover(String valor) {
        boolean removido = super.remover(valor);
        if (removido) {
            balancearArvore();
        }
        return removido;
    }

    @Override
    protected boolean isBalance() {
        return verificarBalanceamento(0);
    }

    private boolean verificarBalanceamento(int indice) {
        if (indice >= capacidade || heap[indice] == null) {
            return true;
        }

        int nosEsquerda = countNodes(nodeLeft(indice));
        int nosDireita = countNodes(nodeRight(indice));
        int diferenca = Math.abs(nosEsquerda - nosDireita);

        return diferenca <= 1 
               && verificarBalanceamento(nodeLeft(indice)) 
               && verificarBalanceamento(nodeRight(indice));
    }

    private void balancearArvore() {
        if (!isBalance()) {
            // Extrai todos os elementos da árvore
            String[] elementos = new String[tamanho];
            int pos = 0;
            pos = coletarElementos(0, elementos, pos);

            // Limpa a árvore atual
            limparArvore();

            // Reinsere os elementos de forma balanceada
            reinserirBalanceado(elementos, 0, elementos.length - 1, 0);
        }
    }

    private int coletarElementos(int indice, String[] elementos, int pos) {
        if (indice >= capacidade || heap[indice] == null) {
            return pos;
        }

        // Percorre em ordem (esquerda, raiz, direita)
        pos = coletarElementos(nodeLeft(indice), elementos, pos);
        elementos[pos++] = heap[indice];
        pos = coletarElementos(nodeRight(indice), elementos, pos);
        return pos;
    }

    private void limparArvore() {
        for (int i = 0; i < capacidade; i++) {
            heap[i] = null;
        }
        tamanho = 0;
    }

    private void reinserirBalanceado(String[] elementos, int inicio, int fim, int indiceAtual) {
        if (inicio > fim) {
            return;
        }

        // Encontra o elemento do meio
        int meio = (inicio + fim) / 2;
        String valorMeio = elementos[meio];

        // Insere o elemento do meio na posição atual
        setNode(indiceAtual, valorMeio);

        // Balanceia recursivamente as subárvores esquerda e direita
        reinserirBalanceado(elementos, inicio, meio - 1, nodeLeft(indiceAtual));
        reinserirBalanceado(elementos, meio + 1, fim, nodeRight(indiceAtual));
    }
}