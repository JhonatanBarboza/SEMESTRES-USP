public class ArvAVL extends ArvBin {
    private class NodeInfo {
        int altura;
        NodeInfo() {
            this.altura = 1;
        }
    }
    
    private NodeInfo[] infoNos;

    public ArvAVL(int capacidade) {
        super(capacidade);
        this.infoNos = new NodeInfo[capacidade];
        for (int i = 0; i < capacidade; i++) {
            infoNos[i] = new NodeInfo();
        }
    }

    @Override
    public void inserir(String valor) {
        super.inserir(valor);
        // Balanceia a partir da raiz após inserção
        balancearNo(0);
    }

    @Override
    public boolean remover(String valor) {
        boolean removido = super.remover(valor);
        if (removido) {
            // Balanceia a partir da raiz após remoção
            balancearNo(0);
        }
        return removido;
    }

    private void balancearNo(int indice) {
        if (indice < 0 || indice >= capacidade || heap[indice] == null) {
            return;
        }
        
        atualizarAltura(indice);
        int fator = getFatorBalanceamento(indice);
        
        // Caso Esquerda-Esquerda
        if (fator > 1 && getFatorBalanceamento(nodeLeft(indice)) >= 0) {
            rotacaoDireita(indice);
        }
        // Caso Esquerda-Direita (LR)
        else if (fator > 1 && getFatorBalanceamento(nodeLeft(indice)) < 0) {
            // Rotação à esquerda no filho
            int filhoEsq = nodeLeft(indice);
            rotacaoEsquerda(filhoEsq);
            
            // Rotação à direita no nó original
            rotacaoDireita(indice);
        }
        // Caso Direita-Direita
        else if (fator < -1 && getFatorBalanceamento(nodeRight(indice)) <= 0) {
            rotacaoEsquerda(indice);
        }
        // Caso Direita-Esquerda (RL)
        else if (fator < -1 && getFatorBalanceamento(nodeRight(indice)) > 0) {
            // Rotação à direita no filho
            int filhoDir = nodeRight(indice);
            rotacaoDireita(filhoDir);
            
            // Rotação à esquerda no nó original
            rotacaoEsquerda(indice);
        }
        
        // Balanceia recursivamente para cima
        if (indice != 0) {
            balancearNo(getPai(indice));
        }
    }

    private void rotacaoDireita(int y) {

        int x = nodeLeft(y);
        if (x >= capacidade || heap[x] == null) return;
        
        // 1. Salvamento das informações originais
        String tempValor = heap[y];
        NodeInfo tempInfo = infoNos[y];
        
        // 2. Movimentação do nó x para a posição y
        heap[y] = heap[x];
        infoNos[y] = infoNos[x];
        
        // 3. Tratamento da subárvore direita de x (T2)
        int T2 = nodeRight(x);
        if (T2 < capacidade && heap[T2] != null) {
            setNode(nodeLeft(y), heap[T2]);
            infoNos[nodeLeft(y)] = infoNos[T2];
            setNode(T2, null);
            infoNos[T2] = new NodeInfo();
        } else {
            setNode(nodeLeft(y), null);
        }
        
        // 4. Movimentação do nó original y para a direita de x
        setNode(x, tempValor);
        infoNos[x] = tempInfo;
        
        // 5. Atualização de alturas
        atualizarAltura(y);
        atualizarAltura(x);
    }

    private void rotacaoEsquerda(int x) {
        
        int y = nodeRight(x);
        if (y >= capacidade || heap[y] == null) return;
        
        // 1. Salvamento das informações originais
        String tempValor = heap[x];
        NodeInfo tempInfo = infoNos[x];
        
        // 2. Movimentação do nó y para a posição x
        heap[x] = heap[y];
        infoNos[x] = infoNos[y];
        
        // 3. Tratamento da subárvore esquerda de y (T2)
        int T2 = nodeLeft(y);
        if (T2 < capacidade && heap[T2] != null) {
            setNode(nodeRight(x), heap[T2]);
            infoNos[nodeRight(x)] = infoNos[T2];
            setNode(T2, null);
            infoNos[T2] = new NodeInfo();
        } else {
            setNode(nodeRight(x), null);
        }
        
        // 4. Movimentação do nó original x para a esquerda de y
        setNode(y, tempValor);
        infoNos[y] = tempInfo;
        
        // 5. Atualização de alturas
        atualizarAltura(x);
        atualizarAltura(y);
    }

    // Restante dos métodos permanecem iguais...
    private int getFatorBalanceamento(int indice) {
        if (indice >= capacidade || heap[indice] == null) return 0;
        return getAltura(nodeLeft(indice)) - getAltura(nodeRight(indice));
    }

    private int getAltura(int indice) {
        if (indice >= capacidade || heap[indice] == null) return 0;
        return infoNos[indice].altura;
    }

    private void atualizarAltura(int indice) {
        if (indice >= capacidade || heap[indice] == null) return;
        infoNos[indice].altura = 1 + Math.max(getAltura(nodeLeft(indice)), 
                                            getAltura(nodeRight(indice)));
    }

    private int getPai(int indice) {
        if (indice <= 0) return -1;
        return (indice - 1) / 2;
    }

    @Override
    protected void setNode(int i, String v) {
        super.setNode(i, v);
        if (i >= 0 && i < capacidade) {
            if (v != null && heap[i] != null) {
                infoNos[i] = new NodeInfo();
            } else {
                infoNos[i] = new NodeInfo();
                infoNos[i].altura = 0;
            }
        }
    }
}