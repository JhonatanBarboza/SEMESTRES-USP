import java.util.LinkedList;
import java.util.Queue;

public class ArvBin {
    protected String[] heap;
    protected int tamanho;
    protected int capacidade;

    // Construtor que cria uma árvore vazia
    public ArvBin(int capacidade) {
        this.capacidade = capacidade;
        this.heap = new String[capacidade];
        this.tamanho = 0;
    }

    // Método para inserir um valor na árvore
    public void inserir(String valor) {
        if (valor == null || valor.trim().isEmpty()) {
            return;
        }
        
        if (tamanho == 0) {
            heap[0] = valor;
            tamanho++;
            return;
        }
        
        // Verifica se precisa redimensionar o array
        if (tamanho >= capacidade) {
            redimensionar();
        }
        
        // Insere na posição correta seguindo as propriedades da BST
        inserirRecursivo(0, valor);
    }

    // Método auxiliar recursivo para inserção
    private void inserirRecursivo(int indice, String valor) {
        if (indice >= capacidade) {
            redimensionar();
        }
        
        if (heap[indice] == null) {
            heap[indice] = valor;
            tamanho++;
            return;
        }
        
        int comparacao = valor.compareTo(heap[indice]);
        
        if (comparacao < 0) {
            // Vai para a esquerda
            inserirRecursivo(nodeLeft(indice), valor);
        } else if (comparacao > 0) {
            // Vai para a direita
            inserirRecursivo(nodeRight(indice), valor);
        }
        // Se for igual, não faz nada (não permite duplicados)
    }

    // Método para remover um valor da árvore
    public boolean remover(String valor) {
        if (valor == null || tamanho == 0) {
            return false;
        }
        
        return removerRecursivo(0, valor);
    }

    // Método auxiliar recursivo para remoção
    private boolean removerRecursivo(int indice, String valor) {
        if (indice >= capacidade || heap[indice] == null) {
            return false;
        }
        
        int comparacao = valor.compareTo(heap[indice]);
        
        if (comparacao < 0) {
            return removerRecursivo(nodeLeft(indice), valor);
        } else if (comparacao > 0) {
            return removerRecursivo(nodeRight(indice), valor);
        } else {
            // Encontrou o nó a ser removido
            return removerNo(indice);
        }
    }

    // Método auxiliar para remover um nó específico
    private boolean removerNo(int indice) {
        if (heap[indice] == null) {
            return false;
        }
        
        // Caso 1: Nó sem filhos
        if (nodeLeft(indice) >= capacidade || (heap[nodeLeft(indice)] == null && 
            (nodeRight(indice) >= capacidade || heap[nodeRight(indice)] == null))) {
            heap[indice] = null;
            tamanho--;
            return true;
        }
        
        // Caso 2: Nó com apenas um filho
        if (nodeLeft(indice) < capacidade && heap[nodeLeft(indice)] != null && 
            (nodeRight(indice) >= capacidade || heap[nodeRight(indice)] == null)) {
            // Apenas filho esquerdo existe
            String[] subarvore = extrairSubarvore(nodeLeft(indice));
            heap[indice] = null;
            tamanho--;
            reinserirSubarvore(indice, subarvore);
            return true;
        } else if ((nodeLeft(indice) >= capacidade || heap[nodeLeft(indice)] == null) && 
                   nodeRight(indice) < capacidade && heap[nodeRight(indice)] != null) {
            // Apenas filho direito existe
            String[] subarvore = extrairSubarvore(nodeRight(indice));
            heap[indice] = null;
            tamanho--;
            reinserirSubarvore(indice, subarvore);
            return true;
        }
        
        // Caso 3: Nó com dois filhos
        int sucessorIndice = encontrarSucessor(nodeRight(indice));
        heap[indice] = heap[sucessorIndice];
        return removerNo(sucessorIndice);
    }

    // Método para encontrar um valor na árvore
    public boolean find(String valor) {
        return findRecursivo(0, valor);
    }

    // Método auxiliar recursivo para busca
    private boolean findRecursivo(int indice, String valor) {
        if (indice >= capacidade || heap[indice] == null) {
            return false;
        }
        
        int comparacao = valor.compareTo(heap[indice]);
        
        if (comparacao < 0) {
            return findRecursivo(nodeLeft(indice), valor);
        } else if (comparacao > 0) {
            return findRecursivo(nodeRight(indice), valor);
        } else {
            return true;
        }
    }

    // Método para obter o número de elementos na árvore
    public int len() {
        return tamanho;
    }

    // Método para gerar a representação Graphviz da árvore com percurso em largura
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("digraph {\n");
        
        if (tamanho == 0) {
            sb.append("}\n");
            return sb.toString();
        }
        else if (tamanho == 1) {
            // Sempre imprime o nó raiz primeiro
            sb.append(String.format("\"%d %s\" }\n", 0, heap[0]));
            return sb.toString();
        }
        
        // Usa uma fila para o percurso em largura
        Queue<Integer> fila = new LinkedList<>();
        fila.add(0); // Começa pela raiz
        
        while (!fila.isEmpty()) {
            int atual = fila.poll();
            
            if (heap[atual] == null) {
                continue;
            }
            
            int esquerda = nodeLeft(atual);
            int direita = nodeRight(atual);
            
            // Processa filho esquerdo
            if (esquerda < capacidade && heap[esquerda] != null) {
                sb.append(String.format("\"%d %s\" ->\"%d %s\"\n", 
                                    atual, heap[atual], esquerda, heap[esquerda]));
                fila.add(esquerda);
            }
            
            // Processa filho direito
            if (direita < capacidade && heap[direita] != null) {
                sb.append(String.format("\"%d %s\" ->\"%d %s\"\n", 
                                    atual, heap[atual], direita, heap[direita]));
                fila.add(direita);
            }

        }
        
        sb.append("}\n");
        return sb.toString();
    }

    // Métodos protegidos para uso pelas subclasses
    protected int countNodes(int i) {
        if (i >= capacidade || heap[i] == null) {
            return 0;
        }
        return 1 + countNodes(nodeLeft(i)) + countNodes(nodeRight(i));
    }

    protected String getNode(int i) {
        if (i < 0 || i >= capacidade) {
            return null;
        }
        return heap[i];
    }

    protected void setNode(int i, String v) {
        if (i >= 0 && i < capacidade) {
            if (heap[i] == null && v != null) {
                tamanho++;
            } else if (heap[i] != null && v == null) {
                tamanho--;
            }
            heap[i] = v;
        }
    }

    protected int nodeLeft(int i) {
        return 2 * i + 1;
    }

    protected int nodeRight(int i) {
        return 2 * i + 2;
    }

    protected boolean isBalance() {
        // Na classe base, não há balanceamento a ser verificado
        return true;
    }

    // Métodos auxiliares privados
    private void redimensionar() {
        int novaCapacidade = capacidade * 2;
        String[] novoHeap = new String[novaCapacidade];
        System.arraycopy(heap, 0, novoHeap, 0, capacidade);
        heap = novoHeap;
        capacidade = novaCapacidade;
    }

    private String[] extrairSubarvore(int indiceRaiz) {
        if (indiceRaiz >= capacidade || heap[indiceRaiz] == null) {
            return new String[0];
        }
        
        int numNodes = countNodes(indiceRaiz);
        String[] subarvore = new String[numNodes];
        extrairRecursivo(indiceRaiz, subarvore, 0);
        return subarvore;
    }

    private int extrairRecursivo(int indice, String[] array, int pos) {
        if (indice >= capacidade || heap[indice] == null) {
            return pos;
        }
        
        array[pos++] = heap[indice];
        pos = extrairRecursivo(nodeLeft(indice), array, pos);
        pos = extrairRecursivo(nodeRight(indice), array, pos);
        return pos;
    }

    private void reinserirSubarvore(int indiceRaiz, String[] subarvore) {
        for (String valor : subarvore) {
            inserirRecursivo(indiceRaiz, valor);
        }
    }

    private int encontrarSucessor(int indice) {
        while (nodeLeft(indice) < capacidade && heap[nodeLeft(indice)] != null) {
            indice = nodeLeft(indice);
        }
        return indice;
    }
}