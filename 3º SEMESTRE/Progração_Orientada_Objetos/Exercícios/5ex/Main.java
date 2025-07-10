import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.LinkedHashMap; 

abstract class Produto {
    protected String codigo;
    protected String nome;
    protected int quantidade;
    
    public Produto(String codigo, String nome) {
        this.codigo = codigo;
        this.nome = nome;
    }
    
    public void adicionarEstoque(int quantidade) {
        if (quantidade > 0) {
            this.quantidade += quantidade;
        }
    }
    
    public boolean vender(int quantidade) {
        if (quantidade > 0 && this.quantidade >= quantidade) {
            this.quantidade -= quantidade;
            return true;
        }
        return false;
    }
    
    public String getCodigo() {
        return codigo;
    }
    
    public String getNome() {
        return nome;
    }
    
    public int getQuantidade() {
        return quantidade;
    }
    
    @Override
    public abstract String toString();
}

// Classe livro herda de produto
class Livro extends Produto {
    private String autor;
    private String editora;
    private int ano;
    private String edicao;
    private int paginas;
    private String idioma;
    
    public Livro(String codigo, String nome, String autor, String editora, int ano, 
                 String edicao, int paginas, String idioma) {
        super(codigo, nome);
        this.autor = autor;
        this.editora = editora;
        this.ano = ano;
        this.edicao = edicao;
        this.paginas = paginas;
        this.idioma = idioma;
    }
    
    @Override
    public String toString() {
        return String.format(
            "Livro\n" +
            "Código: %s\n" +
            "Título: %s\n" +
            "Autor: %s\n" +
            "Editora: %s\n" +
            "Edição: %s\n" +
            "Ano: %d\n" +
            "Páginas: %d\n" +
            "Idioma: %s\n",
            codigo, nome, autor, editora, edicao, ano, paginas, idioma
        );
    }
}

class CD extends Produto {
    private String artista;
    private int trilhas;
    private String gravadora;
    private int ano;
    
    public CD(String codigo, String titulo, String artista, int trilhas, String gravadora, int ano) {
        super(codigo, titulo);
        this.artista = artista;
        this.trilhas = trilhas;
        this.gravadora = gravadora;
        this.ano = ano;
    }
    
    @Override
    public String toString() {
        return String.format(
            "CD\n" +
            "Código: %s\n" +
            "Título: %s\n" +
            "Banda: %s\n" +
            "Gravadora: %s\n" +
            "Ano: %d\n" +
            "trilhas: %d\n",
            codigo, nome, artista, gravadora, ano, trilhas);
    }
}

class DVD extends Produto {
    private String diretor;
    private String idioma;
    private String genero;
    private int ano;
    private String nacionalidade;
    
    public DVD(String codigo, String nome, String diretor, String idioma, 
               String genero, int ano, String nacionalidade) {
        super(codigo, nome);
        this.diretor = diretor;
        this.idioma = idioma;
        this.genero = genero;
        this.ano = ano;
        this.nacionalidade = nacionalidade;
    }
    
    @Override
    public String toString() {
        return String.format(
            "DVD\n" +
            "Código: %s\n" +
            "Título: %s\n" +
            "Diretor: %s\n" +
            "Gênero: %s\n" +
            "Ano: %d\n" +
            "Nacionalidade: %s\n" +
            "Idioma: %s\n",
            
            codigo, nome, diretor, genero, ano, nacionalidade, idioma);
    }
}

class Loja {
    private Map<String, Produto> produtos;

    public Loja() {
        produtos = new LinkedHashMap<>(); 
    }

    public boolean inserirProduto(String tipo, String[] dados) {
    
        Produto produto = null;
        String codigo = dados[0];
  
        if (produtos.containsKey(codigo)) {
            System.out.printf("***Erro: Código já cadastrado: %s\n", codigo);
            return false; // Produto já existe
        }
        
        switch (tipo) {
            case "Livro":
                produto = new Livro(codigo, dados[1], dados[2], dados[3], 
                                    Integer.parseInt(dados[4]), dados[5], 
                                    Integer.parseInt(dados[6]), dados[7]);
                break;
            case "CD":
                produto = new CD(codigo, dados[1], dados[2], 
                                Integer.parseInt(dados[3]), dados[4], 
                                Integer.parseInt(dados[5]));  
                break;
            case "DVD":
                produto = new DVD(codigo, dados[1], dados[2], dados[3], 
                                dados[4], Integer.parseInt(dados[5]), dados[6]);   
                break;
            default:
                System.out.println("Falha ao inserir produto\n");
                return false;
        }
        
        produtos.put(codigo, produto);
        return true;
    
    }

    public Collection<Produto> getTodosProdutos() {
        return produtos.values();
    }

    public List<Produto> getTodosLivros() {
        List<Produto> livros = new ArrayList<>();
        for (Produto produto : produtos.values()) {
            if (produto instanceof Livro) {
                livros.add(produto);
            }
        }
        return livros;
    }

    public List<Produto> getTodosCDs() {
        List<Produto> cds = new ArrayList<>();
        for (Produto produto : produtos.values()) {
            if (produto instanceof CD) {
                cds.add(produto);
            }
        }
        return cds;
    }

    public List<Produto> getTodosDVDs() {
        List<Produto> dvds = new ArrayList<>();
        for (Produto produto : produtos.values()) {
            if (produto instanceof DVD) {
                dvds.add(produto);
            }
        }
        return dvds;
    }
    
    public boolean adicionarEstoque(String codigo, int quantidade) {
        Produto produto = produtos.get(codigo);
        if (produto != null) {
            produto.adicionarEstoque(quantidade);
            return true;
        }
        return false;
    }
    
    public boolean venderProduto(String codigo, int quantidade) {
        Produto produto = produtos.get(codigo);
        if (produto != null) {
            return produto.vender(quantidade);
        }
        return false;
    }
    
    public Produto buscarPorCodigo(String codigo) {
        return produtos.get(codigo);
    }
    
    public List<Produto> buscarPorNome(String nome) {
        List<Produto> encontrados = new ArrayList<>();
        for (Produto produto : produtos.values()) {
            if (produto.getNome().toLowerCase().contains(nome.toLowerCase())) {
                encontrados.add(produto);
            }
        }
        return encontrados;
    }
}


public class Main {
    public static void main(String[] args) {
        Loja loja = new Loja();
        Scanner scanner = new Scanner(System.in);  
        System.out.printf("\n"); 
        
        while (scanner.hasNextLine()) {
            String linha = scanner.nextLine().trim();
            if (linha.isEmpty()) {
                continue;
            }
            
            String[] partes = linha.split(",");
            String comando = partes[0];
            
            try {
                switch (comando) {
                    case "I":
                        String tipo = partes[1];

                        String[] dados = new String[partes.length - 2];
                        System.arraycopy(partes, 2, dados, 0, partes.length - 2);

                        // Remove zeros à esquerda do código
                        dados[0] = dados[0].replaceFirst("^0+(?!$)", "");

                        if (tipo.equals("Livro")) {
                            System.out.printf("Operação inserir livro: %s\n", dados[0]);
                        } else {
                            System.out.printf("Operação inserir %s: %s\n", tipo, dados[0]);
                        }

                        if (loja.inserirProduto(tipo, dados)) {
                            System.out.println("Operação realizada com sucesso\n");
                        }
                        break;
                        
                    case "A":
                        String codigoA = partes[1].replaceFirst("^0+(?!$)", "");

                        System.out.printf("Operação de compra: %s\n", codigoA);

                        int quantidadeA = Integer.parseInt(partes[2]);
                        if (loja.adicionarEstoque(codigoA, quantidadeA)) {
                            System.out.printf("Operação realizada com sucesso: %s\n\n", codigoA);
                        } else {
                            System.out.printf("***Erro: Código inexistente: %s\n\n", codigoA);
                        }
                        break;
                        
                    case "V":
                        String codigoV = partes[1].replaceFirst("^0+(?!$)", "");

                        System.out.printf("Operação de venda: %s\n", codigoV);
                        int quantidadeV = Integer.parseInt(partes[2]);
                        Produto prod = loja.buscarPorCodigo(codigoV);

                        if (prod == null) {
                            System.out.printf("***Erro: Código inexistente: %s\n\n", codigoV);
                            break;
                        }

                        if (loja.venderProduto(codigoV, quantidadeV)) {
                            System.out.printf("Operação realizada com sucesso: %s\n\n", codigoV);
                        } else {
                            System.out.printf("***Erro: Estoque insuficiente: %s Quantidade: %d\n\n", codigoV, quantidadeV);
                        }
                        break;
                        
                    case "P":
                        String parametro = partes[1];
                        System.out.printf("Procurando: %s\n", parametro);
                        parametro = partes[1].replaceFirst("^0+(?!$)", "");
                        if (parametro.matches("\\d+")) { // Busca por código
                            Produto produto = loja.buscarPorCodigo(parametro);
                            if (produto != null) {
                                System.out.printf("Encontrado:\n");
                                System.out.println(produto);
                            } else {
                                System.out.printf("Produto não encontrado: %s\n\n", parametro);
                            }
                        } else { // Busca por nome
                            List<Produto> produtos = loja.buscarPorNome(parametro);
                            if (!produtos.isEmpty()) {
                                for (Produto p : produtos) {
                                    System.out.printf("Encontrado:\n");
                                    System.out.println(p);
                                    break;
                                }
                            } else {
                                System.out.printf("Produto não encontrado: %s\n\n", parametro);
                            }
                        }
                        break;
                        
                        case "S":
                        System.out.println("Operação de sumarização: ");
                        
                        // Contadores para cada tipo de produto
                        int totalLivros = 0;
                        int totalCDs = 0;
                        int totalDVDs = 0;
                        int qtdLivros = 0;
                        int qtdCDs = 0;
                        int qtdDVDs = 0;
                        
                        // Primeiro imprimir todos os produtos e contar quantidades
                        for (Produto produto : loja.getTodosProdutos()) {

                            if (produto instanceof Livro) {
                                totalLivros++;
                                qtdLivros += produto.getQuantidade();
                            } else if (produto instanceof CD) {
                                totalCDs++;
                                qtdCDs += produto.getQuantidade();
                            } else if (produto instanceof DVD) {
                                totalDVDs++;
                                qtdDVDs += produto.getQuantidade();
                            }
                        }

                        // Primeiro imprimir todos os livros e contar quantidades
                        for (Produto produto : loja.getTodosLivros()) {
                            System.out.print(produto);
                            System.out.printf("Quantidade: " + produto.getQuantidade() + "\n\n");
                        }
                        System.out.printf("Quantidade de Livros: %d\n\n", qtdLivros);
                        
                        // Depois imprimir todos os CDs e contar quantidades
                        for (Produto produto : loja.getTodosCDs()) {
                            System.out.print(produto);
                            System.out.printf("Quantidade: " + produto.getQuantidade() + "\n\n");
                        }
                        System.out.printf("Quantidade de CDs: %d\n\n", qtdCDs);

                        // Por último imprimir todos os DVDs e contar quantidades
                        for (Produto produto : loja.getTodosDVDs()) {
                            System.out.print(produto);
                            System.out.printf("Quantidade: " + produto.getQuantidade() + "\n\n");
                        }
                        System.out.printf("Quantidade de DVDs: %d\n\n\n", qtdDVDs);
                        
                        break;
                        
                    default:
                        System.out.println("Comando inválido");
                }
            } catch (Exception e) {
                System.out.println("Erro ao processar comando: " + e.getMessage());
            }
        }
        
        scanner.close();
    }
}

