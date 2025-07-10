// Beatriz Alves dos Santos 15588630
// Jhonatan Barboza da Silva 15645049
// Kevin Ryoji Nakashima 15675936

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

public class Main {
    
    // Classe de dados que representa cada linha do CSV
    static class Dados {
        final String pais;
        final int confirmados;
        final int mortes;
        final int recuperados;
        final int ativos;
        
        // Construtor para inicializar os dados
        public Dados(String pais, int confirmados, int mortes, int recuperados, int ativos) {
            this.pais = pais;
            this.confirmados = confirmados;
            this.mortes = mortes;
            this.recuperados = recuperados;
            this.ativos = ativos;
        }
    }
    
    // Método principal que executa as funções solicitadas
    // Função 1: Soma dos "Ativos" onde "Confirmados" >= n1
    // Função 2: Soma das "Mortes" dos primeiros n2 "Ativos" ordenados decrescentemente e os primeiros n3 "Confirmados" ordenados crescentemente
    // Função 3: Lista de países com os primeiros n4 "Confirmados" ordenados decrescentemente, ordenados alfabeticamente e sem duplicatas
    public static void main(String[] args) throws IOException {
        // Lê a entrada do usuário
        Scanner scanner = new Scanner(System.in);
        String[] entrada = scanner.nextLine().split(" ");
        int n1 = Integer.parseInt(entrada[0]);
        int n2 = Integer.parseInt(entrada[1]);
        int n3 = Integer.parseInt(entrada[2]);
        int n4 = Integer.parseInt(entrada[3]);
        
        // Lê o arquivo CSV
        List<String> linhas = Files.readAllLines(Paths.get("dados.csv"));
        
        // Converte as linhas para objetos Dados
        List<Dados> dados = linhas.stream()
            .map(Main::converterString)
            .collect(Collectors.toList());
        
        // Função 1: Soma dos "Ativos" onde "Confirmados" >= n1
        // 1. Filtra os dados onde confirmados >= n1
        // 2. Mapeia para os ativos
        // 3. Soma os ativos
        // 4. Imprime o resultado
        int somaAtivos = dados.stream()
            .filter(d -> d.confirmados >= n1)
            .mapToInt(d -> d.ativos)
            .sum();
        System.out.println(somaAtivos);
        
        // Função 2: 
        // 1. Ordena por Ativos (decrescente)
        // 2. Pega os primeiros n2
        // 3. Ordena por Confirmados (crescente)
        // 4. Pega os primeiros n3
        // 5. Soma as mortes
        int somaMortes = dados.stream()
            .sorted(Comparator.comparingInt((Dados d) -> d.ativos).reversed())
            .limit(n2)
            .sorted(Comparator.comparingInt(d -> d.confirmados))
            .limit(n3)
            .mapToInt(d -> d.mortes)
            .sum();
        System.out.println(somaMortes);
        
        // Função 3:
        // 1. Ordena por Confirmados (decrescente)
        // 2. Pega os primeiros n4
        // 3. Ordena por país (alfabético)
        // 4. Remove duplicados
        // 5. Mapeia para os nomes dos países
        List<String> paises = dados.stream()
            .sorted(Comparator.comparingInt((Dados d) -> d.confirmados).reversed())
            .limit(n4)
            .sorted(Comparator.comparing(d -> d.pais))
            .map(d -> d.pais)
            .distinct()
            .collect(Collectors.toList());
        
        paises.forEach(System.out::println);
    }
    
    // Método auxiliar para converter uma linha do CSV em um objeto Dados
    private static Dados converterString(String str) {
        String[] partes = str.split(",");
        // Verifica se a linha tem o número correto de partes
        return new Dados(
            partes[0],
            Integer.parseInt(partes[1]),
            Integer.parseInt(partes[2]),
            Integer.parseInt(partes[3]),
            Integer.parseInt(partes[4])
        );
    }
}