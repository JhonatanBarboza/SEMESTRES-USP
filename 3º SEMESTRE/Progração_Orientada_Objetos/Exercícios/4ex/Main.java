import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        ArvBin arvBin = new ArvBin(100); 
        ArvBin arvAVL = new ArvAVL(100);
        ArvBin arvBal = new ArvBal(100);
        Scanner leitor = new Scanner(System.in);
        
        while (leitor.hasNext()) {
            String linha = leitor.nextLine().trim();
            if (linha.isEmpty()) continue;
            
            String[] partes = linha.split(" ", 2);
            
            String comando = partes[0];
            String nome = partes[1];
            
            switch (comando) {
                case "i":
                    arvBin.inserir(nome);
                    arvBal.inserir(nome);
                    arvAVL.inserir(nome);
                    
                    
                    break;
                case "d":
                    arvBin.remover(nome);
                    arvBal.remover(nome);
                    arvAVL.remover(nome);

                    break;
                default:
                    System.out.println("Comando inválido. Use 'i' ou 'd'");
            }
        }
        System.out.println(arvBin.toString());
        System.out.println(arvBal.toString()); 
        System.out.println(arvAVL.toString());
        
        leitor.close();
    }
}
