public class Main {
    public static void main(String[] args) {
        // Criando um carro
        Carro fusca = new Carro("Fusca", 1969, "Azul");

        // Usando métodos
        fusca.ligar();      // Saída: "Ligando..."
        fusca.acelerar();   // Saída: "Acelerando..."
        System.out.println("Modelo: " + fusca.getModelo()); // Saída: "Modelo: Fusca"

        // Com toString() sobrescrito
        System.out.println(fusca); // Saída: "Carro [modelo=Fusca, ano=1969, cor=Azul]"
    }
}