public class Carro {
        // Atributos (caracteristicas)
        private String modelo;
        private int ano;
        private String cor;

        // Construtor
        public Carro(String modelo, int ano, String cor ){
            this.modelo = modelo;
            this.ano = ano;
            this.cor = cor; 
        }

        // metodos (getters e setters)
        public String getModelo() {
            return modelo;
        }
        public int getAno() {
            return ano;
        }
        public String getCor() {
            return cor;
        }

        // metodo (ações)
        public void acelerar() {
            System.out.println("Acelerando...");
        }

        public void frear() {
            System.out.println("Freando...");
        }

        public void ligar() {
            System.out.println("Ligando...");
        }

        public void desligar() {
            System.out.println("Desligando...");
    }
}


 