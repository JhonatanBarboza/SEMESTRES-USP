import java.io.IOException;

public class Agenda {


		private Pessoa[] vet;
		private int n;
		
		public Agenda() {
			vet = new Pessoa[100];
			n = 0;
		}
		
		public void armazenaPessoa(String nome, int idade, double altura) {
			if (n >= vet.length) {
				System.out.println("Capacidade excedida.");
				return;
			}
			Pessoa p = new Pessoa(nome, idade, altura);
			vet[n++] = p;
		}
				
		public void removePessoa(String nome) {
			int i = buscaPessoa(nome);
			if (i < 0) {
				System.out.println("Nome não encontrado");
				return;
			}
			vet[i] = vet[n-1];
			n--;
		}
		
		public int buscaPessoa(String nome) {
			int i;
			for (i = 0; i < n; i++) {
				Pessoa p = vet[i];
				if (p.getNome().equals(nome)) break;
			}
			return i == n ? -1: i;
		}
		
		void imprimeAgenda() {
			for (int i = 0; i < n; i++) {
				Pessoa p = vet[i];
				p.mostraPessoa();
				System.out.println();
			}
		}
		
		void imprimePessoa(int i) {
			if (i > 0 && i <= n) {
				vet[i-1].mostraPessoa();
			}
		}
				
		public static void main(String[] args) throws IOException {
			Agenda ag = new Agenda();
			while (true) {
				System.out.println("\n\n\nSelecione uma das operações: ");
				System.out.println("1) Adiciona pessoa");
				System.out.println("2) Remove pessoa");
				System.out.println("3) Procura pessoa");
				System.out.println("4) Mostra agenda");
				System.out.println("5) Mostra uma pessoa");
				System.out.println("0) Termina");
				System.out.print("Escolha............... ");
	            int op;
				try {
					op = EntradaTeclado.leInt();
				} catch (Exception e) {
					continue;
				}

				switch (op) {
					case 0: return;
					case 1: System.out.print("Nome: ");
							String nome = EntradaTeclado.leString();
							System.out.print("Idade: ");
							int idade = EntradaTeclado.leInt();
							System.out.print("Altura: ");
							double altura = EntradaTeclado.leDouble();
							ag.armazenaPessoa(nome, idade, altura);
					    	break;
					case 2: System.out.print("Nome: ");
							nome = EntradaTeclado.leString();
							ag.removePessoa(nome);
							break;
					case 3: System.out.print("Nome: ");
							nome = EntradaTeclado.leString();
							int k = ag.buscaPessoa(nome);
							if (k < 0)
								System.out.println("Não achou");
							else
								System.out.println("Achou na posição " + k);								
							break;
					case 4: ag.imprimeAgenda();
							break;
					case 5: System.out.print("Qual posição? ");
							k = EntradaTeclado.leInt();
							ag.imprimePessoa(k);
							break;
				}
				System.out.print("Enter para continuar");
				EntradaTeclado.leString();
			}
		}
		
}
