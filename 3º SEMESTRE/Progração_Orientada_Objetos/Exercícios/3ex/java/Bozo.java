import java.io.IOException;


/**
 * Essa é a classe inicial do programa Bozó. Possui apenas o método main, que cuida da execução do jogo.
 * @author delamaro
 *
 */
public class Bozo {
	/**
	 * Número de rodadas do jogo. Como o placar possui dez posições, são dez rodadas.
	 */
	static final int NRODADAS = 10;

	/**
	 * Método inicial do programa. Ele cuida da execução do jogo e possui um laço, 
	 * no qual cada iteração representa uma rodada do jogo. Em cada rodada, o jogador
	 * joga os dados até 3 vezes e depois escolhe a posição do placar que deseja preencher.
	 * No final das rodadas a pontuação total é exibida.
	 * @param args
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
		int rodada;
		System.out.print("Digite a semente (zero para aleatório): ");
		int seed = EntradaTeclado.leInt();
		RolaDados rd = new RolaDados(5, seed);
		Placar pl = new Placar();
		System.out.println(pl);
		
		for (rodada = 0; rodada < NRODADAS; rodada++) {
			System.out.println("****** Rodada " + (rodada+1));
			System.out.println("Pressione ENTER para lançar os dados");
			EntradaTeclado.leString();
			// primeira tentativa
			rd.rolar();
			System.out.println("1          2          3          4          5");
			System.out.println(rd);

			// segunda tentativa
			System.out.println("Digite os números dos dados que quiser TROCAR. Separados por espaços.");
			String muda = EntradaTeclado.leString();
			rd.rolar(muda);
			System.out.println("1          2          3          4          5");
			System.out.println(rd);
			
			// segunda tentativa
			System.out.println("Digite os números dos dados que quiser TROCAR. Separados por espaços.");
			muda = EntradaTeclado.leString();
			int[] values = rd.rolar(muda);
			System.out.println("1          2          3          4          5");
			System.out.println(rd);
			
			System.out.println("\n\n");
			System.out.println(pl);
			int pos = 0;
			while ( pos <= 0 )
			{
				try {
 				   System.out.print("Escolha a posição que quer ocupar com essa jogada ===> ");
				   pos = EntradaTeclado.leInt();
					if ( pos > NRODADAS || pos <= 0 ) pos = 0;
				   pl.add(pos, values);
				}
				catch (Exception e) {
					pos = 0;
				}
				if ( pos == 0 )
					System.out.println("Valor inválido. Posição ocupada ou inexistente.");
			}
			
			System.out.println("\n\n");
			System.out.println(pl);
			
		}
		System.out.println("***********************************");
		System.out.println("***");
		System.out.println("*** Seu escore final foi: " + pl.getScore());
		System.out.println("***");
		System.out.println("***********************************");

	}

}
