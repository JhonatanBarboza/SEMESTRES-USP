import java.util.Random;

/**
 * Simula um dado de número de lados variados. Ao criar o objeto 
 * é possível estabelecer o número de lados. A rolagem do dado é
 * feita por meio de um gerador de números aleatórios ({@link Random}).
 * 
 * @author delamaro
 *
 */
public class Dado {
	private int lados;
	private int atual;
	private Random r;
	
	/**
	 * Cria objeto com um número qualquer de lados
	 * @param n - número de lados do dado
	 */
	public Dado(int n)
	{
		lados = n;
		r = new Random();
		rolar();
	}

	/**
	 * Cria objeto com um número qualquer de lados
	 * @param n - número de lados do dado
	 * @param seed - semente para inicializar gerador aleatório
	 */
	public Dado(int n, int seed)
	{
		lados = n;
		r = new Random(seed);
		rolar();
	}

	/**
	 * Cria um dado com 6 lados (um cubo)
	 */
	public Dado()
	{
		lados = 6;
		r = new Random();
		rolar();
	}
	
	/**
	 * Simula a rolagem do dado por meio de um gerador aleatório.
	 * O número selecionado pode posteriormente ser recuperado com a chamada a 
	 * {@link getLado()}
	 * @return o número que foi sorteado
	 */
	public int rolar() {
		atual = r.nextInt(lados) + 1;
		return atual;
	}
	
	/**
	 * Recupera o último número selecionado.
	 * @return o número do último lado selecionado.
	 */
	public int getLado()
	{
		return atual;
	}

	private String  s010 = "|  *  |\n",
			s100 = "|*    |\n",
			s001 = "|    *|\n",
			s000 = "|     |\n",
			s101 = "|*   *|\n",
			s111 = "|* * *|\n";

	/**
	 * Transforma representação do dado em String. É mostrada
	 * uma representação do dado que está para cima. Note que só
	 * funciona corretamente para dados de 6 lados. Exemplo:<br>
	 * <pre>
+-----+    
|*   *|    
|  *  |    
|*   *|    
+-----+    
	 * </pre><br>
	 */
	@Override
	public String  toString()
	{
		if (lados != 6 ) return "Não há representação para esse dados";
		String s = "+-----+\n";
		switch (getLado()) {
		case 1:
			s += (s000 + s010 + s000);
			break;
		case 2:
			s += (s100 + s000 + s001);
			break;
		case 3:
			s += (s100 + s010 + s001);
			break;
		case 4:
			s += (s101 + s000 + s101);
			break;
		case 5:
			s += (s101 + s010 + s101);
			break;
		case 6:
			s += (s111 + s000 + s111);
			break;
			
		}
		s += ("+-----+\n");
		return s;
	}

	/**
	 * Não tem função real dentro da classe. Foi usada apenas para testar os métodos 
	 * implementados
	 * @param args -- Sem uso.
	 */
	static public void main(String[] args)
	{
		Dado d = new Dado(6,1);
		for (int i = 0; i < 100; i++)
		{
			d.rolar();
			System.out.println(d.getLado() + "");
			System.out.println(i + ") ");
			System.out.println(d);
		}
	}
	
}
