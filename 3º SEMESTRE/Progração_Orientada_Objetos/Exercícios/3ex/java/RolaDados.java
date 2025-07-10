import java.util.Random;
import java.util.Scanner;

/**
 * Essa é uma classe auxiliar que permite gerencia um conjunto de vários dados simultaneamente.
 * Operações como rolar alguns dos dados ou exibir o resultado de todos eles, são implementadas.
 * @author delamaro
 *
 */
public class RolaDados {
	
	private Dado[] dados;


	/**
	 * Construtor que cria e armazena vários objetos do tipo {@link Dado}. Usa para isso o 
	 * construtor padrão daquela classe, ou seja, um dado de 6 lados e gerando sempre uma semente aleatória
	 * para o gerador de números aleatórios. Os dados criados podem ser referenciados por números, entre 1 
	 * e n.
	 * @param n Número de dados a serem criados.
	 * @param seed semente para criar qados. se fo 0 cria sem sement
	 */
	public RolaDados(int n, int seed)
	{
		Random r = new Random(seed);
		dados = new Dado[n];
		for (int i = 0; i < dados.length; i++)
		{
			dados[i] = seed == 0 ? new Dado(): new Dado(6, r.nextInt());
		}
	}	
	
	/**
	 * Rola alguns dos dados.
	 * @param quais É um array de booleanos que indica quais dados devem ser rolados. Cada posição
	 * representa um dos dados. Ou seja, a posição 0 do array indica se o dado 1 deve ser rolado ou não, 
	 * e assim por diante. 
	 * @return Retorna o valor de cada um dos dados, inclusive os que não foram rolados. 
	 * Nesse caso, o valor retornado é o valor anterior que ele já possuia.
	 */
	public int[] rolar(boolean[] quais)
	{
		int i = 0;
		int[] r = new int[dados.length];
		for (boolean b: quais)
		{
			if (i >= dados.length ) break;
			if ( b ) 
				dados[i].rolar();
			r[i] = dados[i].getLado();
			i++;
		}
		return r;
	}
	
	/**
	 * Rola alguns dos dados.
	 * @param s É um String que possui o número dos dados a serem rolados. Por exemplo 
	 * "1 4 5" indica que os dados 1 4 e cinco devem ser rolados. Os números devem ser 
	 * separados por espaços. Se o valor passado no string estiver fora do intervalo 
	 * válido, ele é ignorado simplesmente. 
	 * @return Retorna o valor de cada um dos dados, inclusive os que não foram rolados. 
	 * Nesse caso, o valor retornado é o valor anterior que ele já possuia.
	 */
	public int[] rolar(String s)
	{
		Scanner sc = new Scanner(s);
		boolean[] b = new boolean[dados.length];
		int i = 0;
		while ( sc.hasNextInt()) 
		{
			i = sc.nextInt() - 1;
			if (i >= 0 && i < dados.length )
				b[i] = true;
		}
		sc.close();
		return rolar(b);
	}
	
	/**
	 * Rola todos os dados.
	 * @return Retorna o valor de cada um dos dados, inclusive os que não foram rolados. 
	 * Nesse caso, o valor retornado é o valor anterior que ele já possuia.
	 */
	public int[] rolar()
	{
		int[] r = new int[dados.length];
		for (int i = 0; i < r.length; i++)
		{
			r[i] = dados[i].rolar();
		}
		return r;
	}
	
	/**
	 * Usa a representação em string do dados, para mostrar o valor de todos os dados
	 * do conjunto. Exibe os dados horisontalmente, por exemplo:<br>
	 * <pre>
	 * 1          2          3          4          5
+-----+    +-----+    +-----+    +-----+    +-----+    
|*   *|    |     |    |*    |    |*    |    |*   *|    
|  *  |    |  *  |    |     |    |  *  |    |     |    
|*   *|    |     |    |    *|    |    *|    |*   *|    
+-----+    +-----+    +-----+    +-----+    +-----+    
	 *</pre><br>
	 */
	@Override
	public String toString()
	{
		String s = "";
		for (int i = 0; i < 5; i++)
		{
			int base = i * 8;
			for (Dado d: dados )
			{
				String p = d.toString();
				// System.out.println(p);
				s += p.substring(base, base+7);
				s += "    ";
			}
			s+= "\n";
		}
		return s;
	}
	
	static public void main(String[] args) throws Exception
	{
		RolaDados rd = new RolaDados(5, 0);
		rd.rolar();
		System.out.println("1          2          3          4          5");
		System.out.println(rd);
		while (true)
		{
			String s = EntradaTeclado.leString();
			rd.rolar(s);
			System.out.println(rd);
		}
	}

}
