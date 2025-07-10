
public class Elevador {

	private int capacidade;
	private int andares;
	private int andarCorrente;
	private int pessoasDentro;
	
	public Elevador(int capacidade, int andares) {
		this.capacidade = capacidade;
		this.andares = andares;
		andarCorrente = 0;
		pessoasDentro = 0;
	}
	
	public void entra() {
		if (pessoasDentro < capacidade) 
			pessoasDentro++;
	}
	
	public void sai() {
		if (pessoasDentro > 0) 
			pessoasDentro--;
	}
	
	public void sobe() {
		if (andarCorrente < andares)
			andarCorrente++;
	}

	public void desce() {
		if (andarCorrente > 0)
			andarCorrente--;
	}

	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
