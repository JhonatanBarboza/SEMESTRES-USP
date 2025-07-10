
public class Pessoa {

	private String nome;
	private int idade;
	private double altura;
	
	public Pessoa (String n, int id, double h) {
		nome = n;
		idade = id;
		altura = h;
	}

	public Pessoa() {
		this("",0,0.0);
	}
	
	public String getNome() {
		return nome;
	}

	public void setNome(String nome) {
		this.nome = nome;
	}

	public int getIdade() {
		return idade;
	}

	public void setIdade(int idade) {
		this.idade = idade;
	}

	public double getAltura() {
		return altura;
	}

	public void setAltura(double altura) {
		this.altura = altura;
	}

	public void mostraPessoa() {
		System.out.printf("Nome: %s\n", getNome());
		System.out.printf("Idade: %d anos\n", getIdade());
		System.out.printf("Altura: %.2f m\n", getAltura());
	}
}
