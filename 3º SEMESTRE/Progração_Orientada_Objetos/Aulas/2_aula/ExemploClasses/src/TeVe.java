
public class TeVe {
private int qtosCanais = 100;
private int maxVolume = 60;
private int canalAtual = 1;
private int volumeAtual = 1;
	
	public void setVolumeUp() {
		if ( volumeAtual < maxVolume )
			volumeAtual++;
	}

	public void setVolumeDown() {
		if ( volumeAtual > 0 )
			volumeAtual--;
	}

	public void setCanalUp() {
		if ( canalAtual < qtosCanais )
			canalAtual++;
		else
			canalAtual = 1;
	}

	public void setCanalDown() {
		if ( canalAtual > 1 )
			canalAtual--;
		else
			canalAtual = qtosCanais;
	}

	public void setCanal(int k) {
		if ( k >= 1 && k <= qtosCanais )
			canalAtual = k;
	}

	public void showInfo() {
		System.out.println("O canal corrente é: " + canalAtual);
		System.out.println("O volume corrente é: " + volumeAtual );
	}

}
