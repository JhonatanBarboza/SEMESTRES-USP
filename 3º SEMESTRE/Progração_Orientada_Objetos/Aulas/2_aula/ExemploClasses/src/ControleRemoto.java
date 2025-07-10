
public class ControleRemoto {

	/**
	 * Televisor associado ao controle remoto
	 */
	private TeVe minhaTV;
	
	
	/**
	 * Construtor
	 * @param tv -- Recebe como parâmetro a TV que irá controlar
	 */
	public ControleRemoto(TeVe tv) {
		minhaTV = tv;
	}



	public void volumeUp() {
		minhaTV.setVolumeUp();
	}



	public void volumeDown() {
		minhaTV.setVolumeDown();		
	}



	public void canalUp() {
		minhaTV.setCanalUp();
	}



	public void canalDown() {
		minhaTV.setCanalDown();		
	}



	public void vaiParaCanal() {
		System.out.print("Digita o canal que deseja: ");
		try {
			int k = EntradaTeclado.leInt();
			minhaTV.setCanal(k);
		}
		catch (Exception e)
		{
			;
		}

	}

	public void requestInfo() {
		minhaTV.showInfo();
	}

	
	
}
