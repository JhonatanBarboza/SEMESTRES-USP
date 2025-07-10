
public class ControleMain {
	
	public static void main(String[] args) {
		TeVe tv = new TeVe();
		ControleRemoto cr = new ControleRemoto(tv);
		while (true) {
			System.out.println("\n\n\nSelecione uma das operações: ");
			System.out.println("1) Aumenta volume");
			System.out.println("2) Diminui volume");
			System.out.println("3) Sobe canal");
			System.out.println("4) Desce canal");
			System.out.println("5) Para digitar o canal");
			System.out.println("6) Informações");
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
				case 1: cr.volumeUp();
				    	break;
				case 2: cr.volumeDown();
						break;
				case 3: cr.canalUp();
						break;
				case 4: cr.canalDown();
						break;
				case 5: cr.vaiParaCanal();
						break;
				case 6: cr.requestInfo();
						break;
			}
			
		}

	}
}
