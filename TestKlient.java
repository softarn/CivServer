import java.util.*;

class TestKlient{
	String name = "Xanindorf2";
	Proxy p = new Proxy("130.237.238.239", 1233, new MyPackLyss());
	Scanner sc = new Scanner(System.in);

	public TestKlient(){
		mainMenu();
	}

	public void mainMenu(){

		while(true){
			System.out.println("What you wanna do?\n1: connect\n2: list games\n3: host game\n4: start game\n0:quit");
			int what = sc.nextInt();

			switch(what){
				case 0:
					System.exit(0);
					
				case 1:
					Result conRes = p.connect(name);

					System.out.println(conRes.getOk() + "   " + conRes.getOkMsg());

					break;

				case 2:
					Result lista = p.listGames();

					System.out.println(lista.getSessions());
					
					break;

				case 3:
					Result host = p.host(false);

					System.out.println(host.getName());
					break;

				case 4:
					p.startGame();
					break;



			}
		}

	}

	public class Unit{
		private String type;
		private int manPower;

		public Unit(String type, int mp){
			this.type = type;
			manPower = mp;
		}

		public String toString(){
			return "hej";
		}
	}

	public static void main(String [] args){
		TestKlient flum = new TestKlient();

		

	//	Result host = flum.p.host(false);

	//	System.out.println(host.getName());

//		try{
//			Thread.sleep(10000);
//		}
//		catch(Exception e){
//			e.printStackTrace();
//		}
//		
//		flum.p.startGame();

					

	}

	private class MyPackLyss implements PacketListener{
		
		MyPackLyss(){
		}

		public void newTurn(Result res){
			System.out.println(res.getSessions());
		}

		public void lobbyUpdated(Result res){
			int amount = res.getNumberPlayers();
			for(int i=0; i<amount; i++){
				System.out.println(res.getPlayerName(i));
				System.out.println(res.getPlayerCiv(i));
			}
		}

		public void gameStarted(Result res){
			System.out.println(res.getMap());
		}

		public void chatMessageReceived(Result res){

		}
	}
}
