import java.util.*;

class TestKlient{
	Proxy p = null;
	Scanner sc = new Scanner(System.in);

	public TestKlient(String name, int port){
        p = new Proxy("localhost", port, new MyPackLyss());
		mainMenu(name);
	}

	public void mainMenu(String name){

		while(true){
			System.out.println("What you wanna do?\n"+
                    "1: connect \n"+
                    "2: list games\n"+
                    "3: host game\n"+
                    "4: start game\n"+
                    "5: join game\n"+
                    "0:quit");
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

				case 5:
					Result res = p.joinGame(sc.next());
					System.out.println(res.getOk() + "   " + res.getOkMsg());
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
		TestKlient flum = new TestKlient(args[0], Integer.parseInt(args[1]));

		

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
