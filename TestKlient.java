import java.util.*;

class TestKlient{
	String name, ip = "130.229.157.246";
	Proxy p;
	int port;
	boolean loop = true;
	Scanner sc = new Scanner(System.in);
	Scanner scan = new Scanner(System.in);

	public TestKlient(String name, String ip, int port){
		mainMenu(name, ip, port);
	}

	public void mainMenu(String name, String ip, int port){

        this.name = name;
        this.ip = ip;
        this.port = port;

		p = new Proxy(ip, port, new MyPackLyss());

		Result returned;
		
		int what;

		while(loop){
			System.out.println("What you wanna do?\n1: connect\n2: list games\n3: host game\n4: start game\n5: join game\n0:quit");
			 what = sc.nextInt();

			switch(what){
				case 0:
					System.exit(0);
					
				case 1:
					try{
						returned = p.connect(name);
						System.out.println(returned.getOk());  
					}
					catch(FailedException fe){
						System.out.println(fe);
					}


					break;

				case 2:
					try{
						returned = p.listGames();
						System.out.println(returned.getSessions());
					}
					catch(FailedException fe){
						System.out.println(fe);
					}

					
					break;

				case 3:
					try{
						returned = p.host();
						System.out.println(returned.getHostName());
					}
					catch(FailedException fe){
						System.out.println(fe);
					}

					break;

				case 4:
					try{
						p.startGame();
					}
					catch(FailedException fe){
						System.out.println(fe);
					}
					loop = false;
					return;
				case 5:
					try{
						p.joinGame(scan.next());
					}
					catch(FailedException fe){
						System.out.println(fe);
					}
					break;
			}
		}
			
			

	}


	public void inGame(){
		int what;
		Result returned;

		 while(true){
				System.out.println("What you wanna do?\n1: list unitst\n2: move unit\n3: end turn\n0:quit");
			 what = sc.nextInt();
			    switch(what){
				   case 0:
					 System.exit(0);
					
				    case 1:
				    	try{
					        returned = p.connect(name);
					        System.out.println(returned.getOk());  
					}
					catch(FailedException fe){
						System.out.println(fe);
					}
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
        //Name, ip, port
		TestKlient flum = new TestKlient(args[0], args[1], Integer.parseInt(args[2]));
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
			loop = false;
			int numberOfTiles = res.getNumberTiles();
			for(int i=0; i<numberOfTiles; i++){
				int tileXValue = res.getTileX(i);
				int tileYValue = res.getTileY(i);
			if(res.existUnit(i)){
				String theOwner = res.getUnitOwner(i);
				String theType = res.getUnitType(i);
				System.out.println(theOwner + " " + theType + " x:" + tileXValue+ "y: "+ tileYValue);
				int manPowerLeft = res.getUnitManPower(i);
			}
			if(res.existCity(i)){
				String theOwner = res.getCityOwner(i);
				String theName = res.getCityName(i);
				List<String> buildings = res.getCityBuildings(i);
				int amountCityUnits = res.getAmountCityUnits(i);
				for(int j=0; j<amountCityUnits; j++){
					String cityUnitOwner = res.getCityUnitOwner(i, j);
					String cityUnitType = res.getCityUnitType(i, j);
					int cityUnitManpower = res.getCityUnitManPower(i, j);
				}
			}
		String improvement = res.getImprovement(i);
		}
		}

		public void chatMessageReceived(Result res){
		}

        public void gameClosed(){
            System.out.println("Game closed");
        }
	}
}
