import java.util.*;

class TestKlient{
	String name, ip = "localhost"; //"chylis.dyndns-at-work.com";//"softarn.mine.nu";
	Proxy p;
	int port = 1234;
	boolean loop = true, lobby = true, inGame = false;
	Scanner sc = new Scanner(System.in);
	Scanner scan = new Scanner(System.in);

	public TestKlient(String name, int port){
		p = new Proxy(ip, port, new MyPackLyss());
		mainMenu(name,  port);
	}

	public void mainMenu(String name, int port){

        this.name = name;
        this.port = port;


		Result returned;
		
		int what;

		while(loop){
			System.out.println("What you wanna do?\n1: connect\n2: list games\n3: host game\n4: join game\n0:quit");
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
						loop = false;
					}
					catch(FailedException fe){
						System.out.println(fe);
					}
					break;

				case 4:
					try{
						p.joinGame(scan.next());
						loop = false;
					}
					catch(FailedException fe){
						System.out.println(fe);
					}
					break;
			}
		}
			
		gameLobby();
			

	}
	public void gameLobby(){
		int choice;
		while(lobby){
			System.out.println("What you wanna do?\n1: Lock game\n2: Change civ\n4: Start game\n0: Leave lobby");
			choice = sc.nextInt();
			switch(choice){
				   case 0:
					   try{
						p.leaveGame();
						lobby = false;
					   }
					   catch(FailedException fe){
						   System.out.println(fe);
					   }
					 break;
				   case 1:
					 try{
						 System.out.println("Lock or unlock? (1 for lock, 0 for unlock)");
						 if(sc.nextInt() == 1){
							 p.lockGame(true);
						 }
						 else{
							 p.lockGame(false);
						 }
					 }
					 catch(FailedException fe){
						 System.out.println(fe);
					 }
					 break;

				case 2:
					try{
						System.out.println("What you wanna change to?");
						String newCiv = scan.next();
						p.changeCiv(newCiv);
					}
					catch(FailedException fe){
						System.out.println(fe);
					}
					break;
				case 4:
					try{
						p.startGame();
						lobby = false;
						inGame = true;
					}
					catch(FailedException fe){
						System.out.println(fe);
					}
					break;
			}

			if(inGame){
				lobby = false;
				inGame();
			}
		}
		loop = true;
		mainMenu(name, port);
	}

	public void inGame(){
		int what;
		Result returned;
		boolean continuous = true;

		 while(continuous){
				System.out.println("What you wanna do?\n1: list units\n2: move unit\n3: end turn\n4: attack\n5: Build unit\n6: Build city\n0: leave game");
			 what = sc.nextInt();
			    switch(what){
				   case 0:
					   try{
						p.leaveGame();
						continuous = false;
					   }
					   catch(FailedException fe){
						   System.out.println(fe);
					   }
					 break;
					
				    case 1:
				    //	try{
					        System.out.println("inte implementerat");  
				//	}
				//	catch(FailedException fe){
				//		System.out.println(fe);
				//	}
					break;
				    case 2:
				    	try{
						System.out.println("First starting x and y.");
					    int xMove = sc.nextInt();
					    int yMove = sc.nextInt();
						System.out.println("Then the next tile x and y.");
					    int xMoveTo = sc.nextInt();
					    int yMoveTo = sc.nextInt();

					   ArrayList<Integer> movement =  new ArrayList<Integer>();
					   movement.add(xMove);
					   movement.add(yMove);
					   movement.add(xMoveTo);
					   movement.add(yMoveTo);
					    p.moveUnit(movement);
					}
					catch(FailedException fe){
						System.out.println(fe);
					}
					break;

				    case 3:
					try{
						p.endTurn();
					}
					catch(FailedException fe){
						System.out.println(fe);
					}
					break;
				    case 4:
					try{
						System.out.println("Attacker position X:");
						int attX = sc.nextInt();
						System.out.println("Attacker position Y:");
						int attY = sc.nextInt();
						System.out.println("Defender position X:");
						int defX = sc.nextInt();
						System.out.println("Defender position Y:");
						int defY = sc.nextInt();

						returned = p.combatRequest(attX, attY, defX, defY);
						System.out.println("The attacker has " + returned.getAttackerLeft() + " manpower left.\nThe defender has " + returned.getDefenderLeft() + " manpower left");
					}
					catch(FailedException fe){
						System.out.println(fe);
					}
					break;
				case 5:
					try{
						System.out.println("Where is the unit built? (first x then y)");
						int unitX = sc.nextInt();
						int unitY = sc.nextInt();
						System.out.println("What kind of unit is it?");
						String unitType = scan.next();
						p.madeUnit(unitX, unitY, name, unitType, 100);
					}
					catch(FailedException fe){
						System.out.println(fe);
					}
					break;
				case 6:
					try{
						System.out.println("Where is the city built? (first x then y)");
						int cityX = sc.nextInt();
						int cityY = sc.nextInt();
						System.out.println("Name of city?");
						String cityName = scan.next();
						p.builtCity(cityX, cityY, cityName);
					}
					catch(FailedException fe){
						System.out.println(fe);
					}
			    }
			 }
		 loop = true;
		 mainMenu(name, port);

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
		TestKlient flum = new TestKlient(args[0], Integer.parseInt(args[1]));
	}

	private class MyPackLyss implements PacketListener{
		
		MyPackLyss(){
		}

		public void newTurn(Result res){
			System.out.println("It's your turn now!");
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
			lobby = false;
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
					int amountCityUnits = res.getAmountCityUnits(i);
					for(int j=0; j<amountCityUnits; j++){
						String cityUnitOwner = res.getCityUnitOwner(i, j);
						String cityUnitType = res.getCityUnitType(i, j);
						int cityUnitManpower = res.getCityUnitManPower(i, j);
					}
				}
				String improvement = res.getImprovement(i);
			}
			inGame = true;
		}

		public void chatMessageReceived(Result res){
		}

        public void gameClosed(){
            System.out.println("Game closed");
        }
	}
}
