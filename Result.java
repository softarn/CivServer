import java.util.*;

class Result{
	
	// Här är medlemsvariabler som ska kunna returneras.
	private boolean ok = true, locked;
	private String failMsg, hostName, chatMessage, chatName, fromWhom;
	private List sessions, players, map, updatedTiles;
	private int requestFail, requestOk, attackerLeft, defenderLeft;

	// Här är medlemsvariabler som bara är hjälpvariabler till vissa metoder.
	private List cityUnits = new ArrayList<Unit>();
	private Tile temp;

	public Result(){
	}

	// --------------------------------------------
	// Här under är setters/tilläggsfunktioner!
	//
	// Först så kommer booleans.
	
	public void addOk(boolean ok){
		this.ok = ok;
	}

	public void addLocked(boolean locked){
		this.locked = locked;
	}

	// Sen så kommer strängar.

	public void addFailMsg(String msg){
		failMsg = msg;
	}

	public void addHostName(String name){
		hostName = name;
	}

	public void addChatMessage(String message){
		chatMessage = message;
	}

	public void addFromWhom(String name){
		fromWhom = name;
	}

	// Här nedan är det för listor.
	
	public void addSessions(List sessions){
		this.sessions = sessions;
	}

	public void addMap(List map){
		this.map = map;
	}

	// Och sist heltal.

	public void addRequestFail(int requestFail){
		this.requestFail = requestFail;
	}

	public void addRequestOk(int requestOK){
		this.requestOk = requestOk;
	}

	public void addAttackerLeft(int attLeft){
		attackerLeft = attLeft;
	}

	public void addDefenderLeft(int defLeft){
		defenderLeft = defLeft;
	}

	// Sets för komplexa strukturer.
	
	// För listan med spelare först.
	
	public void setupPlayers(){
		players = new ArrayList<Player>();
	}

	public void addPlayer(String name, String civ){
		players.add(new Player(name, civ));
	}

	// Sen till den aningen större för listan med uppdaterade tiles.

	public void setupTiles(){
		updatedTiles = new ArrayList<Tile>();
	}

	public void setUpdatedTile(int x, int y){
		temp = new Tile(new Position(x, y));
	}

	public void setUnit(String owner, String type, int manPower){
		temp.addUnit(new Unit(owner, type, manPower));
	}

	public void addCityUnit(String owner, String type, int manPower){
		cityUnits.add(new Unit(owner, type, manPower));
	}

	public void setCity(String owner, String name, List<String> buildings){
		temp.addCity(new City(owner, name, buildings, cityUnits));
	}

	public void setImprovement(String name){
		temp.addImprovement(name);
	}

	public void addUpdatedTile(){
		updatedTiles.add(temp);
	}

	// Här slutar setters/tilläggsfunktioner.
	// --------------------------------------------
	// Här under är getters!
	//
	// Först booleans.

	public boolean getOk(){
		return ok;
	}

	public boolean getLocked(){
		return locked;
	}

	// Sen Strängar.

	public String getFailMsg(){
		return failMsg;
	}

	public String getHostName(){
		return hostName;
	}

	public String getChatMessage(){
		return chatName;
	}

	public String getChatFromWhom(){
		return fromWhom;
	}

	// Under finns getters för listor.

	public List getSessions(){
		return sessions;
	}

	public List getMap(){
		return map;
	}

	// Här kommer getters för heltal.

	public int getRequestFail(){
		return requestFail;
	}

	public int getRequestOk(){
		return requestOk;
	}

	public int getAttackerLeft(){
		return attackerLeft;
	}

	public int getDefenderLeft(){
		return defenderLeft;
	}

	// Här under kommer det att finnas getters för dom mer komplexa strukturerna.
	//
	// Börjar med get för spelare.
	
	public int getNumberPlayers(){
		return players.size();
	}

	public String getPlayerName(int n){
		return ((Player)players.get(n)).getName();
	}

	public String getPlayerCiv(int n){
		return ((Player)players.get(n)).getCiv();
	}

	// Och sen för den gigantiska Tile-klassen som dessutom då är i en lista.

	public int getNumberTiles(){
		return updatedTiles.size();
	}

	public int getTileX(int n){
		return ((Tile)updatedTiles.get(n)).getPosition().getX();
	}

	public int getTileY(int n){
		return ((Tile)updatedTiles.get(n)).getPosition().getY();
	}

	public boolean existUnit(int n){
		if(((Tile)updatedTiles.get(n)).getUnit() != null){
			return true;
		}
		else{
			return false;
		}
	}

	public String getUnitOwner(int n){
		return ((Tile)updatedTiles.get(n)).getUnit().getOwner();
	}

	public String getUnitType(int n){
		return ((Tile)updatedTiles.get(n)).getUnit().getType();
	}

	public int getUnitManPower(int n){
		return ((Tile)updatedTiles.get(n)).getUnit().getManPower();
	}

	public boolean existCity(int n){
		if(((Tile)updatedTiles.get(n)).getCity() != null){
			return true;
		}
		else{
			return false;
		}
	}

	public String getCityOwner(int n){
		return ((Tile)updatedTiles.get(n)).getCity().getOwner();
	}

	public String getCityName(int n){
		return ((Tile)updatedTiles.get(n)).getCity().getName();
	}

	public List<String> getCityBuildings(int n){
		return ((Tile)updatedTiles.get(n)).getCity().getBuildings();
	}

	public int getAmountCityUnits(int n){
		return ((Tile)updatedTiles.get(n)).getCity().getAmountUnits();
	}

	public String getCityUnitOwner(int n, int k){
		return ((Tile)updatedTiles.get(n)).getCity().getUnit(k).getOwner();
	}

	public String getCityUnitType(int n, int k){
		return ((Tile)updatedTiles.get(n)).getCity().getUnit(k).getType();
	}

	public int getCityUnitManPower(int n, int k){
		return ((Tile)updatedTiles.get(n)).getCity().getUnit(k).getManPower();
	}

	public String getImprovement(int n){
		return ((Tile)updatedTiles.get(n)).getImprovement();
	}

	// Här slutar getters.
	// --------------------------------------------
	// Här under är nestlade klasser för ytterligare containers!
	
	private class Unit{
		private String owner, type;
		private int manPower;

		public Unit(String owner, String type, int manPower){
			this.owner = owner;
			this.type = type;
			this.manPower = manPower;
		}

		public String getOwner(){
			return owner;
		}

		public String getType(){
			return type;
		}

		public int getManPower(){
			return manPower;
		}

		public String toString(){
			return "Unit type: " + type + "\nUnit owner: " + owner + "\nUnit manpower: " + manPower + "\n";
		}
	}

	private class City{
		private String owner, name;
		private List<String> buildings;
		private List<Unit> units;

		public City(String owner, String name, List<String> buildings, List<Unit> units){
			this.owner = owner;
			this.name = name;
			this.buildings = buildings;
			this.units = units;
		}

		public String getOwner(){
			return owner;
		}

		public String getName(){
			return name;
		}

		public List<String> getBuildings(){
			return buildings;
		}

		public int getAmountUnits(){
			return units.size();
		}

		

		public Unit getUnit(int n){
			return units.get(n);
		}

		public String toString(){
			return "City name: " + name + "\nCity owner: " + owner + "\nBuildings in city: " + buildings + "\nUnits in city: " + units + "\n";
		}
	}

	private class Position{
		private int x, y;

		public Position(int x, int y){
			this.x = x;
			this.y = y;
		}

		public int getX(){
			return x;
		}

		public int getY(){
			return y;
		}

		public String toString(){
			return x + "\t" + y + "\n";
		}
	}

	private class Tile{
		private Position placement;
		private Unit unit = null;
		private City city = null;
		private String improvement = "";

		public Tile(Position placement){
			this.placement = placement;
		}

		public void addUnit(Unit unit){
			this.unit = unit;
		}

		public void addCity(City city){
			this.city = city;
		}

		public void addImprovement(String improvement){
			this.improvement = improvement;
		}

		public Position getPosition(){
			return placement;
		}

		public Unit getUnit(){
			return unit;
		}

		public City getCity(){
			return city;
		}

		public String getImprovement(){
			return improvement;
		}

		public String toString(){
			return "Tile at position: " + placement + "\nUnit on tile: " + unit + "\nCity on tile: " + city + "\nImprovement on tile: " + improvement + "\n";
		}
	}

	private class Player{
		private String name, civilization;

		public Player(String name, String civilization){
			this.name = name;
			this.civilization = civilization;
		}

		public String getName(){
			return name;
		}

		public String getCiv(){
			return civilization;
		}

		public String toString(){
			return name + "\t" + civilization + "\n";
		}
	}

	// Tänkte lägga en toString här nedan, om man vill se det mesta kommer den bli cp-lång.
	
	public String toString(){
		return "Request accepted: " + ok + "\n" + updatedTiles;
	}
}
