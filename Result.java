import java.util.*;

class Result{
	
	// Här är medlemsvariabler som ska kunna returneras.
	private boolean ok, locked;
	private String okMsg, failMsg, name;
	private List sessions, players, map, updatedTiles;
	private int size, requestFail, failNumber, requestOk;

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

	public void addOkMsg(String msg){
		okMsg = msg;
	}

	public void addFailMsg(String msg){
		failMsg = msg;
	}

	public void addName(String name){
		this.name = name;
	}

	// Här nedan är det för listor.
	
	public void addSessions(List sessions){
		this.sessions = sessions;
	}

	public void addMap(List map){
		this.map = map;
	}

	// Och sist heltal.

	public void addSize(int size){
		this.size = size;
	}

	public void addRequestFail(int requestFail){
		this.requestFail = requestFail;
	}

	public void addFailNumber(int failNumber){
		this.failNumber = failNumber;
	}

	public void addRequestOk(int requestOK){
		this.requestOk = requestOk;
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

	public String getOkMsg(){
		return okMsg;
	}

	public String getFailMsg(){
		return failMsg;
	}

	public String getName(){
		return name;
	}

	// Under finns getters för listor.

	public List getSessions(){
		return sessions;
	}

	public List getMap(){
		return map;
	}

	// Här kommer getters för heltal.

	public int getSize(){
		return size;
	}

	public int getRequestFail(){
		return requestFail;
	}

	public int getFailNumber(){
		return failNumber;
	}

	public int getRequestOk(){
		return requestOk;
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

		public List<Unit> getUnits(){
			return units;
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
	}

	private class Tile{
		private Position placement;
		private Unit unit;
		private City city;
		private String improvement;

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
	}

	// Tänkte lägga en toString här nedan, om man vill se det mesta kommer den bli cp-lång.
	
	public String toString(){
		return "Request accepted: " + ok;
	}
}
