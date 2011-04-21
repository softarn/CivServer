import java.util.*;

class Result{
	
	private boolean ok;
	private String okMsg, failMsg, name;
	private List sessions;
	private int size, requestFail, failNumber, requestOk;

	public Result(){
	}

	// --------------------------------------------
	// Här under är setters/tilläggsfunktioner!
	
	public void add(boolean ok){
		this.ok = ok;
	}

	public void addOkMsg(String msg){
		okMsg = msg;
	}

	public void addFailMsg(String msg){
		failMsg = msg;
	}

	public void addName(String name){
		this.name = name;
	}

	public void addSessions(List sessions){
		this.sessions = sessions;
	}

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

	// Här slutar setters/tilläggsfunktioner.
	// --------------------------------------------
	// Här under är getters!

	public boolean getOk(){
		return ok;
	}

	public String getOkMsg(){
		return okMsg;
	}

	public String getFailMsg(){
		return failMsg;
	}

	public String getName(){
		return name;
	}

	public List getSessions(){
		return sessions;
	}

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
	}
}
