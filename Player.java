package proxy;


public class Player
{
	private String name, civilization;
	
	public Player(String name, String civilization)
	{
		this.name = name;
		this.civilization = civilization;
	}
	
	public String getName()
	{
		return name;
	}
	
	public String getCivilization()
	{
		return civilization;
	}
}
