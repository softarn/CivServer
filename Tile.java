package proxy;


public class Tile
{
	private int x, y;
	private Unit unit;
	private City city;
	
	public Tile(int x, int y, Unit unit, City city)
	{
		this.x = x;
		this.y = y;
		this.unit = unit;
		this.city = city;
	}
	
	public int getX()
	{
		return x;
	}
	
	public int getY()
	{
		return y;
	}
	
	public Unit getUnit()
	{
		return unit;
	}
	
	public City getCity()
	{
		return city;
	}
}
