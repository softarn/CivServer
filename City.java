package proxy;


import java.util.List;

public class City
{
	private String name, owner;
	private List<Unit> unitList;
	
	public City(String owner, List<Unit> unitList, String name)
	{
		this.owner = owner;
		this.unitList = unitList;
		this.name = name;
	}
	
	public String getOwner()
	{
		return owner;
	}
	
	public List<Unit> getUnitList()
	{
		return unitList;
	}
	
	public String getName()
	{
		return name;
	}
}
