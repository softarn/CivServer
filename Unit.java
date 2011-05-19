package proxy;


import java.util.List;

public class Unit
{
	private String owner, unitType;
	private int manpower;
	private List<Unit> containedUnits;
	
	public Unit(String owner, String unitType, int manpower, List<Unit> containedUnits)
	{
		this.owner = owner;
		this.unitType = unitType;
		this.manpower = manpower;
		this.containedUnits = containedUnits;
	}
	
	public String getOwner()
	{
		return owner;
	}
	
	public String getUnitType()
	{
		return unitType;
	}
	
	public int getManpower()
	{
		return manpower;
	}
	
	public List<Unit> getContainedUnits()
	{
		return containedUnits;
	}
}
