package proxy;


public class CombatResult
{
	private int attackersRemainingManpower;
	private int defendersRemainingManpower;
	
	public CombatResult(int attackersRemainingManpower, int defendersRemainingManpower)
	{
		this.attackersRemainingManpower = attackersRemainingManpower;
		this.defendersRemainingManpower = defendersRemainingManpower;
	}
	
	public int getAttackersRemainingManpower()
	{
		return attackersRemainingManpower;
	}
	
	public int getDefendersRemainingManpower()
	{
		return defendersRemainingManpower;
	}
}
