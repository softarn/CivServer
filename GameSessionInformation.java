package proxy;


import java.util.List;

public class GameSessionInformation
{
	private List<Player> playerList;
	private boolean locked;
	
	public GameSessionInformation(List<Player> playerList, boolean locked)
	{
		this.playerList = playerList;
		this.locked = locked;
	}
	
	public List<Player> getPlayerList()
	{
		return playerList;
	}
	
	public boolean isLocked()
	{
		return locked;
	}
}
