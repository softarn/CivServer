package proxy;

import java.util.List;
import java.util.ArrayList;

public class LocalServer implements GameLogix
{
	private String ourName = null;
	
	public void connect(String name) throws FailedException
	{
		if(ourName != null)
			throw new FailedException(-1, "Invalid state - already connected");
		
		ourName = name;
	}
	
	public List<String> listGames() throws FailedException
	{
		return new ArrayList<String>();
	}
	
	public String host() throws FailedException
	{
		return ourName;
	}
	
	public String joinGame(String game) throws FailedException
	{
		throw new FailedException(-1, "Cannot join games in singleplayer mode");
	}
	
	public void changeCiv(String newCiv) throws FailedException
	{
	}
	
	public void lockGame(boolean lock) throws FailedException
	{
	}
	
	public void startGame() throws FailedException
	{
	}
	
	public void moveUnit(List<Integer> positions) throws FailedException
	{
	}
	
	public void endTurn() throws FailedException
	{
	}
	
	public CombatResult combatRequest(int attX, int attY, int defX, int defY) throws FailedException
	{
	}
	
	public void sendChatMessage(String toWhom, String message) throws FailedException
	{
	}
	
	public void builtCity(int x, int y, String name) throws FailedException
	{
	}
	
	public void madeUnit(int x, int y, String owner, String type, int manPower) throws FailedException
	{
	}
	
	public void disbandUnit(int x, int y) throws FailedException
	{
	}
	
	public void enterContainer(int x, int y, int containerX, int containerY) throws FailedException
	{
	}
	
	public void removeFromContainer(int containerX, int containerY, String unitType, int manpower) throws FailedException
	{
	}
	
	public void leaveGame() throws FailedException
	{
	}
}
