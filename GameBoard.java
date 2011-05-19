package proxy;


import java.util.List;
import java.util.ArrayList;

public class GameBoard
{
	ArrayList<ArrayList<String>> terrainData;
	List<Tile> tileList;
	
	public GameBoard(ArrayList<ArrayList<String>> terrainData, List<Tile> tileList)
	{
		this.terrainData = terrainData;
		this.tileList = tileList;
	}
	
	public ArrayList<ArrayList<String>> getTerrainData()
	{
		return terrainData;
	}
	
	public List<Tile> getTileList()
	{
		return tileList;
	}
}
