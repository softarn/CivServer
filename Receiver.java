package proxy;

import java.util.*;
import java.io.*;
import java.net.*;

public class Receiver implements Runnable
{

	private Result packet = null;
	private InputStream m_inStream;
	private OutputStream m_outStream;
	private PacketListener pl;

	public Receiver(PacketListener pl, InputStream inStream, OutputStream outStream)
	{
		this.pl = pl;
		m_inStream = inStream;
		m_outStream = outStream;
	}

	public void run()
	{
		try
		{
			while(true)
			{
				receive();
			}
		}
		catch(IOException e)
		{
			e.printStackTrace();
		}
	}

	public Result getResult() throws FailedException
	{
		while(getPacket() == null)
		{
			try
			{
				Thread.sleep(100);
			}
			catch(Throwable t){}

		//	System.out.println("getResult()");
		}
		Result toReturn = getPacket();
       		setPacket(null);
		if(!toReturn.getOk()){
			throw new FailedException(toReturn.getRequestFail(), toReturn.getFailMsg());
		}
		return toReturn;
	}

    private synchronized void setPacket(Result res){
        packet = res;
    }

    private synchronized Result getPacket(){
        return packet;
    }

	// Sends the packet.
	private void send(Packet p) throws IOException
	{
		m_outStream.write(p.getBuffer());
		m_outStream.flush();
	}

	// Waits for an answer from the server and parses what it gets.
	// Puts what it gets into an object of the Result class, which is then returned.
	private void receive() throws IOException
	{
		while(getPacket() != null)
		{
			try
			{
				Thread.sleep(100);
			}
			catch(Throwable t){}
		}
		
		Result toReturn = new Result();

		// Receive the header
		switch(receiveByte())
		{
		// Header 3, welcome to the real world.
		case 3:
			toReturn.addOk(true);
			setPacket(toReturn);
			break;

		// Header 0, fail'd.
		case 0:
			toReturn.addOk(false);
			toReturn.addRequestFail(receiveByte());
			toReturn.addFailMsg(receiveString());
			setPacket(toReturn);
			break;

		// Header 1, the confirm'd, but right now we receive a string for testing purposes.
		case 1:
			toReturn.addRequestOk(receiveByte());
			toReturn.addOk(true);
			setPacket(toReturn);
			break;
	
		// Header 4, a ping and answers directly with a pong.
		case 4:
			receiveBool();
			Packet toSend = new Packet((byte)4);
			toSend.add(true);
			send(toSend);
			break;

		// Header 6, listGameAnswer, returns a list of games.
		case 6:
			toReturn.addSessions(receiveStringList());
			setPacket(toReturn);
			break;

		// Header 9, Join answer, returns what game you are connected to.
		case 9:
			toReturn.addOk(true);
			toReturn.addHostName(receiveString());
			setPacket(toReturn);
			break;

		// Header 10, Game session info, returns all the other players and if the game is locked.
		case 10:
			pl.lobbyUpdated(receiveGameSessionInformation());
			break;

		// Header 14, Start game answer, receives the map and hopefully returns it.
		case 14:
			pl.gameStarted(receiveGameBoard());
			break;

		// Header 17, It's your turn, receives a list of updated tiles.
		case 17:
			pl.newTurn(receiveTileList());
			break;

		// Header 19, Combat result, receives what's left of the fighting units.
		case 19:
			toReturn.addAttackerLeft(receiveInt());
			toReturn.addDefenderLeft(receiveInt());

			setPacket(toReturn);
			break;

		// Header 20, Message for you, sir!, receives a chat message.
		case 20:
			{
				String sender = receiveString();
				String message = receiveString();

				pl.chatMessageReceived(new ChatMessage(sender ,message));
			}
			break;
		
		// Header 25, game was closed
		case 25:
			pl.gameClosed();
			break;
		}
	
	}
	
	
	private int receiveByte() throws IOException
	{
		int b = m_inStream.read();
		if(b < 0)
			throw new IOException("InputStream.read returned a negative value");
		
		return b;
	}
	
	// Method for reading a boolean from the inputStream.
	private boolean receiveBool() throws IOException
	{
		return receiveByte() != 0;
	}

	// Läser in ett heltal från inStream:en.
	private int receiveInt() throws IOException
	{
		int toReturn = 0;
		
		for(int i = 0; i < 4; ++i)
			toReturn |= receiveByte() << (8 * (3 - i));
		
		return toReturn;
	}
	
	// To receive Strings from the server.
	private String receiveString()  throws IOException
	{
		String toReturn = "";
		for(;;)
		{
			int chara = receiveByte();
			if(chara == 0)
				break;

			toReturn += (char)chara;
		}
		return toReturn;
	}

	// Läser in en lista av heltal och returnerar den.
	private ArrayList<Integer> receiveIntegerList(int size) throws IOException
	{
		ArrayList<Integer> toReturn = new ArrayList<Integer>();
		for(int i = 0; i < size; ++i)
		{
			toReturn.add(receiveInt());
		}
		return toReturn;
	}
	
	// Läser in en lista av strängar och returnerar den.
	private ArrayList<String> receiveStringList() throws IOException
	{
		int size = receiveInt();
		ArrayList<String> toReturn = new ArrayList<String>();
		for(int i = 0; i < size; ++i)
		{
			toReturn.add(receiveString());
		}
		return toReturn;
	}

	// Läser in en lista av listor av strängar, 2D-lista av strängar.
	private ArrayList<ArrayList<String>> receiveStringMatrix() throws IOException
	{
		int size = receiveInt();
		ArrayList<ArrayList<String>> toReturn = new ArrayList<ArrayList<String>>();
		for(int i = 0; i < size; ++i)
		{
			toReturn.add(receiveStringList());
		}
		return toReturn;
	}

	
	private ArrayList<Tile> receiveTileList() throws IOException
	{
		int size = receiveInt();
		ArrayList<Tile> toReturn = new ArrayList<Tile>();
		for(int i = 0; i < size; ++i)
		{
			toReturn.add(receiveTile());
		}
		return toReturn;
	}
	
	private List<Unit> receiveUnitList() throws IOException
	{
		int size = receiveInt();
		List<Unit> unitList = new ArrayList<Unit>();
		for(int i = 0; i < size; ++i)
		{
			String owner = receiveString();
			String unitType = receiveString();
			int manpower = receiveInt();
			Unit unit = new Unit(owner, unitType, manpower, receiveUnitList());
			unitList.add(unit);
		}
		
		return unitList;
	}

	// Method to receive a tile from the server.
	private Tile receiveTile() throws IOException
	{
        int x = receiveInt();
        int y = receiveInt();
        Unit unit = null;
        City city = null;
        
		if(receiveBool())
		{
			String owner = receiveString();
			String unitType = receiveString();
			int manpower = receiveInt();
			unit = new Unit(owner, unitType, manpower, receiveUnitList());
		}

		if(receiveBool())
		{
			String owner = receiveString();
			List<Unit> unitList = receiveUnitList();
			String name = receiveString();
			
			city = new City(owner, unitList, name);
		}
		
		return new Tile(x, y, unit, city);
	}
	
	private GameSessionInformation receiveGameSessionInformation() throws IOException
	{
		int size = receiveInt();
		List<Player> playerList = new LinkedList<Player>();
		for(int i = 0; i < size; ++i)
		{
			Player player = new Player(receiveString(), receiveString());
			playerList.add(player);
		}
		
		return new GameSessionInformation(playerList, receiveBool());
	}
	
	private GameBoard receiveGameBoard() throws IOException
	{
		ArrayList<ArrayList<String>> terrainData = receiveStringMatrix();
		List<Tile> tileList = receiveTileList();
		return new GameBoard(terrainData, tileList);
	}
}
