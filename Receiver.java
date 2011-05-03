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
		while(true)
		{
			receive();
		}
	}

	public Result getResult()
	{
		while(packet == null){}
		Result toReturn = packet;
		packet = null;
		return toReturn;
	}

	// Sends the packet.
	private void send(Packet p)
	{
		try
		{
			m_outStream.write(p.getBuffer());
			m_outStream.flush();
		} 
		catch (IOException ioe)
		{
			ioe.printStackTrace();
		}
	}

	// Waits for an answer from the server and parses what it gets.
	// Puts what it gets into an object of the Result class, which is then returned.
	private void receive()
	{
		while(packet != null){}
		Result toReturn = new Result();
		try
		{
			int header = m_inStream.read();		// First the header.

			// Header 3, welcome to the real world.
			if(header == 3)
			{
				toReturn.addOk(true);
				toReturn.addOkMsg("Welcome to the Real World");
				packet = toReturn;
			}

			// Header 0, fail'd.
			else if(header == 0)
			{
				toReturn.addOk(false);
				toReturn.addFailNumber(receiveInt());
				toReturn.addRequestFail(m_inStream.read());
				toReturn.addFailMsg(receiveString());
				packet = toReturn;
			}

			// Header 1, the confirm'd, but right now we receive a string for testing purposes.
			else if(header == 1)
			{
				toReturn.addRequestOk(m_inStream.read());
				toReturn.addOk(true);
				packet = toReturn;
			}
		
			// Header 4, a ping and answers directly with a pong.
			else if(header == 4)
			{
				receiveBool();
				Packet toSend = new Packet((byte)4);
				toSend.add(true);
				send(toSend);
			}	
	
			// Header 6, listGameAnswer, returns a list of games.
			else if(header == 6)
			{
				int size = receiveInt();
				toReturn.addSessions(receiveListString(size));
				pl.newTurn(toReturn);
			}

			// Header 9, Join answer, returns what game you are connected to.
			else if(header == 9)
			{
				toReturn.addOk(true);
				toReturn.addName(receiveString());
				packet = toReturn;
			}

			// Header 10, Game session info, returns all the other players and if the game is locked.
			else if(header == 10)
			{
				toReturn.setupPlayers();
				int size = receiveInt();
				for(int i=0; i<size; i++)
				{
					toReturn.addPlayer(receiveString(), receiveString());
				}
				toReturn.addLocked(receiveBool());

				pl.lobbyUpdated(toReturn);
				
			}

			// Header 14, Start game answer, receives the map and hopefully returns it.
			else if(header == 14)
			{
				toReturn.addMap(receiveListList(receiveInt()));

				pl.gameStarted(toReturn);
			}

			// Header 16, Tile Update, this one needs more work.
			else if(header == 16)
			{
				toReturn.setupTiles();
				int size = receiveInt();
				for(int i=0; i<size; i++)
				{
					receiveTile(toReturn);
				}

				pl.newTurn(toReturn);
			}

			// Test
			else if(header == 99)
			{
				int size = receiveInt();
				toReturn.addSessions(receiveListString(size));
			packet = toReturn;
			}

			// Testing perhaps
			else if(header == 101)
			{
				if(receiveBool())
				{
					System.out.println(receiveString());
				}
			packet = toReturn;
			}
	
		}catch(IOException e)
		{
			e.printStackTrace();
		}	
	}

	// Läser in ett heltal från inStream:en.
	private int receiveInt()
	{
		byte[] toParse = new byte[4];
		try
		{
			m_inStream.read(toParse);
		}catch(IOException e)
		{
			e.printStackTrace();
		}
		int toReturn = ((int)toParse[0]<<24)
				+(makeInt(toParse[1])<<16)
				+(makeInt(toParse[2])<<8)
				+(makeInt(toParse[3]));
		return toReturn;
	}

	// Gör om 4 bytes till ett heltal.
	private int makeInt(byte b)
	{
		if((int)b < 0)
		{
			return ((int)b)+256;
		}
		else
		{
			return (int)b;
		}
	}
	
	// To receive Strings from the server.
	private String receiveString() 
	{
		String toReturn = "";
		for(;;)
		{
			try
			{
				int chara = m_inStream.read();
				if(chara == -1)
				{
					break;
				}
				else if(chara == 0)
				{
					break;
				}
				else
				{
					toReturn+=(char)chara;
				}
			}catch(IOException e)
			{
				e.printStackTrace();
			}
		}
		return toReturn;
	}

	// Läser in en lista av heltal och returnerar den.
	private ArrayList<Integer> receiveListInteger(int size)
	{
		ArrayList<Integer> toReturn = new ArrayList<Integer>();
		for(int i=0; i<size; i++)
		{
			toReturn.add(receiveInt());
		}
		return toReturn;
	}
	
	// Läser in en lista av strängar och returnerar den.
	
	private ArrayList<String> receiveListString(int size)
	{
		ArrayList<String> toReturn = new ArrayList<String>();
		for(int i=0; i<size; i++)
		{
			toReturn.add(receiveString());
		}
		return toReturn;
	}

	// Läser in en lista av listor av strängar, 2D-lista av strängar.
	private ArrayList<ArrayList<String>> receiveListList(int size)
	{
		ArrayList<ArrayList<String>> toReturn = new ArrayList<ArrayList<String>>();
		for(int i=0; i<size; i++)
		{
			int nextSize = receiveInt();
			toReturn.add(receiveListString(nextSize));
		}
		return toReturn;
	}

	// Method for reading a boolean from the inputStream.
	private boolean receiveBool()
	{
		boolean bool = false;
		int boolVal = 0;
		try
		{
			boolVal = m_inStream.read();
		}
		catch(IOException e)
		{
			e.printStackTrace();
		}
		if(boolVal != 0)
		{
			bool = true;
		}
		return bool;
	}

	// Method to receive a tile from the server.
	private void receiveTile(Result toAddTo)
	{
		toAddTo.setUpdatedTile(receiveInt(), receiveInt());
		if(receiveBool())
		{
			toAddTo.setUnit(receiveString(), receiveString(), receiveInt());
		}
		if(receiveBool())
		{
			String owner = receiveString();
			int size = receiveInt();
			for(int i=0; i<size; i++)
			{
				toAddTo.addCityUnit(receiveString(), receiveString(), receiveInt());
			}
			size = receiveInt();
			ArrayList<String> buildings = receiveListString(size);
			toAddTo.setCity(owner, receiveString(), buildings);
		}
		if(receiveBool())
		{
			toAddTo.setImprovement(receiveString());
		}
		toAddTo.addUpdatedTile();
	}
}
