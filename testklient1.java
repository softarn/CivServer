import java.io.*;
import java.net.*;
import java.util.*;

public class testklient1
{
	private Socket 		m_clientSocket;
	private OutputStream 	m_outStream;
	private InputStream	m_inStream;
	private boolean		turn = false;
	private final int	protocolVersion = 0;
	
	public testklient1(String host, int port)
	{
		try
		{
			m_clientSocket 	= new Socket(host, port);
			m_outStream 	= m_clientSocket.getOutputStream();
			m_inStream	= m_clientSocket.getInputStream();
			
			System.out.println("[+] Connected to server \""+ host +"\" on port "+ port);
		}
		catch(UnknownHostException e)
		{
			e.printStackTrace();
		}
		catch(IOException e)
		{
			e.printStackTrace();
		}
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
	private Result receive()
	{
		Result toReturn = new Result();
		try
		{
			int header = m_inStream.read();		// First the header.

			// Header 3, welcome to the real world.
			if(header == 3)
			{
				toReturn.addOk(true);
				toReturn.addOkMsg("Welcome to the Real World");
			}

			// Header 0, fail'd.
			else if(header == 0)
			{
				toReturn.addOk(false);
				toReturn.addFailNumber(receiveInt());
				toReturn.addRequestFail(m_inStream.read());
				toReturn.addFailMsg(receiveString());
			}

			// Header 1, the confirm'd, but right now we receive a string for testing purposes.
			else if(header == 1)
			{
				toReturn.addRequestOk(m_inStream.read());
				toReturn.addOk(true);
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
			}

			// Header 9, Join answer, returns what game you are connected to.
			else if(header == 9)
			{
				toReturn.addOk(true);
				toReturn.addName(receiveString());
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
			}

			// Header 14, Start game answer, receives the map and hopefully returns it.
			else if(header == 14)
			{
				toReturn.addMap(receiveListList(receiveInt()));
			}

			// Header 16, Tile Update, this one needs more work.
			else if(header == 16)
			{

			}
	
		}catch(IOException e)
		{
			e.printStackTrace();
		}	
		return toReturn;
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
		/*	try
			{
				toReturn.add((int)m_inStream.read());
			}
			catch(IOException e)
			{
				e.printStackTrace();
			}
		*/
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

	// Method to receive the perhaps-type according to the protocol, 
	// should be checked and perhaps fixed before released and tested.
/*	private List receivePerhaps()
	{
		List toReturn = null;
		boolean read = receiveBool();
		if(read)
		{
			String type = receiveString();
			if(type.equals("String"))
			{
				toReturn = new ArrayList<String>();
				toReturn.add(receiveString());
			}
			else if(type.equals("Integer"))
			{
				toReturn = new ArrayList<Integer>();
				toReturn.add(receiveInt());
			}	
			else if(type.equals("List"))
			{
				int size = receiveInt();
				String type2 = receiveString();
				if(type2.equals("String"))
				{
					toReturn = new ArrayList<List>();
					toReturn.add(receiveListString(size));
				}
				else if(type2.equals("Integer"))
				{
					toReturn = new ArrayList<List>();
					toReturn.add(receiveListInteger(size));
				}
			}
		}
		return toReturn;
	}
*/

	// Public method for connecting to the server.
	public Result connect(String name)
	{
		Packet toSend = new Packet((byte)2);
		toSend.add(protocolVersion);
                toSend.add(name);
		send(toSend);
		return receive();
	}
	
	// Method to test sending and receiving lists.
	public Result listTest(ArrayList list)
	{
		Packet toSend = new Packet((byte)13);
		toSend.add(list);
		send(toSend);
		return receive();
	}

	// Method for testing-purposes
	public Result serverTest()
	{
		send(new Packet((byte)1));
		return receive();
	}

/*
	private void sendInt(int msg)
	{
		byte[] newMsg = new byte[4];
		newMsg[0] = (byte)(msg >>> 24);
		newMsg[1] = (byte)((msg << 8) >> 24);
		newMsg[2] = (byte)((msg << 16) >> 24);
		newMsg[3] = (byte)((msg << 24) >> 24);
		try
		{
			out.write(newMsg);
			out.flush();
		}
		catch(IOException e)
		{
			System.out.println("Något gick fel när det skulle skickas.");
		}
	}

*/
	public static void main(String [] args)
	{
		testklient1 k = new testklient1("localhost", 1234);//130.229.128.72", 1234);
		/*
		Packet test = new Packet((byte)2);
		test.add(0);
		test.add("Kalle");
		k.send(test, true);
		*/
		System.out.println(k.connect("Kalle"));
		ArrayList<String> skaMed = new ArrayList<String>();
		skaMed.add("Hej");
		skaMed.add("Magnus");
		System.out.println(k.listTest(skaMed));
		System.out.println(k.serverTest());
	}
}
