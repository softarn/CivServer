import java.io.*;
import java.net.*;
import java.util.*;

public class Proxy
{
	private Socket 		m_clientSocket;
	private OutputStream 	m_outStream;
	private final int	protocolVersion = 2;
	private Receiver	receiver;
	
	public Proxy(String host, int port, PacketListener pl)
	{
		try
		{
			m_clientSocket 	= new Socket(host, port);
			m_outStream 	= m_clientSocket.getOutputStream();

			receiver = new Receiver(pl, m_clientSocket.getInputStream(), m_outStream);
			Thread recvThread = new Thread(receiver);

			recvThread.start();
			
		//	System.out.println("[+] Connected to server \""+ host +"\" on port "+ port);
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

	// Public method for connecting to the server.
	public Result connect(String name) throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)2);
			toSend.add(protocolVersion);
                	toSend.add(name);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	// Public method for requesting a list of games.
	public Result listGames() throws FailedException
	{
		try
		{
			send(new Packet((byte)5));
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	// Public method for requesting to host a game.
	public Result host() throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)7);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	// public method for requesting to join a game.
	public Result joinGame(String game) throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)8);
			toSend.add(game);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	// Public method for requesting to change civilization when in a game.
	public Result changeCiv(String newCiv) throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)11);
			toSend.add(newCiv);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe){
			throw fe;
		}
	}

	// Public method for locking a game, only a host can lock and un-lock a game.
	public Result lockGame(boolean lock) throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)12);
			toSend.add(lock);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	// Public method for starting a game, only the host can start a game.
	public Result startGame() throws FailedException
	{
		try
		{
			send(new Packet((byte)13));
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	// Public method for moving a unit, takes a list of positions
	// , this list always starts with the position the unit starts at.
	// The list is of type Integer and should always be of even number,
	// first the x-value then the y-value.
	public Result moveUnit(List<Integer> positions) throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)15);
			toSend.addPosList(positions);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	public Result endTurn() throws FailedException
	{
		try
		{
			send(new Packet((byte)16));
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	public Result combatRequest(int attX, int attY, int defX, int defY) throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)18);
			toSend.add(attX);
			toSend.add(attY);
			toSend.add(defX);
			toSend.add(defY);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	public Result sendChatMessage(String toWhom, String message) throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)20);
			toSend.add(toWhom);
			toSend.add(message);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	public Result builtCity(int x, int y, String name) throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)21);
			toSend.add(x);
			toSend.add(y);
			toSend.add(name);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	public Result builtImprovement(int x, int y, String improvement) throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)22);
			toSend.add(x);
			toSend.add(y);
			toSend.add(improvement);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	public Result madeUnit(int x, int y, String owner, String type, int manPower) throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)23);
			toSend.add(x);
			toSend.add(y);
			toSend.add(owner);
			toSend.add(type);
			toSend.add(manPower);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	public Result leaveGame() throws FailedException
	{
		try
		{
			send(new Packet((byte)24));
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	// Method to test sending and receiving lists.
	private Result listTest(int x, int y) throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)99);
			toSend.add(x);
			toSend.add(y);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	public static void main(String [] args)
	{
	//	Proxy k = new Proxy("130.237.238.239", 1234);//130.229.128.72", 1234);
		/*
		Packet test = new Packet((byte)2);
		test.add(0);
		test.add("Kalle");
		k.send(test, true);
		*/
	/*	ArrayList<Integer> skaMed = new ArrayList<Integer>();
		skaMed.add(1);
		skaMed.add(3);
		skaMed.add(1);
		skaMed.add(4);
		skaMed.add(2);
		skaMed.add(5);
		skaMed.add(3);
		skaMed.add(5);
	*///	System.out.println(k.listTest(14,13));
	}
}
