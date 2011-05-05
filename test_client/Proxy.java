import java.io.*;
import java.net.*;
import java.util.*;

public class Proxy
{
	private Socket 		m_clientSocket;
	private OutputStream 	m_outStream;
	private final int	protocolVersion = 0;
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



	// Public method for connecting to the server.
	public Result connect(String name)
	{
		Packet toSend = new Packet((byte)2);
		toSend.add(protocolVersion);
                toSend.add(name);
		send(toSend);
		return receiver.getResult();
	}

	// Public method for requesting a list of games.
	public Result listGames()
	{
		send(new Packet((byte)5));
		return receiver.getResult();
	}

	// Public method for requesting to host a game.
	public Result host(Boolean load)
	{
		Packet toSend = new Packet((byte)7);
		toSend.add(load);
		send(toSend);
		return receiver.getResult();
	}

	// public method for requesting to join a game.
	public Result joinGame(String game)
	{
		Packet toSend = new Packet((byte)8);
		toSend.add(game);
		send(toSend);
		return receiver.getResult();
	}

	// Public method for requesting to change civilization when in a game.
	public Result changeCiv(String newCiv)
	{
		Packet toSend = new Packet((byte)11);
		toSend.add(newCiv);
		send(toSend);
		return receiver.getResult();
	}

	// Public method for locking a game, only a host can lock and un-lock a game.
	public void lockGame(boolean lock)
	{
		Packet toSend = new Packet((byte)12);
		toSend.add(lock);
		send(toSend);
	//	return receiver.getResult();
	}

	// Public method for starting a game, only the host can start a game.
	public void startGame()
	{
		send(new Packet((byte)13));
	//	return receiver.getResult();
	}

	// Public method for moving a unit, takes a list of positions
	// , this list always starts with the position the unit starts at.
	// The list is of type Integer and should always be of even number,
	// first the x-value then the y-value.
	public Result moveUnit(List<Integer> positions)
	{
		Packet toSend = new Packet((byte)15);
		toSend.add(positions);
		return receiver.getResult();
	}
	
	// Method to test sending and receiving lists.
	public Result listTest(int x, int y)
	{
		Packet toSend = new Packet((byte)99);
		toSend.add(x);
		toSend.add(y);
		send(toSend);
		return receiver.getResult();
	}
}
