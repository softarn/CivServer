package proxy;

import java.io.*;
import java.net.*;
import java.util.*;

public class Proxy implements GameLogix
{
	private Socket 		m_clientSocket;
	private OutputStream 	m_outStream;
	private final int	protocolVersion = 0x03;
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
	public void connect(String name) throws FailedException
	{
		Packet toSend = new Packet((byte)2);
		toSend.add(protocolVersion);
				toSend.add(name);
		send(toSend);
		
		// Will throw on error
		receiver.getResult();
	}

	// Public method for requesting a list of games.
	public List<String> listGames() throws FailedException
	{
		send(new Packet((byte)5));
		return receiver.getResult().getSessions();
	}

	// Public method for requesting to host a game.
	// returns the game that was actually joined
	public String host() throws FailedException
	{
		Packet toSend = new Packet((byte)7);
		send(toSend);
		
		// Will throw on error
		return receiver.getResult().getHostName();
	}

	// public method for requesting to join a game.
	// returns the game that was actually joined
	public String joinGame(String game) throws FailedException
	{
		Packet toSend = new Packet((byte)8);
		toSend.add(game);
		send(toSend);
		
		// Will throw on error
		return receiver.getResult().getHostName();
	}

	// Public method for requesting to change civilization when in a game.
	public void changeCiv(String newCiv) throws FailedException
	{
		Packet toSend = new Packet((byte)11);
		toSend.add(newCiv);
		send(toSend);
		
		// Will throw on error
		receiver.getResult();
	}

	// Public method for locking a game, only a host can lock and un-lock a game.
	public void lockGame(boolean lock) throws FailedException
	{
		Packet toSend = new Packet((byte)12);
		toSend.add(lock);
		send(toSend);
		
		// Will throw on error
		receiver.getResult();
	}

	// Public method for starting a game, only the host can start a game.
	public void startGame() throws FailedException
	{
		send(new Packet((byte)13));
		
		// Will throw on error
		receiver.getResult();
	}

	// Public method for moving a unit, takes a list of positions
	// , this list always starts with the position the unit starts at.
	// The list is of type Integer and should always be of even number,
	// first the x-value then the y-value.
	public void moveUnit(List<Integer> positions) throws FailedException
	{
		Packet toSend = new Packet((byte)15);
		toSend.addPosList(positions);
		send(toSend);
		
		// Will throw on error
		receiver.getResult();
	}

	public void endTurn() throws FailedException
	{
		send(new Packet((byte)16));
		
		// Will throw on error
		receiver.getResult();
	}

	public CombatResult combatRequest(int attX, int attY, int defX, int defY) throws FailedException
	{
		Packet toSend = new Packet((byte)18);
		toSend.add(attX);
		toSend.add(attY);
		toSend.add(defX);
		toSend.add(defY);
		send(toSend);
		
		// Will throw on error
		Result result = receiver.getResult();
		
		return new CombatResult(result.getAttackerLeft(), result.getDefenderLeft());
	}

	public void sendChatMessage(String toWhom, String message) throws FailedException
	{
		Packet toSend = new Packet((byte)20);
		toSend.add(toWhom);
		toSend.add(message);
		send(toSend);
		
		// Will throw on error
		receiver.getResult();
	}

	public void builtCity(int x, int y, String name) throws FailedException
	{
		Packet toSend = new Packet((byte)21);
		toSend.add(x);
		toSend.add(y);
		toSend.add(name);
		send(toSend);
		
		// Will throw on error
		receiver.getResult();
	}

	public void madeUnit(int x, int y, String owner, String type, int manPower) throws FailedException
	{
		Packet toSend = new Packet((byte)23);
		toSend.add(x);
		toSend.add(y);
		toSend.add(owner);
		toSend.add(type);
		toSend.add(manPower);
		send(toSend);
		
		// Will throw on error
		receiver.getResult();
	}
	
	public void disbandUnit(int x, int y) throws FailedException
	{
		Packet toSend = new Packet((byte)29);
		toSend.add(x);
		toSend.add(y);
		send(toSend);
		
		// Will throw on error
		receiver.getResult();
	}
	
	public void enterContainer(int x, int y, int containerX, int containerY) throws FailedException
	{
		Packet toSend = new Packet((byte)26);
		toSend.add(x);
		toSend.add(y);
		toSend.add(containerX);
		toSend.add(containerY);
		send(toSend);
		
		// Will throw on error
		receiver.getResult();
	}
	
	public void removeFromContainer(int containerX, int containerY, String unitType, int manpower) throws FailedException
	{
		Packet toSend = new Packet((byte)28);
		toSend.add(containerX);
		toSend.add(containerY);
		toSend.add(unitType);
		toSend.add(manpower);
		send(toSend);
		
		// Will throw on error
		receiver.getResult();
	}

	public void leaveGame() throws FailedException
	{
		send(new Packet((byte)24));
		
		// Will throw on error
		receiver.getResult();
	}

}
