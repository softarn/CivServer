import java.io.*;
import java.net.*;
import java.util.*;

public class Proxy
{
	private Socket 		m_clientSocket;
	private OutputStream 	m_outStream;
	private final int	protocolVersion = 3;
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
			Packet toSend = new Packet((byte)13);
			toSend.add(20);
			toSend.add(20);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
	}

	public Result startGame(int width, int height) throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)13);
			if(width < 20)
			{
				toSend.add(20);
			}
			else
			{
				toSend.add(width);
			}
			if(height < 20)
			{
				toSend.add(20);
			}
			else
			{
				toSend.add(height);
			}
			send(toSend);
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

	public Result madeUnit(int x, int y, String owner, String type,
           int manPower) throws FailedException
	{
		try
		{
			Packet toSend = new Packet((byte)23);
			toSend.add(x);
			toSend.add(y);
			toSend.add(owner);
			toSend.add(type);
			toSend.add(manPower);
            toSend.add(new ArrayList());
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

    public Result insertUnit(int fromX, int fromY, int toX, int toY) throws FailedException
    {
		try
		{
			Packet toSend = new Packet((byte)26);
			toSend.add(fromX);
			toSend.add(fromY);
			toSend.add(toX);
			toSend.add(toY);
			send(toSend);
			return receiver.getResult();
		}
		catch(FailedException fe)
		{
			throw fe;
		}
    }

    public Result moveOutUnit(int fromX, int fromY, String type, int mp,
           int toX, int toY) throws FailedException
    {
        try
        {
		Packet toSend = new Packet((byte)28);
		toSend.add(fromX);
		toSend.add(fromY);
        toSend.add(type);
        toSend.add(mp);
		toSend.add(toX);
		toSend.add(toY);
		send(toSend);
		return receiver.getResult();
        }
	catch(FailedException fe)
	{
		throw fe;
	}
    }

    public Result disbandUnit(int px, int py) throws FailedException
    {
	    try
	    {
		    Packet toSend = new Packet((byte)29);
		    toSend.add(px);
		    toSend.add(py);
		    send(toSend);
		    return receiver.getResult();
	    }
	    catch(FailedException fe)
	    {
		    throw fe;
	    }
    }

    public Result fortify(int px, int py) throws FailedException
    {
	    try
	    {
		    Packet toSend = new Packet((byte)31);
		    toSend.add(px);
		    toSend.add(py);
		    send(toSend);
		    return receiver.getResult();
	    }
	    catch(FailedException fe)
	    {
		    throw fe;
	    }
    }

    public Result unFortify(int px, int py) throws FailedException
    {
	    try
	    {
		    Packet toSend = new Packet((byte)32);
		    toSend.add(px);
		    toSend.add(py);
		    send(toSend);
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

}
