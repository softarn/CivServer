import java.io.*;
import java.net.*;

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

	private String receive()		// Waits for an answer from the server and parses what it gets.
	{
		try
		{
			int header = m_inStream.read();		// First the header.
			if(header == 3)
			{
				System.out.println("Welcome to the Real World");
				return "Welcome to the Real World";
			}
			else if(header == 0)			// Possible fail'd.
			{
				int theFail = m_inStream.read();
				int reqFail = m_inStream.read();
				System.out.println("Fail'd\nWhat fail: " + theFail + "\nRequest that fail'd: " + reqFail + "\n" + receiveString());
				return "Fail'd";
			}
		}catch(IOException e)
		{
			e.printStackTrace();
		}	
		return "Fail'd";
	}
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

	private String receiveString() // To receive Strings from the server.
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

	// Public method for connecting to the server.
	public String connect(String name)
	{
		Packet toSend = new Packet((byte)13);
                send(toSend);
		//toSend.add(protocolVersion);
		String integer = receiveString();
                //toSend.add(name);
		//send(toSend);
		//return receive();
                System.out.println(integer);
                return "hej";
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
		testklient1 k = new testklient1("localhost", 1235);//130.229.128.72", 1234);
		/*
		Packet test = new Packet((byte)2);
		test.add(0);
		test.add("Kalle");
		k.send(test, true);
		*/
		System.out.println(k.connect("Kalle"));
	}
}
