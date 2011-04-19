import java.io.*;
import java.net.*;
import java.util.ArrayList;

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
			System.out.println(header);
			if(header == 3)
			{
				return "Welcome to the Real World";
			}
			else if(header == 0)			// Possible fail'd.
			{
				int theFail = m_inStream.read();
				int reqFail = m_inStream.read();
				System.out.println("Fail'd\nWhat fail: " + theFail + "\nRequest that fail'd: " + reqFail + "\n" + receiveString());
				return "Fail'd";
			}
			else if(header == 1)
			{
				return "Confirm'd";
			}
			else if(header == 6)
			{
				ArrayList toReturn = null;
				int size = m_inStream.read();
				System.out.println(size);
				String type = receiveString();
				System.out.println(type);
				if(type.equals("String"))
				{
					toReturn = receiveListString(size);
				}
				else if(type.equals("Integer"))
				{
					toReturn = receiveListInteger(size);
				}
				return toReturn.toString();
			}
		}catch(IOException e)
		{
			e.printStackTrace();
		}	
		return "Fail'd";
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

	// Läser in en lista och returnerar den.
	private ArrayList<Integer> receiveListInteger(int size)
	{
		ArrayList<Integer> toReturn = new ArrayList<Integer>();
		for(int i=0; i<size; i++)
		{
			toReturn.add(receiveInt());
		}
		return toReturn;
	}

	// Läser in en lista och returnerar den.
	private ArrayList<String> receiveListString(int size)
	{
		ArrayList<String> toReturn = new ArrayList<String>();
		for(int i=0; i<size; i++)
		{
			toReturn.add(receiveString());
		}
		return toReturn;
	}

	// Public method for connecting to the server.
	public String connect(String name)
	{
		Packet toSend = new Packet((byte)2);
		toSend.add(protocolVersion);
                toSend.add(name);
		send(toSend);
		return receive();
	}

	public String listTest(ArrayList list)
	{
		Packet toSend = new Packet((byte)13);
		toSend.add(list);
		send(toSend);
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
		testklient1 k = new testklient1("130.237.238.225", 1234);//130.229.128.72", 1234);
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
	}
}
