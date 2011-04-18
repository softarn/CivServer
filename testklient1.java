import java.io.*;
import java.net.*;
import java.util.Scanner;

public class testklient1
{
	private Socket 		m_clientSocket;
	private OutputStream 	m_outStream;
	private InputStream	m_inStream;
	//BufferedReader in;
	//Scanner sc;
	//public String name;
	
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

	public void send(Packet p)
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
		receive();
	}

	public void receive()		// Waits for an answer from the server and parses what it gets.
	{
		try
		{
			int header = m_inStream.read();		// First the header.
			if(header == 3)
			{
				System.out.println("Welcome to the Real World");
			}
			else if(header == 0)			// Possible fail'd.
			{
				int theFail = m_inStream.read();
				int reqFail = m_inStream.read();
				System.out.println("Fail'd\nWhat fail: " + theFail + "\nRequest that fail'd: " + reqFail + "\n" + receiveString());
			}
		}catch(IOException e)
		{
			e.printStackTrace();
		}	
	}

	public String receiveString() // To receive Strings from the server.
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
/*
	private void sendString(String msg)
	{
		byte[] message = new byte[msg.length() + 1];
		for(int i = 0; i < msg.length(); ++i)
			message[i] = (byte)msg.charAt(i);
		
		// Null termination
		message[msg.length()] = 0;
		
		try
		{
			out.write(message);
			out.flush();
		}
		catch(IOException e)
		{
			System.out.println("Något gick fel när det skulle skickas.");
		}
		
	}
	

	private void sendByte(byte msg)
	{
		try
		{
			out.write(msg);
			out.flush();
		}
		catch(IOException e)
		{
			System.out.println("Något gick fel när det skulle skickas.");
		}
	}

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

	public static void main(String[] args)
	{
		testklient1 tk = new testklient1();
		try
		{
			tk.sc = new Scanner(System.in);
			tk.client = new Socket("localhost", 1233);
			System.out.println("connected");
			tk.out = tk.client.getOutputStream();
			tk.in = new BufferedReader(new InputStreamReader(tk.client.getInputStream()));
		} 
		catch (IOException ioe)
		{
			ioe.printStackTrace();
		}
		
		System.out.println("Name: ");
		tk.name = tk.sc.nextLine();
		while (true)
		{
			System.out.println("Msg att skicka: ");
			//tk.sendInt(tk.sc.nextInt());
			tk.sendByte((byte)tk.sc.nextInt());
		}
	}
*/
	public static void main(String [] args)
	{
		testklient1 k = new testklient1("localhost", 1234);//130.229.128.72", 1234);
		
		Packet test = new Packet((byte)2);
		test.add(0);
		k.send(test);
	}
}
