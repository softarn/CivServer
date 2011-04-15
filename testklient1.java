import java.io.*;
import java.net.*;
import java.util.Scanner;

public class testklient1
{

	Socket client;
	OutputStream out;
	BufferedReader in;
	Scanner sc;
	public String name;

	private void sendString(String msg)
	{
		byte[] message = new byte[msg.length()];
		for(int i = 0; i < msg.length(); ++i)
			message[i] = (byte)msg.charAt(i);
		
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
}
