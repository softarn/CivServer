import java.net.DatagramSocket;
import java.net.DatagramPacket;
import java.io.IOException;

//The class responsible for handling remote input.

class ListenerThread extends Thread {
	
	private DatagramSocket input;
	private Canvas output;
	
	//Constructor
	
	public ListenerThread(Canvas c, int inPort) {
		
		try {
			
			input = new DatagramSocket(inPort);
			
		} catch(IOException ioe) {
			System.out.println("Reception socket failed to open.");
			System.exit(6);
		}
		output = c;
		start();
	}
	
	
	/*
	Listens for data from the remote host and adds points when they 
	are recieved.
	*/
	
	public void run() {
		while(!isInterrupted()) {
			
			byte[] buffer = new byte[8];
			DatagramPacket inPacket = new DatagramPacket(buffer, buffer.length);
			
			try {
				
				input.receive(inPacket);
				
			} catch(IOException ioe) {
				
				System.out.println("An IO error occurred while recieving a packet.");
				System.exit(7);
				
			}
			
			buffer = inPacket.getData();
			
			System.out.println(buffer[0]+" "+buffer[1]+" "+buffer[2]+" "+buffer[3]+" "+buffer[4]+" "+buffer[5]+" "+buffer[6]+" "+buffer[7]);
			
			int x = 										//Fill int x with buffer indices 0 through 3
				((int)buffer[0]<<24)
				+(unfuckByte(buffer[1])<<16)
				+(unfuckByte(buffer[2])<<8)
				+(unfuckByte(buffer[3]));
				
			int y = 										//Fill int y with buffer indices 4 through 7
				((int)buffer[4]<<24)
				+(unfuckByte(buffer[5])<<16)
				+(unfuckByte(buffer[6])<<8)
				+(unfuckByte(buffer[7]));
				
			System.out.println("x: "+x+", y: "+y);
			
			output.addPoint(x, y);
		}
	}
	
	//"Unsigns" the bytes that java in its infinite wisdom has decided to treat as signed, making it impossible to properly cast between int and byte.
	
	private int unfuckByte(byte b) {
			
		if((int)b < 0) {
			
			return ((int)b)+256;
			
		} else {
			
			return (int)b;
			
		}
	}
}