import javax.swing.event.MouseInputAdapter;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.net.DatagramPacket;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.io.IOException;

/*
The class responsible for handling local input and sending it to 
the remote host.
*/

class MouseHandler extends MouseInputAdapter {
	
	private Canvas output;
	private DatagramSocket sender;
	private int destPort;
	private InetAddress destination = null;
	
	/*
	Constructor. The validity of the hostname parameter given when 
	starting the program is checked here.
	*/
	
	public MouseHandler(Canvas c, int i, String ia) {
		
		try {
			
			destination = InetAddress.getByName(ia);
			
		} catch(UnknownHostException uhe) {
			
			System.out.println("The provided host parameter is not a valid hostname.");
			System.exit(3);
			
		}
		
		output = c;
		
		try {
			
			sender = new DatagramSocket();
			
		} catch(IOException ioe) {
			
			System.out.println("Sending socket failed to open.");
			System.exit(4);
		}
		destPort = i;
	}
	
	/*
	The actual input handling method. The location of the mouse 
	pointer is translated into two integers that are then fed into a 
	byte array and sent to the remote host. The method also adds a 
	point to the local canvas.
	*/
	
	private void act(MouseEvent me) {
		
		Point p = me.getPoint();
		
		int x = p.x;
		int y = p.y;
		
		System.out.println("x: "+x+", y: "+y);
		
		output.addPoint(x,y);
		
		byte[] buffer = new byte[8];
		
		//Fill buffer with int x
		buffer[0] = (byte)(x>>>24);
		buffer[1] = (byte)((x<<8)>>>24);
		buffer[2] = (byte)((x<<16)>>>24);
		buffer[3] = (byte)((x<<24)>>>24);
		
		//Fill buffer with int y
		buffer[4] = (byte)(y>>>24);
		buffer[5] = (byte)((y<<8)>>>24);
		buffer[6] = (byte)((y<<16)>>>24);
		buffer[7] = (byte)((y<<24)>>>24);
		
		System.out.println(buffer[0]+" "+buffer[1]+" "+buffer[2]+" "+buffer[3]+" "+buffer[4]+" "+buffer[5]+" "+buffer[6]+" "+buffer[7]);
		
		try {
			
			sender.send(new DatagramPacket(buffer, buffer.length, destination, destPort));
			
		} catch(IOException ioe) {
			
			System.out.println("An IO error occurred while sending a packet.");
			System.exit(5);
			
		}
	}
	
	//Event handlers that call the act method upon activation.
	
	public void mousePressed(MouseEvent me) {
		this.act(me);
	}
	
	public void mouseDragged(MouseEvent me) {
		this.act(me);
	}
}