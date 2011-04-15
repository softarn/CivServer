import java.io.*;
import java.net.*;
import java.util.Scanner;

public class testklient1{

  Socket client;
  OutputStream out;
  BufferedReader in;
  Scanner sc;

  /*  private void sendStr(String msg){
      out.write(msg);
      out.flush();
  }
  */

  private void sendInt(int header, int y){
    byte[] newMsg = new byte[5];
    newMsg[0] = (byte)(header);
    newMsg[1] = (byte)(y>>>24);
    newMsg[2] = (byte)((y<<8)>>>24);
    newMsg[3] = (byte)((y<<16)>>>24);
    newMsg[4] = (byte)((y<<24)>>>24);
    try{
      out.write(newMsg);
      out.flush();
    }
    catch(IOException e){
      System.out.println("Något gick fel när det skulle skickas.");
    }
  }

  public static void main(String[] args){
    testklient1 tk = new testklient1();
    try{
      tk.sc = new Scanner(System.in);
      tk.client = new Socket("localhost", 1234);
      System.out.println("connected");
      tk.out = tk.client.getOutputStream();
      tk.in = new BufferedReader(new InputStreamReader(tk.client.getInputStream()));
    } 
    catch (IOException ioe) { ioe.printStackTrace(); }
    while (true){
      System.out.println("Msg att skicka: ");
      tk.sendInt(tk.sc.nextInt());
    }
  }
}
