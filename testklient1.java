import java.io.*;
import java.net.*;
import java.util.Scanner;

public class testklient1{

  Socket client;
  PrintWriter out;
  BufferedReader in;
  Scanner sc;
  public String name;

  private void sendStr(String msg){
      out.write(name + " says: " + msg);
      out.flush();
  }

  private void sendInt(int msg){
    out.write(msg);
    out.flush();
  }

  public static void main(String[] args){
      testklient1 tk = new testklient1();
    try{
      tk.sc = new Scanner(System.in);
      tk.client = new Socket("localhost", 4711);
      System.out.println("connected");
      tk.out = new PrintWriter(tk.client.getOutputStream());
      tk.in = new BufferedReader(new InputStreamReader(tk.client.getInputStream()));
    } 
    catch (IOException ioe) { ioe.printStackTrace(); }
    System.out.println("Name: ");
    tk.name = tk.sc.nextLine();
    while (true){
      System.out.println("Msg att skicka: ");
      tk.sendStr(tk.sc.nextLine());
    }
  }
}
