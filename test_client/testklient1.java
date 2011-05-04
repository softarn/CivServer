import java.io.*;
import java.net.*;
import java.util.*;

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

    // Waits for an answer from the server and parses what it gets.
    // Puts what it gets into an object of the Result class, which is then returned.
    private Result receive()
    {
        Result toReturn = new Result();
        try
        {
            int header = m_inStream.read();		// First the header.

            // Header 3, welcome to the real world.
            if(header == 3)
            {
                toReturn.addOk(true);
                toReturn.addOkMsg("Welcome to the Real World");
            }

            // Header 0, fail'd.
            else if(header == 0)
            {
                toReturn.addOk(false);
                toReturn.addFailNumber(receiveInt());
                toReturn.addRequestFail(m_inStream.read());
                toReturn.addFailMsg(receiveString());
            }

            // Header 1, the confirm'd, but right now we receive a string for testing purposes.
            else if(header == 1)
            {
                toReturn.addRequestOk(m_inStream.read());
                toReturn.addOk(true);
            }

            // Header 4, a ping and answers directly with a pong.
            else if(header == 4)
            {
                receiveBool();
                Packet toSend = new Packet((byte)4);
                toSend.add(true);
                send(toSend);
            }	

            // Header 6, listGameAnswer, returns a list of games.
            else if(header == 6)
            {
                int size = receiveInt();
                toReturn.addSessions(receiveListString(size));
            }

            // Header 9, Join answer, returns what game you are connected to.
            else if(header == 9)
            {
                toReturn.addOk(true);
                toReturn.addName(receiveString());
            }

            // Header 10, Game session info, returns all the other players and if the game is locked.
            else if(header == 10)
            {
                toReturn.setupPlayers();
                int size = receiveInt();
                for(int i=0; i<size; i++)
                {
                    toReturn.addPlayer(receiveString(), receiveString());
                }
                toReturn.addLocked(receiveBool());
            }

            // Header 14, Start game answer, receives the map and hopefully returns it.
            else if(header == 14)
            {
                toReturn.addMap(receiveListList(receiveInt()));
            }

            // Header 16, Tile Update, this one needs more work.
            else if(header == 16)
            {
                toReturn.setupTiles();
                int size = receiveInt();
                for(int i=0; i<size; i++)
                {
                    receiveTile(toReturn);
                }
            }

            // Test
            else if(header == 99)
            {
                int size = receiveInt();
                toReturn.addSessions(receiveListString(size));
            }

            // Testing perhaps
            else if(header == 101)
            {
                if(receiveBool())
                {
                    System.out.println(receiveString());
                }
            }

        }catch(IOException e)
        {
            e.printStackTrace();
        }	
        return toReturn;
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

    // Läser in en lista av heltal och returnerar den.
    private ArrayList<Integer> receiveListInteger(int size)
    {
        ArrayList<Integer> toReturn = new ArrayList<Integer>();
        for(int i=0; i<size; i++)
        {
            /*	try
                {
                toReturn.add((int)m_inStream.read());
                }
                catch(IOException e)
                {
                e.printStackTrace();
                }
                */
            toReturn.add(receiveInt());
        }
        return toReturn;
    }

    // Läser in en lista av strängar och returnerar den.
    private ArrayList<String> receiveListString(int size)
    {
        ArrayList<String> toReturn = new ArrayList<String>();
        for(int i=0; i<size; i++)
        {
            toReturn.add(receiveString());
        }
        return toReturn;
    }

    // Läser in en lista av listor av strängar, 2D-lista av strängar.
    private ArrayList<ArrayList<String>> receiveListList(int size)
    {
        ArrayList<ArrayList<String>> toReturn = new ArrayList<ArrayList<String>>();
        for(int i=0; i<size; i++)
        {
            int nextSize = receiveInt();
            toReturn.add(receiveListString(nextSize));
        }
        return toReturn;
    }

    // Method for reading a boolean from the inputStream.
    private boolean receiveBool()
    {
        boolean bool = false;
        int boolVal = 0;
        try
        {
            boolVal = m_inStream.read();
        }
        catch(IOException e)
        {
            e.printStackTrace();
        }
        if(boolVal != 0)
        {
            bool = true;
        }
        return bool;
    }

    // Method to receive a tile from the server.
    private void receiveTile(Result toAddTo)
    {
        toAddTo.setUpdatedTile(receiveInt(), receiveInt());
        if(receiveBool())
        {
            toAddTo.setUnit(receiveString(), receiveString(), receiveInt());
        }
        if(receiveBool())
        {
            String owner = receiveString();
            int size = receiveInt();
            for(int i=0; i<size; i++)
            {
                toAddTo.addCityUnit(receiveString(), receiveString(), receiveInt());
            }
            size = receiveInt();
            ArrayList<String> buildings = receiveListString(size);
            toAddTo.setCity(owner, receiveString(), buildings);
        }
        if(receiveBool())
        {
            toAddTo.setImprovement(receiveString());
        }
        toAddTo.addUpdatedTile();
    }

    // Public method for connecting to the server.
    public Result connect(String name)
    {
        Packet toSend = new Packet((byte)2);
        toSend.add(protocolVersion);
        toSend.add(name);
        send(toSend);
        return receive();
    }

    // Public method for requesting a list of games.
    public Result listGames()
    {
        send(new Packet((byte)5));
        return receive();
    }

    // Public method for requesting to host a game.
    public Result host(Boolean load)
    {
        Packet toSend = new Packet((byte)7);
        toSend.add(load);
        send(toSend);
        return receive();
    }

    // public method for requesting to join a game.
    public Result joinGame(String game)
    {
        Packet toSend = new Packet((byte)8);
        toSend.add(game);
        send(toSend);
        return receive();
    }

    // Public method for requesting to change civilization when in a game.
    public Result changeCiv(String newCiv)
    {
        Packet toSend = new Packet((byte)11);
        toSend.add(newCiv);
        send(toSend);
        return receive();
    }

    // Public method for locking a game, only a host can lock and un-lock a game.
    public Result lockGame(boolean lock)
    {
        Packet toSend = new Packet((byte)12);
        toSend.add(lock);
        send(toSend);
        return receive();
    }

    // Public method for starting a game, only the host can start a game.
    public Result startGame()
    {
        send(new Packet((byte)13));
        return receive();
    }

    // Public method for moving a unit, takes a list of positions
    // , this list always starts with the position the unit starts at.
    // The list is of type Integer and should always be of even number,
    // first the x-value then the y-value.
    public Result moveUnit(List<Integer> positions)
    {
        Packet toSend = new Packet((byte)15);
        toSend.add(positions);
        return receive();
    }

    // Method to test sending and receiving lists.
    public Result listTest(int x, int y)
    {
        Packet toSend = new Packet((byte)99);
        toSend.add(x);
        toSend.add(y);
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
        testklient1 k = new testklient1("localhost", 1232);//130.229.128.72", 1234);
        Packet login = new Packet((byte)2);
        login.add(0);
        login.add("Kalle");
        k.send(login);

        Packet list = new Packet((byte)5);
        //        for(int i=0; i <100; i++)
        k.send(list);

        Packet host = new Packet((byte)7);
        host.add(false);
        k.send(host);

        /*
           try{
           Thread.sleep(20000);
           }catch(Throwable t){}*/

        /*	ArrayList<Integer> skaMed = new ArrayList<Integer>();
                skaMed.add(1);
                skaMed.add(3);
                skaMed.add(1);
                skaMed.add(4);
                skaMed.add(2);
                skaMed.add(5);
                skaMed.add(3);
                skaMed.add(5);
                System.out.println(k.listTest(14,13)); */
    }
}
