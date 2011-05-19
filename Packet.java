package proxy;

import java.util.List;

class Packet
{
	private Buffer m_buffer;
	
	public Packet(byte header)
	{
		m_buffer = new Buffer();
		m_buffer.addData(header);
	}
	
	// Method to add a string to the buffer
	public void add(String string)
	{
		byte[] message = new byte[string.length() + 1];
		for(int i = 0; i < string.length(); ++i)
			message[i] = (byte)string.charAt(i);
		
		// Null termination
		message[string.length()] = 0;
		
		m_buffer.addData(message, message.length);
	}
	
	// Method to add an integer to the buffer
	public void add(int integer)
	{
		byte[] newMsg = new byte[4];
		newMsg[0] = (byte)(integer >> 24);
		newMsg[1] = (byte)((integer << 8) >> 24);
		newMsg[2] = (byte)((integer << 16) >> 24);
		newMsg[3] = (byte)((integer << 24) >> 24);
		
		m_buffer.addData(newMsg, newMsg.length);
	}
	
	// Method to add a boolean to the buffer
	public void add(boolean bool)
	{
		if(bool)
			m_buffer.addData((byte)1);
		else
			m_buffer.addData((byte)0);
	}
	
	public <T> void add(List<T> list)
	{
		add(list.size());
		for(T t : list)
		{
			if(t instanceof String)
				add((String)t);
			else if(t instanceof Integer)
				add((Integer)t);
			else if(t instanceof Boolean)
				add((Boolean)t);
			else if(t instanceof List)
				add((List)t);
		}
	}
	/*
	public void add(List<String> list)
	{
	}
	
	public void add(List<Integer> list)
	{
	}
	
	public void add(List<List<String>> list)
	{
	}
	*/
	public byte [] getBuffer()
	{
		return m_buffer.getByteArray();
	}
	
	public <T> void addPosList(List<T> list)
	{
		add(list.size()/2);
		for(T t : list)
		{
			if(t instanceof String)
				add((String)t);
			else if(t instanceof Integer)
				add((Integer)t);
			else if(t instanceof Boolean)
				add((Boolean)t);
			else if(t instanceof List)
				add((List)t);
		}
	}
}
