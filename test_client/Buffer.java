
public class Buffer
{
	private byte[] m_data;
	
	public Buffer()
	{
		m_data = new byte[0];
	}
	
	public void addData(byte[] array, int length)
	{
		byte[] data = new byte[m_data.length + length];
		
		for(int i = 0; i < m_data.length; ++i)
			data[i] = m_data[i];
		
		for(int i = 0; i < length; ++i)
			data[m_data.length + i] = array[i];
		
		m_data = data;
	}

	public void addData(byte b)
	{
		byte[] data = new byte[m_data.length + 1];
		
		for(int i = 0; i < m_data.length; ++i)
			data[i] = m_data[i];
		
		data[m_data.length] = b;
		
		m_data = data;
	}
	
	public byte[] getByteArray()
	{
		return m_data;
	}
}
