package proxy;


public class ChatMessage
{
	private String sender, message;
	
	public ChatMessage(String sender, String message)
	{
		this.sender = sender;
		this.message = message;
	}
	
	public String getSender()
	{
		return sender;
	}
	
	public String getMessage()
	{
		return message;
	}
}
