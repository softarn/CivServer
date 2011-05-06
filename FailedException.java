public class FailedException extends Exception{
	int header;
	String message;

	public FailedException(int header, String message){
		this.header = header;
		this.message = message;
	}

	public String getMessage(){
		return message;
	}

	public int getWhatFailed(){
		return header;
	}

	public String toString(){
		return "The header that failed was " + header + ".\n" + message;
	}
}
