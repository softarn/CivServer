public interface PacketListener{
	
	public void newTurn(Result received);

	public void lobbyUpdated(Result received);

	public void gameStarted(Result received);

	public void chatMessageReceived(Result received);

	public void gameClosed();
}
