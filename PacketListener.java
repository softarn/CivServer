package proxy;


import java.util.List;

public interface PacketListener{
	
	public void newTurn(List<Tile> updatedTiles);

	public void lobbyUpdated(GameSessionInformation gameSessionInformation);

	public void gameStarted(GameBoard gameBoard);

	public void chatMessageReceived(ChatMessage chatMessage);

	public void gameClosed();
}
