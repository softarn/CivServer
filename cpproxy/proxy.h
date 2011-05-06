
#ifndef PROXY_H
#define PROXY_H

#include "socket.h"
#include "networkmanager.h"

namespace proxy
{

	class Proxy
	{
	private:
		NetworkManager m_networkManager;
		std::string m_playerName;
		std::string m_currentGame;

	public:
		Proxy(PacketListener &packetListener);
		~Proxy();


		// Connection functions

		void connect(const std::string &address, unsigned short port);
		void sayHelloWorld(const std::string &name);


		// Game joining functions

		std::vector<std::string> listGames();
		void hostGame();
		void joinGame(const std::string &gameName);


		// Game lobby functions

		void changeCivilization(const std::string &newCivilization);
		void lockGame(bool lock = true);
		void startGame();
		void leaveGame();

		// In-game functions
		void move(const std::vector<protocol::Position> &);
		void move(int fromX, int fromY, int toX, int toY);
		void endTurn();
	};

}


#endif // PROXY_H
