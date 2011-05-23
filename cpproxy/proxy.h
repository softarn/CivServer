
#ifndef PROXY_H
#define PROXY_H

#include "gamelogix.h"
#include "socket.h"
#include "networkmanager.h"

namespace proxy
{

	class Proxy : public GameLogix
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
		protocol::CombatResult combat(int fromX, int fromY, int toX, int toY);
		void endTurn();
		void sendMessage(const std::string &to, const std::string &message);
		void spawnUnit(int x, int y, const std::string &owner, const std::string &unitType, int manpower, const std::vector<protocol::Unit> &containedUnits = std::vector<protocol::Unit>());
		void disbandUnit(int x, int y);
		void buildCity(int x, int y, const std::string &name);
		void enterDragon(int x, int y, int dragonX, int dragonY);
		void removeFromDragon(int dragonX, int dragonY, const std::string &unitType, int manpower);
	};

}


#endif // PROXY_H
