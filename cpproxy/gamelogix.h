
#ifndef GAMELOGIX_H
#define GAMELOGIX_H

#include "protocol.h"

namespace proxy
{

	class GameLogix
	{
	public:
		virtual ~GameLogix() {}


		// Connection functions

		virtual void connect(const std::string &address, unsigned short port) = 0;
		virtual void sayHelloWorld(const std::string &name) = 0;


		// Game joining functions

		virtual std::vector<std::string> listGames() = 0;
		virtual void hostGame() = 0;
		virtual void joinGame(const std::string &gameName) = 0;


		// Game lobby functions

		virtual void changeCivilization(const std::string &newCivilization) = 0;
		virtual void lockGame(bool lock = true) = 0;
		virtual void startGame() = 0;
		virtual void leaveGame() = 0;

		// In-game functions
		virtual void move(const std::vector<protocol::Position> &) = 0;
		virtual void move(int fromX, int fromY, int toX, int toY) = 0;
		virtual protocol::CombatResult combat(int fromX, int fromY, int toX, int toY) = 0;
		virtual void endTurn() = 0;
		virtual void sendMessage(const std::string &to, const std::string &message) = 0;
		virtual void spawnUnit(int x, int y, const std::string &owner, const std::string &unitType, int manpower, const std::vector<protocol::Unit> &containedUnits = std::vector<protocol::Unit>()) = 0;
		virtual void disbandUnit(int x, int y) = 0;
		virtual void buildCity(int x, int y, const std::string &name) = 0;
		virtual void enterDragon(int x, int y, int dragonX, int dragonY) = 0;
		virtual void removeFromDragon(int dragonX, int dragonY, const std::string &unitType, int manpower, int targetX, int targetY) = 0;
	};

}


#endif // GAMELOGIX_H
