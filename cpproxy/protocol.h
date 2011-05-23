
#ifndef PROTOCOL_H
#define PROTOCOL_H

#include <string>
#include "portable.h"
#include <memory>
#include "socket.h"

namespace proxy
{

	namespace protocol
	{
		extern const int VERSION;


		struct Header
		{
			enum
			{
				FAILD = 0,
				CONFIRMD = 1,
				HELLO_WORLD = 2,
				WELCOME_TO_THE_REAL_WORLD = 3,
				PING = 4,
				LIST_GAME_REQUEST = 5,
				LIST_GAME_ANSWER = 6,
				HOST_GAME_REQUEST = 7,
				JOIN_GAME_REQUEST = 8,
				JOIN_ANSWER = 9,
				GAME_SESSION_INFORMATION = 10,
				CHANGE_CIVILIZATION_REQUEST = 11,
				LOCK_GAME_REQUEST = 12,
				START_GAME_REQUEST = 13,
				START_GAME_ANSWER = 14,
				MOVE_REQUEST = 15,
				END_TURN = 16,
				ITS_YOUR_TURN = 17,
				COMBAT_REQUEST = 18,
				COMBAT_RESULT = 19,
				MESSAGE_FOR_YOU_SIR = 20,
				BUILD_UNIT = 21,
				SPAWN_UNIT = 23,
				EXIT_GAME = 24,
				GAME_CLOSED = 25,
				ENTER_THE_DRAGON = 26,
				EXIT_THE_DRAGON = 28,
				DISBAND_UNIT = 29
			};
		};





		struct Player
		{
			std::string name;
			std::string civilization;
		};

		struct Position
		{
			int x, y;

			Position(int x = 0, int y = 0);
		};

		struct Unit
		{
			std::string owner;
			std::string unitType;
			int currentManpower;
			std::vector<Unit> containedUnits;
		};

		struct City
		{
			std::string owner;
			std::vector<Unit> units;
			std::string name;
		};

		struct Tile
		{
			Position position;
			Perhaps<Unit> unit;
			Perhaps<City> city;
		};



		struct DestructablePacket
		{
			virtual ~DestructablePacket() {}
		};


		struct Faild : DestructablePacket
		{
			uint8_t faildPacket;
			std::string description;
		};

		struct Confirmd : DestructablePacket
		{
			uint8_t confirmdPacket;
		};

		struct Ping : DestructablePacket
		{
			bool pong;
		};

		struct ListGameAnswer : DestructablePacket
		{
			std::vector<std::string> gameList;
		};

		struct JoinAnswer : DestructablePacket
		{
			std::string hostName;
		};

		struct GameSessionInformation : DestructablePacket
		{
			std::vector<Player> players;
			bool locked;
		};

		struct StartGameAnswer : DestructablePacket
		{
			std::vector< std::vector<std::string> > mapData;
			std::vector<Tile> presetUnits;
		};

		struct ItsYourTurn : DestructablePacket
		{
			std::vector<Tile> updatedTiles;
		};

		struct CombatResult : DestructablePacket
		{
			int attackerRemainingManpower;
			int defenderRemainingManpower;
		};

		struct ChatMessage : DestructablePacket
		{
			std::string sender;
			std::string message;
		};


		struct Packet
		{
			uint8_t header;

			DestructablePacket *destructablePacket;

			union
			{
				Faild *faild;
				Confirmd *confirmd;
				Ping *ping;
				ListGameAnswer *listGameAnswer;
				JoinAnswer *joinAnswer;
				GameSessionInformation *gameSessionInformation;
				StartGameAnswer *startGameAnswer;
				ItsYourTurn *itsYourTurn;
				CombatResult *combatResult;
				ChatMessage *chatMessage;
			};

			Packet(uint8_t header);
			~Packet();

		private:
			Packet(const Packet &);
			Packet &operator=(const Packet &);
		};


		std::auto_ptr<Packet> readPacket(Socket &socket);
	}

}

#endif // PROTOCOL_H
