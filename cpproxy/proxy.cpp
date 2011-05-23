
#include <stdexcept>
#include <vector>
#include <sstream>
#include "proxy.h"
#include "protocol.h"

#include <iostream>

namespace proxy
{

	void tryPacket(const protocol::Packet &packet, uint8_t header);
	void confirmPacket(const protocol::Packet &packet, uint8_t header);


	Proxy::Proxy(PacketListener &packetListener)
		: m_networkManager(packetListener)
	{
	}

	Proxy::~Proxy()
	{
	}

	void Proxy::connect(const std::string &address, unsigned short port)
	{
		m_networkManager.connect(address, port);
	}



	void Proxy::sayHelloWorld(const std::string &name)
	{
		m_playerName = name;

		// Send a hello world packet
		m_networkManager.send<uint8_t>(protocol::Header::HELLO_WORLD);
		// Send protocol version
		m_networkManager.send<int>(protocol::VERSION);
		// Send player name
		m_networkManager.send(m_playerName);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();


		tryPacket(*packet, protocol::Header::WELCOME_TO_THE_REAL_WORLD);
	}


	std::vector<std::string> Proxy::listGames()
	{
		// Request a game list
		m_networkManager.send<uint8_t>(protocol::Header::LIST_GAME_REQUEST);

		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		tryPacket(*packet, protocol::Header::LIST_GAME_ANSWER);

		return packet->listGameAnswer->gameList;
	}

	void Proxy::hostGame()
	{
		// Request to host a game
		m_networkManager.send<uint8_t>(protocol::Header::HOST_GAME_REQUEST);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a join answer
		tryPacket(*packet, protocol::Header::JOIN_ANSWER);

		// and must be the game we tried to host
		if(packet->joinAnswer->hostName != m_playerName)
			throw std::runtime_error("Something fucked up, tried to host \"" + m_playerName + "\" but actually joined \"" + packet->joinAnswer->hostName + "\"");

		m_currentGame = m_playerName;
	}


	void Proxy::joinGame(const std::string &gameName)
	{
		// Request to join a game
		m_networkManager.send<uint8_t>(protocol::Header::JOIN_GAME_REQUEST);

		// Specify which game to join
		m_networkManager.send(gameName);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a join answer
		tryPacket(*packet, protocol::Header::JOIN_ANSWER);

		// and must be the game we tried to join
		if(packet->joinAnswer->hostName != gameName)
			throw std::runtime_error("Something fucked up, tried to join \"" + gameName + "\" but actually joined \"" + packet->joinAnswer->hostName + "\"");

		m_currentGame = gameName;
	}


	void Proxy::changeCivilization(const std::string &newCivilization)
	{
		// Request to change civ
		m_networkManager.send<uint8_t>(protocol::Header::CHANGE_CIVILIZATION_REQUEST);

		// Send which civ to change to
		m_networkManager.send(newCivilization);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a confirmation
		confirmPacket(*packet, protocol::Header::CHANGE_CIVILIZATION_REQUEST);
	}

	void Proxy::lockGame(bool lock)
	{
		// Request to lock the game
		m_networkManager.send<uint8_t>(protocol::Header::LOCK_GAME_REQUEST);

		// Lock or unlock?
		m_networkManager.send(lock);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a confirmation
		confirmPacket(*packet, protocol::Header::LOCK_GAME_REQUEST);
	}

	void Proxy::startGame()
	{
		// Request to start a game
		m_networkManager.send<uint8_t>(protocol::Header::START_GAME_REQUEST);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a confirmation
		confirmPacket(*packet, protocol::Header::START_GAME_REQUEST);
	}

	void Proxy::leaveGame()
	{
		int h = protocol::Header::EXIT_GAME;
		// Request to leave the game
		m_networkManager.send<uint8_t>(h);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a confirmation
		confirmPacket(*packet, protocol::Header::EXIT_GAME);
	}


	void Proxy::move(const std::vector<protocol::Position> &moveList)
	{
		// Request to move
		m_networkManager.send<uint8_t>(protocol::Header::MOVE_REQUEST);

		// Send the move list
		m_networkManager.send(moveList);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a confirmation
		confirmPacket(*packet, protocol::Header::MOVE_REQUEST);
	}

	void Proxy::move(int fromX, int fromY, int toX, int toY)
	{
		std::vector<protocol::Position> moveList;
		moveList.push_back( protocol::Position(fromX, fromY) );
		moveList.push_back( protocol::Position(toX, toY) );
		move(moveList);
	}

	protocol::CombatResult Proxy::combat(int fromX, int fromY, int toX, int toY)
	{
		// Request to move
		m_networkManager.send<uint8_t>(protocol::Header::COMBAT_REQUEST);

		// Send the attacking unit
		m_networkManager.send(fromX);
		m_networkManager.send(fromY);

		// Send the defending unit
		m_networkManager.send(toX);
		m_networkManager.send(toY);

		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a combat result
		tryPacket(*packet, protocol::Header::COMBAT_RESULT);

		return *packet->combatResult;
	}

	void Proxy::endTurn()
	{
		// Request to end turn
		m_networkManager.send<uint8_t>(protocol::Header::END_TURN);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a confirmation
		confirmPacket(*packet, protocol::Header::END_TURN);
	}

	void Proxy::sendMessage(const std::string &to, const std::string &message)
	{
		// Send a message
		m_networkManager.send<uint8_t>(protocol::Header::MESSAGE_FOR_YOU_SIR);

		// Send to who?
		m_networkManager.send(to);

		// Send the actual message
		m_networkManager.send(message);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a confirmation
		confirmPacket(*packet, protocol::Header::MESSAGE_FOR_YOU_SIR);
	}

	void Proxy::spawnUnit(int x, int y, const std::string &owner, const std::string &unitType, int manpower, const std::vector<protocol::Unit> &containedUnits)
	{
		// Tell the server to spawn a unit
		m_networkManager.send<uint8_t>(protocol::Header::SPAWN_UNIT);

		// Send position
		m_networkManager.send(x);
		m_networkManager.send(y);

		// Send unit
		m_networkManager.send(owner);
		m_networkManager.send(unitType);
		m_networkManager.send(manpower);

		// Send a list of all contained units
		m_networkManager.send(containedUnits);



		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a confirmation
		confirmPacket(*packet, protocol::Header::SPAWN_UNIT);
	}

	void Proxy::disbandUnit(int x, int y)
	{
		// Tell the server to fck a unit
		m_networkManager.send<uint8_t>(protocol::Header::DISBAND_UNIT);

		// Send position
		m_networkManager.send(x);
		m_networkManager.send(y);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a confirmation
		confirmPacket(*packet, protocol::Header::DISBAND_UNIT);
	}

	void Proxy::buildCity(int x, int y, const std::string &name)
	{
		// Tell the server to build
		m_networkManager.send<uint8_t>(protocol::Header::BUILD_UNIT);

		// Send position
		m_networkManager.send(x);
		m_networkManager.send(y);

		// and name
		m_networkManager.send(name);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a confirmation
		confirmPacket(*packet, protocol::Header::BUILD_UNIT);
	}

	void Proxy::enterDragon(int x, int y, int dragonX, int dragonY)
	{
		// Tell the server to enter the Dragon
		m_networkManager.send<uint8_t>(protocol::Header::ENTER_THE_DRAGON);

		// Send position of the unit
		m_networkManager.send(x);
		m_networkManager.send(y);

		// Send position of the dragon
		m_networkManager.send(dragonX);
		m_networkManager.send(dragonY);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a confirmation
		confirmPacket(*packet, protocol::Header::ENTER_THE_DRAGON);
	}

	void Proxy::removeFromDragon(int dragonX, int dragonY, const std::string &unitType, int manpower)
	{
		// Tell the server to exit the Dragon
		m_networkManager.send<uint8_t>(protocol::Header::EXIT_THE_DRAGON);

		// Send position of the dragon
		m_networkManager.send(dragonX);
		m_networkManager.send(dragonY);

		// Tell what unit to remove
		m_networkManager.send(unitType);
		m_networkManager.send(manpower);


		// Receive the answer
		std::auto_ptr<protocol::Packet> packet = m_networkManager.receivePacket();

		// which must be a confirmation
		confirmPacket(*packet, protocol::Header::EXIT_THE_DRAGON);
	}


	void tryPacket(const protocol::Packet &packet, uint8_t header)
	{
		// Just return if the packet matches the demanded header
		if(packet.header == header)
			return;

		// Unexpected header
		if(packet.header != protocol::Header::FAILD)
		{
			std::stringstream sstream;
			sstream << "Unexpected header, required: " << int(header) << ", but actually got: " << int(packet.header);
			throw std::runtime_error(sstream.str());
		}

		throw std::runtime_error(packet.faild->description);
	}

	void confirmPacket(const protocol::Packet &packet, uint8_t header)
	{
		if(packet.header == protocol::Header::CONFIRMD)
		{
			// Just return if the packet matches the demanded header
			if(packet.confirmd->confirmdPacket == header)
				return;

			// If we get here, we've been confirm'd on wrong packet, which isn't what we wanted
			std::stringstream sstream;
			sstream << "Unexpected confirm'd, required: " << int(header) << ", but actually got: " << int(packet.confirmd->confirmdPacket);
			throw std::runtime_error(sstream.str());
		}

		// Unexpected header
		if(packet.header != protocol::Header::FAILD)
		{
			std::stringstream sstream;
			sstream << "Unexpected header, required: " << int(header) << ", but actually got: " << int(packet.header);
			throw std::runtime_error(sstream.str());
		}

		// Unexpected failure
		if(packet.faild->faildPacket != header)
		{
			std::stringstream sstream;
			sstream << "Unexpected failure, required: " << int(header) << ", but actually got: " << int(packet.faild->faildPacket);
			throw std::runtime_error(sstream.str());
		}

		throw std::runtime_error(packet.faild->description);
	}

}
