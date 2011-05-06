
#include "protocol.h"
#include "serializer.h"


namespace proxy
{


	namespace protocol
	{
		Position::Position(int x, int y)
			: x(x), y(y)
		{
		}

		Packet::Packet(uint8_t header)
			: header(header),
			  destructablePacket(NULL)
		{
		}

		Packet::~Packet()
		{
			delete destructablePacket;
		}

		template <typename T>
		T *receiveNewPacket(Socket &socket)
		{
			return new T(socket.recv<T>());
		}

		std::auto_ptr<Packet> readPacket(Socket &socket)
		{
			uint8_t header = socket.recv<uint8_t>();

			std::auto_ptr<Packet> packet(new Packet(header));

			switch(packet->header)
			{
			case Header::FAILD:
				packet->destructablePacket = packet->faild = receiveNewPacket<Faild>(socket);
				break;

			case Header::CONFIRMD:
				packet->destructablePacket = packet->confirmd = receiveNewPacket<Confirmd>(socket);
				break;

			case Header::PING:
				packet->destructablePacket = packet->ping = receiveNewPacket<Ping>(socket);
				break;

			case Header::LIST_GAME_ANSWER:
				packet->destructablePacket = packet->listGameAnswer = receiveNewPacket<ListGameAnswer>(socket);
				break;

			case Header::JOIN_ANSWER:
				packet->destructablePacket = packet->joinAnswer = receiveNewPacket<JoinAnswer>(socket);
				break;

			case Header::GAME_SESSION_INFORMATION:
				packet->destructablePacket = packet->gameSessionInformation = receiveNewPacket<GameSessionInformation>(socket);
				break;

			case Header::START_GAME_ANSWER:
				packet->destructablePacket = packet->startGameAnswer = receiveNewPacket<StartGameAnswer>(socket);
				break;

			case Header::ITS_YOUR_TURN:
				packet->destructablePacket = packet->itsYourTurn = receiveNewPacket<ItsYourTurn>(socket);
				break;
			}

			return packet;
		}
	}



	template <> void Serializer::get(protocol::Player &p)
	{
		get(p.name);
		get(p.civilization);
	}

	template <> void Serializer::put(const protocol::Position &p)
	{
		put(p.x);
		put(p.y);
	}

	template <> void Serializer::get(protocol::Position &p)
	{
		get(p.x);
		get(p.y);
	}

	template <> void Serializer::get(protocol::Unit &u)
	{
		get(u.owner);
		get(u.unitType);
		get(u.currentManpower);
	}

	template <> void Serializer::get(protocol::City &c)
	{
		get(c.owner);
		get(c.units);
		get(c.buildings);
		get(c.name);
	}

	template <> void Serializer::get(protocol::Tile &t)
	{
		get(t.position);
		get(t.unit);
		get(t.city);
		get(t.improvement);
	}

	template <> void Serializer::get(protocol::Faild &f)
	{
		get(f.faildPacket);
		get(f.description);
	}

	template <> void Serializer::get(protocol::Confirmd &c)
	{
		get(c.confirmdPacket);
	}


	template <> void Serializer::get(protocol::Ping &p)
	{
		get(p.pong);
	}


	template <> void Serializer::get(protocol::ListGameAnswer &lga)
	{
		get(lga.gameList);
	}

	template <> void Serializer::get(protocol::JoinAnswer &ja)
	{
		get(ja.hostName);
	}


	template <> void Serializer::get(protocol::GameSessionInformation &gsi)
	{
		get(gsi.players);
		get(gsi.locked);
	}


	template <> void Serializer::get(protocol::StartGameAnswer &sga)
	{
		get(sga.mapData);
		get(sga.resetUnits);
	}

	template <> void Serializer::get(protocol::ItsYourTurn &iyt)
	{
		get(iyt.updatedTiles);
	}


}
