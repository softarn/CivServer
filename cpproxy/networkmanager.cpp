
#include "networkmanager.h"
#include <stdexcept>

#include <iostream>

namespace proxy
{


	NetworkManager::NetworkManager(PacketListener &packetListener)
		: m_running(false),
		  m_packetListener(packetListener)
	{
	}

	NetworkManager::~NetworkManager()
	{
		stop();
		join();
	}

	void NetworkManager::enqueuePacket(std::auto_ptr<protocol::Packet> packet)
	{
		bool currentPacketIsLocked = true;
		while(currentPacketIsLocked)
		{
			Mutex::Locker l(m_mutex);
			if(!m_currentPacket.get())
			{
				m_currentPacket = packet;
				currentPacketIsLocked = false;
			}
		}
	}

	void NetworkManager::run()
	{
		m_running = true;
		while(m_running)
		{
			try
			{

				std::auto_ptr<protocol::Packet> packet = protocol::readPacket(m_socket);

				switch(packet->header)
				{
				case protocol::Header::PING:
					if(packet->ping->pong == false)
					{
						send<uint8_t>(protocol::Header::PING);
						send<bool>(true);
					}
					break;

				case protocol::Header::GAME_SESSION_INFORMATION:
					m_packetListener.gameSessionUpdate(*packet->gameSessionInformation);
					break;

				case protocol::Header::START_GAME_ANSWER:
					m_packetListener.gameStarted(*packet->startGameAnswer);
					break;

				case protocol::Header::ITS_YOUR_TURN:
					m_packetListener.yourTurn(*packet->itsYourTurn);
					break;

				case protocol::Header::GAME_CLOSED:
					m_packetListener.gameClosed();
					break;

				default:
					enqueuePacket(packet);
				}

			}
			catch(SocketError &e)
			{
				std::cout << "Connection closed\n";
				m_running = false;
			}


		}
	}

	void NetworkManager::connect(const std::string &address, unsigned short port)
	{
		m_socket.connect(address, port);
		start();
	}

	void NetworkManager::stop()
	{
		m_socket.close();
		m_running = false;
	}

	bool NetworkManager::isOpen()
	{
		return isRunning();
	}

	std::auto_ptr<protocol::Packet> NetworkManager::receivePacket()
	{
		while(true)
		{
			Mutex::Locker l(m_mutex);
			if(!isOpen())
				throw std::runtime_error("Unable to receive packet: No connection");

			if(m_currentPacket.get())
				return m_currentPacket;
		}
	}

}
