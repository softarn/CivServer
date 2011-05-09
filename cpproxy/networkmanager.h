
#ifndef NETWORKMANAGER_H
#define NETWORKMANAGER_H

#include "thread.h"
#include "mutex.h"
#include "socket.h"
#include "protocol.h"


namespace proxy
{


	class PacketListener
	{
	public:
		virtual ~PacketListener() {}

		virtual void gameSessionUpdate(protocol::GameSessionInformation &) = 0;

		virtual void gameStarted(protocol::StartGameAnswer &) = 0;

		virtual void yourTurn(protocol::ItsYourTurn &) = 0;

		virtual void chatMessageReceived(protocol::ChatMessage &) = 0;

		virtual void gameClosed() = 0;
	};



	class NetworkManager : Thread
	{
	private:
		bool m_running;
		PacketListener &m_packetListener;
		Mutex m_mutex;
		Socket m_socket;

		std::auto_ptr<protocol::Packet> m_currentPacket;

		void enqueuePacket(std::auto_ptr<protocol::Packet> packet);
		void run();

	public:
		NetworkManager(PacketListener &packetListener);
		~NetworkManager();

		void connect(const std::string &address, unsigned short port);
		void stop();
		bool isOpen();


		template <typename T>
		void send(const T &data)
		{
			m_socket.send(data);
		}

		void waitForRequest();
		std::auto_ptr<protocol::Packet> receivePacket();
	};

}

#endif // NETWORKMANAGER_H
