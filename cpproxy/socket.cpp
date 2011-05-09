
#include "socket.h"


// Includes for using sockets
#ifdef _WIN32
	// Use winsock if we're using windoze
	#include <winsock2.h>
	#define ECONNREFUSED WSAECONNREFUSED
	#define ENOTCONN WSAENOTCONN
	#define ENOTSOCK WSAENOTSOCK
	#ifdef _MSC_VER
		typedef int socklen_t;
	#endif
#else
	// Else we should use berkeley sockets
	#include <sys/socket.h>
	#include <sys/types.h>
	#include <netinet/in.h>
	#include <netdb.h>
#endif

#include <iostream>

#include <cerrno>

namespace proxy
{

	bool setupSocketAPI();
	void cleanupSocketAPI();
	sockaddr_in createSocketAddress(unsigned short port);


	int Socket::sm_socketCount = 0;


	Socket::Socket()
		: m_stream(*this)
	{
		// If this is the first socket to be created, we must first setup the
		// networking library we're using
		if(!sm_socketCount)
			if(!setupSocketAPI())
				throw SocketError("Unable to setup the networking library");


		m_socket = socket(AF_INET, SOCK_STREAM, 0);

		if(m_socket < 0)
			throw SocketError("Unable to open socket");

		// Finally increase socket count if this socket was successfully created
		++sm_socketCount;
	}

	Socket::Socket(int socketHandle)
		: m_socket(socketHandle),
		  m_stream(*this)
	{
		if(m_socket < 0)
			throw SocketError("Invalid socket");

		++sm_socketCount;
	}

	Socket::~Socket()
	{
		close();

		// Cleanup the networking library if we're destroying the last socket
		if(!--sm_socketCount)
			cleanupSocketAPI();
	}

	void Socket::close()
	{
		if(m_socket < 0)
			return;

	#ifdef _WIN32
		::closesocket(m_socket);
	#else
		::shutdown(m_socket, 2);
		::close(m_socket);
	#endif

		m_socket = -1;
	}

	void Socket::connect(const std::string &address, unsigned short port)
	{
		hostent *host = gethostbyname(address.c_str());

		if(!host)
			throw SocketError("Unable to find host " + address);

		sockaddr_in socketAddress = createSocketAddress(port);
		socketAddress.sin_addr.s_addr = *reinterpret_cast<unsigned int*>(host->h_addr);

		if(::connect(m_socket, reinterpret_cast<sockaddr *>(&socketAddress), sizeof(socketAddress)) < 0)
			throw SocketError("Unable to connect");
	}

	void Socket::listen(unsigned short port)
	{
		sockaddr_in socketAddress = createSocketAddress(port);
		socketAddress.sin_addr.s_addr = INADDR_ANY;

		if(::bind(m_socket, reinterpret_cast<sockaddr *>(&socketAddress), sizeof(socketAddress)) < 0)
			throw SocketError("Unable to bind socket");

		// Will not fail
		::listen(m_socket, 5);
	}

	std::auto_ptr<Socket> Socket::accept()
	{
		sockaddr_in socketAddress;
		socklen_t len = sizeof(socketAddress);
		int newSocket = ::accept(m_socket, reinterpret_cast<sockaddr *>(&socketAddress), &len);

		if(newSocket < 0)
			throw SocketError("Unable to accept");

		return std::auto_ptr<Socket>(new Socket(newSocket));
	}


	unsigned int Socket::send(const char *data, unsigned int size)
	{
		std::cout << "Sending " << size << " bytes\n";
		int sendSize = ::send(m_socket, data, size, 0);
		if(sendSize < 0)
			throw SocketError("Unable to send");

		return sendSize;
	}

	unsigned int Socket::recv(char *data, unsigned int size)
	{
		int recvSize = ::recv(m_socket, data, size, 0);

		if(recvSize < 0)
		{
			std::cout << "Errno: " << errno << std::endl;

			std::string error;
			switch(errno)
			{
			case EAGAIN:
				error = "EAGAIN/EWOULDBLOCK";
				break;

			case EBADF:
				error = "EBADF";
				break;

			case ECONNREFUSED:
				error = "ECONNREFUSED";
				break;

			case EFAULT:
				error = "EFAULT";
				break;

			case EINTR:
				error = "EINTR";
				break;

			case EINVAL:
				error = "EINVAL";
				break;

			case ENOMEM:
				error = "ENOMEM";
				break;

			case ENOTCONN:
				error = "ENOTCONN";
				break;

			case ENOTSOCK:
				error = "ENOTSOCK";
				break;

			default:
				error = "Unknown error";
			}

			throw SocketError("Unable to receive: " + error);
		}

		return recvSize;
	}



	bool setupSocketAPI()
	{
		#ifdef _WIN32
			WSADATA data;
			return WSAStartup(MAKEWORD(2, 2), &data) == 0;
		#else
			return true;
		#endif
	}

	void cleanupSocketAPI()
	{
		#ifdef _WIN32
			WSACleanup();
		#endif
	}

	sockaddr_in createSocketAddress(unsigned short port)
	{
		sockaddr_in socketAddress;

		socketAddress.sin_family = AF_INET;
		socketAddress.sin_port = htons(port);
		for(int i = 0; i < 8; ++i)
			socketAddress.sin_zero[i] = 0;

		return socketAddress;
	}

}
