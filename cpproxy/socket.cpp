
#include "socket.h"
#include <stdexcept>


// Includes for using sockets
#ifdef _WIN32
	// Use winsock if we're using windoze
	#include <winsock2.h>
#else
	// Else we should use berkeley sockets
	#include <sys/socket.h>
	#include <sys/types.h>
	#include <netinet/in.h>
	#include <netdb.h>
#endif

#include <iostream>



bool setupSocketAPI();
void cleanupSocketAPI();
sockaddr_in createSocketAddress(unsigned short port);


int Socket::ms_socketCount = 0;


Socket::Socket()
	: m_stream(*this)
{
	// If this is the first socket to be created, we must first setup the
	// networking library we're using
	if(!ms_socketCount)
		if(!setupSocketAPI())
			throw std::runtime_error("Unable to setup the networking library");


	m_socket = socket(AF_INET, SOCK_STREAM, 0);

	if(m_socket < 0)
		throw std::runtime_error("Unable to open socket");

	// Finally increase socket count if this socket was successfully created
	++ms_socketCount;
}

Socket::Socket(int socketHandle)
	: m_socket(socketHandle),
	  m_stream(*this)
{
	if(m_socket < 0)
		throw std::runtime_error("Invalid socket");

	++ms_socketCount;
}

Socket::~Socket()
{
	#ifdef _WIN32
		closesocket(m_socket);
	#else
		close(m_socket);
	#endif

	// Cleanup the networking library if we're destroying the last socket
	if(!--ms_socketCount)
		cleanupSocketAPI();
}

void Socket::connect(const std::string &address, unsigned short port)
{
	hostent *host = gethostbyname(address.c_str());

	if(!host)
		throw std::runtime_error("Unable to find host " + address);

	sockaddr_in socketAddress = createSocketAddress(port);
	socketAddress.sin_addr.s_addr = *reinterpret_cast<unsigned int*>(host->h_addr);

	if(::connect(m_socket, reinterpret_cast<sockaddr *>(&socketAddress), sizeof(socketAddress)) < 0)
		throw std::runtime_error("Unable to connect");
}

void Socket::listen(unsigned short port)
{
	sockaddr_in socketAddress = createSocketAddress(port);
	socketAddress.sin_addr.s_addr = INADDR_ANY;

	if(::bind(m_socket, reinterpret_cast<sockaddr *>(&socketAddress), sizeof(socketAddress)) < 0)
		throw std::runtime_error("Unable to bind socket");

	// Will not fail
	::listen(m_socket, 5);
}

std::auto_ptr<Socket> Socket::accept()
{
	sockaddr_in socketAddress;
	int len = sizeof(socketAddress);
	int newSocket = ::accept(m_socket, reinterpret_cast<sockaddr *>(&socketAddress), &len);

	if(newSocket < 0)
		throw std::runtime_error("Unable to accept");

	return std::auto_ptr<Socket>(new Socket(newSocket));
}


unsigned int Socket::send(const char *data, unsigned int size)
{
	std::cout << "Sending " << size << " bytes\n";
	int sendSize = ::send(m_socket, data, size, 0);
	if(sendSize < 0)
		throw std::runtime_error("Unable to send");

	return sendSize;
}

unsigned int Socket::recv(char *data, unsigned int size)
{
	int recvSize = ::recv(m_socket, data, size, 0);
	if(recvSize < 0)
		throw std::runtime_error("Unable to receive");

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
