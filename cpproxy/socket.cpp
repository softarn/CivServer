
#include "socket.h"
#include <stdexcept>

#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>

#include <iostream>


Socket::Socket()
	: m_stream(*this)
{
	m_socket = socket(AF_INET, SOCK_STREAM, 0);

	if(m_socket < 0)
		throw std::runtime_error("Unable to open socket");
}

Socket::~Socket()
{
	close();
}

void Socket::close()
{
	if(m_socket >= 0)
	{
		std::cout << "Shutdown: " << ::shutdown(m_socket, 2) << std::endl;
		std::cout << "Close: " << ::close(m_socket) << std::endl;
	}
	m_socket = -1;
}

void Socket::connect(const std::string &address, unsigned short port)
{
	hostent *host = gethostbyname(address.c_str());

	if(!host)
		throw std::runtime_error("Unable to find host " + address);

	sockaddr_in socketAddress;
	socketAddress.sin_family = AF_INET;
	socketAddress.sin_port = htons(port);
	for(int i = 0; i < 8; ++i)
		socketAddress.sin_zero[i] = 0;

	socketAddress.sin_addr.s_addr = *reinterpret_cast<unsigned int*>(host->h_addr);

	if(::connect(m_socket, reinterpret_cast<sockaddr *>(&socketAddress), sizeof(socketAddress)) < 0)
		throw std::runtime_error("Unable to connect");
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
