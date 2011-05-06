
#include "socketstream.h"
#include "socket.h"

namespace proxy
{

	SocketStream::SocketStream(Socket &socket)
		: m_socket(socket)
	{
	}


	void SocketStream::write(const uint8_t *data, unsigned int len)
	{
		if(m_socket.send(reinterpret_cast<const char *>(data), len) < len)
			throw SocketError("Could not send with SocketStream");
	}

	void SocketStream::read(uint8_t *data, unsigned int len)
	{
		if(m_socket.recv(reinterpret_cast<char *>(data), len) < len)
			throw SocketError("Could not recv with SocketStream");
	}

}
