
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
		int retval = m_socket.recv(reinterpret_cast<char *>(data), len);
		while(retval < len)
		{
			int extra = m_socket.recv(reinterpret_cast<char *>(data+retval), len-retval);
			if(extra == 0)
				throw SocketError("Remote got pissed");
			retval += extra;
		}

	}

}
