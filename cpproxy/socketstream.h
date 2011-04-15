
#ifndef SOCKETSTREAM_H
#define SOCKETSTREAM_H

#include "bytestream.h"

class Socket;

class SocketStream : public ByteStream
{
private:
	Socket &m_socket;
public:
	SocketStream(Socket &socket);

	void write(const uint8_t *data, unsigned int len);
	void read(uint8_t *data, unsigned int len);
};

#endif // SOCKETSTREAM_H
