#ifndef SOCKET_H
#define SOCKET_H

#include <string>
#include "serializer.h"
#include "socketstream.h"

class Socket
{
private:
	int m_socket;
	SocketStream m_stream;

public:
	Socket();
	~Socket();

	void close();
	void connect(const std::string &address, unsigned short port);
	unsigned int send(const char *data, unsigned int size);
	unsigned int recv(char *data, unsigned int size);


	template <typename T>
	void send(const T &data)
	{
		Serializer serializer(&m_stream);
		serializer.put(data);
	}

	template <typename T>
	T recv()
	{
		Serializer serializer(&m_stream);
		return serializer.get<T>();
	}

	template <typename T>
	void recv(T &data)
	{
		Serializer serializer(&m_stream);
		serializer.get(data);
	}
};

#endif // SOCKET_H
