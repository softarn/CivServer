#ifndef SOCKET_H
#define SOCKET_H

#include <string>
#include <exception>
#include "serializer.h"
#include "socketstream.h"

namespace proxy
{


	class SocketError : public std::exception
	{
	private:
		std::string m_what;
	public:
		SocketError(const std::string &what) : m_what(what) {}
		~SocketError() throw() {}

		const char *what() const throw() { return m_what.c_str(); }
	};


	class Socket
	{
	private:
		static int sm_socketCount;
		int m_socket;
		SocketStream m_stream;

		// Disable copying
		Socket(Socket &);
		Socket &operator=(Socket &);

		// Private constructor for creating a Socket object from a handle
		Socket(int socketHandle);

	public:
		Socket();
		~Socket();

		void close();
		void connect(const std::string &address, unsigned short port);
		void listen(unsigned short port);
		std::auto_ptr<Socket> accept();

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

}

#endif // SOCKET_H
