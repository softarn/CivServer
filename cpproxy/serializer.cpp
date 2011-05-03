
#include "serializer.h"
#include "buffer.h"

// Includes for byte order conversion (eg. htonl, ntohl)
#ifdef _WIN32
	#include <winsock2.h>
#else
	#include <arpa/inet.h>
#endif




Serializer::Serializer(ByteStream *byteStream)
	: m_byteStream(byteStream)
{
}


template <> void Serializer::put(const uint8_t &b)
{
	m_byteStream->write(&b, sizeof(b));
}

template <> void Serializer::get(uint8_t &b)
{
	m_byteStream->read(&b, sizeof(b));
}


template <> void Serializer::put(const int8_t &b)
{
	put(uint8_t(b));
}

template <> void Serializer::get(int8_t &b)
{
	get(reinterpret_cast<uint8_t &>(b));
}


template <> void Serializer::put(const bool &b)
{
	put(b ? uint8_t(1) : uint8_t(0));
}

template <> void Serializer::get(bool &b)
{
	b = get<uint8_t>() ? true : false;
}

template <> void Serializer::put(const int &l)
{
	int32_t val = htonl(l);

	m_byteStream->write(reinterpret_cast<const uint8_t *>(&val), sizeof(val));
}

template <> void Serializer::get(int &l)
{
	int32_t val;
	m_byteStream->read(reinterpret_cast<uint8_t *>(&val), sizeof(val));
	l = ntohl(val);
}



template <> void Serializer::put(const std::string &str)
{
	m_byteStream->write(reinterpret_cast<const uint8_t *>(str.c_str()), str.size() + 1);
}

template <> void Serializer::get(std::string &str)
{
	uint8_t c;
	do
	{
		m_byteStream->read(&c, sizeof(c));
		str += c;
	}
	while(c);
}

