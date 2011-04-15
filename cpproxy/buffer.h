#ifndef BUFFER_H
#define BUFFER_H

#include "bytestream.h"

#include <vector>


class Buffer : public ByteStream
{
private:
	std::vector<uint8_t> m_data;
	unsigned int m_current;

public:
	Buffer();
	Buffer(const uint8_t *data, unsigned int len);

	void write(const uint8_t *data, unsigned int len);
	void read(uint8_t *data, unsigned int len);
	void peek(uint8_t *data, unsigned int len) const;
	unsigned int getSize() const;
};


#endif // BUFFER_H
