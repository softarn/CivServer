
#ifndef BYTESTREAM_H
#define BYTESTREAM_H

#include "portable.h"

class ByteStream
{
public:
	virtual ~ByteStream() { }

	virtual void write(const uint8_t *data, unsigned int len) = 0;
	virtual void read(uint8_t *data, unsigned int len) = 0;
};


#endif // BYTESTREAM_H
