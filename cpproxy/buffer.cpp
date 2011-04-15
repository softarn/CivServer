
#include "buffer.h"

Buffer::Buffer() : m_current(0)
{
}

Buffer::Buffer(const uint8_t *data, unsigned int len) : m_current(0)
{
	write(data, len);
}

void Buffer::write(const uint8_t *data, unsigned int len)
{
	unsigned int oldSize = m_data.size();
	m_data.resize(oldSize + len);

	for(unsigned int i = 0; i < len; ++i)
		m_data[oldSize + i] = data[i];
}

void Buffer::read(uint8_t *data, unsigned int len)
{
	for(unsigned int i = 0; i < len; ++i)
		data[i] = m_data[m_current++];
}


void Buffer::peek(uint8_t *data, unsigned int len) const
{
	for(unsigned int i = 0; i < len; ++i)
		data[i] = m_data[m_current + i];
}


unsigned int Buffer::getSize() const
{
	return m_data.size() - m_current;
}

