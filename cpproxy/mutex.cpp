
#include <stdexcept>
#include "mutex.h"


Mutex::Mutex()
{
	if(pthread_mutex_init(&m_mutex, NULL) != 0)
		throw std::runtime_error("Unable to create mutex");
}

Mutex::~Mutex()
{
	pthread_mutex_destroy(&m_mutex);
}

void Mutex::lock()
{
	pthread_mutex_lock(&m_mutex);
}

void Mutex::unlock()
{
	pthread_mutex_unlock(&m_mutex);
}



Mutex::Locker::Locker(Mutex &mutex)
	: m_mutex(mutex)
{
	m_mutex.lock();
}

Mutex::Locker::~Locker()
{
	m_mutex.unlock();
}
