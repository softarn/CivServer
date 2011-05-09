
#include <stdexcept>
#include "mutex.h"


Mutex::Mutex()
{
#ifdef _MSC_VER
	InitializeCriticalSection(&m_mutex);
#else
	if(pthread_mutex_init(&m_mutex, NULL) != 0)
		throw std::runtime_error("Unable to create mutex");
#endif
}

Mutex::~Mutex()
{
#ifdef _MSC_VER
	DeleteCriticalSection(&m_mutex);
#else
	pthread_mutex_destroy(&m_mutex);
#endif
}

void Mutex::lock()
{
#ifdef _MSC_VER
	EnterCriticalSection(&m_mutex);
#else
	pthread_mutex_lock(&m_mutex);
#endif
}

void Mutex::unlock()
{
#ifdef _MSC_VER
	LeaveCriticalSection(&m_mutex);
#else
	pthread_mutex_unlock(&m_mutex);
#endif
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
