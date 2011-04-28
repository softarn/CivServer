
#ifndef MUTEX_H
#define MUTEX_H

#include <pthread.h>

class Mutex
{
public:
	class Locker
	{
	private:
		Mutex &m_mutex;
		Locker(const Locker &);
		Locker &operator=(const Locker &);
	public:
		Locker(Mutex &);
		~Locker();
	};

private:
	pthread_mutex_t m_mutex;

	Mutex(const Mutex &);
	Mutex &operator=(const Mutex &);
public:
	Mutex();
	~Mutex();

	void lock();
	void unlock();
};

#endif // MUTEX_H
