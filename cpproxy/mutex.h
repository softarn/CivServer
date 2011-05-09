
#ifndef MUTEX_H
#define MUTEX_H

#ifdef _MSC_VER
	#include <windows.h>
	typedef CRITICAL_SECTION mutex_handle;
#else
	#include <pthread.h>
	typedef pthread_mutex_t mutex_handle;
#endif

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
	mutex_handle m_mutex;

	Mutex(const Mutex &);
	Mutex &operator=(const Mutex &);
public:
	Mutex();
	~Mutex();

	void lock();
	void unlock();
};

#endif // MUTEX_H
