
#ifndef THREAD_H
#define THREAD_H

#include <pthread.h>


class Thread
{
private:
	pthread_t m_handle;


	Thread(const Thread &);
	Thread &operator=(const Thread &);

	static void *internalRun(void *);


protected:
	virtual void run() = 0;

public:
	Thread();
	virtual ~Thread();

	void start();
	void join();
};

#endif // THREAD_H
