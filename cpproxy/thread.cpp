
#include <iostream>
#include <stdexcept>

#include "thread.h"

namespace proxy
{

	Thread::Thread()
		: m_handle(0)
	{
	}

	Thread::~Thread()
	{
		if(m_handle)
		{
			std::cerr << "Warning: A thread should be ended before destructed" << std::endl;
			pthread_detach(m_handle);
		}
	}

	void *Thread::internalRun(void *customData)
	{
		Thread *thisThread = static_cast<Thread *>(customData);
		try
		{
			thisThread->run();
		}
		catch(std::exception &e)
		{
			std::cerr << "Uncaught exception in thread with id " << thisThread->m_handle << ": " << e.what() << std::endl;
		}
		catch(...)
		{
			std::cerr << "Unknown and uncaught exception in thread with id " << thisThread->m_handle << std::endl;
		}

		thisThread->m_handle = 0;

		return NULL;
	}


	void Thread::start()
	{
		if(m_handle)
			throw std::runtime_error("Thread is already running!");

		if(pthread_create(&m_handle, NULL, internalRun, this) != 0)
			throw std::runtime_error("Unable to start thread");
	}

	void Thread::join()
	{
		if(m_handle)
		{
			pthread_join(m_handle, NULL);
		}
	}

	bool Thread::isRunning()
	{
		return m_handle != 0;
	}

}
