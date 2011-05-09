
#include <iostream>
#include <stdexcept>
#include <memory>

#include "thread.h"

namespace proxy
{

	Thread::Thread()
		: m_running(false)
	{
	}

	Thread::~Thread()
	{
		if(m_running)
		{
			std::cerr << "Warning: A thread should be ended before destructed" << std::endl;
			//pthread_detach(m_handle);
		}
	}

	void *Thread::internalRun(void *customData)
	{
		Thread *thisThread = static_cast<Thread *>(customData);
		thisThread->m_running = true;
		try
		{
			thisThread->run();
		}
		catch(std::exception &e)
		{
			std::cerr << "Uncaught exception in thread " << customData << ": " << e.what() << std::endl;
		}
		catch(...)
		{
			std::cerr << "Unknown and uncaught exception in thread " << customData << std::endl;
		}

		thisThread->m_running = false;

		return NULL;
	}


#ifdef _MSC_VER
	struct AdapterInfo
	{
		(void *)(*run)(void *);
		void *customData;
	};

	DWORD WINAPI runAdapter(void *arg)
	{
		AdapterInfo *ai = reinterpret_cast<AdapterInfo *>(arg);
		ai->run(ai->customData);
		delete ai;
		return 0;
	}

#endif

	void Thread::start()
	{
		if(m_running)
			throw std::runtime_error("Thread is already running!");

#ifdef _MSC_VER
		std::auto_ptr<AdapterInfo> ai(new AdapterInfo);
		ai->run = internalRun;
		ai->customData = this;

		m_handle = CreateThread(NULL, 0, runAdapter, ai.get(), 0, NULL);
		if(m_handle == NULL)
			throw std::runtime_error("Unable to start thread");

		ai.release();
#else
		if(pthread_create(&m_handle, NULL, internalRun, this) != 0)
			throw std::runtime_error("Unable to start thread");
#endif
	}

	void Thread::join()
	{
#ifdef _MSC_VER
		WaitForSingleObject (m_handle, INFINITE);
#else
		pthread_join(m_handle, NULL);
#endif
	}

	bool Thread::isRunning()
	{
		return m_running;
	}

}
