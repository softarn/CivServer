
#ifndef THREAD_H
#define THREAD_H

#ifdef _MSC_VER
	#include <windows.h>
	typedef HANDLE thread_handle;
#else
	#include <pthread.h>
	typedef pthread_t thread_handle;
#endif

namespace proxy
{

	class Thread
	{
	private:
		thread_handle m_handle;
		bool m_running;


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
		bool isRunning();
	};
}

#endif // THREAD_H
