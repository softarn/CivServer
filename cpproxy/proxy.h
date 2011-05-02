
#ifndef PROXY_H
#define PROXY_H

#include "socket.h"
#include "result.h"
#include "faildresult.h"


class Proxy
{
private:
	Socket m_socket;

	std::auto_ptr<FaildResult> recvFaildResult();

public:
	Proxy();

	void connect(const std::string &address);

	std::auto_ptr<Result> sayHelloWorld();
};

#endif // PROXY_H
