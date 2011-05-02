
#include <stdexcept>
#include "proxy.h"
#include "confirmdresult.h"



Proxy::Proxy()
{
}

void Proxy::connect(const std::string &address)
{
	m_socket.connect(address, 1233);
}

std::auto_ptr<FaildResult> Proxy::recvFaildResult()
{
	return std::auto_ptr<FaildResult>(new FaildResult(m_socket.recv<int>(), m_socket.recv<int>(), m_socket.recv<std::string>()));
}


std::auto_ptr<Result> Proxy::sayHelloWorld()
{
	std::auto_ptr<Result> result;

	m_socket.send<uint8_t>(2);
	m_socket.send<int>(0);
	m_socket.send<std::string>("C++proxxyn");

	uint8_t header = m_socket.recv<uint8_t>();

	if(header == 3)
	{
		result = std::auto_ptr<Result>(new ConfirmdResult(header));
	}
	else if(header == 0)
	{
		result = recvFaildResult();
	}
	else
	{
		throw std::runtime_error("INVALID HEADER");
	}

	return result;
}
