
#include <iostream>
#include <exception>


#include "proxy.h"

#include "buffer.h"



int main()
{

	try
	{
		Proxy proxy;
		proxy.connect("130.237.238.229");

		std::auto_ptr<Result> result = proxy.sayHelloWorld();
		std::cout << "Result: " << result->getType() << std::endl;
	}
	catch(std::exception &e)
	{
		std::cout << e.what() << std::endl;
	}

    return 0;
}
