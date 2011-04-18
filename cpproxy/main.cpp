
#include <iostream>
#include <exception>


#include "socket.h"

#include "buffer.h"



int main()
{

	try
	{
		Socket socket;
		// socket.connect("130.229.128.72", 1233);
		socket.connect("localhost", 1234);

		int8_t header = 2;
		socket.send(header);
		socket.send(int(0));
		socket.send(std::string("Kebab"));



		std::cout << "Receiving...\n";

		header = socket.recv<int8_t>();
		//int failure = socket.recv<int>();
		//int8_t h = socket.recv<int8_t>();

		std::cout << "Header: " << int(header) << std::endl;
		//std::cout << "Failure: " << failure << std::endl;
		//std::cout << "Failed header: " << h << std::endl;

		//std::cout << socket.recv<std::string>() << std::endl;

	}
	catch(std::exception &e)
	{
		std::cout << e.what() << std::endl;
	}

    return 0;
}
