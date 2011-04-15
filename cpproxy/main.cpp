
#include <iostream>
#include <exception>


#include "socket.h"

#include "buffer.h"



int main()
{

	try
	{
		Socket socket;
		socket.connect("130.229.128.72", 1233);

		std::string s = "Ohai";
		int8_t header = 1;
		socket.send(header);
		socket.send(12345);

		char lol[1];

		std::cout << "Receiving...\n";

		std::cout << socket.recv(lol, sizeof(lol)) << std::endl;

		for(unsigned int i = 0; i < sizeof(lol); ++i)
			std::cout << lol[i];
		std::cout << std::endl;
	}
	catch(std::exception &e)
	{
		std::cout << e.what() << std::endl;
	}

    return 0;
}
