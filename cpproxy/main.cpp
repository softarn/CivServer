
#include <iostream>
#include <sstream>
#include <exception>


#include "proxy.h"

#include "buffer.h"

using namespace proxy;


class MyPacketListener : public PacketListener
{
public:

	void gameSessionUpdate(protocol::GameSessionInformation &gsi)
	{
		std::cout << gsi.players.size() << " players in this game: " << std::endl;
		for(size_t i = 0; i < gsi.players.size(); ++i)
			std::cout << gsi.players[i].name << ", " << gsi.players[i].civilization << std::endl;
	}

	void gameStarted(protocol::StartGameAnswer &sga)
	{
		std::cout << "Map size: " << sga.mapData.size() << std::endl;
	}

	void yourTurn(protocol::ItsYourTurn &itsYourTurn)
	{
		std::cout << "IT'S YOUR TURN!\n";
	}

	void chatMessageReceived() {}

	void gameClosed()
	{
		std::cout << "Game closed\n";
	}
};

void listGames(Proxy &proxy)
{
	std::vector<std::string> gameList = proxy.listGames();

	std::cout << "List size: " << gameList.size() << std::endl;
	for(size_t i = 0; i < gameList.size(); ++i)
		std::cout << "  " << gameList[i] << std::endl;
}

void gameLobby(Proxy &proxy, std::string gameName)
{
	std::cout << "Welcome to " << gameName << "'s game!\n";

	int option;
	do
	{
		std::cout << "What do you want to do?\n";
		std::cout << "1. Change civilization\n";
		std::cout << "2. Lock game\n";
		std::cout << "3. Start game\n";
		std::cout << "4. Leave\n";
		std::cin >> option;

		switch(option)
		{
		case 1:
			{
				std::cout << "Enter new civilization: ";
				std::string civilization;
				std::cin >> civilization;
				proxy.changeCivilization(civilization);
			}
			break;

		case 2:
			proxy.lockGame();
			break;

		case 3:
			proxy.startGame();
			break;

		case 4:
			proxy.leaveGame();
			break;

		default:
			std::cout << "Invalid command\n";
		}
	}
	while(option != 4);
}


void mainMenu(Proxy &proxy, std::string playerName)
{
	int option;
	do
	{
		std::cout << "What do you want to do?\n";
		std::cout << "1. List games\n";
		std::cout << "2. Host game\n";
		std::cout << "3. Join game\n";
		std::cout << "4. Quit\n";
		std::cin >> option;

		switch(option)
		{
		case 1:
			listGames(proxy);
			break;

		case 2:
			proxy.hostGame();
			gameLobby(proxy, playerName);
			break;

		case 3:
			{
				std::string gameName;
				std::cout << "Which game to join? ";
				std::cin >> gameName;
				proxy.joinGame(gameName);
				gameLobby(proxy, gameName);
			}
			break;

		case 4:
			break;

		default:
			std::cout << "Invalid command\n";
		}
	}
	while(option != 4);
}


int main()
{
	for(int i = 0; i < 1; ++i)
	{
		unsigned short port;
		std::cin >> port;
		try
		{
			MyPacketListener pl;
			Proxy proxy(pl);
			//proxy.connect("130.229.134.186", port);
			//proxy.connect("130.237.238.239", port);
			proxy.connect("130.229.157.246", port);
			//proxy.connect("localhost");

			std::stringstream stream;
			stream << i;

			std::cout << "Connected, what is your name? ";
			std::string name;
			std::cin >> name;


			proxy.sayHelloWorld(name);// + stream.str());

			std::cout << "Welcome to the real world!\n";

			mainMenu(proxy, name);


			/*
			list(proxy);

			//proxy.hostGame();
			proxy.joinGame("Kalle");

			list(proxy);
			*/

			std::cin.get();
		}
		catch(std::exception &e)
		{
			std::cout << "Failure: " << e.what() << std::endl;
		}

	}

    return 0;
}
