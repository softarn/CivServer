
#include <iostream>
#include <sstream>
#include <exception>


#include "proxy.h"

#include "buffer.h"

using namespace proxy;

bool g_lockedGame = false;
bool g_yourTurn = false;
bool g_gameRunning = false;

void displayUnits(const std::vector<protocol::Tile> &tileList)
{
	for(size_t i = 0; i < tileList.size(); ++i)
	{
		if(const protocol::Unit *unit = tileList[i].unit.getValue())
		{
			std::cout << unit->owner << " got a " << unit->unitType << " at (" << tileList[i].position.x << ", " << tileList[i].position.y << ")\n";
		}

	}
}


class MyPacketListener : public PacketListener
{
public:


	void gameSessionUpdate(protocol::GameSessionInformation &gsi)
	{
		std::cout << gsi.players.size() << " players in this game: " << std::endl;
		for(size_t i = 0; i < gsi.players.size(); ++i)
			std::cout << gsi.players[i].name << ", " << gsi.players[i].civilization << std::endl;
		g_lockedGame = gsi.locked;
	}

	void gameStarted(protocol::StartGameAnswer &sga)
	{
		std::cout << "Map size: " << sga.mapData.size() << std::endl;

		displayUnits(sga.presetUnits);
	}

	void yourTurn(protocol::ItsYourTurn &itsYourTurn)
	{
		std::cout << "IT'S YOUR TURN!\n";
		g_yourTurn = true;

		displayUnits(itsYourTurn.updatedTiles);
	}

	void chatMessageReceived(protocol::ChatMessage &cm)
	{
		std::cout << cm.sender << ": " << cm.message << std::endl;
	}

	void gameClosed()
	{
		std::cout << "Game closed\n";
		g_gameRunning = false;
	}

	void casualtyReport(proxy::protocol::CasualtyReport &)
	{
	}
};


void gameLobby(GameLogix &proxy, std::string gameName)
{
	std::cout << "Welcome to " << gameName << "'s game!\n";
	g_gameRunning = true;

	int option;
	do
	{
		std::cout << "What do you want to do?\n";
		std::cout << "1. Change civilization\n";
		if(g_lockedGame)
			std::cout << "2. Unlock game\n";
		else
			std::cout << "2. Lock game\n";
		std::cout << "3. Start game\n";
		std::cout << "4. Leave\n";
		std::cout << "5. Move unit\n";
		std::cout << "6. End turn\n";
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
			proxy.lockGame(!g_lockedGame);
			break;

		case 3:
			{
				std::cout << "Enter map size: ";
				int mapSize;
				std::cin >> mapSize;
				proxy.startGame(mapSize);
			}
			break;

		case 4:
			proxy.leaveGame();
			break;

		case 5:
			if(g_yourTurn)
			{
				int x1, y1, x2, y2;
				std::cout << "Move unit from: ";
				std::cin >> x1 >> y1;
				std::cout << "Move unit to: ";
				std::cin >> x2 >> y2;

				proxy.move(x1, y1, x2, y2);
			}
			break;

		case 6:
			if(g_yourTurn)
			{
				proxy.endTurn();
			}
			break;

		default:
			std::cout << "Invalid command\n";
		}
	}
	while(option != 4 && g_gameRunning);
}


void listGames(GameLogix &proxy)
{
	std::vector<std::string> gameList = proxy.listGames();

	std::cout << "List size: " << gameList.size() << std::endl;
	for(size_t i = 0; i < gameList.size(); ++i)
		std::cout << "  " << gameList[i] << std::endl;
}

void mainMenu(GameLogix &proxy, std::string playerName)
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
	try
	{
		MyPacketListener pl;
		Proxy p(pl);
		GameLogix &proxy = p;

		proxy.connect("dvk.fishface.se", 1339);

		std::cout << "Connected, what is your name? ";
		std::string name;
		std::cin >> name;


		proxy.sayHelloWorld(name);

		std::cout << "Welcome to the real world!\n";

		mainMenu(proxy, name);
	}
	catch(std::exception &e)
	{
		std::cout << "Failure: " << e.what() << std::endl;
	}

    return 0;
}
