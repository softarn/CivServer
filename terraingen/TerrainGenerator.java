//~ Litet program som skriver ut en matris av heltal
//~ med storlek 20x20 (gar att andra enkelt, kolla main-metoden),
//~ som representerar en slumpgenererad karta. ar inte sa
//~ bra an sa lange, och genererar bara kvadratiska tiles.
//~ Jag ska fylla pa med kommentarer i koden asap.
//~
//~ Terrangtyperna ar som foljer:
//~ 1 = water
//~ 2 = plains
//~ 3 = hills
//~ 4 = desert / Mountains??
//~ 5 = marsh
//~ 6 = tundra
//~ 7 = rainforest
//~ 8 = broadleafForest
//~ 9 = coniferForest
//~
//~ Testa att generera lite olika kartor. an sa lange kan
//~ de skilja sig at ganska markant. Vissa har valdigt mycket
//~ vatten, medan vissa knappt har nagot. Skulle det vara
//~ eftertraktat att lagga in en minimum/maximum vattenmangd?
//~
//~ Har ni nagra fragor sa fraga garna!
//~ 	Arian Jafari

import java.util.Random;
import java.util.ArrayList;

public class TerrainGenerator {

	private int[][] map;
	Random random = new Random();
	private int width;
	private int height;
	int waterAmount = 0;

	public int[][] generateMap(int width, int height) {
		this.width = width;
		this.height = height;
		System.out.println(width + " " + height);
		map = new int[width][height];
		fill();		//Borjar med att fylla hela kartan med bara vatten.
		for (int i = 0; i < map.length; i++) {
			for (int j = 0; j < map[i].length; j++) {
				generateSq(i, j);
			}
		}
		return map;
	}

	//~ Funktion som bestammer vad en ruta ska ha for terrangtyp.
	//~ Slumpar forst fram huruvida det ska vara vatten eller land, men
	//~ viktar resultatet baserat pa vad terrangbitens intilliggande rutor
	//~ har for terrang. Om det visar sig att det blir en landruta, sa gor man
	//~ ungefar samma sak for att bestamma vilken typ av landterrang det blir.
	private void generateSq(int x, int y) {
		ArrayList<Integer> neighbours = getNeighbours(x, y);
		int water = countTerrain(1, neighbours);
		int nonWater = 0;

		int plains = countTerrain(2, neighbours);
		int hills = countTerrain(3, neighbours);
		int desert = countTerrain(4, neighbours);

		int marsh = countTerrain(5, neighbours);
		int tundra = countTerrain(6, neighbours);
	   // int rainforest = countTerrain(7, neighbours);
		int broadleafForest = countTerrain(7, neighbours);
		int coniferForest = countTerrain(8, neighbours);


		nonWater += plains;
		nonWater += hills;
		nonWater += desert;

		nonWater += marsh;
		nonWater += tundra;
		//nonWater += rainforest;
		nonWater += broadleafForest;
		nonWater += coniferForest;

		int weight = water - nonWater;

		int waterOrLand = random.nextInt(6);

		if (waterOrLand - weight < 0) {
			map[x][y] = 1;
			return;
		}

		int plainsChance = random.nextInt(5) + plains;
		int hillsChance = random.nextInt(5) + hills;
		int desertChance = random.nextInt(5) + desert;

		int marshChance = random.nextInt(5) + marsh;
		int tundraChance = random.nextInt(5) + tundra;
	   // int rainforestChance = random.nextInt(12) + rainforest;
		int broadleafForestChance = random.nextInt(5) + broadleafForest;
		int coniferForestChance = random.nextInt(5) + coniferForest;


		if (plainsChance > hillsChance && plainsChance > desertChance && plainsChance > marshChance && plainsChance > tundraChance/* && plainsChance > rainforestChance */&& plainsChance > broadleafForestChance && plainsChance > coniferForestChance)
			map[x][y] = 2;
		else if (hillsChance > plainsChance && hillsChance > desertChance && hillsChance > marshChance && hillsChance > tundraChance /*&& hillsChance > rainforestChance */&& hillsChance > broadleafForestChance && hillsChance > coniferForestChance)
			map[x][y] = 3;
		else if (desertChance > plainsChance && desertChance > hillsChance && desertChance > marshChance && desertChance > tundraChance /*&& desertChance > rainforestChance */&& desertChance > broadleafForestChance && desertChance > coniferForestChance)
			map[x][y] = 4;
		else if (marshChance > plainsChance && marshChance > hillsChance && marshChance > desertChance && marshChance > tundraChance /*&& marshChance > rainforestChance */&& marshChance > broadleafForestChance && marshChance > coniferForestChance)
			map[x][y] = 5;
		else if (tundraChance > plainsChance && tundraChance > hillsChance && tundraChance > desertChance && tundraChance > marshChance /*&& tundraChance > rainforestChance */&& tundraChance > broadleafForestChance && tundraChance > coniferForestChance)
			map[x][y] = 6;
	  //  else if (rainforestChance > plainsChance && rainforestChance > hillsChance && rainforestChance > desertChance && rainforestChance > marshChance && rainforestChance > tundraChance && rainforestChance > broadleafForestChance && rainforestChance > coniferForestChance)
	   //         map[x][y] = 7;
		else if (broadleafForestChance > plainsChance && broadleafForestChance > hillsChance && broadleafForestChance > desertChance && broadleafForestChance > marshChance && broadleafForestChance > tundraChance /*&& broadleafForestChance > rainforestChance */&& broadleafForestChance > coniferForestChance)
			map[x][y] = 7;
		else if (coniferForestChance > plainsChance && coniferForestChance > hillsChance && coniferForestChance > desertChance && coniferForestChance > marshChance && coniferForestChance > tundraChance /*&& coniferForestChance > rainforestChance */&& coniferForestChance > broadleafForestChance)
			map[x][y] = 8;
		else
			map[x][y] = random.nextInt(8) + 1;
	}

	//~ Raknar hur manga ganger ett visst heltal (terrang)
	//~ dyker upp i en viss lista.
	private int countTerrain(int type, ArrayList<Integer> squares) {
		int counter = 0;
		for (int s : squares)
			if (s == type)
				counter++;
		return counter;
	}

	//~ Returnerar en lista med alla grannarnas terrangtyper
	private ArrayList<Integer> getNeighbours(int x, int y) {
		ArrayList<Integer> neighbours = new ArrayList<Integer>();

		if (y+1 < height) neighbours.add(map[x][y+1]);
		if (y-1 > -1) neighbours.add(map[x][y-1]);
		if (x-1 > -1) neighbours.add(map[x-1][y]);
		if (x+1 < width) neighbours.add(map[x+1][y]);

		if (x % 2 == 0) {
			if (y+1 < height && x-1 > -1)
				neighbours.add(map[x-1][y+1]);
			if (y+1 < height && x+1 < width)
				neighbours.add(map[x+1][y+1]);
		}
		else {
			if (y-1 < -1 && x-1 > -1)
				neighbours.add(map[x-1][y-1]);
			if (y-1 < -1 && x+1 < width)
				neighbours.add(map[x+1][y-1]);
		}
		return neighbours;
	}


	//~ Fyller hela kartan med vatten till att borja med
	private void fill() {
		for (int i = 0; i < map.length; i++)
			for (int j = 0; j < map[i].length; j++)
				map[i][j] = 1;
	}

	public void printMap() {
		for (int i = 0; i < map.length; i++) {
			for (int j = 0; j < map[i].length; j+=2) {
				System.out.print(map[i][j] + " ");
			}
			System.out.print("\n ");
			for (int k = 1; k < map[i].length; k+=2) {
				System.out.print(map[i][k] + " ");
			}
			System.out.print("\n");
		}
	}

	public static void main(String[] args) {
		TerrainGenerator r = new TerrainGenerator();
		int[][] matrix = r.generateMap(10, 30);	//andra talet till nagot annat har for att fa annan storlek pa kartan.
		r.printMap();
	}
}