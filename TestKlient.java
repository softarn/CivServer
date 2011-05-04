class TestKlient{
	String name = "Kalle";
	Proxy p = new Proxy("localhost", 1237, new MyPackLyss());

	public TestKlient(){
	}

	public static void main(String [] args){
		TestKlient flum = new TestKlient();

		Result conRes = flum.p.connect(flum.name);

		System.out.println(conRes.getOk() + "   " + conRes.getOkMsg());
		
		flum.p.listGames();

		System.out.println("Weird");
		System.exit(0);
	}

	private class MyPackLyss implements PacketListener{
		
		MyPackLyss(){
		}

		public void newTurn(Result res){
			System.out.println(res.getSessions());
		}

		public void lobbyUpdated(Result res){

		}

		public void gameStarted(Result res){

		}

		public void chatMessageReceived(Result res){

		}
	}
}
