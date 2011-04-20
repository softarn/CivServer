import java.util.*;

class Result{
	
	private boolean ok;
	private String msg, type;
	private List list1, list2, list3;
	private int size, annat;

	public Result(){
	}

	public void add(boolean ok){
		this.ok = ok;
	}

	public void addMsg(String msg){
		this.msg = msg;
	}

	public void addType(String type){
		this.type = type;
	}

	public void addList1(List list){
		list1 = list;
	}

	public void addList2(List list){
		list2 = list;
	}

	public void addSize(int size){
		this.size = size;
	}

	public void addAnnat(int annat){
		this.annat = annat;
	}

	public String getMsg(){
		return msg;
	}

	public String getType(){
		return type;
	}
}
