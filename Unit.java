import java.util.*;

public class Unit{
    private String owner, type;
    private int manPower;
    private List<Unit> units;

    public Unit(String owner, String type, int manPower, List<Unit> units){
        this.owner = owner;
        this.type = type;
        this.manPower = manPower;
        this.units = units;
    }

    public String getOwner(){
        return owner;
    }

    public String getType(){
        return type;
    }

    public int getManPower(){
        return manPower;
    }

    public List<Unit> getUnits(){
        return units;
    }

    public String toString(){
        return "Unit type: " + type + "\nUnit owner: " + owner + "\nUnit manpower: " + manPower + "\n";
    }
}
