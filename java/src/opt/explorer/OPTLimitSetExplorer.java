package opt.explorer;

import group.SL2C;

import java.util.ArrayList;

import number.Complex;
import mobius.Mobius;

public class OPTLimitSetExplorer {
	private SL2C[] gens;
	private SL2C[] words;
	private Complex[] fixPoints;
	private int[] tags;
	private int level;
	private ArrayList<Complex> pointsList;
	
	public OPTLimitSetExplorer(SL2C[] gens){
		this.gens = new SL2C[gens.length];
		fixPoints = new Complex[gens.length];
		for(int i = 0 ; i < gens.length ; i++){
			this.gens[i] = gens[i];
			fixPoints[i] = Mobius.onPoint(gens[i], Complex.INFINITY);
		}
	}

	private void init(int maxLevel){
		tags = new int[maxLevel + 1];
		words = new SL2C[maxLevel + 1];
		words[0] = SL2C.UNIT;
		tags[0] = -1;
		level = 0;
		pointsList = new ArrayList<>();

		words[1] = gens[1];
		tags[1] = 1;
		level++;
	}
	
	public ArrayList<Complex> run(int maxLevel, double epsilon){
		init(maxLevel);
		pointsList.add(Complex.ZERO);
		do{
			while(branchTermination(maxLevel, epsilon) == false){
				goForward();
			}
			do{
				goBackward();
			}while(level != 0 && isAvailableTurn() == false);
			turnAndGoForward();
		}while(level != 1 || tags[1] != 0);
		pointsList.add(Complex.ONE);
		return pointsList;
	}
	
	public ArrayList<Complex> run(int maxLevel, double epsilon, long timeOutMillis){
		init(maxLevel);
		pointsList.add(Complex.ZERO);
		long pre = System.currentTimeMillis();
		outer:{
			do{
				while(branchTermination(maxLevel, epsilon) == false){
					goForward();
					if(getElapsedTime(pre) > timeOutMillis) break outer;
				}
				do{
					goBackward();
					if(getElapsedTime(pre) > timeOutMillis) break outer;
				}while(level != 0 && isAvailableTurn() == false);
				turnAndGoForward();
				if(getElapsedTime(pre) > timeOutMillis) break outer;
			}while(level != 1 || tags[1] != 0);
		}
		pointsList.add(Complex.ONE);
		return pointsList;
	}
	
	private long getElapsedTime(long previousMillis){
		return System.currentTimeMillis() - previousMillis;
	}
	
	
	private void goForward(){
		level++;
		tags[level] = (tags[level-1] + 1) % 3; 

		words[level] = words[level -1].mult(gens[tags[level]]);
	}

	private boolean isAvailableTurn(){
		if((tags[level] + 2) % 3 != (tags[level + 1] + 1) % 3 ||
		   (level == 2 && tags[1] == 2 && tags[2] == 1 && tags[3] == 2)){
			return false;
		}
		return true;
	}
	
	private void goBackward(){
		level--;
	}
	
	private void turnAndGoForward(){
		tags[level + 1] = (tags[level + 1] + 1)%3;

		words[level + 1] = words[level].mult(gens[tags[level + 1]]);
		level++;
	}

	private boolean branchTermination(int maxLevel, double epsilon){
		Complex[] z = {Mobius.onPoint(words[level], fixPoints[(tags[level] + 1) % 3]),
					   Mobius.onPoint(words[level], fixPoints[(tags[level] + 2) % 3])};
		
		if((z[0].dist(z[1])) < epsilon || level == maxLevel){
			pointsList.add(z[0]);
			pointsList.add(z[1]);
			return true;
		}
		
		return false;
	}
}
