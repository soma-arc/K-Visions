package explorer;

import java.util.ArrayList;

import mobius.Mobius;
import number.Complex;
import group.SL2C;

public class LimitSetExplorer {
	private SL2C[] gens;
	private int[] tags;
	private SL2C[] words;
	private int level = 0;
	private ArrayList<Complex> pointsList;
	private Complex[][] fixPoints;

	public LimitSetExplorer(SL2C[] gens){
		this.gens = gens;

		fixPoints = new Complex[4][4];
		fixPoints[0][0] = Mobius.getPlusFixPoint(gens[1].mult(gens[2]).mult(gens[3]).mult(gens[0]));
	    fixPoints[0][1] = Mobius.getPlusFixPoint(gens[0]);
	    fixPoints[0][2] = Mobius.getPlusFixPoint(gens[3].mult(gens[2]).mult(gens[1]).mult(gens[0]));

	    fixPoints[1][0] = Mobius.getPlusFixPoint(gens[2].mult(gens[3]).mult(gens[0]).mult(gens[1]));
	    fixPoints[1][1] = Mobius.getPlusFixPoint(gens[1]);
	    fixPoints[1][2] = Mobius.getPlusFixPoint(gens[0].mult(gens[3]).mult(gens[2]).mult(gens[1]));

	    fixPoints[2][0] = Mobius.getPlusFixPoint(gens[3].mult(gens[0]).mult(gens[1]).mult(gens[2]));
	    fixPoints[2][1] = Mobius.getPlusFixPoint(gens[2]);
	    fixPoints[2][2] = Mobius.getPlusFixPoint(gens[1].mult(gens[0]).mult(gens[3]).mult(gens[2]));

	    fixPoints[3][0] = Mobius.getPlusFixPoint(gens[0].mult(gens[1]).mult(gens[2]).mult(gens[3]));
	    fixPoints[3][1] = Mobius.getPlusFixPoint(gens[3]);
	    fixPoints[3][2] = Mobius.getPlusFixPoint(gens[2].mult(gens[1]).mult(gens[0]).mult(gens[3]));
	}

	private void init(int maxLevel){
		tags = new int[maxLevel + 1];
		words = new SL2C[maxLevel + 1];
		words[0] = SL2C.UNIT;
		tags[0] = -1;
		level = 0;
		pointsList = new ArrayList<>();

		words[1] = gens[0];
		tags[1] = 0;
		level++;
	}

	public ArrayList<Complex> runDFS(int maxLevel, double threshold){
		init(maxLevel);
		do{
			while(branchTermination(maxLevel, threshold) == false){
				goForward();
			}
			do{
				goBackward();
			}while(level != 0 && isAvailableTurn() == false);
			turnAndGoForward();
		}while(level != 1 || tags[1] != 0);
		return pointsList;
	}
	
	public ArrayList<Complex> runDFS(int maxLevel, double threshold, Thread calcThread) throws InterruptedException{
		init(maxLevel);
		long pre = System.currentTimeMillis();
		do{
			while(branchTermination(maxLevel, threshold) == false){
				goForward();
			}
			do{
				goBackward();
				if(Thread.interrupted()){
					throw new InterruptedException();
				}
			}while(level != 0 && isAvailableTurn() == false);
			turnAndGoForward();
		}while(level != 1 || tags[1] != 0);
		System.out.println((System.currentTimeMillis() - pre));
		System.out.println(pointsList.size());
		return pointsList;
	}
	
	private void goForward(){
		level++;
		tags[level] = (tags[level - 1] + 1) % 4;

		words[level] = words[level -1].mult(gens[tags[level]]);
	}

	private boolean isAvailableTurn(){
		int t1 = (tags[level] + 2) % 4;
	    int t2 = tags[level + 1] - 1;
	    if (t2 == -1) {
	        t2 = 3;
	    };
	    return t1 == t2 ? false : true;
	}
	
	private void goBackward(){
		level--;
	}

	private void turnAndGoForward(){
		tags[level + 1] = (tags[level + 1] - 1) % 4;
	    if (tags[level + 1] == -1) {
	        tags[level + 1] = 3;
	    };
	    if (level == 0) {
	        words[1] = gens[tags[1]];
	    } else {
	        words[level + 1] = words[level].mult(gens[tags[level + 1]]);
	    };
	    ++level;
	}

	private boolean branchTermination(int maxLevel, double epsilon){
		Complex[] z = new Complex[3];
		for(int i = 0 ; i < 3 ; i++){
			z[i] = Mobius.onPoint(words[level], fixPoints[tags[level]][i]);
		}
		if((z[0].dist(z[1]) < epsilon  && z[1].dist(z[2]) < epsilon)|| level == maxLevel){
			pointsList.add(z[0]);
			pointsList.add(z[1]);
			pointsList.add(z[2]);
			return true;
		}
		return false;
	}
}
