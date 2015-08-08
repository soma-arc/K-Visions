package schottky.explorer;

import java.sql.Array;
import java.util.ArrayList;

import schottky.figure.Circle;
import mobius.Mobius;
import group.SL2C;
import group.SchottkyGroup;

public class SchottkyExplorer {
	private SL2C[] gens;
	private Circle[] baseCircles;
	private ArrayList<ArrayList<Circle>> circles = new ArrayList<ArrayList<Circle>>();

	public SchottkyExplorer(SchottkyGroup group) {
		gens = group.getGens();
		baseCircles = group.getCircles();
	}
	
	public void init(){
		circles.clear();
		circles.add(new ArrayList<Circle>());
		for(int i = 0 ; i < baseCircles.length ; i++) {
			circles.get(0).add(baseCircles[i]);
		}
	}

	public void run(int maxLevel, double epsilon){
		init();

		for(int level = 1 ; level < maxLevel ; level++){
			circles.add(new ArrayList<Circle>());
			for(Circle c : circles.get(level - 1)){
				for(int i = 0 ; i < 4 ; i++){
					if((c.getTag() + 2) % 4 == i ) continue;
					Circle newCircle = Mobius.onCircle(gens[i], c);
					if(newCircle.getR() < epsilon) continue;

					newCircle.setTag(i);
					circles.get(level).add(newCircle);
				}
			}
		}
	}
	
	public void run(int maxLevel, double epsilon, double stopMillis){
		init();
		double previousMillis = System.currentTimeMillis();

		outer:{
			for(int level = 1 ; level < maxLevel ; level++){
				if((System.currentTimeMillis() - previousMillis) > stopMillis) break outer;
				circles.add(new ArrayList<Circle>());
				for(Circle c : circles.get(level - 1)){
					for(int i = 0 ; i < 4 ; i++){
						if((System.currentTimeMillis() - previousMillis) > stopMillis) {
							break outer;
						}
						if((c.getTag() + 2) % 4 == i ) continue;
						Circle newCircle = Mobius.onCircle(gens[i], c);
						if(newCircle.getR() < epsilon) continue;

						newCircle.setTag(i);
						circles.get(level).add(newCircle);
						if(circles.get(level).size() > 1000) break outer;
					}
				}
			}
		}
	}
	
	public SL2C[] getGens(){
		return gens;
	}

	public ArrayList<ArrayList<Circle>> getCircles(){
		return circles;
	}
}
