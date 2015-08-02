package group;

import mobius.Mobius;
import group.SL2C;
import schottky.figure.Circle;
import schottky.twinCircles.TwinCircles;

public class TwoGensGroup implements SchottkyGroup{
	private SL2C[] gens = new SL2C[4];
	private Circle[] circles = new Circle[4];
	public TwoGensGroup(TwinCircles tc1, TwinCircles tc2) {
		gens[0] = Mobius.getMobiusTransform(tc1);
		gens[1] = Mobius.getMobiusTransform(tc2);
		gens[2] = gens[0].inverse();
		gens[3] = gens[1].inverse();
		
		circles[0] = tc1.getC1();
		circles[1] = tc2.getC1();
				
		circles[2] = tc1.getC2(); 
		circles[3] = tc2.getC2();
		for(int i = 0 ; i < 4 ; i++){
			circles[i].setTag((i + 2)%4);
		}
	}
	
	@Override
	public SL2C[] getGens() {
		return gens;
	}

	@Override
	public Circle[] getCircles() {
		return circles;
	}

}
