package generator;

import group.SL2C;
import number.Complex;

public class Recipe {
	private Recipe(){}
	
	public static SL2C[] parabolicCommutatorGroup(Complex t_a, Complex t_b, boolean isT_abPlus){
		Complex t_ab;
		SL2C[] gens = new SL2C[4];
		if(isT_abPlus){
			t_ab = t_a.mult(t_b).add(Complex.sqrt( t_a.mult(t_a).mult(t_b.mult(t_b)).sub(t_a.mult(t_a).add(t_b.mult(t_b)).mult(4.0)))).mult(0.5);			
		}else{
			t_ab = t_a.mult(t_b).sub(Complex.sqrt( t_a.mult(t_a).mult(t_b.mult(t_b)).sub(t_a.mult(t_a).add(t_b.mult(t_b)).mult(4.0)))).mult(0.5);
		}
		
		Complex z0 = t_ab.sub(2.0).mult(t_b).div(t_b.mult(t_ab).sub(t_a.mult(2.0)).add(t_ab.mult(new Complex(0, 2.0))));

	    gens[0] = new SL2C(t_a.mult(0.5),
	                       t_a.mult(t_ab).sub(t_b.mult(2.0)).add(new Complex(0, 4.0)).div(z0.mult(t_ab.mult(2.0).add(4.0))),
	                       z0.mult(t_a.mult(t_ab).sub(t_b.mult(2.0)).sub(new Complex(0, 4.0)).div(t_ab.mult(2.0).sub(4.0))),
	                       t_a.mult(0.5));
	    gens[1] = new SL2C(t_b.sub(new Complex(0, 2.0)).mult(0.5),
	                       t_b.mult(0.5),
	                       t_b.mult(0.5),
	                       t_b.add(new Complex(0, 2.0)).mult(0.5));
	    gens[2] = gens[0].inverse();
	    gens[3] = gens[1].inverse();
	    
	    return gens;
	}
}
