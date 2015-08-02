package mobius;

import group.SL2C;
import number.Complex;

public class Mobius {
	private Mobius(){
	}
	
	public static Complex getPlusFixPoint(SL2C t){
		Complex num =  t.a.sub(t.d).add( Complex.sqrt(t.trace().mult(t.trace()).sub(4.0f)));
		return num.div(t.c.mult(2.0f));
	}
	
	public static Complex getMinusFixPoint(SL2C t){
		Complex num =  t.a.sub(t.d).sub( Complex.sqrt(t.trace().mult(t.trace()).sub(4.0f)));
		return num.div(t.c.mult(2.0f));
	}

	public static Complex onPoint(SL2C t, Complex z){
		if(z.isInfinity()){
			if(!t.c.isZero()){
				return Complex.div(t.a, t.c);
			}else{
				return Complex.INFINITY;
			}
		}else{
			Complex numerix = Complex.add( Complex.mult(t.a, z), t.b);
			Complex denominator = Complex.add( Complex.mult(t.c, z), t.d);

			if(denominator.isZero()){
				return Complex.INFINITY;
			}else{
				return Complex.div( numerix, denominator);
			}
		}
	}
}
