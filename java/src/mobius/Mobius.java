package mobius;

import schottky.figure.Circle;
import schottky.twinCircles.TwinCircles;
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
	
	public static SL2C getSymmetricTransformation(Circle c1, Circle c2){
		Complex P = c1.getCenter();
		double r = c1.getR();
		Complex Q = c2.getCenter();
		double s = c2.getR();

		return new SL2C(
				Q, new Complex(r * s, 0).sub(P.mult(Q)),
				Complex.ONE, P.mult(-1));
	}

	public static SL2C getSymmetricTransformation(Circle c1, Circle c2, Complex u, Complex v){
		Complex P = c1.getCenter();
		double r = c1.getR();
		Complex Q = c2.getCenter();
		double s = c2.getR();

		return new SL2C(
				u.conjugate().mult(s), v.conjugate().mult(r).sub(u.conjugate().mult(P).mult(s)).sub(P.mult(Q).mult(u)).add(v.mult(Q.mult(r))),
				u, v.mult(r).sub(u.mult(P)));
	}
	
	public static Circle onCircle(SL2C t, Circle c){
		Complex rad = new Complex(c.getR(), 0);
		Complex z = c.getCenter().sub( rad.mult(rad).div( Complex.conjugate(t.d.div(t.c).add(c.getCenter()))));
		Complex newCenter = onPoint(t, z);
		double newR = Complex.abs(newCenter.sub( onPoint(t, c.getCenter().add(c.getR()))));
		return new Circle(newCenter, newR);
	}

	public static SL2C getMobiusTransform(Circle c1, Circle c2){
		Complex z1 = c1.getP1();
		Complex z2 = c1.getP2();
		Complex z3 = c1.getP3();
		Complex w1 = c2.getP1();
		Complex w2 = c2.getP2();
		Complex w3 = c2.getP3();

		SL2C m1 = new SL2C(
				z2.sub(z3), z1.mult(-1).mult(z2.sub(z3)),
				z2.sub(z1), z3.mult(-1).mult(z2.sub(z1)));
		SL2C m2 = new SL2C(
				w2.sub(w3), w1.mult(-1).mult(w2.sub(w3)),
				w2.sub(w1), w3.mult(-1).mult(w2.sub(w1)));
		return m2.inverse().mult(m1);
	}

	public static SL2C getMobiusTransform(TwinCircles tc){
		Circle c1 = tc.getC1();
		Circle c2 = tc.getC2();
		Complex z1 = c1.getP1();
		Complex z2 = c1.getP2();
		Complex z3 = c1.getP3();
		Complex w1 = c2.getP1();
		Complex w2 = c2.getP2();
		Complex w3 = c2.getP3();
//		System.out.println("w2"+ w2);
//		System.out.println("w3"+ w3);
		SL2C m1 = new SL2C(
				z2.sub(z3), z1.mult(-1).mult(z2.sub(z3)),
				z2.sub(z1), z3.mult(-1).mult(z2.sub(z1)));
//		System.out.println("m1 "+ m1);
		SL2C m2 = new SL2C(
				w2.sub(w3), w1.mult(-1).mult(w2.sub(w3)),
				w2.sub(w1), w3.mult(-1).mult(w2.sub(w1)));
//		System.out.println("m2 inv");
//		System.out.println(m2.inverse());
		return m2.inverse().mult(m1);
	}
	
	public static SL2C getLoxodromicTransformation(Complex fixA, Complex fixB, Complex lambda){
		SL2C f = new SL2C(lambda, Complex.ZERO, Complex.ZERO, Complex.ONE.div(lambda));
		SL2C g = new SL2C(Complex.ONE, fixA.mult(-1), Complex.ONE, fixB.mult(-1));
		return g.inverse().mult(f).mult(g);
	}
}
