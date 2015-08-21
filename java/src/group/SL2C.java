package group;
import number.Complex;

public class SL2C {
	public Complex a, b, c, d;
	public static SL2C UNIT = new SL2C(1, 0, 0 , 1);

	public SL2C(Complex a, Complex b, Complex c, Complex d){
		this.a = a;
	    this.b = b;
	    this.c = c;
	    this.d = d;
	}

	public SL2C(double a, double b, double c, double d){
		this.a = new Complex(a);
	    this.b = new Complex(b);
	    this.c = new Complex(c);
	    this.d = new Complex(d);
	}

	public SL2C mult(SL2C n){
		return new SL2C(Complex.add(Complex.mult(a, n.a), Complex.mult(b, n.c)),
	                      Complex.add(Complex.mult(a, n.b), Complex.mult(b, n.d)),
	                      Complex.add(Complex.mult(c, n.a), Complex.mult(d, n.c)),
	                      Complex.add(Complex.mult(c, n.b), Complex.mult(d, n.d)));
	}

	public SL2C mult(double coefficient){
		return new SL2C(a.mult(coefficient),
	                      b.mult(coefficient),
	                      c.mult(coefficient),
	                      d.mult(coefficient));
	}

	public SL2C mult(Complex coefficient){
		return new SL2C(a.mult(coefficient),
				b.mult(coefficient),
				c.mult(coefficient),
				d.mult(coefficient));
	}

	public SL2C inverse(){
		Complex one = new Complex(1.0, 0.0);
		return new SL2C(d, b.mult(-1.0), c.mult(-1.0), a).mult(one.div(a.mult(d).sub(b.mult(c))));
	}

	public Complex trace(){
		return a.add(d);
	}

	public SL2C conjugate(SL2C T){
		return T.mult(this).mult(T.inverse());
	}

	public String toString(){
		return "{"+ a.toString() +","+ b.toString() +"\n"+ c.toString() +","+ d.toString() +"}";
	}
}
