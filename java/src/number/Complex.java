package number;

public class Complex {
	private double re;
	private double im;
	public static final Complex INFINITY = new Complex( Double.POSITIVE_INFINITY, 0.0);
	public static final Complex ONE = new Complex(1, 0);
	public static final Complex ZERO = new Complex(0, 0);
	public static final Complex I = new Complex(0, 1);
	
	public Complex(double re, double im) {
		this.re = re;
		this.im = im;
	}
	
	public Complex(double re){
		this.re = re;
		this.im = 0;
	}

	public double re() {
		return re;
	}
	public double im() {
		return im;
	}

	public void setRe(double re){
		this.re = re;
	}

	public void setIm(double im){
		this.im = im;
	}

	public Complex add(Complex c) {
		return new Complex(re + c.re, im + c.im());
	}
	public Complex add(double c){
		return new Complex(re + c, im);
	}

	public Complex sub(Complex c) {
		return new Complex(re - c.re, im - c.im);
	}

	public Complex sub(double c){
		return new Complex(re - c, im);
	}

	public Complex mult(Complex c) {
		return new Complex(re*c.re - im*c.im, re*c.im + im*c.re);
	}

	public Complex mult(double a){
		return new Complex(re * a, im * a);
	}

	public Complex div(Complex c) {
		double denominator = c.re*c.re + c.im*c.im;
		if(denominator == 0){
			return INFINITY;
		}else if(denominator == Double.POSITIVE_INFINITY){
			return new Complex(0.0, 0.0);
		}
		return new Complex(
				(re*c.re + im*c.im)/denominator,
				(im*c.re - re*c.im)/denominator);
	}

	public Complex conjugate() {
		return new Complex(re, -im);
	}

	public double abs() {
		return (double)Math.sqrt(re*re + im*im);
	}

	public double arg() {
		return (double)Math.atan(im/re);
	}

	public String toString() {
		if (im >= 0)
			return "(" + re + " + " + im + "i" + ")";
		else
			return "(" + re + " - " + -im + "i" + ")";
	}

	public boolean isInfinity(){
		if(re == Double.POSITIVE_INFINITY || im == Double.POSITIVE_INFINITY ||re == Double.NEGATIVE_INFINITY || im == Double.NEGATIVE_INFINITY)
			return true;
		return false;
	}
	
	public boolean isZero(){
		if(re == 0 && im == 0)
			return true;
		else
			return false;
	}

	public static Complex div(Complex a, Complex b){
		return a.div(b);
	}

	public static Complex add(Complex a, Complex b){
		return a.add(b);
	}

	public static Complex mult(Complex a, Complex b){
		return a.mult(b);
	}

	public static Complex conjugate(Complex a){
		return new Complex(a.re(), -1 * a.im());
	}

	public static Complex sub(Complex a, Complex b){
		return a.sub(b);
	}

	public double dist(Complex p){
		return Math.sqrt(Math.pow(re - p.re(), 2) + Math.pow(im - p.im, 2));
	}

	public static double abs(Complex a){
		return Math.sqrt(a.re*a.re + a.im*a.im);
	}

	public static Complex sqrt(Complex c){
		if(c.im() > 0){
			return new Complex((double)(Math.sqrt(c.re() + Math.sqrt(c.re()*c.re() + c.im()*c.im())) / Math.sqrt(2)),
					(double)(Math.sqrt(-c.re() + Math.sqrt(c.re()*c.re() + c.im()*c.im())) / Math.sqrt(2)));

		}else if(c.im() < 0){
			return new Complex((double)(Math.sqrt(c.re() + Math.sqrt(c.re()*c.re() + c.im()*c.im())) / Math.sqrt(2)),
					(double)(-Math.sqrt(-c.re() + Math.sqrt(c.re()*c.re() + c.im()*c.im())) / Math.sqrt(2)));
		}

		if(c.re() < 0){
			return new Complex(0.0, (double)Math.sqrt(Math.abs(c.re())));
		}
		return new Complex((double)Math.sqrt(c.re()), 0.0);
	}
}
