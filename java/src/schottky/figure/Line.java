package schottky.figure;

import number.Complex;

public class Line {
	private Complex p1, p2;
	private double a, b, c;
	private static final double EPSILON = 0.00001;
	public Line(Complex p1, Complex p2){
		this.p1 = p1;
		this.p2 = p2;
		
		double xDiff = p2.re() - p1.re();
		double yDiff = p2.im() - p1.im();
		if(Math.abs(xDiff) < EPSILON){
			//x = c
			a = 1;
			b = 0;
			c = p1.re();
		}else if(Math.abs(yDiff) < EPSILON){
			//y = c
			a = 0;
			b = 1;
			c = p1.im();
		}else{
			// y = ax + b
			a = yDiff / xDiff;
			b = p1.im() - p1.re() * a;
			c = 0;
		}
	}
	
	public Line(double a, double b, double c){
		this.a = a;
		this.b = b;
		this.c = c;
	}
	
	public double getA(){
		return a;
	}
	
	public double getB(){
		return b;
	}
	
	public double getC(){
		return c;
	}

	public double getX(double y){
		if(c == 0){
			return (y - b) / a;
		}else{
			//xŽ²‚É•½s‚ÌÛ‚àc‚ª•Ô‚Á‚Ä‚­‚é
			return c;
		}
	}
	
	public double getY(double x){
		if(c == 0){
			return a * x + b;
		}else{
			//yŽ²‚É•½s‚ÌÛ‚àc‚ª•Ô‚Á‚Ä‚­‚é
			return c;
		}
	}
	
	public Complex getIntersection(Line l){
		if(c == 0 && l.getC() == 0){
			double x1 = 1;
			double x2 = 5;
			double y1 = getY(x1);
			double y2 = getY(x2);

			double x3 = 4;
			double x4 = 8;
			double y3 = l.getY(x3);
			double y4 = l.getY(x4);

			double A = (y1 - y2) / (x1 - x2);
			double B = (x1 * y2 - x2 * y1) / (x1 - x2);
			double C = (y3 - y4) / (x3 - x4);
			double D = (x3 * y4 - x4 * y3) / (x3 - x4);
//			return new Complex(
//					(D - B) / (A - C), 
//					(A * D - B * C) / (A - c));
			double ksi, eta, delta;
			double ramda, mu;

			ksi = ( y4-y3 )*( x4-x1 ) - 
				( x4-x3 )*( y4-y1 );

			eta = ( x2-x1 )*( y4-y1 ) -
				( y2-y1 )*( x4-x1 );

			delta = ( x2-x1 )*( y4-y3 ) -
				( y2-y1 )*( x4-x3 );

			ramda = ksi / delta;
			mu    = eta / delta;

			//if ( ( ramda >= 0 && ramda <= 1 ) && ( mu >= 0 && mu <= 1 ) )
			//{
				double nx = x1 + ramda*( x2-x1 );
				double ny = y1 + ramda*( y2-y1 );
				return new Complex(nx, ny);
				//Œð“_‚Ì‚‚³
				//CrossP.z = L1.z + ramda*( L2.z-L1.z )
		}else{
			if(a == 1){
				return new Complex(c, l.getY(c));
			}else if(b == 1){
				return new Complex(l.getX(c), c);
			}else if(l.getA() == 1){
				return new Complex(l.getC(), getY(l.getC()));
			}else{
				return new Complex(getX(l.getC()), l.getC());
			}
		}
	}
	
	public String toString(){
		return a +", "+ b + ", "+ c;
	}
}
