package opt.figure;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

import number.Complex;

public class Circle {
	private Complex center;
	private double r;
	public static final int CENTER_POINT_R = 5;
	
	public Circle(Complex center, double r){
		this.center = center;
		this.r = r;

	}
	
	public void draw(Graphics2D g2, double magnification){
		g2.drawOval((int) ((center.re() - r) * magnification), (int) ((center.im() - r) * magnification), 
				   (int) (2*r * magnification), (int) (2*r * magnification));
		drawCenter(g2, magnification);
	}
	
	public void drawCenter(Graphics g, double magnification){
		g.fillOval((int)(center.re() * magnification - CENTER_POINT_R),
				   (int)(center.im() * magnification - CENTER_POINT_R), 
				   2 * CENTER_POINT_R, 2 * CENTER_POINT_R);
	}
	
	public Complex getCenter(){
		return center;
	}
	
	public double getR(){
		return r;
	}
	
	public String toString(){
		return "{"+ center +" r = "+ r +"}";
	}
	
	public static Complex[] getIntersections(Circle c1, Circle c2){
		Complex[] intersections = new Complex[2];
		double x1 = c2.getCenter().re() - c1.getCenter().re();
		double y1 = c2.getCenter().im() - c1.getCenter().im();
		double r1 = c1.getR();
		double r2 = c2.getR();
		double a = (x1 * x1 + y1 * y1 + r1 * r1 - r2 * r2) / 2;
		
		double denom = x1 * x1 + y1 * y1;
		double sqrtElem = Math.sqrt(denom * r1 * r1 - a * a);
		
		double re1 = (a * x1 + y1 * sqrtElem) / denom;
		double im1 = (a * y1 - x1 * sqrtElem) / denom;
		intersections[0] = new Complex(re1 + c1.getCenter().re(), im1 + c1.getCenter().im());
		
		double re2 = (a * x1 - y1 * sqrtElem) / denom;
		double im2 = (a * y1 + x1 * sqrtElem) / denom;
		intersections[1] = new Complex(re2 + c1.getCenter().re(), im2 + c1.getCenter().im());
		
		return intersections;
	}
}
