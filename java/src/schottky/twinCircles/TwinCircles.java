package schottky.twinCircles;

import java.awt.Graphics;

import schottky.figure.Circle;
import number.Complex;

public class TwinCircles {
	private Circle c1, c2;

	public TwinCircles(Circle c1, Circle c2) {
		this.c1 = c1;
		this.c2 = c2;
	}
	
	public Circle getC1(){
		return c1;
	}
	
	public Circle getC2(){
		return c2;
	}
	
	public void drawPairArrow(Graphics g, double expansion){
		Complex z1 = c1.getP1().mult(expansion);
		Complex z2 = c1.getP2().mult(expansion);
		Complex z3 = c1.getP3().mult(expansion);
		Complex w1 = c2.getP1().mult(expansion);
		Complex w2 = c2.getP2().mult(expansion);
		Complex w3 = c2.getP3().mult(expansion);

		g.drawLine((int) z1.re(), (int) z1.im(), (int) w1.re(), (int) w1.im());
		g.drawLine((int) z2.re(), (int) z2.im(), (int) w2.re(), (int) w2.im());
		g.drawLine((int) z3.re(), (int) z3.im(), (int) w3.re(), (int) w3.im());
	}
}
