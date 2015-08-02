package opt.fuchs;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;
import java.util.ArrayList;

import opt.figure.Circle;
import group.SL2C;
import number.Complex;

public class ComplexProbability {
	private Complex origin;
	private Complex a0, a1, a2, x, y, z;
	private Complex p0, q0, r0;
	private Complex mirrorVecX, mirrorVecY, mirrorVecZ;
	private Circle cP0, cQ0, cR0;
	private Color color = Color.white;
	//‰~PQR‚ÌŒð“_
	private Complex aboveIntersectP0Q0, aboveIntersectQ0R0, aboveIntersectR0P1, aboveIntersectR_1P0;
	private Complex bottomIntersectP0Q0, bottomIntersectQ0R0, bottomIntersectR0P1, bottomIntersectR_1P0;
	
	public ComplexProbability(Complex a1, Complex a2, Complex origin){
		this.origin = origin;
		this.a1 = a1;
		this.a2 = a2;
		this.a0 = Complex.ONE.sub(a1).sub(a2);
		setData();
	}
	
	public void setQ(Complex Q){
		this.a1 = Q.sub(origin);
		this.a2 = r0.sub(Q);
		this.a0 = Complex.ONE.sub(a1).sub(a2);
		setData();
	}
	
	public void moveQ(Complex step){
		Complex Q = q0.add(step);
		this.a1 = Q.sub(origin);
		this.a2 = r0.sub(Q);
		this.a0 = Complex.ONE.sub(a1).sub(a2);
		setData();
	}
	
	public void setR(Complex R){
		this.a2 = R.sub(origin).sub(a1);
		this.a0 = Complex.ONE.sub(a1).sub(a2);
		setData();
	}
	
	public void moveR(Complex step){
		Complex R = r0.add(step);
		this.a2 = R.sub(origin).sub(a1);
		this.a0 = Complex.ONE.sub(a1).sub(a2);
		setData();
	}

	private void setData(){
		x = Complex.sqrt(Complex.ONE.div(a0.mult(a1)));
		y = Complex.sqrt(Complex.ONE.div(a1.mult(a2)));
		z = Complex.sqrt(Complex.ONE.div(a2.mult(a0)));
		
		mirrorVecX = Complex.I.div(x);
		mirrorVecY = Complex.I.div(y);
		mirrorVecZ = Complex.I.div(z);
		
		p0 = origin;
		q0 = p0.add(a1);
		r0 = q0.add(a2);

		cP0 = new Circle(p0, mirrorVecX.abs());
		cQ0 = new Circle(q0, mirrorVecY.abs());
		cR0 = new Circle(r0, mirrorVecZ.abs());
		Circle cP1 = new Circle(p0.add(Complex.ONE), cP0.getR());

		Complex[] intersectP0Q0 = Circle.getIntersections(cP0, cQ0);
		Complex[] intersectQ0R0 = Circle.getIntersections(cQ0, cR0);
		Complex[] intersectR0P1 = Circle.getIntersections(cR0, cP1);
		if(intersectP0Q0[0].im() > intersectP0Q0[1].im()){
			aboveIntersectP0Q0 = intersectP0Q0[0];
			bottomIntersectP0Q0 = intersectP0Q0[1];
		}else{
			aboveIntersectP0Q0 = intersectP0Q0[1];
			bottomIntersectP0Q0 = intersectP0Q0[0];
		}
		if(intersectQ0R0[0].im() > intersectQ0R0[1].im()){
			aboveIntersectQ0R0 = intersectQ0R0[0];
			bottomIntersectQ0R0 = intersectQ0R0[1];
		}else{
			aboveIntersectQ0R0 = intersectQ0R0[1];
			bottomIntersectQ0R0 = intersectQ0R0[0];
		}
		if(intersectR0P1[0].im() > intersectR0P1[1].im()){
			aboveIntersectR0P1 = intersectR0P1[0];
			bottomIntersectR0P1 = intersectR0P1[1];
		}else{
			aboveIntersectR0P1 = intersectR0P1[1];
			bottomIntersectR0P1 = intersectR0P1[0];
		}

		aboveIntersectR_1P0 = aboveIntersectR0P1.sub(Complex.ONE);
		bottomIntersectR_1P0 = bottomIntersectR0P1.sub(Complex.ONE);
	}

	public SL2C[] getGens(){
		SL2C[] gens = new SL2C[3];
		gens[0] = calcGen(p0, x);
		gens[1] = calcGen(q0, y);
		gens[2] = calcGen(r0, z);
		return gens;
	}
	
	private SL2C calcGen(Complex p, Complex x){
		return new SL2C(x.mult(p), x.mult(p.mult(p)).mult(-1).sub(Complex.ONE.div(x)),
						  x, x.mult(p).mult(-1));
	}
	
	public void draw(Graphics2D g2, double magnification, int width, int height){
		drawCircles(g2, magnification, width, height);
		drawPath(g2, magnification, width, height);
		drawVectors(g2, magnification, width, height);
	}
	
	public void drawTriangles(Graphics2D g2, double magnification, int width, int height){
		AffineTransform af = AffineTransform.getTranslateInstance(width/2 , height/2);
		g2.setTransform(af);
		g2.setColor(Color.white);
		drawTriangle(g2, magnification, p0, aboveIntersectP0Q0, q0);
		drawTriangle(g2, magnification, p0, bottomIntersectP0Q0, q0);

		drawTriangle(g2, magnification, q0, aboveIntersectQ0R0, r0);
		drawTriangle(g2, magnification, q0, bottomIntersectQ0R0, r0);

		drawTriangle(g2, magnification, r0, aboveIntersectR0P1, p0.add(Complex.ONE));
		drawTriangle(g2, magnification, r0, bottomIntersectR0P1, p0.add(Complex.ONE));

		drawTriangle(g2, magnification, r0.sub(Complex.ONE), aboveIntersectR_1P0, p0);
		drawTriangle(g2, magnification, r0.sub(Complex.ONE), bottomIntersectR_1P0, p0);
		
		g2.setTransform(new AffineTransform());
	}
	
	public ComplexProbability replaceAbove(){
		boolean aboveR_1P0Q0 = isIntersectAboveTriangle(aboveIntersectR_1P0, p0, aboveIntersectP0Q0);
		boolean aboveP0Q0R0 = isIntersectAboveTriangle(aboveIntersectP0Q0, q0, aboveIntersectQ0R0);
		boolean aboveQ0R0P1 = isIntersectAboveTriangle(aboveIntersectQ0R0, r0, aboveIntersectR0P1);
		
		if(aboveR_1P0Q0 && aboveP0Q0R0){
			double tLeft = segmentSection(aboveIntersectP0Q0, bottomIntersectP0Q0, aboveIntersectR_1P0, bottomIntersectR_1P0);
			double tRight = segmentSection(aboveIntersectP0Q0, bottomIntersectP0Q0, aboveIntersectQ0R0, bottomIntersectQ0R0);
			if(tLeft > tRight){
				return replace(0); 
			}else{
				return replace(1); 
			}
		}else if(aboveP0Q0R0 && aboveQ0R0P1){
			double tLeft = segmentSection(aboveIntersectQ0R0, bottomIntersectQ0R0, aboveIntersectP0Q0, bottomIntersectP0Q0);
			double tRight = segmentSection(aboveIntersectQ0R0, bottomIntersectQ0R0, aboveIntersectR0P1, bottomIntersectR0P1);
			if(tLeft > tRight){
				return replace(1); 
			}else{
				return replace(2); 
			}
		}else if(aboveR_1P0Q0 && aboveQ0R0P1){
			double tLeft = segmentSection(aboveIntersectP0Q0, bottomIntersectP0Q0, aboveIntersectR_1P0, bottomIntersectR_1P0);
			double tRight = segmentSection(aboveIntersectQ0R0, bottomIntersectQ0R0, aboveIntersectR0P1, bottomIntersectR0P1);
			if(tLeft > tRight){
				return replace(0); 
			}else{
				return replace(2); 
			}
		}else{
			if(aboveR_1P0Q0){
				return replace(0);
			}else if(aboveP0Q0R0){
				return replace(1);
			}else if(aboveQ0R0P1){
				return replace(2);
			}
		}
		return null;
	}
	
	public ComplexProbability replaceBottom(){
		boolean bottomR_1P0Q0 = isIntersectBottomTriangle(bottomIntersectR_1P0, p0, bottomIntersectP0Q0);
		boolean bottomP0Q0R0 = isIntersectBottomTriangle(bottomIntersectP0Q0, q0, bottomIntersectQ0R0);
		boolean bottomQ0R0P1 = isIntersectBottomTriangle(bottomIntersectQ0R0, r0, bottomIntersectR0P1);
		
		if(bottomR_1P0Q0 && bottomP0Q0R0){
			double tLeft = segmentSection(aboveIntersectP0Q0, bottomIntersectP0Q0, aboveIntersectR_1P0, bottomIntersectR_1P0);
			double tRight = segmentSection(aboveIntersectP0Q0, bottomIntersectP0Q0, aboveIntersectQ0R0, bottomIntersectQ0R0);
			if(tLeft < tRight){
				return replace(0); 
			}else{
				return replace(1); 
			}
		}else if(bottomP0Q0R0 && bottomQ0R0P1){
			double tLeft = segmentSection(aboveIntersectQ0R0, bottomIntersectQ0R0, aboveIntersectQ0R0, bottomIntersectQ0R0);
			double tRight = segmentSection(aboveIntersectQ0R0, bottomIntersectQ0R0, aboveIntersectR_1P0, bottomIntersectR_1P0);
			if(tLeft < tRight){
				return replace(1); 
			}else{
				return replace(2); 
			}
		}else if(bottomR_1P0Q0 && bottomQ0R0P1){
			double tLeft = segmentSection(aboveIntersectQ0R0, bottomIntersectQ0R0, aboveIntersectP0Q0, bottomIntersectP0Q0);
			double tRight = segmentSection(aboveIntersectQ0R0, bottomIntersectQ0R0, aboveIntersectR0P1, bottomIntersectR0P1);
			if(tLeft < tRight){
				return replace(0); 
			}else{
				return replace(2); 
			}
		}else{
			if(bottomR_1P0Q0){
				return replace(0);
			}else if(bottomP0Q0R0){
				return replace(1);
			}else if(bottomQ0R0P1){
				return replace(2);
			}
		}
		return null;
	}

	public double segmentSection(Complex a, Complex b, Complex c, Complex d){
		double a11 = b.re() - a.re();
		double a12 = c.re() - d.re();
		double b1 = c.re() - a.re();
		double a21 = b.im() - a.im();
		double a22 = c.im() - d.im();
		double b2 = c.im() - a.im();
		return (b1*a22-b2*a12)/(a11*a22-a21*a12);
	}

	public void drawTriangle(Graphics2D g2, double magnification, Complex p1, Complex p2, Complex p3){
		g2.drawLine((int)(p1.re() * magnification), (int)(p1.im() * magnification),
					(int)(p2.re() * magnification), (int) (p2.im() * magnification));
		g2.drawLine((int)(p2.re() * magnification), (int)(p2.im() * magnification),
					(int)(p3.re() * magnification), (int) (p3.im() * magnification));
	}

	public void drawControlPoints(Graphics2D g2, double magnification, int width, int height){
		AffineTransform af = AffineTransform.getTranslateInstance(width/2 , height/2);
		g2.setTransform(af);
		g2.setColor(Color.ORANGE);
		cQ0.drawCenter(g2, magnification);
		cR0.drawCenter(g2, magnification);
		g2.setTransform(new AffineTransform());
	}

	public void drawCircles(Graphics2D g2, double magnification, int width, int height){
		g2.setColor(color);
		int n = (int)(width / 2 / (1 * magnification)) + 2; 
		for(int i = -n ; i <= n ; i++){
			AffineTransform af = AffineTransform.getTranslateInstance(width/2 + i * magnification , height/2);
			g2.setTransform(af);
			cP0.draw(g2, magnification);
			cQ0.draw(g2, magnification);
			cR0.draw(g2, magnification);
			g2.setTransform(new AffineTransform());
		}
	}
	
	public void drawPath(Graphics2D g2, double magnification, int width, int height){
		g2.setColor(Color.white);

		int n = (int)(width / 2 / (1 * magnification)) + 2; 
		for(int i = -n ; i <= n ; i++){
			AffineTransform af = AffineTransform.getTranslateInstance(width/2 + (i + origin.re()) * magnification , origin.im() * magnification  + height/2);
			g2.setTransform(af);

			drawVectorFromOrigin(a1, g2, magnification);

			af.translate(a1.re() * magnification, a1.im() * magnification);
			g2.setTransform(af);

			drawVectorFromOrigin(a2, g2, magnification);

			af.translate(a2.re() * magnification, a2.im() * magnification);
			g2.setTransform(af);

			drawVectorFromOrigin(a0, g2, magnification);

			g2.setTransform(new AffineTransform());
		}
	}
	
	public void drawVectors(Graphics2D g2, double magnification, int width, int height){
		g2.setColor(Color.white);

		int n = (int)(width / 2 / (1 * magnification)) + 2; 
		for(int i = -n ; i <= n ; i++){
			AffineTransform af = AffineTransform.getTranslateInstance(width/2 + (i + origin.re()) * magnification , origin.im() * magnification  + height/2);
			g2.setTransform(af);

			drawVectorFromOrigin(mirrorVecX, g2, magnification);
			drawVectorFromOrigin(mirrorVecX.mult(-1), g2, magnification);

			af.translate(a1.re() * magnification, a1.im() * magnification);
			g2.setTransform(af);

			drawVectorFromOrigin(mirrorVecY, g2, magnification);
			drawVectorFromOrigin(mirrorVecY.mult(-1), g2, magnification);

			af.translate(a2.re() * magnification, a2.im() * magnification);
			g2.setTransform(af);

			drawVectorFromOrigin(mirrorVecZ, g2, magnification);
			drawVectorFromOrigin(mirrorVecZ.mult(-1), g2, magnification);

			g2.setTransform(new AffineTransform());
		}
	}

	public void drawVectorFromOrigin(Complex to, Graphics2D g2, double magnification){
		g2.drawLine(0, 0, (int) (to.re() * magnification), (int) (to.im() * magnification));
	}
	
	public ComplexProbability replace(int index){
		if(index == 1){
			return new ComplexProbability(a1.add(a2), a2.mult(a0).div(a1.add(a2)), p0);
		}else if(index == 2){
			return new ComplexProbability(a2.add(a0), a0.mult(a1).div(a2.add(a0)), q0);
		}else{// 0
			return new ComplexProbability(a0.add(a1), a1.mult(a2).div(a0.add(a1)), r0);
		}
	}

	public void setColor(Color color){
		this.color = color;
	}

	public boolean isClickedQ(double mouseX, double mouseY, double magnification){
		if(q0.mult(magnification).dist(new Complex(mouseX, mouseY)) < Circle.CENTER_POINT_R){
			return true;
		}
		return false;
	}

	public boolean isClickedR(double mouseX, double mouseY, double magnification){
		if(r0.mult(magnification).dist(new Complex(mouseX, mouseY)) < Circle.CENTER_POINT_R){
			return true;
		}
		return false;
	}

	public boolean isIntersectAboveTriangle(Complex leftIntersect, Complex m, Complex rightIntersect){
		return leftIntersect.sub(m).div(rightIntersect.sub(m)).im() < 0;
	}

	public boolean isIntersectBottomTriangle(Complex leftIntersect, Complex m, Complex rightIntersect){
		return leftIntersect.sub(m).div(rightIntersect.sub(m)).im() > 0;
	}
}
