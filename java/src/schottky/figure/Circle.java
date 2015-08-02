package schottky.figure;

import java.awt.Graphics;

import number.Complex;

public class Circle {
	private double r;
	private Complex center, p1, p2, p3;
	private int tag = -1; //index of gens
	
	public Circle(Complex center, double r){
		this.r = r;
		this.center = center;
		p1 = new Complex(center.re() , center.im() + r);
		p2 = new Complex(center.re() + r, center.im());
		p3 = new Complex(center.re() , center.im() - r);
	}
	
	public Circle(int tag, Complex center, double r){
		this.tag = tag;
		this.r = r;
		this.center = center;
	}

	public double getR() {
		return r;
	}

	public void setR(double r) {
		this.r = r;
	}

	public Complex getCenter() {
		return center;
	}
	
	public Complex getP1(){
		return p1;
	}
	
	public Complex getP2(){
		return p2;
	}
	
	public Complex getP3(){
		return p3;
	}

	public void setCenter(Complex center) {
		this.center = center;
	}
	
	public void setP1(Complex p1) {
		this.p1 = p1;
	}
	
	public void setP2(Complex p2) {
		this.p2 = p2;
	}
	
	public void setP3(Complex p3){
		this.p3 = p3;
	}
	
	public Line getTangentLine(Complex p){
		if(Double.compare(p.re(), r) == 0){
			return new Line(1, 0, p.re());
		}else if(Double.compare(p.im(), r) == 0){
			return new Line(0, 1, p.im());
		}else{
			return new Line(-p.re() / p.im(), r * r / p.im(),0);
		}
	}

	public String toString(){
		return "("+center.re()+","+center.im()+") r = "+ r;
	}
	
	public void setTag(int tag){
		this.tag = tag;
	}
	
	public int getTag(){
		return tag;
	}
	
	public void draw(Graphics g){
		g.drawOval((int) (center.re() -r), (int) (center.im() -r),(int) (2 * r), (int) (2 * r));
	}
	
	public void draw(Graphics g, double expansion){
		g.fillOval((int) ((center.re() -r) * expansion), (int) ((center.im() -r) * expansion),(int) (2 * r * expansion), (int) (2 * r * expansion));
	}
	
	private static final int CONTROL_POINT_RADIUS = 5;
	public void drawCenter(Graphics g, double expansion){
		g.fillOval((int) (center.re() * expansion) - CONTROL_POINT_RADIUS, (int) (center.im() * expansion) - CONTROL_POINT_RADIUS, CONTROL_POINT_RADIUS * 2, CONTROL_POINT_RADIUS * 2);
	}
	
	public void drawP1(Graphics g, double expansion){
		g.fillOval((int) (p1.re() * expansion) - CONTROL_POINT_RADIUS, (int) (p1.im() * expansion) - CONTROL_POINT_RADIUS, CONTROL_POINT_RADIUS * 2, CONTROL_POINT_RADIUS * 2);
	}
	
	public void drawP2(Graphics g, double expansion){
		g.fillOval((int) (p2.re() * expansion) - CONTROL_POINT_RADIUS, (int) (p2.im() * expansion) - CONTROL_POINT_RADIUS, CONTROL_POINT_RADIUS * 2, CONTROL_POINT_RADIUS * 2);
	}
	
	public void drawP3(Graphics g, double expansion){
		g.fillOval((int) (p3.re() * expansion) - CONTROL_POINT_RADIUS, (int) (p3.im() * expansion) - CONTROL_POINT_RADIUS, CONTROL_POINT_RADIUS * 2, CONTROL_POINT_RADIUS * 2);
	}
	
	public boolean isClickedCenter(int mouseX, int mouseY, double expansion){
		return isClicked(center, mouseX, mouseY, expansion);
	}
	
	public boolean isClickedP1(int mouseX, int mouseY, double expansion){
		return isClicked(p1, mouseX, mouseY, expansion);
	}
	
	public boolean isClickedP2(int mouseX, int mouseY, double expansion){
		return isClicked(p2, mouseX, mouseY, expansion);
	}
	
	public boolean isClickedP3(int mouseX, int mouseY, double expansion){
		return isClicked(p3, mouseX, mouseY, expansion);
	}

	public boolean isClickedCircumference(int mouseX, int mouseY, double expansion){
		return Math.abs(Math.sqrt(Math.pow(mouseX - center.re() * expansion, 2) + Math.pow(mouseY - center.im() * expansion, 2)) - r * expansion) < 5;
	}

	private boolean isClicked(Complex point, int mouseX, int mouseY, double expansion){
		return Math.sqrt(Math.pow(point.re() * expansion - mouseX, 2) + Math.pow(point.im() * expansion - mouseY, 2)) < 20;
	}
	
	public SelectedCircleElement getClickedPoint(int mouseX, int mouseY, double expansion){
		if(isClickedCenter(mouseX, mouseY, expansion)) return SelectedCircleElement.CENTER;
		if(isClickedP1(mouseX, mouseY, expansion)) return SelectedCircleElement.P1;
		if(isClickedP2(mouseX, mouseY, expansion)) return SelectedCircleElement.P2;
		if(isClickedP3(mouseX, mouseY, expansion)) return SelectedCircleElement.P3;
		if(isClickedCircumference(mouseX, mouseY, expansion)) return SelectedCircleElement.CIRCUMFERENCE;
		return null;
	}
}
