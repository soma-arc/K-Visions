package schottky.figure;

import group.TwoGensGroup;

import java.awt.Graphics;
import java.util.ArrayList;

import schottky.explorer.SchottkyExplorer;
import schottky.twinCircles.TwinCircles;
import number.Complex;

public class CommonCircle {
	Complex center;
	Complex a;//最初の円の中心を決めるパラメータ
	Complex p, q, r, s;//共通円上の4点
	double rad;
	
	public CommonCircle(Complex center, double rad){
		this.center = center;
		this.rad = rad;
		
		p = new Complex(center.re(), center.im() + rad);
		q = new Complex(center.re() - rad, center.im());
		r = new Complex(center.re(), center.im() - rad);
		s = new Complex(center.re() + rad, center.im());
		
		Complex pqMidPoint = new Complex((p.re() + q.re())/2,(p.im() + q.im())/2);
		double dist = center.dist(pqMidPoint);
		double ux = (pqMidPoint.re() - center.re()) / dist;
		double uy = (pqMidPoint.im() - center.im()) / dist;
		a = new Complex(center.re() + ux * rad * 1, center.im() + uy * rad * 1);
	}
	
	public Complex getCenter() {
		return center;
	}
	public void setCenter(Complex center) {
		this.center = center;
	}
	public Complex getP() {
		return p;
	}
	public void setP(Complex p) {
		this.p = p;
	}
	public Complex getQ() {
		return q;
	}
	public void setQ(Complex q) {
		this.q = q;
	}
	public Complex getR() {
		return r;
	}
	public void setR(Complex r) {
		this.r = r;
	}
	public Complex getS() {
		return s;
	}
	public void setS(Complex s) {
		this.s = s;
	}
	public double getRad() {
		return rad;
	}
	public void setRad(double rad) {
		this.rad = rad;
	}
	
	private TwinCircles tc1, tc2;
	Circle[] contactCircles = new Circle[4];
	public void calcContactCircles(){
		//System.out.println(Line.getMidperpendicular(Complex.ZERO, new Complex(1, 1)).getX(-0.5));
		Circle[] circles = new Circle[4];
		Complex pqMidPoint = new Complex((p.re() + q.re())/2,(p.im() + q.im())/2);
		//Line pqMid = new Line(center, pqMidPoint);//Line.getMidperpendicular(p, q);
		//System.out.println("pq mid"+ pqMid);
		//System.out.println("tan "+ getTangentLine(p));
		//a = getTangentLine(p).getIntersection(pqMid);
		
		//System.out.println("a"+ a);
		Line qrMid = new Line(center, new Complex((q.re() + r.re())/2,(q.im() + r.im())/2));
		Line rsMid = new Line(center, new Complex((r.re() + s.re())/2,(r.im() + s.im())/2));
		Line spMid = new Line(center, new Complex((s.re() + p.re())/2,(s.im() + p.im())/2));
		Line aq = new Line(a, q);
		Complex b = aq.getIntersection(qrMid);
		Line br = new Line(b, r);
		Complex c = br.getIntersection(rsMid);
		Line cs = new Line(c, s);
		Complex d = cs.getIntersection(spMid);

		//circles[2]のp1p3とが交差して移りあわないようにする
		circles[0] = new Circle(a, a.dist(p));
		circles[0].setP1(p);
		circles[0].setP2(calcNewP2(circles[0]));
		circles[0].setP3(q);
		//System.out.println(circles[0]);
		
		circles[1] = new Circle(b, b.dist(q));
		circles[1].setP1(q);
		circles[1].setP2(calcNewP2(circles[1]));
		circles[1].setP3(r);
		
		circles[2] = new Circle(c, c.dist(r));
		circles[2].setP1(s);
		circles[2].setP2(calcNewP2(circles[2]));
		circles[2].setP3(r);
		
		circles[3] = new Circle(d, d.dist(s));
		circles[3].setP1(p);
		circles[3].setP2(calcNewP2(circles[3]));
		circles[3].setP3(s);
		tc1 = new TwinCircles(circles[0], circles[2]);
		tc2 = new TwinCircles(circles[1], circles[3]);
		contactCircles = circles;
	}
	
	//共通円の中心側にP2をとる
	private Complex calcNewP2(Circle c){
		double d = center.dist(c.getCenter());
		double ux = (center.re() - c.getCenter().re()) / d;
		double uy = (center.im() - c.getCenter().im()) / d;
		return new Complex(c.getCenter().re() + ux * c.getR(), c.getCenter().im() + uy * c.getR());
	}
	
	public Circle[] getContactCircles(){
		return contactCircles;
	}
	
	public ArrayList<ArrayList<Circle>> runBFS(int maxLevel, double expansion){
		TwoGensGroup g = new TwoGensGroup(tc1, tc2);
		SchottkyExplorer bfs = new SchottkyExplorer(g);
		bfs.run(maxLevel, expansion);
		return bfs.getCircles();
	}
	
	public void drawArrow(Graphics g, double expansion){
		tc1.drawPairArrow(g, expansion);
		tc2.drawPairArrow(g, expansion);
	}
	
	private Circle getIntersectCircle(Complex c1, Complex c2){
		double c1x = c1.re();
		double c1y = c1.im();
		double c2x = c2.re();
		double c2y = c2.im();
		double nx = ((1 + c1x * c1x + c1y * c1y) * c2y - (1 + c2x * c2x + c2y * c2y) * c1y) / 2 * (c1x * c2y - c2x * c1y);
		double ny = ((1 + c2x * c2x + c2y * c2y) * c1x - (1 + c1x * c1x + c1y * c1y) * c2x) / 2 * (c1x * c2y - c2x * c1y);
		double rad = Math.sqrt(nx*nx + ny*ny -1);
		return new Circle(new Complex(nx, ny), rad);
	}
	
	public Line getTangentLine(Complex p){
		if(Double.compare(p.re(), rad) == 0){
			return new Line(1, 0, p.re());
		}else if(Double.compare(p.im(), rad) == 0){
			return new Line(0, 1, p.im());
		}else{
			double coeffX = p.re() - center.re();
			double coeffY = p.im() - center.im();
			return new Line(-coeffX / coeffY, (rad * rad + coeffX * center.re() + coeffY * center.im() ) / coeffY,0);
			//return new Line(-p.re() / p.im(), rad * rad / p.im(),0);
		}
	}
	
//	public Circle[] getContactCircles(){
//		Circle[] circles = new Circle[4];
//		circles[0] = getIntersectCircle(p, q);
//		circles[1] = getIntersectCircle(q, r);
//		circles[2] = getIntersectCircle(r, s);
//		circles[3] = getIntersectCircle(s, p);
//		return circles;
//	}
//	
//	private Circle getIntersectCircle(Complex c1, Complex c2){
//		double c1x = c1.re();
//		double c1y = c1.im();
//		double c2x = c2.re();
//		double c2y = c2.im();
//		double nx = ((1 + c1x * c1x + c1y * c1y) * c2y - (1 + c2x * c2x + c2y * c2y) * c1y) / 2 * (c1x * c2y - c2x * c1y);
//		double ny = ((1 + c2x * c2x + c2y * c2y) * c1x - (1 + c1x * c1x + c1y * c1y) * c2x) / 2 * (c1x * c2y - c2x * c1y);
//		double rad = Math.sqrt(nx*nx + ny*ny -1);
//		return new Circle(new Complex(nx, ny), rad);
//	}
	
	public void draw(Graphics g, double expansion){
		g.drawOval((int) ((center.re() -rad) * expansion), (int) ((center.im() -rad) * expansion),(int) (2 * rad * expansion), (int) (2 * rad * expansion));
	}
	
	private static final int CONTROL_POINT_RADIUS = 5;
	public void drawCenter(Graphics g, double expansion){
		g.fillOval((int) (center.re() * expansion) - CONTROL_POINT_RADIUS, (int) (center.im() * expansion) - CONTROL_POINT_RADIUS, CONTROL_POINT_RADIUS * 2, CONTROL_POINT_RADIUS * 2);
	}
	
	public void drawA(Graphics g, double expansion){
		g.fillOval((int) (a.re() * expansion) - CONTROL_POINT_RADIUS, (int) (a.im() * expansion) - CONTROL_POINT_RADIUS, CONTROL_POINT_RADIUS * 2, CONTROL_POINT_RADIUS * 2);
	}
	
	public void drawP(Graphics g, double expansion){
		g.fillOval((int) (p.re() * expansion) - CONTROL_POINT_RADIUS, (int) (p.im() * expansion) - CONTROL_POINT_RADIUS, CONTROL_POINT_RADIUS * 2, CONTROL_POINT_RADIUS * 2);
	}
	
	public void drawQ(Graphics g, double expansion){
		g.fillOval((int) (q.re() * expansion) - CONTROL_POINT_RADIUS, (int) (q.im() * expansion) - CONTROL_POINT_RADIUS, CONTROL_POINT_RADIUS * 2, CONTROL_POINT_RADIUS * 2);
	}
	
	public void drawR(Graphics g, double expansion){
		g.fillOval((int) (r.re() * expansion) - CONTROL_POINT_RADIUS, (int) (r.im() * expansion) - CONTROL_POINT_RADIUS, CONTROL_POINT_RADIUS * 2, CONTROL_POINT_RADIUS * 2);
	}
	
	public void drawS(Graphics g, double expansion){
		g.fillOval((int) (s.re() * expansion) - CONTROL_POINT_RADIUS, (int) (s.im() * expansion) - CONTROL_POINT_RADIUS, CONTROL_POINT_RADIUS * 2, CONTROL_POINT_RADIUS * 2);
	}
	
	public boolean isClickedCircumference(int mouseX, int mouseY, double expansion){
		return Math.abs(Math.sqrt(Math.pow(mouseX - center.re() * expansion, 2) + Math.pow(mouseY - center.im() * expansion, 2)) - rad * expansion) < 5;
	}
	
	private boolean isClicked(Complex point, int mouseX, int mouseY, double expansion){
		return Math.sqrt(Math.pow(point.re() * expansion - mouseX, 2) + Math.pow(point.im() * expansion - mouseY, 2)) < 20;
	}
	
	public SelectedCommonCircleElement getClickedPoint(int mouseX, int mouseY, double expansion){
		if(isClicked(center, mouseX, mouseY, expansion)) return SelectedCommonCircleElement.CENTER;
		if(isClicked(p, mouseX, mouseY, expansion)) return SelectedCommonCircleElement.P;
		if(isClicked(q, mouseX, mouseY, expansion)) return SelectedCommonCircleElement.Q;
		if(isClicked(r, mouseX, mouseY, expansion)) return SelectedCommonCircleElement.R;
		if(isClicked(s, mouseX, mouseY, expansion)) return SelectedCommonCircleElement.S;
		if(isClicked(a, mouseX, mouseY, expansion)) return SelectedCommonCircleElement.A;
		if(isClickedCircumference(mouseX, mouseY, expansion)) return SelectedCommonCircleElement.CIRCUMFERENCE;
		return null;
	}
	
	private SelectedCommonCircleElement selectedCommonCircleElem;
	private SelectedCircleElement selectedCircleElem;
	private Complex prevCenter;
	private Circle selectedCircle;
	public void mousePressed(int mouseX, int mouseY, double expansion){
		selectedCommonCircleElem = getClickedPoint(mouseX, mouseY, expansion);
		if(selectedCommonCircleElem != null){
			prevCenter = center;
			return;
		}
		for(Circle c : contactCircles){
			selectedCircleElem = c.getClickedPoint(mouseX, mouseY, expansion);
			if(selectedCircleElem != null){
				selectedCircle = c;
				prevCenter = c.getCenter();
				break;
			}
		}
	}
	
	public void mouseReleased(){
		selectedCommonCircleElem = null;
		selectedCircleElem = null;
	}
	
	public void mouseDragged(double mouseX, double mouseY, double expansion){
		if(selectedCommonCircleElem != null) {
			if(selectedCommonCircleElem == SelectedCommonCircleElement.CENTER){
				double nx = mouseX / expansion;
				double ny = mouseY / expansion;
				double diffX = nx - prevCenter.re();
				double diffY = ny - prevCenter.im();
				prevCenter = new Complex(nx, ny);
				setCenter(new Complex(nx, ny));
				setP(getP().add(new Complex(diffX, diffY)));
				setQ(getQ().add(new Complex(diffX, diffY)));
				setR(getR().add(new Complex(diffX, diffY)));
				setS(getS().add(new Complex(diffX, diffY)));
				a = a.add(new Complex(diffX, diffY));
			}else if(selectedCommonCircleElem == SelectedCommonCircleElement.P){
				double cX = center.re();
				double cY = center.im();
				double theta = Math.atan2(mouseY/expansion -cY, mouseX/expansion -cX);
				setP(new Complex(cX + rad * Math.cos(theta), cY + rad * Math.sin(theta)));
				Complex pqMidPoint = new Complex((p.re() + q.re())/2,(p.im() + q.im())/2);
				double dist = center.dist(pqMidPoint);
				double ux = (pqMidPoint.re() - center.re()) / dist;
				double uy = (pqMidPoint.im() - center.im()) / dist;
				a = new Complex(center.re() + ux * a.dist(center), center.im() + uy * a.dist(center));
			}else if(selectedCommonCircleElem == SelectedCommonCircleElement.Q){
				double cX = center.re();
				double cY = center.im();
				double theta = Math.atan2(mouseY/expansion -cY, mouseX/expansion -cX);
				setQ(new Complex(cX + rad * Math.cos(theta), cY + rad * Math.sin(theta)));
				Complex pqMidPoint = new Complex((p.re() + q.re())/2,(p.im() + q.im())/2);
				double dist = center.dist(pqMidPoint);
				double ux = (pqMidPoint.re() - center.re()) / dist;
				double uy = (pqMidPoint.im() - center.im()) / dist;
				a = new Complex(center.re() + ux * a.dist(center), center.im() + uy * a.dist(center));
			}else if(selectedCommonCircleElem == SelectedCommonCircleElement.R){
				double cX = center.re();
				double cY = center.im();
				double theta = Math.atan2(mouseY/expansion -cY, mouseX/expansion -cX);
				setR(new Complex(cX + rad * Math.cos(theta), cY + rad * Math.sin(theta)));
			}else if(selectedCommonCircleElem == SelectedCommonCircleElement.S){
				double cX = center.re();
				double cY = center.im();
				double theta = Math.atan2(mouseY/expansion -cY, mouseX/expansion -cX);
				setS(new Complex(cX + rad * Math.cos(theta), cY + rad * Math.sin(theta)));
			}else if(selectedCommonCircleElem == SelectedCommonCircleElement.A){
				recalcA(mouseX, mouseY, expansion);
			}else if(selectedCommonCircleElem == SelectedCommonCircleElement.CIRCUMFERENCE){
				double cX = center.re();
				double cY = center.im();
				rad = Math.sqrt(Math.pow(mouseX - cX * expansion, 2) + Math.pow(mouseY - cY * expansion, 2)) / expansion;
				double theta = Math.atan2(p.im() -cY, p.re() -cX);
				setP(new Complex(cX + rad * Math.cos(theta), cY + rad * Math.sin(theta)));
				theta = Math.atan2(q.im() -cY, q.re() -cX);
				setQ(new Complex(cX + rad * Math.cos(theta), cY + rad * Math.sin(theta)));
				theta = Math.atan2(r.im() -cY, r.re() -cX);
				setR(new Complex(cX + rad * Math.cos(theta), cY + rad * Math.sin(theta)));
				theta = Math.atan2(s.im() -cY, s.re() -cX);
				setS(new Complex(cX + rad * Math.cos(theta), cY + rad * Math.sin(theta)));
				Complex pqMidPoint = new Complex((p.re() + q.re())/2,(p.im() + q.im())/2);
				double dist = center.dist(pqMidPoint);
				double ux = (pqMidPoint.re() - center.re()) / dist;
				double uy = (pqMidPoint.im() - center.im()) / dist;
				a = new Complex(center.re() + ux * a.dist(center), center.im() + uy * a.dist(center));
			}
			calcContactCircles();
		}else if(selectedCircleElem != null){
			if(selectedCircleElem == SelectedCircleElement.P2){
				double r = selectedCircle.getR();
				double cX = selectedCircle.getCenter().re();
				double cY = selectedCircle.getCenter().im();
				double theta = Math.atan2(mouseY/expansion -cY, mouseX/expansion -cX);
				selectedCircle.setP2(new Complex(cX + r * Math.cos(theta), cY + r * Math.sin(theta)));
			}
		}
	}
	
	private void recalcA(double mouseX, double mouseY, double expansion){
		Complex pqMidPoint = new Complex((p.re() + q.re())/2,(p.im() + q.im())/2);
		Line pqMid = new Line(center, pqMidPoint);
		a = new Complex(mouseX / expansion, pqMid.getY(mouseX / expansion));
	}
}
