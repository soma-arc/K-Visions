package schottky.ui;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;

import javax.swing.JOptionPane;
import javax.swing.JPanel;

import schottky.figure.Circle;
import schottky.figure.CommonCircle;
import schottky.figure.SelectedCircleElement;
import schottky.figure.SelectedCommonCircleElement;
import schottky.twinCircles.TwinCircles;
import number.Complex;

public class SchottkyDisplay extends JPanel implements MouseListener, MouseMotionListener{
	private static SchottkyDisplay instance = new SchottkyDisplay();
	private ArrayList<Complex> points = new ArrayList<Complex>();
	//private ArrayList<ArrayList<Circle>> circles = new ArrayList<ArrayList<Circle>>();
	private ArrayList<Circle> circles = new ArrayList<Circle>();
	private Circle selectedCircle = null;
	private SelectedCircleElement selectedCircleElem = null;
	private Circle c1, c2, c3, c4;
	private TwinCircles tc1, tc2;
	private ArrayList<ArrayList<Circle>> results = new ArrayList<ArrayList<Circle>>();
	private CommonCircle commonCircle;
	private SelectedCommonCircleElement selectedCommonCircleElem = null;
	private int maxLevel = 3;
	private double epsilon = 0.02;
	private SchottkyDisplay(){
		c1 = new Circle(new Complex(-2, -2), 1);
		c2 = new Circle(new Complex(1, 0), 0.6);
		c3 = new Circle(new Complex(1.2, -2), 1);
		c4 = new Circle(new Complex(-1.1, 1), 0.8);

		commonCircle = new CommonCircle(Complex.ZERO, 100);
		addKeyListener(new KeyPressedAdapter());
		addMouseListener(this);
		addMouseMotionListener(this);
		setFocusable(true);
		requestFocusInWindow();
		commonCircle.calcContactCircles();
	}
	
	public static SchottkyDisplay getInstance(){
		return instance;
	}
	
	private double expansion = 1;
	public void paintComponent(Graphics g){
		Graphics2D g2 = (Graphics2D)g;
		  g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
		                      RenderingHints.VALUE_ANTIALIAS_ON);
		
		g.setColor(Color.BLACK);
		g.fillRect(0, 0, this.getWidth(), this.getHeight());
		g.translate(this.getWidth() / 2, this.getHeight() / 2);
		
		
		int co = 0;
		for(ArrayList<Circle> circlesList : commonCircle.runBFS(maxLevel, epsilon)){
			g.setColor(new Color(255, co, 0));
			co += 80;
			if(co > 255) co = 50;
			for(Circle c : circlesList){
				c.draw(g, expansion);
			}
		}
		
		
		g.setColor(Color.white);
		//commonCircle.drawArrow(g, expansion);
		for(Circle c: commonCircle.getContactCircles()){
			g.setColor(Color.white);
			c.drawP2(g, expansion);
		}
		commonCircle.drawA(g2, expansion);
		
		g.setColor(Color.blue);
		commonCircle.draw(g2, expansion);
		g.setColor(Color.RED);
		commonCircle.drawCenter(g, expansion);
		commonCircle.drawP(g, expansion);
		commonCircle.drawQ(g, expansion);
		commonCircle.drawR(g, expansion);
		commonCircle.drawS(g, expansion);
	}

	@Override
	public void mouseClicked(MouseEvent arg0) {
	}

	@Override
	public void mouseEntered(MouseEvent arg0) {
	}

	@Override
	public void mouseExited(MouseEvent arg0) {
	}

	private Complex prevCenter = null;
	@Override
	public void mousePressed(MouseEvent e) {
		int mouseX = e.getX() - this.getWidth()/2;
		int mouseY = e.getY() - this.getHeight()/2;
		
		commonCircle.mousePressed(mouseX, mouseY, expansion);
		
		for(Circle c : circles){
			selectedCircleElem = c.getClickedPoint(mouseX, mouseY, expansion);
			if(selectedCircleElem != null){
				selectedCircle = c;
				prevCenter = c.getCenter();
				break;
			}
		}
	}

	@Override
	public void mouseReleased(MouseEvent arg0) {
		selectedCircle = null;
		selectedCircleElem = null;
		commonCircle.mouseReleased();
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		double mouseX = e.getX() - this.getWidth()/2;
		double mouseY = e.getY() - this.getHeight()/2;
		
		commonCircle.mouseDragged(mouseX, mouseY, expansion);
		
		if(selectedCircle != null){
			if(selectedCircleElem == SelectedCircleElement.CENTER){
				double nx = mouseX / expansion;
				double ny = mouseY / expansion;
				double diffX = nx - prevCenter.re();
				double diffY = ny - prevCenter.im();
				prevCenter = new Complex(nx, ny);
				selectedCircle.setCenter(new Complex(nx, ny));
				selectedCircle.setP1(selectedCircle.getP1().add(new Complex(diffX, diffY)));
				selectedCircle.setP2(selectedCircle.getP2().add(new Complex(diffX, diffY)));
				selectedCircle.setP3(selectedCircle.getP3().add(new Complex(diffX, diffY)));
				
			}else if(selectedCircleElem == SelectedCircleElement.P1){
				double r = selectedCircle.getR();
				double cX = selectedCircle.getCenter().re();
				double cY = selectedCircle.getCenter().im();
				double theta = Math.atan2(mouseY/expansion -cY, mouseX/expansion -cX);
				selectedCircle.setP1(new Complex(cX + r * Math.cos(theta), cY + r * Math.sin(theta)));
			}else if(selectedCircleElem == SelectedCircleElement.P2){
				double r = selectedCircle.getR();
				double cX = selectedCircle.getCenter().re();
				double cY = selectedCircle.getCenter().im();
				double theta = Math.atan2(mouseY/expansion -cY, mouseX/expansion -cX);
				selectedCircle.setP2(new Complex(cX + r * Math.cos(theta), cY + r * Math.sin(theta)));
			}else if(selectedCircleElem == SelectedCircleElement.P3){
				double r = selectedCircle.getR();
				double cX = selectedCircle.getCenter().re();
				double cY = selectedCircle.getCenter().im();
				double theta = Math.atan2(mouseY/expansion -cY, mouseX/expansion -cX);
				selectedCircle.setP3(new Complex(cX + r * Math.cos(theta), cY + r * Math.sin(theta)));
			}else if(selectedCircleElem == SelectedCircleElement.CIRCUMFERENCE){
				double cX = selectedCircle.getCenter().re();
				double cY = selectedCircle.getCenter().im();
				double r = Math.sqrt(Math.pow(mouseX - cX * expansion, 2) + Math.pow(mouseY - cY * expansion, 2)) / expansion;
				selectedCircle.setR(r);
				double theta = Math.atan2(selectedCircle.getP1().im() -cY, selectedCircle.getP1().re() -cX);
				selectedCircle.setP1(new Complex(cX + r * Math.cos(theta), cY + r * Math.sin(theta)));
				theta = Math.atan2(selectedCircle.getP2().im() -cY, selectedCircle.getP2().re() -cX);
				selectedCircle.setP2(new Complex(cX + r * Math.cos(theta), cY + r * Math.sin(theta)));
				theta = Math.atan2(selectedCircle.getP3().im() -cY, selectedCircle.getP3().re() -cX);
				selectedCircle.setP3(new Complex(cX + r * Math.cos(theta), cY + r * Math.sin(theta)));
			}
		}

		repaint();
	}
	class KeyPressedAdapter extends KeyAdapter{
		@Override
		public void keyPressed(KeyEvent e){
			if(e.getKeyCode() == KeyEvent.VK_PLUS || e.getKeyChar() == '+'){
				maxLevel++;
				repaint();
			}else if(e.getKeyCode() == KeyEvent.VK_MINUS){
				if(maxLevel != 0){
					maxLevel--;
					repaint();
				}
			}
		}
	}
	@Override
	public void mouseMoved(MouseEvent arg0) {
	}
}
