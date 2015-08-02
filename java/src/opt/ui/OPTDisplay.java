package opt.ui;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.geom.AffineTransform;
import java.text.DecimalFormat;
import java.util.ArrayList;

import javax.swing.JPanel;

import opt.discriminator.DiscretenessDiscriminator;
import opt.explorer.OPTLimitSetExplorer;
import opt.fuchs.ComplexProbability;
import number.Complex;

public class OPTDisplay extends JPanel{
	private static OPTDisplay instance = new OPTDisplay();
	private ComplexProbability cp, cp2;

	private double magnification = 500;
	private boolean isDraggingQ = false;
	private boolean isDraggingR = false;

	private int maxLevel = 30;
	private double epsilon = 0.0019;
	private static final double EPSILON_STEP = 0.0001;
	private ArrayList<Complex> points = new ArrayList<>();
	private OPTLimitSetExplorer dfs;
	
	private static final int FONT_SIZE = 30;
	private static final int STATUS_POS_X = 10;
	private static final int MAX_LEVEL_POS_Y = 30;
	private static final int EPSILON_POS_Y = 60;
	private static final int DISCRETE_POS_Y = 90;
	private double paramStep = 5.0 / magnification;
	private DecimalFormat epsilonFormatter = new DecimalFormat("0.00000");
	
	private DiscretenessDiscriminator discriminator;

	private boolean drawLimitSet = true;
	private boolean drawIsometricCircles = false;
	private boolean drawTriangles = false;

	private OPTDisplay(){
		Complex a1 = new Complex(0.25, 0);
		Complex a2 = new Complex(0.25, 0);
		cp = new ComplexProbability(a1, a2, Complex.ZERO);
		cp.setColor(Color.red);
		cp2 = cp.replace(1);
		cp2.setColor(Color.green);
		
		recalcLimitSet();
		
		addMouseListener(new MousePressedAdapter());
		addMouseMotionListener(new MouseDraggedAdapter());
		addKeyListener(new KeyPressedAdapter());
		
		discriminator = new DiscretenessDiscriminator(cp);
	}
	
	public static OPTDisplay getInstance(){
		return instance;
	}

	public void paintComponent(Graphics g){
		Graphics2D g2 = (Graphics2D) g;
		g.setColor(Color.black);
		g.fillRect(0, 0, getWidth(), getHeight());
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		drawAxis(g2);
		drawCurrentStatus(g2);
		
		AffineTransform originAf = AffineTransform.getTranslateInstance(getWidth() / 2, getHeight() / 2);
		g2.setTransform(originAf);
		drawLimitSet(g2);
		cp.drawTriangles(g2, magnification, getWidth(), getHeight());

		if(drawIsometricCircles){
			cp.drawCircles(g2, magnification, getWidth(), getHeight());
			for(ComplexProbability cpp : discriminator.getComplexProbabilities()){
				cpp.setColor(Color.red);
				cpp.drawCircles(g2, magnification, getWidth(), getHeight());
			}
		}
		if(drawTriangles){
			cp.drawPath(g2, magnification, getWidth(), getHeight());
			for(ComplexProbability cpp : discriminator.getComplexProbabilities()){
				cpp.setColor(Color.red);
				cpp.drawTriangles(g2, magnification, getWidth(), getHeight());
				cpp.drawPath(g2, magnification, getWidth(), getHeight());
				cpp.drawControlPoints(g2, magnification, getWidth(), getHeight());
			}
		}

		cp.drawControlPoints(g2, magnification, getWidth(), getHeight());
	}
	
	private float initialHue = 0.0f;
	private float hueStep = 0.00001f;
	private void drawLimitSet(Graphics2D g2){
		g2.setColor(Color.ORANGE);
		float hue = initialHue;
		for(int i = 0 ; i < points.size()-1; i++){
			g2.setColor(Color.getHSBColor(hue, 0.5f, 1.0f));
			Complex point = points.get(i);
			Complex point2 = points.get(i+1);
			g2.drawLine((int) (point.re() * magnification), (int) (point.im() * magnification), (int) (point2.re() * magnification), (int) (point2.im() * magnification));
			hue += hueStep;
		}
	}
	
	private void drawCurrentStatus(Graphics2D g2){
		g2.setFont(new Font("Times New Roman", Font.BOLD, FONT_SIZE));
		g2.setColor(Color.white);
		g2.drawString("max level "+ maxLevel, STATUS_POS_X, MAX_LEVEL_POS_Y);
		g2.drawString("epsilon "+ epsilonFormatter.format(epsilon), STATUS_POS_X, EPSILON_POS_Y);
		g2.drawString("isDiscrete : "+ discriminator.isDiscrete(), STATUS_POS_X, DISCRETE_POS_Y);
	}

	private void drawAxis(Graphics2D g2){
		g2.setColor(Color.gray);
		g2.drawLine(0, getHeight()/2, getWidth(), getHeight()/2); //x axis
		int n = (int)(getWidth() / 2 / (1 * magnification)); 
		for(int i = -n ; i <= n ; i++){
			g2.drawLine((int) magnification * i + getWidth()/2, 0, (int) magnification * i + getWidth()/2, getHeight());
		}
	}
	
	private class MousePressedAdapter extends MouseAdapter{
		public void mousePressed(MouseEvent e){
			double mouseX = e.getX() - getWidth() / 2;
			double mouseY = e.getY() - getHeight() / 2;
			if(cp.isClickedQ(mouseX , mouseY, magnification)){
				isDraggingQ = true;
			}else if(cp.isClickedR(mouseX, mouseY, magnification)){
				isDraggingR = true;
			}
		}
		
		public void mouseReleased(MouseEvent e){
			isDraggingQ = false;
			isDraggingR = false;
		}
	}
	
	private void recalcLimitSet(){
		if(drawLimitSet){
			dfs = new OPTLimitSetExplorer(cp.getGens());
			points = dfs.run(maxLevel, epsilon);
		}
	}

	private class MouseDraggedAdapter extends MouseMotionAdapter{
		@Override
		public void mouseDragged(MouseEvent e){
			double mouseX = e.getX() - getWidth() / 2;
			double mouseY = e.getY() - getHeight() / 2;
			Complex np = new Complex(mouseX/magnification, mouseY / magnification);
			if(isDraggingQ){
				cp.setQ(np);
			}else if(isDraggingR){
				cp.setR(np);
			}
			recalcLimitSet();
			discriminator.discriminate();
			repaint();
		}
	}

	private class KeyPressedAdapter extends KeyAdapter{
		@Override
		public void keyPressed(KeyEvent e){
			char keyChar = e.getKeyChar();
			if(e.getKeyChar() == '+'){
				maxLevel++;
				recalcLimitSet();
				repaint();
				return;
			}else if(keyChar == '-'){
				if(maxLevel != 1){
					maxLevel--;
					recalcLimitSet();
				}
				repaint();
				return;
			}else if(keyChar == 'p'){
				epsilon += EPSILON_STEP;
				recalcLimitSet();
				repaint();
				return;
			}else if(keyChar == 'n'){
				epsilon -= EPSILON_STEP;
				recalcLimitSet();
				repaint();
				return;
			}else if(keyChar == 'w'){
				cp.moveQ(new Complex(0, -paramStep));
			}else if(keyChar == 's'){
				cp.moveQ(new Complex(0, paramStep));
			}else if(keyChar == 'd'){
				cp.moveQ(new Complex(paramStep, 0));
			}else if(keyChar == 'a'){
				cp.moveQ(new Complex(-paramStep, 0));
			}else if(keyChar == 'i'){
				cp.moveR(new Complex(0, -paramStep));
			}else if(keyChar == 'k'){
				cp.moveR(new Complex(0, paramStep));
			}else if(keyChar == 'l'){
				cp.moveR(new Complex(paramStep, 0));
			}else if(keyChar == 'j'){
				cp.moveR(new Complex(-paramStep, 0));
			}
			recalcLimitSet();
			discriminator.discriminate();
			
			repaint();
		}
	}
}
