package ui;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import javax.imageio.ImageIO;
import javax.swing.JPanel;

import com.sun.xml.internal.bind.annotation.OverrideAnnotationOf;

import number.Complex;
import pointSeries.PointSeries;
import explorer.LimitSetExplorer;
import explorer.TransformationExplorer;
import generator.Recipe;
import group.SL2C;

public class ParabolicCommutatorGroupsDisplay extends JPanel{
	private static ParabolicCommutatorGroupsDisplay instance = new ParabolicCommutatorGroupsDisplay();
	private ArrayList<Complex> points = new ArrayList<>();
	private double limitSetMagnification = 300;
	private int limitSetMaxLevel = 30;
	private int pointSeriesMaxLevel = 4;
	private double threshold = 0.003;
	private SL2C[] gens;
	private PointSeries rootButterfly = null;
	private PointSeries stepButterfly, initialButterfly;
	private ArrayList<PointSeries> butterflies = new ArrayList<>();
	private Complex translation;
	private Complex t_a, t_b;
	private boolean isT_abPlus = true;
	private Thread calcLimitSetThread = new Thread();
	private PointSeriesDisplayMode pointSeriesDisplayMode = PointSeriesDisplayMode.SEARCH;
	private boolean drawRootButterflyPosition = false;
	private Color backgroundColor = Color.black;
	private double rotation = 0;
	
	private ParabolicCommutatorGroupsDisplay(){
		t_a = new Complex(-2, 0.0);
		t_b = new Complex(-2, 0.0);
		gens = Recipe.parabolicCommutatorGroup(t_a, t_b, isT_abPlus);

		LimitSetExplorer lsExp = new LimitSetExplorer(gens);
		points = lsExp.runDFS(limitSetMaxLevel, threshold);
		
		try {
			initialButterfly = PointSeries.readData(PointSeries.DATA_DIR_NAME+"butterfly.points").scale(0.125).translate(new Complex(0.5));
		} catch (IOException e) {
			e.printStackTrace();
		}

		stepButterfly = initialButterfly.copy(); 
		rootButterfly = initialButterfly.copy();

		recalcPointSeries();

		addMouseListener(new MousePressedAdapter());
		addMouseMotionListener(new MouseDraggedAdapter());
		addKeyListener(new KeyPressedAdapter());
		requestFocus();
	}
	
	public static ParabolicCommutatorGroupsDisplay getInstance(){
		return instance;
	}
	
	public void paintComponent(Graphics g){
		requestFocus();
		Graphics2D g2 = (Graphics2D) g;
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
                RenderingHints.VALUE_ANTIALIAS_ON);
		g2.setColor(backgroundColor);
		g2.fillRect(0, 0, getWidth(), getHeight());

		translation = new Complex(getWidth() / 2, getHeight() / 2);
		g2.translate((int)translation.re(), (int) translation.im());
		g2.rotate(rotation);
		drawLimitSet(g2);
		drawPointSeries(g2);
		
	}
	
	private float initialHue = 0.0f;
	private float hueStep = 0.00001f;
	private void drawLimitSet(Graphics2D g2){
		float hue = initialHue;
		for(int i = 0 ; i < points.size(); i+= 3){
			g2.setColor(Color.getHSBColor(hue, 1.0f, 1.0f));
			Complex point = points.get(i);
			Complex point2 = points.get(i+1);
			Complex point3 = points.get(i+2);
			g2.drawLine((int) (point.re() * limitSetMagnification), (int) (point.im() * limitSetMagnification), (int) (point2.re() * limitSetMagnification), (int) (point2.im() * limitSetMagnification));
			g2.drawLine((int) (point2.re() * limitSetMagnification), (int) (point2.im() * limitSetMagnification), (int) (point3.re() * limitSetMagnification), (int) (point3.im() * limitSetMagnification));

			hue += hueStep;
		}
	}
	
	private Point2D pointSeriesGradientPoint1 = new Point2D.Double(10, 10);
	private Point2D pointSeriesGradientPoint2 = new Point2D.Double(200, 10);
	private Color pointSeriesGradientColor1 = Color.green;
	private Color pointSeriesGradientColor2 = Color.blue;
	private boolean cyclic = true;
	private void drawPointSeries(Graphics2D g2){
		GradientPaint gp = new GradientPaint(pointSeriesGradientPoint1, pointSeriesGradientColor1, pointSeriesGradientPoint2, pointSeriesGradientColor2, cyclic);
		g2.setPaint(gp);

		if(pointSeriesDisplayMode == PointSeriesDisplayMode.SEARCH){
	    	for(PointSeries butterfly : butterflies){
				butterfly.draw(g2, limitSetMagnification);
			}
	    	if(drawRootButterflyPosition){
	    		rootButterfly.drawBounds(g2, limitSetMagnification);
	    	}
		}
	}

	public void setT_a(Complex t_a){
		this.t_a = t_a;
	}
	
	public void setT_b(Complex t_b){
		this.t_b = t_b;
	}
	
	public void setIsT_abPlus(boolean isT_abPlus){
		this.isT_abPlus = isT_abPlus;
	}
	
	public void setLimitSetMaxLevel(int maxLevel){
		this.limitSetMaxLevel = maxLevel;
	}
	
	public void setPointSeriesMaxLevel(int pointSeriesMaxLevel){
		this.pointSeriesMaxLevel = pointSeriesMaxLevel;
	}
	
	public void setThreshold(double threshold){
		this.threshold = threshold;
	}
	
	public void setLimitSetMagnification(int limitSetMagnification){
		this.limitSetMagnification = limitSetMagnification;
	}
	
	public void setPointSeriesDisplayMode(PointSeriesDisplayMode mode){
		this.pointSeriesDisplayMode = mode;
	}
	
	public void setDrawRootButterflyPosition(boolean drawRootButterflyPosition){
		this.drawRootButterflyPosition = drawRootButterflyPosition;
	}
	
	public void setBackgroundColor(Color backgroundColor){
		this.backgroundColor = backgroundColor;
		repaint();
	}

	public void setInitialHue(float initialHue){
		this.initialHue = initialHue;
		repaint();
	}
	
	public void setHueStep(float hueStep){
		this.hueStep = hueStep;
		repaint();
	}
	
	public Point2D getPointSeriesGradientPoint1() {
		return pointSeriesGradientPoint1;
	}

	public void setPointSeriesGradientPoint1(Point2D pointSeriesGradientPoint1) {
		this.pointSeriesGradientPoint1 = pointSeriesGradientPoint1;
	}

	public Point2D getPointSeriesGradientPoint2() {
		return pointSeriesGradientPoint2;
	}

	public void setPointSeriesGradientPoint2(Point2D pointSeriesGradientPoint2) {
		this.pointSeriesGradientPoint2 = pointSeriesGradientPoint2;
	}

	public Color getPointSeriesGradientColor1() {
		return pointSeriesGradientColor1;
	}

	public void setPointSeriesGradientColor1(Color pointSeriesGradientColor1) {
		this.pointSeriesGradientColor1 = pointSeriesGradientColor1;
		repaint();
	}

	public Color getPointSeriesGradientColor2() {
		return pointSeriesGradientColor2;
	}

	public void setPointSeriesGradientColor2(Color pointSeriesGradientColor2) {
		this.pointSeriesGradientColor2 = pointSeriesGradientColor2;
		repaint();
	}

	public boolean isCyclic() {
		return cyclic;
	}

	public void setCyclic(boolean cyclic) {
		this.cyclic = cyclic;
	}

	public void recalc(){
		gens = Recipe.parabolicCommutatorGroup(t_a, t_b, isT_abPlus);

		if(calcLimitSetThread.isAlive())
			stopCalculation();
		calcLimitSetThread = new Thread(new CalcLimitSetTask());
		calcLimitSetThread.start();

		recalcPointSeries();
	}
	
	public void recalcPointSeries(){
		if(rootButterfly == null) return;
		if(pointSeriesDisplayMode == PointSeriesDisplayMode.SEARCH){
			TransformationExplorer tExp = new TransformationExplorer(gens);
			butterflies = tExp.runBFS(pointSeriesMaxLevel, rootButterfly, limitSetMagnification);
		}else{
			
		}
		repaint();
	}
	
	public void stepPointSeries(int generatorIndex){
		if(generatorIndex < 0 || gens.length <= generatorIndex) return;
		stepButterfly = stepButterfly.transform(gens[generatorIndex]);
	}
	
	public void initPointSeries(){
		if(pointSeriesDisplayMode == PointSeriesDisplayMode.SEARCH){
			rootButterfly = initialButterfly.copy();
			recalcPointSeries();
		}
	}
	
	public void stopCalculation(){
		if(calcLimitSetThread.isAlive()){
			calcLimitSetThread.interrupt();
		}
	}
	
	private class CalcLimitSetTask implements Runnable{
		@Override
		public void run(){
			synchronized (points) {
				LimitSetExplorer lsExp = new LimitSetExplorer(gens);
				try {
					points = lsExp.runDFS(limitSetMaxLevel, threshold, calcLimitSetThread);
				} catch (InterruptedException e) {
					return;
				}
			}

			repaint();
		}
	}
	
	private class KeyPressedAdapter extends KeyAdapter{
		@Override
		public void keyPressed(KeyEvent e){
			char key = e.getKeyChar();
			if(key == 'r'){
				rotation += Math.PI / 16;
				limitSetMagnification += 100;
				if(limitSetMagnification > 3000){
					t_b = t_b.add(new Complex(0.0, 0.01));
					recalc();
					rotation = 0;
					limitSetMagnification = 100;
				}
				repaint();
			}
		}
	}

	private Complex previousPos = null;
	private class MousePressedAdapter extends MouseAdapter{
		@Override
		public void mousePressed(MouseEvent e){
			if(rootButterfly.isClicked(e.getX(), e.getY(), limitSetMagnification, translation)){
				previousPos = new Complex((e.getX() - translation.re()) / limitSetMagnification, (e.getY()- translation.im()) / limitSetMagnification);
			}
		}
		
		@Override
		public void mouseReleased(MouseEvent e){
			previousPos = null;
		}
	}

	private class MouseDraggedAdapter extends MouseMotionAdapter{
		@Override
		public void mouseDragged(MouseEvent e){
			if(previousPos != null && pointSeriesDisplayMode == PointSeriesDisplayMode.SEARCH){
				Complex currentPos = new Complex((e.getX() - translation.re()) / limitSetMagnification, (e.getY()- translation.im()) / limitSetMagnification);
				rootButterfly.translate(currentPos.sub(previousPos));
				previousPos = currentPos;
				
				TransformationExplorer tExp = new TransformationExplorer(gens);
				butterflies = tExp.runBFS(pointSeriesMaxLevel, rootButterfly, limitSetMagnification);
				repaint();
			}
		}
	}
}
