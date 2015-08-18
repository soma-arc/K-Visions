package ui;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.geom.Point2D;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Timer;
import java.util.TimerTask;





import launcher.Launcher;
import midi.KorgNanoControl2;
import midi.MidiControlChangedListener;
import midi.MidiHandler;
import number.Complex;
import pointSeries.PointSeries;
import schottky.ui.SchottkyDisplay;
import explorer.LimitSetExplorer;
import explorer.TransformationExplorer;
import generator.Recipe;
import group.SL2C;

public class ParabolicCommutatorGroupsDisplay extends Display{
	private static ParabolicCommutatorGroupsDisplay instance = new ParabolicCommutatorGroupsDisplay();
	private ArrayList<Complex> points = new ArrayList<>();
	private ArrayList<Complex> nextPoints = new ArrayList<>();
	private Thread calcNextPointsThread = new Thread();
	private double magnification = 300;
	private int limitSetMaxLevel = 30;
	private int pointSeriesMaxLevel = 2;
	private double threshold = 0.005;
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
	private Timer timer;
	private boolean operateButterfly = true;
	private double butterflyRotation = 0.0;
	
	private ParabolicCommutatorGroupsDisplay(){
		super();
		t_a = new Complex(1.91, 0.05);
		t_b = new Complex(1.91, 0.05);
		
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
		
		shown();
	}
		
	@Override
	protected void shown(){
		super.shown();
		
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB1, new InitialHueTweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB2, new HueStepTweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.SLIDER1, new MagnificationTweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB3, new RotationTweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_S2, new PointSeriesLevelUpListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_M2, new PointSeriesLevelDownListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_MARKER_SET, new InitButtonListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_PREV_TRACK, new ChangeToSchottkyListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_R1, new ToggleOperateButterflyListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_MARKER_PREV, new PrevButtonListener());
		
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_CYCLE, new MidiControlChangedListener() {
			@Override
			public void changed(int controlPort, float value) {
				if(value == 127)
					prepareForSchottky();
			}
		});

		timer = new Timer();
		timer.schedule(new AnimationTask(), 0, 100);
	}
	
	private class InitialHueTweakListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			initialHue = value / 127;
			repaint();
		}
	}
	
	private class HueStepTweakListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			hueStep = 0.00001f + value / 5270000;
			repaint();
		}
	}
	
	private class PointSeriesLevelUpListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			if(value == 127){
				pointSeriesMaxLevel++;
				recalcPointSeries();
				repaint();
			}
		}
	}
	
	private class PointSeriesLevelDownListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			if(value == 127 && pointSeriesMaxLevel != 0){
				pointSeriesMaxLevel--;
				recalcPointSeries();
				repaint();
			}
		}
	}
	
	private class MagnificationTweakListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			magnification = 200 + 20 * value;
			repaint();
		}
	}
	
	private class RotationTweakListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			rotation = (value - 67)/12;
			repaint();
		}
	}
	
	private class InitButtonListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			if(value == 127){
				init();
			}
		}
	}
	
	private class ChangeToSchottkyListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			if(value == 127){
				Launcher.changeDisplayMode(DisplayMode.SCHOTTKY);
				SchottkyDisplay.getInstance().changedFromParabolic();
			}
		}
	}
	
	private class ToggleOperateButterflyListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			if(value == 127){
				operateButterfly = !operateButterfly;
				repaint();
			}
		}
	}
	
	@Override
	protected void hidden(){
		super.hidden();
		if(timer != null)
			timer.cancel();
	}
	
	private void init(){
		t_a = new Complex(1.91, 0.05);
		t_b = new Complex(1.91, 0.05);
		limitSetTranslation = Complex.ZERO;
		initPointSeries();
		recalc();
		operateButterfly = true;
		repaint();
	}
	
	private class PrevButtonListener implements MidiControlChangedListener{

		@Override
		public void changed(int controlPort, float value) {
			if(value == 127){
				t_a = new Complex(1.914123, 1.2223);
				t_b = new Complex(-2, 0.0);
				limitSetTranslation = Complex.ZERO;
				initPointSeries();
				recalc();
				operateButterfly = true;
				repaint();
			}
		}
		
	}
	
	private void prepareForSchottky(){
		t_a = new Complex(3);
		t_b = new Complex(3);
		rootButterfly = initialButterfly.copy();
		recalcPointSeries();
		recalc();
		repaint();
		operateButterfly = false;
	}
	
	int maxStep = 100;
	int step = 0;
	boolean stop = false;
	int stopCount = 0;
	int maxStop = 10;
	private class AnimationTask extends TimerTask{
		@Override
		public void run() {
			if(isClickedRightButton){
				Point mouse = getMousePosition();
				if(mouse == null) return;
				double mouseX = mouse.getX();
				double mouseY = mouse.getY();
				Complex currentPos = new Complex(-(mouseX - getWidth() / 2), -(mouseY- getHeight() / 2));
				limitSetTranslation = limitSetTranslation.add(currentPos.div(new Complex(10)));
				repaint();
				return;
			}

			if(rootButterfly.upperLeft.re() > 5){
				previousPos = new Complex(-5, rootButterfly.upperLeft.im());
				rootButterfly.translate(new Complex(-10));
				translation = new Complex(getWidth()/2 -5 * magnification, getHeight()/2 + rootButterfly.upperLeft.im() * magnification);

				points = nextPoints;
				if(t_a.re() < 3){
					t_a = t_a.add(0.05);
				}else if(t_b.re() < 3){
					t_b = t_b.add(0.1);
				}else{
					rootButterfly = initialButterfly.copy();
					recalcPointSeries();
					repaint();
					operateButterfly = false;
				}
				System.out.println(t_a +"  "+ t_b);
				calcNextPoints();
			}
		}
	}
	
	public static ParabolicCommutatorGroupsDisplay getInstance(){
		return instance;
	}
	
	public Complex limitSetTranslation = Complex.ZERO;
	public void paintComponent(Graphics g){
		requestFocus();
		Graphics2D g2 = (Graphics2D) g;
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
							RenderingHints.VALUE_ANTIALIAS_ON);
		g2.setColor(backgroundColor);
		g2.fillRect(0, 0, getWidth(), getHeight());

		translation = new Complex(getWidth() / 2, getHeight() / 2);
		if(isClickedRightButton){
			translation = translation.add(limitSetTranslation);
		}else if(operateButterfly){
			translation = translation.add(new Complex(-(rootButterfly.upperLeft.re() + rootButterfly.width/2) * magnification,
													  -(rootButterfly.upperLeft.im() - rootButterfly.height/2) * magnification));
		}

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
			g2.drawLine((int) (point.re() * magnification), (int) (point.im() * magnification), (int) (point2.re() * magnification), (int) (point2.im() * magnification));
			g2.drawLine((int) (point2.re() * magnification), (int) (point2.im() * magnification), (int) (point3.re() * magnification), (int) (point3.im() * magnification));

			hue += hueStep;
		}
	}
	
	private Point2D pointSeriesGradientPoint1 = new Point2D.Double(10, 10);
	private Point2D pointSeriesGradientPoint2 = new Point2D.Double(500, 10);
	private Color pointSeriesGradientColor1 = Color.green;
	private Color pointSeriesGradientColor2 = Color.blue;
	private boolean cyclic = true;
	private void drawPointSeries(Graphics2D g2){
		GradientPaint gp = new GradientPaint(pointSeriesGradientPoint1, pointSeriesGradientColor1, pointSeriesGradientPoint2, pointSeriesGradientColor2, cyclic);
		g2.setPaint(gp);
		if(pointSeriesDisplayMode == PointSeriesDisplayMode.SEARCH){
			
	    	for(PointSeries butterfly : butterflies){
	    		butterfly.draw(g2, magnification);
			}
	    	if(drawRootButterflyPosition){
	    		rootButterfly.drawBounds(g2, magnification);
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
		this.magnification = limitSetMagnification;
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
	
	public void calcNextPoints(){
		gens = Recipe.parabolicCommutatorGroup(t_a, t_b, isT_abPlus);

		if(calcNextPointsThread.isAlive()) return;
		calcNextPointsThread = new Thread(new CalcNextLimitSetTask());
		calcNextPointsThread.start();
	}
	
	public void recalcPointSeries(){
		if(rootButterfly == null) return;
		if(pointSeriesDisplayMode == PointSeriesDisplayMode.SEARCH){
			TransformationExplorer tExp = new TransformationExplorer(gens);
			butterflies = tExp.runBFS(pointSeriesMaxLevel, rootButterfly, magnification);
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
	
	private class CalcNextLimitSetTask implements Runnable{
		@Override
		public void run(){
			LimitSetExplorer lsExp = new LimitSetExplorer(gens);
			try {
				nextPoints = lsExp.runDFS(limitSetMaxLevel, threshold, calcLimitSetThread);
			} catch (InterruptedException e) {
				return;
			}
		}
	}
	
	private class KeyPressedAdapter extends KeyAdapter{
		@Override
		public void keyPressed(KeyEvent e){
			char key = e.getKeyChar();
			if(key == 'r'){
				rotation += Math.PI / 16;
				magnification += 100;
				if(magnification > 3000){
					t_b = t_b.add(new Complex(0.0, 0.01));
					recalc();
					rotation = 0;
					magnification = 100;
				}
				repaint();
			}
		}
	}

	private Complex previousPos = null;
	private boolean isClickedRightButton = false;
	private class MousePressedAdapter extends MouseAdapter{
		@Override
		public void mousePressed(MouseEvent e){
			if(e.getButton() == MouseEvent.BUTTON3){
				isClickedRightButton = true;
			}
			
			if(rootButterfly.isClicked(e.getX(), e.getY(), magnification, translation)){
				previousPos = new Complex((e.getX() - translation.re()) / magnification, (e.getY()- translation.im()) / magnification);
			}
		}

		@Override
		public void mouseReleased(MouseEvent e){
			previousPos = null;
			isClickedRightButton = false;
		}
	}

	private class MouseDraggedAdapter extends MouseMotionAdapter{
		@Override
		public void mouseDragged(MouseEvent e){
			if(previousPos != null && pointSeriesDisplayMode == PointSeriesDisplayMode.SEARCH){
				Complex currentPos = new Complex((e.getX() - translation.re()) / magnification, (e.getY()- translation.im()) / magnification);
				Complex diff = currentPos.sub(previousPos);
				rootButterfly.translate(diff);
				previousPos = currentPos;
				
				butterflyRotation = Math.atan2(diff.im(), diff.re());
				rootButterfly.rotate(butterflyRotation);
				
				TransformationExplorer tExp = new TransformationExplorer(gens);
				butterflies = tExp.runBFS(pointSeriesMaxLevel, rootButterfly, magnification);
				
				repaint();
			}
		}
	}
}
