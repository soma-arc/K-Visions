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
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.JPanel;

import launcher.Launcher;
import midi.KorgNanoControl2;
import midi.MidiControlChangedListener;
import midi.MidiHandler;
import opt.discriminator.DiscretenessDiscriminator;
import opt.explorer.OPTLimitSetExplorer;
import opt.fuchs.ComplexProbability;
import ui.Display;
import number.Complex;

public class OPTDisplay extends Display{
	private static OPTDisplay instance = new OPTDisplay();
	private ComplexProbability cp;
	private Complex baseQ, baseR;

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

	private Timer timer;
	private double rotation = 0.0;
	private boolean isRotating = false;
	private boolean cyclicDraw = false;
	private boolean drawLimitSetWithPoint = false;
	
	private OPTDisplay(){
		super();
		init();
		
		recalcLimitSet();
		
		addMouseListener(new MousePressedAdapter());
		addMouseMotionListener(new MouseDraggedAdapter());
		addKeyListener(new KeyPressedAdapter());
		
		discriminator = new DiscretenessDiscriminator(cp);
	}
	
	private void init(){
		Complex a1 = new Complex(0.25, 0);
		Complex a2 = new Complex(0.25, 0);
		cp = new ComplexProbability(a1, a2, Complex.ZERO);
		cp.moveQ(new Complex(0, 0.3));
		baseQ = cp.getQ();
		baseR = cp.getR();
		
		magnification = 500;
		rotation = 0;
		maxLevel = 30;
		epsilon = 0.0019;
		
		recalcLimitSet();
	}
	
	@Override
	protected void shown(){
		super.shown();
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB1, new InitialHueTweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB2, new HueStepTweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.SLIDER1, new MagnificationTweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB3, new RotationTweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB4, new MaxLevelListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_S1, new ToggleRotateListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_M1, new ToggleCyclicButtonListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_R1, new ToggleDrawWithPointListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_MARKER_SET, new InitButtonListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.SLIDER5, new TweakQYListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB5, new TweakQXListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.SLIDER6, new TweakRYListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB6, new TweakRXListener());
		timer = new Timer();
		timer.schedule(new AnimationTask(), 0, 10);
	}
	
	@Override
	protected void hidden(){
		super.hidden();
		if(timer != null)
			timer.cancel();
	}
	
	private class AnimationTask extends TimerTask{
		@Override
		public void run() {
			if(isRotating){
				rotation += 0.5;
				repaint();
			}
		}
	}
	
	private class ToggleCyclicButtonListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			if(value == 127)
				cyclicDraw = !cyclicDraw;
			repaint();
		}
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
			hueStep = 0.00001f + value / 1270000;
			repaint();
		}
	}
	
	private class MagnificationTweakListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			magnification = 500 + 20 * value;
			repaint();
		}
	}
	
	private class MaxLevelListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			maxLevel = (int)( 10 + value / (127 / 20));
			recalcLimitSet();
			repaint();
		}
	}
	
	private class ToggleRotateListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			if(value == 127){
				isRotating = !isRotating;
				if(isRotating == false){
					rotation = 0;
					repaint();
				}
			}
		}
	}
	
	private class ToggleDrawWithPointListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			if(value == 127){
				drawLimitSetWithPoint = !drawLimitSetWithPoint;
				repaint();
			}
		}
	}
	
	private class InitButtonListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			init();
			repaint();
		}
		
	}
	
	private class RotationTweakListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			rotation = value/4;
			repaint();
		}
	}
	
	double tweakedQY = 0;
	double tweakedQX = 0;
	private class TweakQYListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			double m = -2*(value - 68)/magnification;
			cp.setQ(baseQ.add(new Complex(tweakedQX, m)));
			recalcLimitSet();
			repaint();
			tweakedQY = m;
		}
	}
	private class TweakQXListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			double m = 2*(value - 68)/magnification;
			cp.setQ(baseQ.add(new Complex(m, tweakedQY)));
			recalcLimitSet();
			repaint();
			tweakedQX = m;
		}
	}
	
	double tweakedRY = 0;
	double tweakedRX = 0;
	private class TweakRYListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			double m = -2*(value - 68)/magnification;
			cp.setR(baseR.add(new Complex(tweakedRX, m)));
			recalcLimitSet();
			repaint();
			tweakedRY = m;
		}
	}
	private class TweakRXListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			double m = 2*(value - 68)/magnification;
			cp.setR(baseR.add(new Complex(m, tweakedRY)));
			recalcLimitSet();
			repaint();
			tweakedRX = m;
		}
	}
	
	public static OPTDisplay getInstance(){
		return instance;
	}

	int cycleStep = 8;
	public void paintComponent(Graphics g){
		Graphics2D g2 = (Graphics2D) g;
		g.setColor(Color.black);
		g.fillRect(0, 0, getWidth(), getHeight());
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
//		drawAxis(g2);
//		drawCurrentStatus(g2);
		
		AffineTransform originAf = AffineTransform.getTranslateInstance(getWidth() / 2, getHeight() / 2);
		g2.setTransform(originAf);
		
//		for(int n = -1 ; n <= 1 ; n++){
//		originAf.translate(n * magnification, 0);
//		g2.setTransform(originAf);
		g2.rotate(rotation);
		if(cyclicDraw){
			for(float r = 0 ; r <  Math.PI ; r += Math.PI/cycleStep){
				g2.rotate(r);
				drawLimitSet(g2);
			}
		}else{
			drawLimitSet(g2);
		}
		
//		}
//		cp.drawTriangles(g2, magnification, getWidth(), getHeight());

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
		for(int n = (int) (-Math.ceil((getWidth()/2)/magnification)) ; n <= (getWidth()/2)/magnification ; n++){
			if(drawLimitSetWithPoint){
				for(int i = 0 ; i < points.size()-1; i++){
					if(i >= points.size()) return;
					g2.setColor(Color.getHSBColor(hue, 1.0f, 1.0f));
					Complex point = points.get(i);
					g2.fillRect((int) ((point.re() + n) * magnification), (int) ((point.im()) * magnification),
							1, 1);
					hue += hueStep;
				}
			}else{
				for(int i = 0 ; i < points.size()-1; i++){
					if(i >= points.size()) return;
					g2.setColor(Color.getHSBColor(hue, 1.0f, 1.0f));
					Complex point = points.get(i);
					Complex point2 = points.get(i+1);

					g2.drawLine((int) ((point.re() + n) * magnification), (int) ((point.im()) * magnification),
							(int) ((point2.re() + n) * magnification), (int) ((point2.im() ) * magnification));

					hue += hueStep;
				}
			}
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
			points = dfs.run(maxLevel, epsilon, 100);
		}
	}

	private class MouseDraggedAdapter extends MouseMotionAdapter{
		@Override
		public void mouseDragged(MouseEvent e){
			double mouseX = e.getX() - getWidth() / 2;
			double mouseY = e.getY() - getHeight() / 2;
			Complex np = new Complex(mouseX/magnification, mouseY / magnification);
			if(isDraggingQ){
				baseQ = np;
				tweakedQX = 0;
				tweakedQY = 0;
				cp.setQ(np);
			}else if(isDraggingR){
				baseR = np;
				tweakedRX = 0;
				tweakedRY = 0;
				cp.setR(np);
			}
			recalcLimitSet();
//			discriminator.discriminate();
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
//			discriminator.discriminate();

			repaint();
		}
	}
}
