package schottky.ui;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseMotionListener;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.JOptionPane;
import javax.swing.JPanel;

import launcher.Launcher;
import midi.KorgNanoControl2;
import midi.MidiControlChangedListener;
import midi.MidiHandler;
import schottky.figure.Circle;
import schottky.figure.CommonCircle;
import schottky.figure.SelectedCircleElement;
import schottky.figure.SelectedCommonCircleElement;
import schottky.twinCircles.TwinCircles;
import ui.Display;
import number.Complex;

public class SchottkyDisplay extends Display{
	private static SchottkyDisplay instance = new SchottkyDisplay();
	
	private Timer timer;
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
	private int maxLevel = 10;
	private double epsilon = 0.02;
	private double rotation = 0.0;
	private boolean isRotating = false;
	private boolean traceLocus = false;
	private SchottkyDisplay(){
		super();
		c1 = new Circle(new Complex(-2, -2), 1);
		c2 = new Circle(new Complex(1, 0), 0.6);
		c3 = new Circle(new Complex(1.2, -2), 1);
		c4 = new Circle(new Complex(-1.1, 1), 0.8);

		commonCircle = new CommonCircle(Complex.ZERO, 100);
		addKeyListener(new KeyPressedAdapter());
		addMouseListener(new MousePressedAdapter());
		addMouseMotionListener(new MouseDraggedAdapter());
		setFocusable(true);
		requestFocusInWindow();
		commonCircle.calcContactCircles();
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB1, new InitialHueTweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB2, new HueStepTweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.SLIDER1, new MagnificationTweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB3, new RotationTweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB4, new PointATweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB5, new MaxLevelTweakListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.KNOB6, new LocusTraceLevelTweakListener());
		
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_M1, new ToggleTraceLocusListener());
		
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_S1, new ToggleRotateListener());
		timer = new Timer();
		timer.schedule(new AnimationTask(), 0, 10);
		recalcCircles();
	}
	
	private class MaxLevelTweakListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			maxLevel = (int)( 1 + value / (127 / 13));
			recalcCircles();
			repaint();
		}
	}
	
	private class LocusTraceLevelTweakListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			traceLocusLevel = (int)( 1 + value / (127 / 13));
			recalcCircles();
			repaint();
		}
	}
	
	private class PointATweakListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			commonCircle.recalcA((-51+ -5*value) * magnification, 0, magnification);
			commonCircle.calcContactCircles();
			recalcCircles();
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
			hueStep = 0.1f + value / 127;
			repaint();
		}
	}
	
	private class MagnificationTweakListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			magnification = 0.1 + value/4;
			recalcCircles();
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
	
	private class ToggleRotateListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			if(value == 127)
				isRotating = !isRotating;
		}
	}
	
	private class ToggleTraceLocusListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			if(value == 127)
				traceLocus = !traceLocus;
		}
	}
	
	private class AnimationTask extends TimerTask{
		@Override
		public void run() {
			if(isRotating || traceLocus){
				if(isRotating){
					rotation += 0.5;
				}
				repaint();
			}
		}
	}
	
	public static SchottkyDisplay getInstance(){
		return instance;
	}
	int locusIndex = 0;
	private double magnification = 1;
	private float initialHue = 0.0f;
	private float hueStep = 0.1f;
	private ArrayList<ArrayList<Circle>> circlesList = new ArrayList<>();
	public void paintComponent(Graphics g){
		Graphics2D g2 = (Graphics2D)g;
		  g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
		                      RenderingHints.VALUE_ANTIALIAS_ON);
		
		g2.setColor(Color.BLACK);
		g2.fillRect(0, 0, this.getWidth(), this.getHeight());

		if(traceLocus && circlesList.size() >= traceLocusLevel){
			ArrayList<Circle> list = circlesList.get(traceLocusLevel);

			Circle ci = list.get(locusIndex);
			g2.translate(this.getWidth() / 2 + ci.getCenter().re() * magnification, this.getHeight() / 2 + ci.getCenter().im() * magnification);
			locusIndex++;
			if(list.size() == locusIndex) locusIndex = 0;
		}else{
			g2.translate(this.getWidth() / 2 , this.getHeight() / 2);
		}

		g2.rotate(rotation);

		float hue = initialHue;
		for(ArrayList<Circle> circles : circlesList){
			g.setColor(Color.getHSBColor(hue, 1.0f, 1.0f));
			hue += hueStep;
			for(Circle c : circles){
				c.draw(g, magnification);
			}
		}
		
	}

	private Complex prevCenter = null;
	private class MousePressedAdapter extends MouseAdapter{
		@Override
		public void mousePressed(MouseEvent e) {
			int mouseX = e.getX() - SchottkyDisplay.this.getWidth()/2;
			int mouseY = e.getY() - SchottkyDisplay.this.getHeight()/2;
			
			commonCircle.mousePressed(mouseX, mouseY, magnification);
			
			for(Circle c : circles){
				selectedCircleElem = c.getClickedPoint(mouseX, mouseY, magnification);
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
	}
	
	private class MouseDraggedAdapter extends MouseMotionAdapter{
		@Override
		public void mouseDragged(MouseEvent e) {
			double mouseX = e.getX() - SchottkyDisplay.this.getWidth()/2;
			double mouseY = e.getY() - SchottkyDisplay.this.getHeight()/2;
			
			commonCircle.mouseDragged(mouseX, mouseY, magnification);
			
			if(selectedCircle != null){
				if(selectedCircleElem == SelectedCircleElement.CENTER){
					double nx = mouseX / magnification;
					double ny = mouseY / magnification;
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
					double theta = Math.atan2(mouseY/magnification -cY, mouseX/magnification -cX);
					selectedCircle.setP1(new Complex(cX + r * Math.cos(theta), cY + r * Math.sin(theta)));
				}else if(selectedCircleElem == SelectedCircleElement.P2){
					double r = selectedCircle.getR();
					double cX = selectedCircle.getCenter().re();
					double cY = selectedCircle.getCenter().im();
					double theta = Math.atan2(mouseY/magnification -cY, mouseX/magnification -cX);
					selectedCircle.setP2(new Complex(cX + r * Math.cos(theta), cY + r * Math.sin(theta)));
				}else if(selectedCircleElem == SelectedCircleElement.P3){
					double r = selectedCircle.getR();
					double cX = selectedCircle.getCenter().re();
					double cY = selectedCircle.getCenter().im();
					double theta = Math.atan2(mouseY/magnification -cY, mouseX/magnification -cX);
					selectedCircle.setP3(new Complex(cX + r * Math.cos(theta), cY + r * Math.sin(theta)));
				}else if(selectedCircleElem == SelectedCircleElement.CIRCUMFERENCE){
					double cX = selectedCircle.getCenter().re();
					double cY = selectedCircle.getCenter().im();
					double r = Math.sqrt(Math.pow(mouseX - cX * magnification, 2) + Math.pow(mouseY - cY * magnification, 2)) / magnification;
					selectedCircle.setR(r);
					double theta = Math.atan2(selectedCircle.getP1().im() -cY, selectedCircle.getP1().re() -cX);
					selectedCircle.setP1(new Complex(cX + r * Math.cos(theta), cY + r * Math.sin(theta)));
					theta = Math.atan2(selectedCircle.getP2().im() -cY, selectedCircle.getP2().re() -cX);
					selectedCircle.setP2(new Complex(cX + r * Math.cos(theta), cY + r * Math.sin(theta)));
					theta = Math.atan2(selectedCircle.getP3().im() -cY, selectedCircle.getP3().re() -cX);
					selectedCircle.setP3(new Complex(cX + r * Math.cos(theta), cY + r * Math.sin(theta)));
				}
			}

			recalcCircles();
			repaint();
		}
	}


	class KeyPressedAdapter extends KeyAdapter{
		@Override
		public void keyPressed(KeyEvent e){
			if(e.getKeyCode() == KeyEvent.VK_PLUS || e.getKeyChar() == '+'){
				maxLevel++;
				recalcCircles();
				repaint();
			}else if(e.getKeyCode() == KeyEvent.VK_MINUS){
				if(maxLevel != 0){
					maxLevel--;
					recalcCircles();
					repaint();
				}
			}else if(e.getKeyCode() == KeyEvent.VK_P){
				initialHue += 0.1;
				repaint();
			}else if(e.getKeyCode() == KeyEvent.VK_N){
				initialHue -= 0.1;
				repaint();
			}else if(e.getKeyCode() == KeyEvent.VK_C){
				Launcher.nextCard();
			}else if(e.getKeyCode() == KeyEvent.VK_ESCAPE){
				Launcher.setWindowMode();
			}else if(e.getKeyCode() == KeyEvent.VK_Q){
				Launcher.setFullScreenMode();
			}
		}
	}
	
	int traceLocusLevel = 5;
	private void recalcCircles(){
		circlesList = commonCircle.runBFS(maxLevel, 1.0/magnification);
		if(maxLevel <= traceLocusLevel){
			traceLocusLevel = circlesList.size() -1;
		}

		ArrayList<Circle> list = circlesList.get(circlesList.size() -1);
		if(list != null){
			Collections.sort(list, new Comparator<Circle>(){
				@Override
				public int compare(Circle o1, Circle o2) {
					double rad1 = Math.atan2(o1.getCenter().im(), o1.getCenter().re());
					double rad2 = Math.atan2(o2.getCenter().im(), o2.getCenter().re());
					return (int) Math.signum(rad1-rad2);
				}
			});
		}
		if(locusIndex >= list.size())
			locusIndex = 0;
	}
}
