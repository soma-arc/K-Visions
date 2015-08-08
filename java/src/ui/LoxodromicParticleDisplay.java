package ui;

import group.SL2C;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.util.ArrayList;
import java.util.Timer;
import java.util.TimerTask;

import number.Complex;
import midi.KorgNanoControl2;
import midi.MidiHandler;
import mobius.Mobius;

public class LoxodromicParticleDisplay extends Display{
	private static LoxodromicParticleDisplay instance = new LoxodromicParticleDisplay();
	private ArrayList<Complex> points = new ArrayList<>();
	private Timer timer;
	private Complex fixA, fixB;
	private SL2C transformation;
	private double magnification = 100;
	int num = 300;
	private LoxodromicParticleDisplay() {
		fixA = new Complex(1);
		fixB = new Complex(-1);
		transformation = Mobius.getLoxodromicTransformation(fixA, fixB, new Complex(lambdaRe, lambdaIm));
//		System.out.println(transformation);
		for(double i = -num ; i < num ; i++){
			for(double j = -num ; j < num ; j++){
				points.add(new Complex(i/magnification, j/magnification));
			}
		}
		addMouseListener(new mousePressedAdapter());
		addMouseMotionListener(new mouseDraggedAdapter());
		addKeyListener(new KeyPressedAdapter());
	}
	
	private float initialHue = 0.0f;
	private float hueStep = 0.1f;
	public void paintComponent(Graphics g){
		Graphics2D g2 = (Graphics2D)g;
		g2.setColor(Color.BLACK);
		g2.fillRect(0, 0, this.getWidth(), this.getHeight());
		g2.translate(this.getWidth() / 2 , this.getHeight() / 2);
		
		ArrayList<Complex> list = new ArrayList<>();
		float hue = initialHue;
		for(Complex p : points){
			g2.setColor(Color.getHSBColor(hue, 1.0f, 1.0f));
			Complex p2 = Mobius.onPoint(transformation, p);
//			System.out.println(p +"   "+ p2);
//			System.out.println((Math.random() * getWidth() - getWidth()/2));
			if(((p2.re() - fixA.re()) * (p2.re() - fixA.re()) + (p2.im() - fixA.im()) * (p2.im() - fixA.im())) < 0.01){
				p2 = Mobius.onPoint(transformation, new Complex((Math.random() * getWidth() - getWidth()/2)/magnification, 
								 (Math.random() * getHeight() - getHeight()/2)/magnification));
//				p2 = fixB.add(new Complex(Math.random() - 0.5, Math.random() - 0.5));
			}
			
			if(((p2.re() - fixB.re()) * (p2.re() - fixB.re()) + (p2.im() - fixB.im()) * (p2.im() - fixB.im())) < 0.01){
				p2 = Mobius.onPoint(transformation,new Complex((Math.random() * getWidth() - getWidth()/2)/magnification, 
								 (Math.random() * getHeight() - getHeight()/2)/magnification));
//				p2 = fixA.add(new Complex(Math.random() - 0.5, Math.random() - 0.5));
			}
			list.add(p2);
			g2.fillRect((int) (p2.re() * magnification),(int) (p2.im() * magnification), 1, 1);
			hue += hueStep;
		}
		points = list;
		
		g2.setColor(Color.orange);
		g2.fillOval((int)( fixA.re() * magnification), (int)( fixA.im() * magnification), 10, 10);
		g2.fillOval((int)( fixB.re() * magnification), (int)( fixB.im() * magnification), 10, 10);
	}
	
	public static LoxodromicParticleDisplay getInstance(){
		return instance;
	}

	
	@Override
	protected void shown(){
		super.shown();;
		timer = new Timer();
		timer.schedule(new AnimationTask(), 0, 100);
	}
	
	@Override
	protected void hidden(){
		if(timer != null)
			timer.cancel();
	}
	
	private class AnimationTask extends TimerTask{
		@Override
		public void run() {
			repaint();
		}
	}
	
	
	private class mousePressedAdapter extends MouseAdapter{
		@Override
		public void mousePressed(MouseEvent e){
			double mouseX = e.getX() - getWidth() / 2;
			double mouseY = e.getY() - getHeight() / 2;
			System.out.println(Math.sqrt((mouseX - fixB.re() * magnification) * (mouseX - fixB.re() * magnification) + (mouseY - fixB.im() * magnification) * (mouseY- fixB.im() * magnification)));
			if(Math.sqrt((mouseX - fixA.re() * magnification) * (mouseX - fixA.re() * magnification) + (mouseY - fixA.im() * magnification) * (mouseY- fixA.im() * magnification)) < 10){
				clickedA = true;
			}else if(Math.sqrt((mouseX - fixB.re() * magnification) * (mouseX - fixB.re() * magnification) + (mouseY - fixB.im() * magnification) * (mouseY- fixB.im() * magnification)) < 10){
				clickedB = true;
			}
		}
		
		@Override
		public void mouseReleased(MouseEvent e){
			clickedA = false;
			clickedA = false;
		}
	}
	
	boolean clickedA = false;
	boolean clickedB = false;
	private class mouseDraggedAdapter extends MouseMotionAdapter{
		@Override
		public void mouseDragged(MouseEvent e){
			double mouseX = e.getX() - getWidth() / 2;
			double mouseY = e.getY() - getHeight() / 2;
			if(clickedA){
				fixA = new Complex(mouseX / magnification, mouseY / magnification);
				transformation = Mobius.getLoxodromicTransformation(fixA, fixB, new Complex(lambdaRe, lambdaIm));
			}else if(clickedB){
				fixB = new Complex(mouseX / magnification, mouseY / magnification);
				transformation = Mobius.getLoxodromicTransformation(fixA, fixB, new Complex(lambdaRe, lambdaIm));
			}
		}
	}
	
	double lambdaRe = -0.5;
	double lambdaIm = -0.5;
	private class KeyPressedAdapter extends KeyAdapter{
		@Override
		public void keyPressed(KeyEvent e){
			if(e.getKeyChar() == 'p'){
				lambdaRe += 0.01;
				transformation = Mobius.getLoxodromicTransformation(fixA, fixB, new Complex(lambdaRe, lambdaIm));
			}else if(e.getKeyChar() == 'n'){
				lambdaRe -= 0.01;
				transformation = Mobius.getLoxodromicTransformation(fixA, fixB, new Complex(lambdaRe, lambdaIm));
			}else if(e.getKeyChar() == 'f'){
				lambdaIm += 0.01;
				transformation = Mobius.getLoxodromicTransformation(fixA, fixB, new Complex(lambdaRe, lambdaIm));
			}else if(e.getKeyChar() == 'b'){
				lambdaIm -= 0.01;
				transformation = Mobius.getLoxodromicTransformation(fixA, fixB, new Complex(lambdaRe, lambdaIm));
			}
			System.out.println(lambdaRe +"  "+ lambdaIm);
		}
	}
}
