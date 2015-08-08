package launcher;

import java.awt.CardLayout;
import java.awt.Graphics2D;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.event.ComponentListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;
import java.awt.Color;

import osc.OSCHandler;
import midi.MidiHandler;
import opt.ui.OPTDisplay;
import schottky.ui.SchottkyDisplay;
import ui.DisplayMode;
import ui.ParabolicCommutatorGroupsDisplay;

public class Launcher {

	private static final int FRAME_WIDTH = 1500;
	private static final int FRAME_HEIGHT = 1500;

	private static GraphicsDevice device;
	private static JFrame frame;
	private static JPanel cardPanel;
	public static void main(String[] args) {
		new Launcher().start();
	}

	public void start(){
		frame = new JFrame();
		frame.setSize(FRAME_WIDTH, FRAME_HEIGHT);
		frame.setTitle("K-Visions");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent e){
				MidiHandler.close();
				OSCHandler.close();
			}
		});
		cardPanel = new JPanel();
		cardPanel.setLayout(new CardLayout());
		
		frame.getContentPane().add(cardPanel);

		cardPanel.add(ParabolicCommutatorGroupsDisplay.getInstance(), DisplayMode.PARABOLIC.name());
		cardPanel.add(SchottkyDisplay.getInstance(), DisplayMode.SCHOTTKY.name());
		cardPanel.add(OPTDisplay.getInstance(), DisplayMode.OPT.name());
		
		BufferedImage image = new BufferedImage(64,64,  
                BufferedImage.TYPE_4BYTE_ABGR);  
		Graphics2D g2 = image.createGraphics();  
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g2.setColor(Color.white);     
		g2.fillOval(0,0, 8,8); 
		g2.dispose();  
		cardPanel.setCursor(Toolkit.getDefaultToolkit().createCustomCursor(  
				image, new Point(4,4), "null_cursor")); 
		GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
		device = ge.getDefaultScreenDevice();
		frame.setUndecorated(true);
		device.setFullScreenWindow(frame);;
		MidiHandler.listenStart();
	}
	
	public static void setFullScreenMode(){
		frame.dispose();
		frame = new JFrame();
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().add(cardPanel);
		frame.setUndecorated(true);
		device.setFullScreenWindow(frame);
	}
	
	public static void setWindowMode(){
		device.setFullScreenWindow(null);
		frame.dispose();
		frame = new JFrame();
		frame.setSize(FRAME_WIDTH, FRAME_HEIGHT);
		frame.setTitle("K-Visions");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().add(cardPanel);
		frame.setVisible(true);
	}
	
	public static void nextCard(){
		((CardLayout) cardPanel.getLayout()).next(cardPanel);
		cardPanel.getComponent(0).requestFocus();
	}
	
	public static void changeDisplayMode(DisplayMode mode){
		((CardLayout) cardPanel.getLayout()).show(cardPanel, mode.name());
	}
}
