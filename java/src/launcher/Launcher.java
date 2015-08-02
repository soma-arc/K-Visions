package launcher;

import javax.swing.BoxLayout;
import javax.swing.JFrame;

import ui.ParabolicCommutatorGroupsDisplay;

public class Launcher {

	private static final int FRAME_WIDTH = 1500;
	private static final int FRAME_HEIGHT = 1500;

	public static void main(String[] args) {
		new Launcher().start();
	}

	public void start(){
		JFrame frame = new JFrame();
		frame.setSize(FRAME_WIDTH, FRAME_HEIGHT);
		frame.setTitle("K-Visions");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().add(ParabolicCommutatorGroupsDisplay.getInstance());
		frame.setVisible(true);
	}
}
