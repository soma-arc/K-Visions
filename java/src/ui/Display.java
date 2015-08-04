package ui;

import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.JPanel;

import launcher.Launcher;
import midi.KorgNanoControl2;
import midi.MidiControlChangedListener;
import midi.MidiHandler;

public class Display extends JPanel{
	protected Display(){
		setMidiHandlers();
		addComponentListener(new DisplayShownAdapter());
	}
	
	protected class DisplayShownAdapter extends ComponentAdapter{
		@Override
		public void componentShown(ComponentEvent e){
			setMidiHandlers();
			requestFocus();
		}
	}
	
	private void setMidiHandlers(){
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_BACKWARD, new ChangeToSchottkyListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_FORWARD, new ChangeToOPTListener());
		MidiHandler.setMidiControlChangedListener(KorgNanoControl2.BUTTON_STOP, new ChangeToParabolicListener());
	}
	
	private class ChangeToSchottkyListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			if(value == 127)
				Launcher.changeDisplayMode(DisplayMode.SCHOTTKY);
		}
	}
	
	private class ChangeToOPTListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			if(value == 127)
				Launcher.changeDisplayMode(DisplayMode.OPT);
		}
	}
	
	private class ChangeToParabolicListener implements MidiControlChangedListener{
		@Override
		public void changed(int controlPort, float value) {
			if(value == 127)
				Launcher.changeDisplayMode(DisplayMode.PARABOLIC);
		}
	}
}
