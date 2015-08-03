package midi;

import java.util.HashMap;

import javax.sound.midi.MidiMessage;
import javax.sound.midi.Receiver;
import javax.sound.midi.ShortMessage;

public class MidiInputReceiver implements Receiver{

	public MidiInputReceiver() {}

	private HashMap<Integer, MidiControlChangedListener> listenersDic = new HashMap<>();

	public void send(MidiMessage message, long timeStamp) {
		if (message instanceof ShortMessage) {
			ShortMessage sm = ((ShortMessage)message);
			switch(sm.getCommand()) {
			case ShortMessage.CONTROL_CHANGE:
				System.out.println(sm.getChannel());
				System.out.println(sm.getData1());
				System.out.println(sm.getData2());
				MidiControlChangedListener listener = listenersDic.get(sm.getData1());
				if(listener != null){
					listener.changed(sm.getData1(), sm.getData2());
				}
				break;
			}
		}
		System.out.println("midi received");
	}
	
	public void setMidiControlChangedListener (MidiController control, MidiControlChangedListener listener){
		listenersDic.put(control.getId(), listener);
	}
	
	public void clearMidiControlChangedListeners(){
		listenersDic.clear();
	}

	public void close() {}
}
