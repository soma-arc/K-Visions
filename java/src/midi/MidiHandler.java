package midi;

import java.util.ArrayList;

import javax.sound.midi.MidiDevice;
import javax.sound.midi.MidiSystem;
import javax.sound.midi.MidiUnavailableException;
import javax.sound.midi.Transmitter;

public class MidiHandler {
	private static MidiHandler instance = new MidiHandler();
	private static ArrayList<MidiDevice> openedDevices = new ArrayList<>();
	private static MidiInputReceiver receiver = new MidiInputReceiver();
	
	private MidiHandler(){
	}
	
	public static void listenStart(){
		close();
		openedDevices.clear();
		MidiDevice device;
		MidiDevice.Info[] infos = MidiSystem.getMidiDeviceInfo();
		for (int i = 0; i < infos.length; i++) {
			try {
				device = MidiSystem.getMidiDevice(infos[i]);
				System.out.println(infos[i] +" -- "+ infos[i].getDescription());
				java.util.List<Transmitter> transmitters = device.getTransmitters();
				for(int j = 0; j<transmitters.size();j++) {
					transmitters.get(j).setReceiver(receiver);
				}

				Transmitter trans = device.getTransmitter();
				trans.setReceiver(receiver);

				device.open();
				openedDevices.add(device);
				System.out.println(device.getDeviceInfo()+" Was Opened");
			} catch (MidiUnavailableException e) {
//				e.printStackTrace();
			}
		}
	}
	
	public static void setMidiControlChangedListener(MidiController control, MidiControlChangedListener listener){
		receiver.setMidiControlChangedListener(control, listener);
	}
	
	public static void clearMidiControlChangedListeners(){
		receiver.clearMidiControlChangedListeners();
	}
	
	public static MidiHandler getInsrance(){
		return instance;
	}
	
	public static void close(){
		for(MidiDevice device : openedDevices){
			if(device.isOpen())
				device.close();
		}
	}
}
