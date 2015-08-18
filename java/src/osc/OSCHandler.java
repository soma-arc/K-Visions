package osc;

import java.net.SocketException;
import java.util.Timer;
import java.util.TimerTask;


import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortIn;

public class OSCHandler {
	private static OSCHandler instance = new OSCHandler(9000);
	private static OSCPortIn receiver;
	private static OSCReceivedAction loudAction = null;
	private Timer timer;
	public OSCHandler(int port){
		receiver = null;
		try {
			receiver = new OSCPortIn(port);
		} catch (SocketException e2) {
			e2.printStackTrace();
		}
		OSCListener loudListener = new OSCListener() {
			public void acceptMessage(java.util.Date time, OSCMessage message) {
				if(loudAction != null){
					loudAction.doAction(message.getArguments());
				}
			}
		};
		receiver.addListener("/audio/loud", loudListener);
		receiver.startListening();
		timer = new Timer();
		timer.schedule(new TimerRestarter(), 0, 1000);
		
	}
	
	private class TimerRestarter extends TimerTask{
		@Override
		public void run() {
			if(receiver.isListening() == false){
				System.out.println("restart");
				receiver.startListening();
			}
		}
		
	}
	
	public static void setLoudAction(OSCReceivedAction loudAction){
		OSCHandler.loudAction = loudAction;
	}
	
	public static OSCHandler getInstance(){
		return instance;
	}
	
	public static void close(){
		receiver.close();
	}
}
