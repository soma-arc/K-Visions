package osc;

import java.net.SocketException;

import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortIn;

public class OSCHandler {
	private static OSCHandler instance = new OSCHandler(9000);
	private static OSCPortIn receiver;
	public OSCHandler(int port){
		receiver = null;
		try {
			receiver = new OSCPortIn(port);
		} catch (SocketException e2) {
			e2.printStackTrace();
		}
		OSCListener loudListener = new OSCListener() {
			public void acceptMessage(java.util.Date time, OSCMessage message) {
				System.out.println("loud received!");
				System.out.println(message.getAddress());
				for(Object ob : message.getArguments()){
					System.out.println((float) ob);
				}
			}
		};
		OSCListener fftlistener = new OSCListener() {
			public void acceptMessage(java.util.Date time, OSCMessage message) {
				System.out.println("FFT received!");
				System.out.println(message.getAddress());
				for(Object ob : message.getArguments()){
					System.out.println((float) ob);
				}
				
			}
		};
		receiver.addListener("/audio/loud", loudListener);
		receiver.addListener("/audio/attack", fftlistener);
		receiver.startListening();
	}
	
	public static OSCHandler getInstance(){
		return instance;
	}
	
	public static void close(){
		receiver.close();
	}
}
