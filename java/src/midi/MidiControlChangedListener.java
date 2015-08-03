package midi;

public interface MidiControlChangedListener {
	public void changed(int controlPort, float value);
}
