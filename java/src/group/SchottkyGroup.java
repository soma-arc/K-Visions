package group;

import schottky.figure.Circle;
import group.SL2C;

public interface SchottkyGroup {
	public SL2C[] getGens();
	public Circle[] getCircles();
}
