package opt.discriminator;

import java.util.ArrayList;

import opt.fuchs.ComplexProbability;
import number.Complex;

public class DiscretenessDiscriminator {
	private ComplexProbability root;
	private int maxLevel = 50;
	private ArrayList<ComplexProbability> list = new ArrayList<>();
	private boolean discreteness = true;
	
	public DiscretenessDiscriminator(ComplexProbability root){
		this.root = root;
	}
	
	public boolean discriminate(){
		discreteness = true;
		list.clear();
		ComplexProbability above = root.replaceAbove();
		int level = 1;
		while(above != null){
			list.add(above);
			above = above.replaceAbove();
			level++;
			if(level > maxLevel){
				discreteness = false;
				break;
			}
		}

		ComplexProbability bottom = root.replaceBottom();
		level = 1;
		while(bottom != null){
			list.add(bottom);
			bottom = bottom.replaceBottom();
			level++;
			if(level > maxLevel){
				discreteness = false;
				break;
			}
		}

		return discreteness;
	}
	
	public ArrayList<ComplexProbability> getComplexProbabilities(){
		return list;
	}
	
	public boolean isDiscrete(){
		return discreteness;
	}
}