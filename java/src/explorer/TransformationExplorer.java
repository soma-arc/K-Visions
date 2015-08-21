package explorer;

import java.util.ArrayList;

import pointSeries.PointSeries;
import group.SL2C;

public class TransformationExplorer {
	private SL2C[] gens;
	
	public TransformationExplorer(SL2C[] gens){
		this.gens = gens;
	}
	
	private int cutBranchCount = 4;
	public ArrayList<PointSeries> runBFS(int maxLevel, PointSeries root, double magnification){
		ArrayList<PointSeries> transformedFigures = new ArrayList<>();
		ArrayList<ArrayList<TaggedPointSeries>> figures = new ArrayList<>();
		figures.add(new ArrayList<TaggedPointSeries>());
		figures.get(0).add(new TaggedPointSeries(-1, root));

		for(int level = 0 ; level < maxLevel ; level++){
			figures.add(new ArrayList<TaggedPointSeries>());
			for(TaggedPointSeries figure : figures.get(level)){
				if(figure.shrinkCount >= cutBranchCount){
					continue;
				}
				for(int tag = 0 ; tag < gens.length ; tag++){
					if((tag + 2) % 4 == figure.tag) continue;
					SL2C word = figure.word.mult(gens[tag]);
					figures.get(level + 1).add(new TaggedPointSeries(root.transform(word), word, tag, figure.pointSeries, figure.shrinkCount, magnification));
				}
			}
		}
		System.gc();
		
		for(ArrayList<TaggedPointSeries> list : figures){
			for(TaggedPointSeries figure : list){
				transformedFigures.add(figure.pointSeries);
			}
		}
		System.gc();
		return transformedFigures;
	}
	
	private class TaggedPointSeries{
		protected int tag = -1;
		protected PointSeries pointSeries;
		protected int shrinkCount = 0;
		protected SL2C word = SL2C.UNIT;

		public TaggedPointSeries(int tag, PointSeries pointSeries) {
			this.tag = tag;
			this.pointSeries = pointSeries;
		}
		
		public TaggedPointSeries(PointSeries pointSeries, SL2C word, int tag, PointSeries previousPoints, int shrinkCount, double magnification) {
			this.pointSeries = pointSeries;
			this.word = word;
			this.tag = tag;
			this.shrinkCount = shrinkCount;
			
			if(pointSeries.getWidth() * magnification < 10 || pointSeries.getHeight() * magnification < 10){
				this.shrinkCount++;
			}
			if(previousPoints.getWidth() < pointSeries.getWidth() || previousPoints.getHeight() < pointSeries.getHeight()){
				this.shrinkCount = 0;
			}
		}

	}
}
