package it.unive.dsi.stringanalysis

import ch.ethz.inf.pm.sample.userinterfaces.SemanticAnalysis
  
object Main {
  def main(args : Array[String]) : Unit = {/*
    		var or = new Node(NodeType.or);
		//var or1 = new Node(NodeType.or);
		//var or2 = new Node(NodeType.or);
		var a = new Node(NodeType.simple);
		a.character = 'a';
		var a2 = new Node(NodeType.simple);
		a2.character = 'a';
		var b = new Node(NodeType.simple);
		b.character = 'b';
		var b2 = new Node(NodeType.simple);
		b2.character = 'b';
		var zero = new Node(NodeType.simple);
		zero.character = '0';
		var zero2 = new Node(NodeType.simple);
		zero2.character = '0';
		var concat = new Node(NodeType.concat);
		//var top = new Node(NodeType.top); 
		//var top2 = new Node(NodeType.top); 
		//var concat2 = new Node(NodeType.concat);
		//concat.sons += a.id;//b.id;
		concat.sons = concat.sons ::: a.id :: Nil;//or2.id;
		concat.sons = concat.sons ::: zero.id :: Nil;
		//concat.sons = concat.sons ::: bid :: Nil;
		//concat2.sons += d.id;
		//concat2.sons += top2.id;
		//concat2.sons += b.id;
  
		var sg = new StringGraph(concat.id, false);
		//sg.addNode(or);
		//sg.addNode(or1);
		//sg.addNode(or2);
		sg.addNode(a);
		sg.addNode(b);
		//sg.addNode(c);
		sg.addNode(zero);
		sg.addNode(concat);
		//sg.addNode(top);
		//sg.addNode(top2);
		//sg.addNode(concat2);
		//sg.forwardArcs += Pair(concat2.id, d.id);
		//sg.forwardArcs += Pair(concat2.id, b.id);
		//sg.forwardArcs += Pair(or1.id, concat.id);
		//sg.forwardArcs += Pair(or1.id, b.id);
		//sg.forwardArcs += Pair(or2.id, concat2.id);
		//sg.forwardArcs += Pair(or2.id, c.id);
		//sg.forwardArcs += Pair(concat2.id, or2.id);
		//sg.forwardArcs += Pair(concat2.id, d.id);
		//sg.forwardArcs += Pair(concat2.id, top2.id);
		sg.forwardArcs += Pair(concat.id, a.id);//b.id);
		sg.forwardArcs += Pair(concat.id, zero.id);//or2.id);
		sg.forwardArcs += Pair(concat.id, b.id);
		//sg.backwardArcs += Pair(concat2.id,or2.id);
		
		//var concat22 = new Node(NodeType.concat);
		var sg2 = new StringGraph(zero2.id, false);
		//concat22.sons += b.id;
		//concat22.sons += a.id;
		//sg2.addNode(a);
		//sg2.addNode(b);
		sg2.addNode(zero2);
		//sg2.forwardArcs += Pair(concat22.id, b.id);
		//sg2.forwardArcs += Pair(concat22.id, a.id);
  
		var SG = new StringGraphsDomain();
		SG.sg = sg;
		var SG2 = new StringGraphsDomain();
		SG2.sg = sg2;
		var SGA = new StringGraphsDomain();
		SGA.sg = new StringGraph(a2.id,false);
		SGA.sg.addNode(a2);
		var SGB = new StringGraphsDomain();
		SGB.sg = new StringGraph(b2.id,false);
		SGB.sg.addNode(b2);

		var a3 = a2.clone();
		var b3 = b2.clone();
		var SGA2 = new StringGraphsDomain();
		SGA2.sg = new StringGraph(a3.id,false);
		SGA2.sg.addNode(a3);
		var SGB2 = new StringGraphsDomain();
		SGB2.sg = new StringGraph(b3.id,false);
		SGB2.sg.addNode(b3);

		Console.println("SG1: " + SG2.toString());
		Console.println("SG2: " + SG.toString());
		var SG3 = SG.widening(SG,SG2);
		Console.println("SG3: " + SG3.toString());
		var SG3bis = SG3.concatenate(SG3.concatenate(SGA,SG3),SGB);
		Console.println("SG3bis: " + SG3bis.toString());
		var SG4 = SG3bis.widening(SG3,SG3bis);
		Console.println("SG4: " + SG4.toString());
  		var SG4bis = SG4.concatenate(SG4.concatenate(SGA2,SG4),SGB2);
  		Console.println("SG4bis: " + SG4bis.toString());
  		var SG5 = SG4bis.widening(SG4,SG4bis);
  		Console.println("SG5: " + SG4.toString());

  
		//Console.println(sg2.validate());
		//Console.println(SG2.lessEqual(SG));
		//for(n <- SG2.sg.nodes.values)
		//	Console.println(SG2.sg.prlb(n));
		//Console.println(SG.sg.path(or,b).map(n => n.label()));
		//Console.println(SG.sg.isNormalized());
		//var newSG = SG.sg.normalize();
		//Console.println(newSG.toString());
		//newSG = newSG.replaceEdge(Pair(concat.id,top2.id),Pair(concat2.id,concat.id));
		//newSG = newSG.replaceNode(concat2, concat2);
		//Console.println(newSG.toString());
		//newSG.addNode(top2);
		//newSG = newSG.replaceEdge(Pair(concat2.id,concat.id),Pair(concat.id,top2.id));
		//Console.println(newSG.toString());
		//Console.println(newSG.samepf(top,top2));
		//Console.println(SG.sg.tanc(or).map(n => n.id));
		*/
		SemanticAnalysis.analyze("Simple", "CreateString", 
	    	"C:\\Users\\Pietro\\workspace\\StringAnalysis\\src\\Examples\\Simple.scala", 
	         //new SurelyAndMaybeContainedCharacters(new SurelyContainedCharacters(), new MaybeContainedCharacters()),
	         //new PrefixAndSuffix(new Prefix(), new Suffix()),
	         //new Bricks(),
	         new StringGraphs(),
	         StringSemantics)
      
		
		//Console.println(sg.outdegree(or1));
		//sg.sons(son2).foreach(p => Console.println(p.label));
		//sg.tanc(son2).foreach(n => Console.println(n.label));
  }
}
