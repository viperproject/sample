package it.unive.dsi.stringanalysis
import ch.ethz.inf.pm.sample.userinterfaces.SemanticAnalysis
  
object DebugHelper {
  def main(args : Array[String]) : Unit = {
	  /*
		  val suffix1 : SuffixDomain = new SuffixDomain();
    	  suffix1.stringValue = "cloudy";
    	  val suffix2 : SuffixDomain = new SuffixDomain();
    	  suffix2.stringValue = "sunny";
    	  val suffix3 : SuffixDomain = suffix2.lub(suffix1,suffix2);
    	  Console.println(suffix1.toString() + "; " + suffix2.toString() + "; " + suffix3.toString())
       */
       Console.println("hello world");
       
       SemanticAnalysis.analyze("Simple", "CreateString", 
	    	"C:\\Users\\Puffy\\Desktop\\PhD Giulia\\Assegno di ricerca pre-PhD\\Eclipse Workspace\\StringAnalyzer New\\src\\Examples\\Simple.scala", 
	         //new SurelyAndMaybeContainedCharacters(new SurelyContainedCharacters(), new MaybeContainedCharacters()),
	         //new PrefixAndSuffix(new Prefix(), new Suffix()),
	         //new Bricks(),
	         new StringGraphs(),
	         StringSemantics)
      
  }
}
