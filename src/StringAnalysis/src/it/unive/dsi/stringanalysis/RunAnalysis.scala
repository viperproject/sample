/*import ch.ethz.inf.pm.sample.userinterfaces.SemanticAnalysis
import foo._;

object RunAnalysis {
  def main(args : Array[String]) : Unit = {
    var bd = new BricksDomain()
    bd.bricksList = List(new Brick(1,1,Set("a")), new Brick(0,0,Set.empty), new Brick(2,3,Set("a","b")), 
                         new Brick(0,1,Set("a","b")), new Brick(3,3,Set("c")))
    //Console.println(bd.normalize())

    var bd2 = new BricksDomain()
    bd2.bricksList = List(new Brick(3,4,Set("a","b")))
    var bd3 = new BricksDomain()
    bd3.bricksList = List(new Brick(2,6,Set("a","b","c")),new Brick(0,2,Set("a","b")),new Brick(0,2,Set("a","b")))
    //Console.println(bd2.widening(bd2,bd3))
    
    var sg1 = new StringGraphsDomain();
    var sons = List(new SimpleNode('0'),new SimpleNode('a'),new SimpleNode('1'));
    sg1.root = new FunctorNode(FunctorType.concat);
    sg1.root.asInstanceOf[FunctorNode].nodeSons = sons;
    var sg2 = new StringGraphsDomain();
    sg2.root = new FunctorNode(FunctorType.concat);
    var son = new FunctorNode(FunctorType.or);
    son.asInstanceOf[FunctorNode].nodeSons = List(new SimpleNode('1'),sg2.root); 
    sg2.root.asInstanceOf[FunctorNode].nodeSons = List(
      new SimpleNode('0'), 
      new SimpleNode('a'), 
      son); 
    Console.println(sg1.toString());
    Console.println(sg2.toString());
    //Console.println(sg1.lessEqual(sg2));
    
    SemanticAnalysis.analyze("Simple", "CreateString", 
    	"C:\\Users\\pulcy\\Desktop\\Università\\Tesi Puffina\\Implementazione\\MyStringAnalyzer\\src\\Examples\\Simple.scala", 
         //new SurelyAndMaybeContainedCharacters(new SurelyContainedCharacters(), new MaybeContainedCharacters()),
         //new PrefixAndSuffix(new Prefix(), new Suffix()),
         //new Bricks(),
         new StringGraphs(),
         StringSemantics)
  }
}*/