package ch.ethz.inf.pm.sample.oorepresentation

trait Compiler {
	def compileFile(path : String) : List[ClassDefinition];
  def getLabel() : String;
}