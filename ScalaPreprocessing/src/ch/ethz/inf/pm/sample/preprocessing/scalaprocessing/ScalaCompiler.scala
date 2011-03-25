package ch.ethz.inf.pm.sample.preprocessing.scalaprocessing

import ch.ethz.inf.pm.sample._
import scala.tools.nsc._
import ch.ethz.inf.pm.sample.oorepresentation._

object ScalaClasses {
	var classes : List[ClassDefinition] = Nil;
}

object ScalaCompiler extends Compiler {
	def compileFile(path : String) : List[ClassDefinition] = {
		SystemParameters.nativeMethodsSemantics=SystemParameters.nativeMethodsSemantics ::: ObjectNativeMethodSemantics :: IntegerNativeMethodSemantics :: BooleanNativeMethodSemantics :: Nil;
  	    //SystemParameters.typ is initialized inside the parser
		
	    val settings = new Settings
    settings.classpath.value = System.getProperty("java.class.path")

        // WORK
	    val command = new CompilerCommand(List(path), settings) {
     
	      /** The command name that will be printed in in the usage message.
	       *  This is automatically set to the value of 'plugin.commandname' in the
	       *  file build.properties.
	       */
	      override val cmdName = "scala2cfg"
	    
	    }
	
	    if (!command.ok)
	      return Nil;
	
	    /** The version number of this plugin is read from the properties file
	     */
	    if (settings.version.value) {
	      println(command.cmdName +" version 1.0")
	      return Nil;
	    }
	    if (settings.help.value) {
	      println(command.usageMsg)
	      return Nil;
	    }
	
	    val runner = new PluginRunner(settings)
	    val run = new runner.Run
	    run.compile(command.files)
	    return ScalaClasses.classes;
	}
	
}