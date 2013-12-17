package ch.ethz.inf.pm.sample.oorepresentation.scalalang

import ch.ethz.inf.pm.sample._
import scala.tools.nsc._
import ch.ethz.inf.pm.sample.oorepresentation._
import java.io._

object ScalaClasses {
	var classes : List[ClassDefinition] = Nil;
}

class ScalaCompiler extends Compiler {

  private var parsedclasses : List[ClassDefinition] = Nil;

  def getLabel() : String = "Scala"

   def extensions() : List[String] = "scala" :: Nil;

  def getNativeMethodsSemantics() : List[NativeMethodSemantics] = List(BooleanNativeMethodSemantics, IntegerNativeMethodSemantics, ObjectNativeMethodSemantics)

  def getSourceCode(path : String) : String = getOriginalCode(new BufferedReader(new FileReader(path)))

	def compileFile(path : String) : List[ClassDefinition] = {
		SystemParameters.addNativeMethodsSemantics(ObjectNativeMethodSemantics :: IntegerNativeMethodSemantics :: BooleanNativeMethodSemantics :: Nil);
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
      parsedclasses = parsedclasses ::: ScalaClasses.classes;
	    return ScalaClasses.classes;
	}

  def getMethod(name : String, classType : Type, parameters : List[Type]) : Option[(MethodDeclaration, Type)] = {
    getClassDeclaration(classType) match {
      case Some(classe) =>
        for(m <- classe.methods)
          if(m.name.toString.equals(name) && m.arguments.apply(0).size==parameters.size) {
            var ok : Boolean = true;
            if(m.arguments.size!=1) throw new ScalaException("Not yet supported")
            for(i <- 0 to m.arguments.apply(0).size-1) {
              if(! parameters.apply(i).lessEqual(m.arguments.apply(0).apply(i).typ))
                ok=false;
            }
            if(ok) return new Some[(MethodDeclaration, Type)]((m, classType));
          }
        for(ext <- classe.extend)
          getMethod(name, ext.getThisType(), parameters) match {
            case Some(s) => return Some(s);
            case None =>
          }
        return None;
      case None => return None;
    }
  }

  def getMethods(name:String): List[(ClassDefinition,MethodDeclaration)] =
    for (clazz <- parsedclasses; method <- clazz.methods; if method.name.toString == name) yield (clazz,method)

  private def getClassDeclaration(t : Type) : Option[ClassDefinition] = {
    for(c <- parsedclasses)
      if(c.typ.equals(t))
        return Some(c);
    return None;
  }

  def reset() {
    parsedclasses = Nil
  }

  def generateTopType() {
    val suffix: String = ".scala"
    val file: File = File.createTempFile("Dummy", suffix)
    val className: String = file.getName.substring(0, file.getName.length - suffix.length)
    val source: String = "class " + className + " {}"

    val out: FileWriter = new FileWriter(file)
    out.write(source)
    out.close

    val classes = compileFile(file.getAbsolutePath)

    if (classes.length > 0) {
      SystemParameters.typ = classes.head.typ.top
    }
    else {
      throw new Exception("Could not generate type information")
    }

    val classFile: File = new File(className + ".class")
    if (classFile.exists) classFile.delete
  }
	
}