package ch.ethz.inf.pm.sample.preprocessing.scalaprocessing.plugin


import ch.ethz.inf.pm.sample._
import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Settings
import java.io._
	
	/** An object for running the plugin as standalone application.
	 */
	object MainDirectory {
	  def main(args: Array[String]) {
	    apply("C:\\Users\\Pietro\\Documents\\Lavoro\\ScalaAnalyzer\\toAnalyze");
        System.out.println(SystemParameters.output.output()+"STATISTICS\n"+SystemParameters.output.statistics()+"\n\nAnalyzed classes: "+SystemParameters.analyzedClasses+"\nAnalyzed methods: "+SystemParameters.analyzedMethods+"\nOverall time of analysis (sec): "+(AnalysisTimer.totalTime/1000.0)+"\n")
     }
     
	  def apply(f : String) {
	    val file : File = new File(f);
        if(file.isDirectory()) {
          for(f1 <- file.listFiles())
            apply(f1.getAbsolutePath())
            //try{apply(f1.getAbsolutePath())} catch{case x => println("Error with file "+f1.getAbsolutePath()+"\n"+x.toString())}
        }
        else singleFile(f)
	  }
	   
	  def singleFile(file : String) {
	    val settings = new Settings
	
	    val command = new CompilerCommand(List(file), settings) {
	      /** The command name that will be printed in in the usage message.
	       *  This is automatically set to the value of 'plugin.commandname' in the
	       *  file build.properties.
	       */
	      override val cmdName = "scala2cfg"
	    }
	
	    if (!command.ok)
	      return()
	
	    /** The version number of this plugin is read from the properties file
	     */
	    if (settings.version.value) {
	      println(command.cmdName +" version 1.0")
	      return()
	    }
	    if (settings.help.value) {
	      println(command.usageMsg)
	      return()
	    }
	
	    val runner = new PluginRunner(settings)
	    val run = new runner.Run
	    //try {
	    	run.compile(command.files)
	    //} catch {
	    //  case _ => System.out.println("Error while analyzing "+file);
	    //}
	  }
	}