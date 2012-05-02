package project

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.scalalang._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property.OutputCollector
import ch.ethz.inf.pm.sample.tracepartitioning.PartitionedState
import ch.ethz.inf.pm.sample.Timer
import ch.ethz.inf.pm.sample.{StringCollector, SystemParameters}
import heapanalysis._
import java.io.File
import javax.swing.JOptionPane
import ch.ethz.inf.pm.sample.userinterfaces.{ShowGraph, AnalysisResults}

/**
 * Created by IntelliJ IDEA.
 * User: Pietro
 * Date: 29/03/12
 * Time: 10.59
 * To change this template use File | Settings | File Templates.
 */

object Main {

   def run[S<:SemanticDomain[S]](filePath : String, method : String, domain : S) = try
   {
        val file = new File(filePath);
        ch.ethz.inf.pm.sample.Main.reset
        SystemParameters.setProgressOutput(new StringCollector)
        SystemParameters.setAnalysisOutput(new StringCollector)
        SystemParameters.heapTimer.reset
        SystemParameters.domainTimer.reset
        println("Setting up the parameters of the analysis")
        val typ=SystemParameters.typ;
        val pp : ProgramPoint =new ScalaProgramPoint(0, 0);
        val id : ProgramPointHeapIdentifier=new NullProgramPointHeapIdentifier(typ, pp)
        val idset : HeapIdSetDomain[ProgramPointHeapIdentifier]=new DefiniteHeapIdSetDomain[ProgramPointHeapIdentifier]()
        var heapDomain : NonRelationalHeapDomain[ProgramPointHeapIdentifier] = new NonRelationalHeapDomain[ProgramPointHeapIdentifier](new VariableEnv(typ, idset),new HeapEnv(typ, idset), idset, id)
        heapDomain.reset
        var compiler: Compiler = new ScalaCompiler();
        SystemParameters.resetNativeMethodsSemantics
        SystemParameters.addNativeMethodsSemantics(heapDomain.getNativeMethodsSemantics)
        SystemParameters.addNativeMethodsSemantics(compiler.getNativeMethodsSemantics)
        SystemParameters.setCompiler(compiler)
        println("Compiling the files")
        var tcompiler: Timer = new Timer
        tcompiler.start
        ch.ethz.inf.pm.sample.Main.compile(file)
        tcompiler.stop
        println("Creating the initial state of the analysis")
        heapDomain.setType(SystemParameters.getType)
        var entrydomain : HeapAndAnotherDomain [S, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier] = new HeapAndAnotherDomain(domain, heapDomain)
        var entryvalue = new SymbolicAbstractValue[GenericAbstractState[S, NonRelationalHeapDomain[ProgramPointHeapIdentifier], ProgramPointHeapIdentifier]](scala.Option.apply(null), scala.Option.apply(null))
        var entryState = new GenericAbstractState(entrydomain, entryvalue)
        entryvalue = new SymbolicAbstractValue(new Some(entryState), new Some(SystemParameters.getType))
        entryState = new GenericAbstractState(entrydomain, entryvalue)
        println("\nRunning the analysis")
        var t: Timer = new Timer
        t.start
        var output: OutputCollector = new OutputCollector
        SystemParameters.property=ShowGraph
        ShowGraph.exitOnClose=true;
        ch.ethz.inf.pm.sample.Main.analyze(method, entryState, output)
        t.stop
        println("\nAnalysis ended")
        SystemParameters.analysisOutput.appendString(output.output + "\n")
        SystemParameters.analysisOutput.appendString("Times spent by the compiler:" + tcompiler.totalTime + " msec")
        SystemParameters.analysisOutput.appendString("Times spent by the overall analysis:" + t.totalTime + " msec")
        SystemParameters.analysisOutput.appendString("Times spent by the heap analysis:" + SystemParameters.heapTimer.totalTime + " msec")
        SystemParameters.analysisOutput.appendString("Times spent by the other analysis:" + SystemParameters.domainTimer.totalTime + " msec")

        var dialog: AnalysisResults = new AnalysisResults(SystemParameters.analysisOutput.getString)
        dialog.pack
        //dialog.setLocationRelativeTo(mainFrame)
        dialog.setVisible(true)
      }
      catch {
        case e: Exception => {
          JOptionPane.showMessageDialog(null, "Error during the analysis", "Error", JOptionPane.ERROR_MESSAGE)
          System.out.println(e.toString)
          e.printStackTrace
          //progressBar.setVisible(false)
          throw e
        }
      }
}