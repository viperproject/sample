import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.property.OutputCollector
import ch.ethz.inf.pm.sample.{StringCollector, SystemParameters}
import ch.ethz.inf.pm.td.compiler.{TouchException, TouchCompiler}
import ch.ethz.inf.pm.td.domain.TouchAnalysisWithApron
import ch.ethz.inf.pm.td.TestRunner
import ch.ethz.inf.pm.td.webapi.{TopScripts}
import heapanalysis.{HeapEnv, VariableEnv, NonRelationalHeapDomain, NullProgramPointHeapIdentifier}
import scala.List

/**
 * User: lucas
 * Date: 11/22/12
 * Time: 5:50 PM
 */

//def analyzer(url:String) {
//  try {
//    var methods = List("main")
//    ch.ethz.inf.pm.sample.Main.reset()
//    SystemParameters.heapTimer.reset()
//    SystemParameters.domainTimer.reset()
//    val s: Analysis = new TouchAnalysisWithApron()
//
//    val id = new NullProgramPointHeapIdentifier(null, null)
//    val typ: Type = null
//    val ids: MaybeHeapIdSetDomain[NullProgramPointHeapIdentifier] = new MaybeHeapIdSetDomain
//    val env: VariableEnv[_] = new VariableEnv[NullProgramPointHeapIdentifier](typ, ids)
//    val heapEnv: HeapEnv[_] = new HeapEnv[NullProgramPointHeapIdentifier](typ, ids)
//    val heap: HeapDomain[_, _] = new NonRelationalHeapDomain[NullProgramPointHeapIdentifier](env, heapEnv, ids, id)
//    heap.reset()
//
//    val compiler: Compiler = new TouchCompiler
//    if (s == null || heap == null || compiler == null) throw new TouchException("Failed to create compiler or domain")
//    SystemParameters.resetNativeMethodsSemantics()
//    SystemParameters.addNativeMethodsSemantics(s.getNativeMethodsSemantics())
//    SystemParameters.addNativeMethodsSemantics(heap.getNativeMethodsSemantics())
//    SystemParameters.addNativeMethodsSemantics(compiler.getNativeMethodsSemantics)
//    SystemParameters.setCompiler(compiler)
//    ch.ethz.inf.pm.sample.Main.compile(url)
//    val heapDomain: HeapDomain[_, _] = getSelectedHeapAnalysis
//    if (heapDomain.isInstanceOf[NonRelationalHeapDomain[_]]) {
//      (heapDomain.asInstanceOf[NonRelationalHeapDomain[_]]).setType(SystemParameters.getType())
//    }
//    val domain: SemanticDomain[_] = s.getInitialState.asInstanceOf[SemanticDomain[_]]
//    val entrydomain: HeapAndAnotherDomain[_, _, _] = new HeapAndAnotherDomain[_, _, _](domain, heapDomain)
//    val entryvalue: ExpressionSet = new ExpressionSet(SystemParameters.getType().top())
//    val entryState: AbstractState[_, _, _] = new AbstractState[_, _, _](entrydomain, entryvalue)
//    val output: OutputCollector = new OutputCollector
//    ch.ethz.inf.pm.sample.Main.analyze(methods, entryState, output)
//  } catch {
//    case e:Exception => println("Analysis failed: "+e.getMessage); e.printStackTrace()
//  }
//
//}

TestRunner.runDirectory("top10")