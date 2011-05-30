package ch.ethz.inf.pm.sample.test

import java.io.{File, FileReader}
import ch.ethz.inf.pm.sample.userinterfaces.InstalledPlugins
import java.lang.Exception
import ch.ethz.inf.pm.sample.oorepresentation.Compiler
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaCompiler
import ch.ethz.inf.pm.sample.oorepresentation.javabytecode.JavaCompiler
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.NonRelationalHeapDomain
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.{StringCollector, SystemParameters}
import ch.ethz.inf.pm.sample.property._
import javax.management.remote.rmi._RMIConnection_Stub

object Run {

  def main(args : Array[String]) = {
    System.out.println("Starting the tests")
    iteratorOverFiles(new File("C:\\Users\\Pietro\\Sample\\Test\\test"), runSingle(_, _));
    iteratorOverFiles(new File("C:\\Users\\Pietro\\Sample\\Test\\test"), inferTestCasesSingle(_, _));
    System.out.println("Tests ended")
  }

  def iteratorOverFiles(file : File, f : (File, File) => Unit) : Unit = {
    if(file.isDirectory)
      for(file2 <- file.listFiles())
        iteratorOverFiles(file2, f);
    else {
        val (pathFile, extensionFile) = splitExtension(file);
        if(! extensionFile.equals("test")) f(file, new File(pathFile+".test"));
      };
  }


  private def inferTestCasesSingle[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](sourceCodeFile : File, testFile : File) = {
    val r = initRun(testFile, sourceCodeFile)
    val expectedOutput: Set[ExpectedOutput] = r._1
    val result: Set[Output] = r._2
    System.out.println("The results of the test is:\n")
    for(s <- outputsToExpectedOutputs(result))
      System.out.println(s.toString);
  }

  private def splitExtension(file : File) = {
    val path : String = file.getAbsolutePath;
    (path.substring(0, path.lastIndexOf('.')), path.substring(path.lastIndexOf('.')+1))
  }

  def initRun[I <: HeapIdentifier[I], H <: HeapDomain[H, I], N <: SemanticDomain[N]](testFile: File, sourceCodeFile: File): (Set[ExpectedOutput], Set[Output]) = {
    SystemParameters.setProgressOutput(new StringCollector)
    SystemParameters.setAnalysisOutput(new StringCollector)

    val (methods, analysis, property, analysisParameters, heapanalysis, heapanalysisParameters, expectedOutput) =
      ExpectedOutputParser.parse(new FileReader(testFile));
    val an: SemanticAnalysis[N] = this.getAnalysis(analysis).asInstanceOf[SemanticAnalysis[N]];
    val pr: Property = this.getProperty(an, property);
    SystemParameters.setProperty(pr);
    for ((id, value) <- analysisParameters)
      an.setParameter(id, value);
    val heapan: H = this.getHeapAnalysis(heapanalysis).asInstanceOf[H];
    for ((id, value) <- heapanalysisParameters)
      heapan.setParameter(id, value);

    val compiler: Compiler = this.getCompiler(splitExtension(sourceCodeFile)._2)
    an.reset()
    heapan.reset()
    SystemParameters.addNativeMethodsSemantics(an.getNativeMethodsSemantics)
    SystemParameters.addNativeMethodsSemantics(heapan.getNativeMethodsSemantics)
    SystemParameters.setCompiler(compiler)
    ch.ethz.inf.pm.sample.Main.compile(sourceCodeFile)
    if (heapan.isInstanceOf[NonRelationalHeapDomain[_]]) {
      (heapan.asInstanceOf[NonRelationalHeapDomain[_]]).setType(SystemParameters.getType)
    }
    var domain = an.getInitialState
    var entrydomain = new HeapAndAnotherDomain[N, H, I](domain, heapan)
    var entryvalue = new SymbolicAbstractValue[GenericAbstractState[N, H, I]](scala.Option.apply(null), scala.Option.apply(null))
    var entryState = new GenericAbstractState[N, H, I](entrydomain, entryvalue)
    entryvalue = new SymbolicAbstractValue(new Some(entryState), new Some(SystemParameters.getType))
    entryState = new GenericAbstractState(entrydomain, entryvalue)
    val output: OutputCollector = new OutputCollector;
    ch.ethz.inf.pm.sample.Main.analyze(methods, entryState, output)
    val result = output.warnings
    (expectedOutput, result)
  }

  private def runSingle[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](sourceCodeFile : File, testFile : File) = {
    val r = initRun(testFile, sourceCodeFile)
    val expectedOutput: Set[ExpectedOutput] = r._1
    val result: Set[Output] = r._2
    val (missingExpected, missingWarnings, matched) = check(expectedOutput, result);
    System.out.println(matched + " outputs were produced out of "+expectedOutput.size+" expected outputs.")
    if(! missingExpected.isEmpty)
      System.out.println("Some expected outputs are not produced:\n"+missingExpected.toString());
    if(! missingWarnings.isEmpty)
      System.out.println("Some outputs were not expected:\n"+missingWarnings.toString());
  }

  private def outputsToExpectedOutputs(o : Set[Output]) : Set[ExpectedOutput] = {
    var result : Set[ExpectedOutput] = Set.empty;
    for(s <- o)
      result=result+singleOutputToExpectedOutput(s);
    return result;
  }

  private def singleOutputToExpectedOutput(o : Output) : ExpectedOutput = o match {
    case WarningProgramPoint(pp, message) => return new WarningPP(pp.getLine(), pp.getColumn());
    case ValidatedProgramPoint(pp, message) => return new ValidatedPP(pp.getLine(), pp.getColumn());
    case ch.ethz.inf.pm.sample.property.WarningMethod(classe, method, message) => return new WarningMethod(classe.getName(), method);
    case ch.ethz.inf.pm.sample.property.ValidatedMethod(classe, method, message) => return new ValidatedMethod(classe.getName(), method);
    case ch.ethz.inf.pm.sample.property.InferredContract(c) => return new ch.ethz.inf.pm.sample.test.InferredContract(contractToExpectedContract(c))
  }

  private def contractToExpectedContract(o : ch.ethz.inf.pm.sample.oorepresentation.Annotation) : Contract = o match {
    case ch.ethz.inf.pm.sample.oorepresentation.Invariant(classe, e) =>
      return new Invariant(classe, e)
    case ch.ethz.inf.pm.sample.oorepresentation.Predicate(classe, predName, e) =>
      return new Predicate(classe, predName, e)
    case ch.ethz.inf.pm.sample.oorepresentation.PreCondition(classe, method, e) =>
      return new PreCondition(classe, method, e)
    case ch.ethz.inf.pm.sample.oorepresentation.PostCondition(classe, method, e) =>
      return new PostCondition(classe, method, e)
    case ch.ethz.inf.pm.sample.oorepresentation.LoopInvariant(pp, e) =>
      return new LoopInvariant(pp.getLine(), pp.getColumn(), e)
  }

  private def getCompiler(s : String) : Compiler = s match {
    case "scala" => return new ScalaCompiler();
    case "class" => return new JavaCompiler();
  }

  private def getHeapAnalysis[T <: HeapDomain[T, I], I <: HeapIdentifier[I]](s : String) : HeapDomain[T, I] = {
    for(a <- InstalledPlugins.heapanalyses)
      if(a.getLabel().equals(s))
        return a.asInstanceOf[HeapDomain[T, I]];
    throw new Exception();
  }

  private def getAnalysis(s : String) : SemanticAnalysis[_] = {
    for(a <- InstalledPlugins.analyses)
      if(a.getLabel().equals(s))
        return a;
    return null;
  }

  private def getProperty(a : Analysis, p : String) : Property = {
    for(p1 : Property <- a.getProperties())
      if(p1.getLabel().equals(p))
        return p1;
    return null;
  }


  private def check(exp : Set[ExpectedOutput], output : Set[Output]) : (Set[ExpectedOutput], Set[Output], Int) = {
    var matched : Int = 0;
    var notCoveredOutput : Set[Output] = output;
    var missedExpectedOutput : Set[ExpectedOutput] = exp;
    for(e : ExpectedOutput <- exp)
      for(w : Output <- output)
        if(e.cover(w)) {
           notCoveredOutput=notCoveredOutput-w;
           missedExpectedOutput=missedExpectedOutput-e;
           matched = matched + 1;
        }
    return (missedExpectedOutput, notCoveredOutput, matched);
  }
}