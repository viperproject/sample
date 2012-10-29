package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.userinterfaces.InstalledPlugins
import java.lang.Exception
import ch.ethz.inf.pm.sample.oorepresentation.Compiler
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaCompiler
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.NonRelationalHeapDomain
import ch.ethz.inf.pm.sample.property._
import java.io._
import scala.Some
import ch.ethz.inf.pm.sample.{Main, StringCollector, SystemParameters}
import ch.ethz.inf.pm.sample.abstractdomain._

object Run {
  var verbose = false;

  /**
   * The automatic run of test cases receive some parameters:
   * - "-p <dir>" the directory that contains the test cases
   * - "-o <filename>" the name of the file where the application will store the comparison between
   *     the actual results and the original ones. The file will be places in <dir>. By default it
   *     is "resultOriginal.test"
   * - "-l <filename>" the name of the file where the application will store the comparison between
   *     the actual results and the last ones. The file will be places in <dir>. By default it
   *     is "resultLast.test"
   * - "-i" specifies that we want to infer the output of the tests and not to compare them.
   * - "-v" will produce a verbose output on the console
   * - "-dl" will delete the results of the last run
   * - "-de" will delete the expected results
   *
   * The analysis without "-i" will put for each source code file f contained in the given directory
   * a file f.test_last with the results of the current run. In addition, it will store the comparison
   * between the current results and the original/last run in the main directory.
   * The analysis with "-i" will put for each source code file f contained in the given directory a file
   * f.test with the results of the run. It could be used the first time in order to avoid to write
   * manually all results of the analysis.
   * Each directory containing the source code files has to contain a settings.test file that specify
   * all the settings to run the analysis.
   */
  def main[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](args : Array[String]) = {
    val originalResult : StringCollector = new StringCollector;
    val lastResult : StringCollector = new StringCollector;
    var terminate : Boolean = false;
    val inference : Boolean = if(args.contains("-i")) true else false;
    val directory = getArg("-p", args) match {
      case None =>
        System.out.println("You should explicitly specify the directory that contains the test cases");
        //"C:\\Users\\Pietro\\Sample\\Test\\test\\HeapAnalysis\\"
        "C:\\Users\\Pietro\\Desktop\\VersionForIMDEA\\sample\\Test\\test\\AccessPermissions\\"
      case Some(s) => s
    }
    if(args.contains("-dl")) {
      terminate=true;
      iteratorOverFiles[N, H, I](new File(directory), delete[I, H, N]("test_last", _, _, _, _, originalResult, lastResult, inference));
    }
    if(args.contains("-de")) {
      terminate=true;
      iteratorOverFiles[N, H, I](new File(directory), delete[I, H, N]("test", _, _, _, _, originalResult, lastResult, inference));
    }
    if(terminate) exit(0);
    val originalFile = getArg("-o", args) match {
      case None => "resultOriginal.test"
      case Some(s) => s
    }
    val lastFile = getArg("-l", args) match {
      case None => "resultLast.test"
      case Some(s) => s
    }

    verbose = args.contains("-v")
    System.out.println("Starting the tests")
    iteratorOverFiles[N, H, I](new File(directory), runSingle[N, H, I](_, _, _, _, originalResult, lastResult, inference));

    new File(directory+originalFile).delete();
    new File(directory+lastFile).delete();
    writeStringInFile(directory+originalFile, originalResult.getString);
    writeStringInFile(directory+lastFile, lastResult.getString);
    System.out.println("Tests ended")
  }

  //Given the argument, return the value specified for that argument
  private def getArg(p : String, args : Array[String]) : Option[String] = {
    for(i <- 0 to args.size-1)
      if(args(i).equals(p) && i <= args.size-2 && ! args(i+1).substring(0, 1).equals("-"))
        return Some(args(i+1));
    return None;
  }

  //Write in the specified file the given content
  private def writeStringInFile(path : String, content : String) = {
    val out1 = new BufferedWriter(new FileWriter(path));
		out1.write(content);
		out1.close();
  }

  //Iterate the given function over all the files and directories that are in the given path
  def iteratorOverFiles[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](file : File, f : (File, File, List[String], HeapAndAnotherDomain[N, H, I]) => Unit) : Unit = {
    if(file.isDirectory) {
      if(! file.getName.equals(".svn")) {
        try {
          val file3 : File = new File(file.getAbsolutePath+"\\settings.test");
          val r : (List[String], HeapAndAnotherDomain[N, H, I], Analysis, H) = setAnalysisParameters(file3)
          val methods: List[String] = r._1
          var entryState: HeapAndAnotherDomain[N, H, I] = r._2
          val analysis = r._3;
          val heapanalysis = r._4;
          for(file2 <- file.listFiles()) {
            Main.reset();
            analysis.reset();
            heapanalysis.reset();
            if(! file2.isDirectory) {
              val (pathFile, extensionFile) = splitExtension(file2)
              if(canBeParserized(extensionFile)) {
                if (verbose) System.out.println("Begin of file "+file2.getAbsolutePath);
                f(file2, new File(pathFile+".test"), methods, entryState);
                if (verbose) System.out.println("End of file "+file2.getAbsolutePath);
              }
            };
          }
        }
        catch {
          case e : FileNotFoundException => System.out.println("Directory "+file.getPath()+" does not contain a settings.test.");
          case e : TestException => System.out.println("Error while setting the parameters of the tests."); e.printStackTrace();
          case e => System.out.println("Settings.test in directory "+file.getPath()+" is malformed."); e.printStackTrace();
        }
        finally {
          //Iterate over the subdirectories
          for(file2 <- file.listFiles())
            if(file2.isDirectory)
              iteratorOverFiles(file2, f);
        }
      }
    }
  }

  private def canBeParserized(extension : String) : Boolean = {
    for(a <- InstalledPlugins.compilers)
      if(a.extensions().contains(extension))
        return true;
    return false
  }

  //Return the path and the extension of the given file
  private def splitExtension(file : File) = {
    val path : String = file.getAbsolutePath;
    (path.substring(0, path.lastIndexOf('.')), path.substring(path.lastIndexOf('.')+1))
  }

  //Parser the and set all the parameters of the analysis and set them
  def setAnalysisParameters[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](testFile: File) : (List[String], HeapAndAnotherDomain[N, H, I], Analysis, H) = {
    val (methods, analysis, property, analysisParameters, heapanalysis, heapanalysisParameters) =
      new ExpectedOutputParser().parseAnalysisSettings(new FileReader(testFile));
    val an: SemanticAnalysis[N] = this.getAnalysis(analysis).asInstanceOf[SemanticAnalysis[N]];
    val pr: Property = this.getProperty(an, property);
    SystemParameters.setProperty(pr);
    for ((id, value) <- analysisParameters)
      an.setParameter(id, value);
    val heapan: H = this.getHeapAnalysis(heapanalysis).asInstanceOf[H];
    for ((id, value) <- heapanalysisParameters)
      heapan.setParameter(id, value);
    SystemParameters.resetNativeMethodsSemantics();
    SystemParameters.addNativeMethodsSemantics(an.getNativeMethodsSemantics)
    SystemParameters.addNativeMethodsSemantics(heapan.getNativeMethodsSemantics)
    if (heapan.isInstanceOf[NonRelationalHeapDomain[_]]) {
      (heapan.asInstanceOf[NonRelationalHeapDomain[_]]).setType(SystemParameters.getType)
    }
    var domain = an.getInitialState
    var entrydomain = new HeapAndAnotherDomain[N, H, I](domain, heapan)
    (methods, entrydomain, an, heapan)
  }

  //Run the analysis and return the output
    SystemParameters.setProgressOutput(new StringCollector)
  def initRun[I <: HeapIdentifier[I], H <: HeapDomain[H, I], N <: SemanticDomain[N]](testFile: File, sourceCodeFile: File, methods : List[String], entryDomain : HeapAndAnotherDomain[N, H, I]): (Set[ExpectedOutput], Set[Output]) = {
    SystemParameters.setAnalysisOutput(new StringCollector)

    //if the file specifying the results of the test does not exist, we suppose that we expect no output
    var expectedOutput = Set.empty[ExpectedOutput];
    try {
      expectedOutput=new ExpectedOutputParser().parseOnlyTestResults(new FileReader(testFile));
    }
    catch {
      case e : FileNotFoundException =>
    }
    SystemParameters.setCompiler(this.getCompiler(splitExtension(sourceCodeFile)._2))
    SystemParameters.addNativeMethodsSemantics(SystemParameters.compiler.getNativeMethodsSemantics())
    ch.ethz.inf.pm.sample.Main.compile(sourceCodeFile)
    val output: OutputCollector = new OutputCollector;
    ch.ethz.inf.pm.sample.Main.analyze(methods, new AbstractState[N, H, I](entryDomain, new ExpressionSet(SystemParameters.typ.top())), output)
    val result = output.outputs
    (expectedOutput, result)
  }

  //Check the actual output w.r.t. the expected one and produce messages of warning or confirmation
  def checkOutput[I <: HeapIdentifier[I], H <: HeapDomain[H, I], N <: SemanticDomain[N]](expectedOutput : Set[ExpectedOutput], result : Set[Output], output : StringCollector) {
    val (missingExpected, missingWarnings, matched) = check(expectedOutput, result);
    output.appendString(matched + " outputs were produced out of " + expectedOutput.size + " expected outputs.")
    if (!missingExpected.isEmpty)
      output.appendString("Some expected outputs are not produced:\n" + missingExpected.mkString("\n"));
    if (!missingWarnings.isEmpty)
      output.appendString("Some outputs were not expected:\n" + missingWarnings.mkString("\n"));
  }


  //Run the tests over a single file, and compare the obtained results with the expected ones.
  private def delete[I <: HeapIdentifier[I], H <: HeapDomain[H, I], N <: SemanticDomain[N]](extension : String, sourceCodeFile : File, testFile : File, methods : List[String], entryState : HeapAndAnotherDomain[N, H, I], original : StringCollector, last : StringCollector, inference : Boolean) = {
    val filepath = splitExtension(sourceCodeFile)._1;  ;
    try {
        val lastResult = new File(filepath+"."+extension)
        if(! lastResult.delete())
          System.out.println("I cannot delete "+lastResult.getPath);
        else System.out.println(lastResult.getPath+" deleted");
    }
    catch {
      case _ => //It means the file didn't exist, that is, it's the first time we run the analysis
    }
  }

  //Run the tests over a single file, and compare the obtained results with the expected ones.
  private def runSingle[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](sourceCodeFile : File, testFile : File, methods : List[String], entryState : HeapAndAnotherDomain[N, H, I], original : StringCollector, last : StringCollector, inference : Boolean) = {
    val r = initRun(testFile, sourceCodeFile, methods, entryState)
    val expectedOutput: Set[ExpectedOutput] = r._1
    val result: Set[Output] = r._2
    original.appendString("File "+sourceCodeFile.getPath);
    last.appendString("File "+sourceCodeFile.getPath);
    if(! inference) checkOutput(expectedOutput, result, original)
    val filepath = splitExtension(sourceCodeFile)._1;  ;
    try {
      if(! inference) {
        val lastResult = new File(filepath+".test_last")
        val lastOutput = new ExpectedOutputParser().parseOnlyTestResults(new FileReader(lastResult))
        checkOutput(lastOutput, result, last)
      }
    }
    catch {
      case _ => //It means the file didn't exist, that is, it's the first time we run the analysis
    }
    if(! inference) writeStringInFile(filepath+".test_last", outputsToExpectedOutputs(result).mkString("\n"));
    else writeStringInFile(filepath+".test", outputsToExpectedOutputs(result).mkString("\n"));
  }

  //Translate the outputs to expected outputs
  private def outputsToExpectedOutputs(o : Set[Output]) : Set[ExpectedOutput] = {
    var result : Set[ExpectedOutput] = Set.empty;
    for(s <- o)
      result=result+singleOutputToExpectedOutput(s);
    return result;
  }

  //Translate a single output to a single expected output
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

  //Given the extension, return the compiler to parser it
  private def getCompiler(s : String) : Compiler = {
    for(a <- InstalledPlugins.compilers)
      if(a.extensions().contains(s))
        return a;
    throw new TestException("Compiler not found");
  }

  //Return the heap analysis represented by the given string using the labels of the existing heap analyses
  private def getHeapAnalysis[T <: HeapDomain[T, I], I <: HeapIdentifier[I]](s : String) : HeapDomain[T, I] = {
    for(a <- InstalledPlugins.heapanalyses)
      if(a.getLabel().equals(s))
        return a.asInstanceOf[HeapDomain[T, I]];
    throw new TestException("Heap analysis not found");
  }

  //Return the analysis represented by the given string using the labels of the existing analyses
  private def getAnalysis(s : String) : SemanticAnalysis[_] = {
    for(a <- InstalledPlugins.analyses)
      if(a.getLabel().equals(s))
        return a;
    throw new TestException("Analysis not found");
  }

  //Return the property represented by the given string using the labels of the existing properties
  private def getProperty(a : Analysis, p : String) : Property = {
    for(p1 : Property <- a.getProperties())
      if(p1.getLabel().equals(p))
        return p1;
    throw new TestException("Property not found");
  }

  //Check if the output matches the expected output, and returns the set of missing expected outputs, outputs
  //that were not expected, and the number of output that were expected.
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

class TestException(s : String) extends Exception(s)