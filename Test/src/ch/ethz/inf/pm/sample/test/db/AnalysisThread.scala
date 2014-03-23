package ch.ethz.inf.pm.sample.test.db

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Compiler
import ch.ethz.inf.pm.sample.property.{ValidatedProgramPoint, WarningProgramPoint, OutputCollector, Property}
import java.util.Date
import ch.ethz.inf.pm.sample.{StringCollector, SystemParameters}
import java.sql.Statement


class AnalysisThread[T <: SemanticDomain[T], N <: SemanticAnalysis[T], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](
                                                                                                                       val pubID: String,
                                                                                                                       val idProgram: Int,
                                                                                                                       val idTestRun: Int,
                                                                                                                       val semanticAnalysis: N,
                                                                                                                       val heapDomain: H,
                                                                                                                       val compiler: Compiler,
                                                                                                                       val property: Property,
                                                                                                                       val stmt: Statement
                                                                                                                       ) extends Thread {

  override def run() {
    System.gc()
    System.runFinalization()

    println("Program: " + pubID + " - " + new Date().toString + "// Efficient version")
    semanticAnalysis.reset()
    heapDomain.reset()
    SystemParameters.setCompiler(compiler)
    SystemParameters.setAnalysisOutput(new StringCollector)
    SystemParameters.setProgressOutput(new StringCollector)
    SystemParameters.compiler.reset()
    SystemParameters.resetNativeMethodsSemantics()
    val classes = try {
      SystemParameters.compilerTimer.start()
      SystemParameters.compiler.compileFile("td://" + pubID)
    }
    catch {
      case e: Throwable => println("Compiler's error: " + e.toString)
        stmt.executeUpdate("INSERT INTO BrokenCompilations(TestRun, Program, Error) VALUES(" + idTestRun + ", " + idProgram + ", '" + e.toString.replace("'", "''") + "')")
        stmt.executeUpdate("DELETE FROM RuntimeErrors WHERE Program=" + idProgram + " AND TestRun=" + idTestRun)
        SystemParameters.compilerTimer.stop()
        return
    }

    SystemParameters.compilerTimer.stop()
    val output: OutputCollector = new OutputCollector
    SystemParameters.setProperty(property)

    SystemParameters.addNativeMethodsSemantics(SystemParameters.compiler.getNativeMethodsSemantics())

    val domain: T = semanticAnalysis.getInitialState()
    val entryDomain: HeapAndAnotherDomain[T, H, I] = HeapAndAnotherDomain[T, H, I](domain, heapDomain)
    val entryValue = ExpressionSet()
    val entryState = new AbstractState[T, H, I](entryDomain, entryValue)
    var methods = List.empty[String]
    for (c <- classes)
      for (m <- c.methods)
        methods = methods ::: m.name.toString :: Nil
    try {
      semanticAnalysis.analyze(methods, entryState, output)
      val outputs = output.outputs


      val warnings = outputs.count {
        case x: WarningProgramPoint => true
        case _ => false
      }
      val computed = outputs.size - warnings

      System.out.println("Warnings:" + warnings)
      System.out.println("Validated:" + computed)
      val compilerTime: Double = SystemParameters.compilerTimer.totalTime.toDouble / 1000
      val analysisTime: Double = (SystemParameters.domainTimer.totalTime + SystemParameters.heapTimer.totalTime).toDouble / 1000
      val propertyTime: Double = SystemParameters.propertyTimer.totalTime.toDouble / 1000
      System.out.println("AnalysisTime:" + analysisTime)

      val sql = "INSERT INTO Analyses(Program, TestRun, CompilerTime, AnalysisTime, PropertyTime, Warnings, Validated) " +
        "VALUES (" + idProgram + ", " + idTestRun + ", " + compilerTime + ", " + analysisTime + ", " + propertyTime + ", " + warnings + ", " + computed + ")"
      stmt.executeUpdate(sql)

      for (res <- outputs) {
        val programpoint = res match {
          case x: WarningProgramPoint => x.pp.toString
          case x: ValidatedProgramPoint => x.pp.toString
          case _ => -1
        }
        val msg = res match {
          case x: WarningProgramPoint => "WARNING:"
          case x: ValidatedProgramPoint => "VALIDATED:"
          case _ => -1
        }
        stmt.executeUpdate("INSERT INTO Output(TestRun, Program, ProgramPoint, Message) " +
          "VALUES (" + idTestRun + ", " + idProgram + ", '" + programpoint.toString.replace("'", "''") + "', '" + msg + res.getMessage().replace("'", "''") + "')")
        stmt.executeUpdate("DELETE FROM RuntimeErrors WHERE Program=" + idProgram + " AND TestRun=" + idTestRun)
      }
    }
    catch {
      case e: Throwable =>
        println("Error when running the analysis: " + e.toString)
        try {
          stmt.executeUpdate("INSERT INTO BrokenAnalyses(TestRun, Program, Error) " +
            "VALUES (" + idTestRun + ", " + idProgram + ", '" + e.toString.replace("'", "''") + "')")
          stmt.executeUpdate("DELETE FROM RuntimeErrors WHERE Program=" + idProgram + " AND TestRun=" + idTestRun)
        } catch {
          case _: Throwable => println("Program " + idProgram + " already in the broken analyses of test run " + idTestRun)
        }
    }
    SystemParameters.domainTimer.reset()
    SystemParameters.heapTimer.reset()
    SystemParameters.propertyTimer.reset()
    SystemParameters.compilerTimer.reset()
  }

}