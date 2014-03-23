package ch.ethz.inf.pm.sample.test.db

import ch.ethz.inf.pm.sample.userinterfaces.InstalledPlugins
import java.lang.Exception
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample._
import abstractdomain._
import java.sql.{SQLException, ResultSet, Statement, DriverManager}
import java.util.Date
import java.io._
import ch.ethz.inf.pm.td.webapi._
import scala.Some


object RunWithDB {

  private def getConnection = DriverManager.getConnection(
    System.getProperty("sql_connection", "jdbc:mysql://127.0.0.1:3306/mydb"),
    System.getProperty("sql_user", "root"),
    System.getProperty("sql_password", "")
  )

  private val c = getConnection
  private val stmt = c.createStatement()

  private var SampleHome = new BufferedReader(new InputStreamReader(Runtime.getRuntime.exec(Array[String]("sh", "-c", "echo ~")).getInputStream())).readLine() + "/Sample/"
  private var OutputDirectory = SampleHome

  def main(args: Array[String]): Unit = {
    extractMode(args.toList) match {
      case "-i" =>
        setOptionalParameters(args)
        mainMenu()
      case "-r" =>
        val (id, timeout) = extractIdTimeout(args)
        setOptionalParameters(args)
        runAnalyses(id, timeout * 1000)
      case "-t" =>
        populateTags()
      case "" =>
        println("Wrong option\n" +
          "One of the two following parameters is mandatory:\n" +
          "-i => run the command line interface for test runs\n" +
          "-t => populate tags\n" +
          "-r <id> <timeout>  => run the testrun with id <id> given a timeout of <timeout> seconds\n\n" +
          "In addition, the following two parameters are optional:\n" +
          "-sh <dir> => set the home directory of Sample to <dir>. By default, <dir>=" + SampleHome + "\n" +
          "-oh <dir> => set the directory where to write the reports of the analysis to <dir>. By default, <dir>=" + OutputDirectory
        )

    }
  }

  def populateTags() {
    val c = getConnection
    val stmt = c.createStatement()
    val iterator: ScriptQuery = new ScriptQuery()
    for (script <- iterator) {
      println("Script " + script.id + " tags " + script.toptagids)
      for (tag <- script.toptagids) {
        val sql = "INSERT INTO ProgramTags(Name, Tag) VALUES ('" + script.id + "', '" + tag.replace("'", "''") + "')"
        try {
          stmt.executeUpdate(sql)
        }
        catch {
          case e: SQLException => println("Database error:\n" + e.getMessage); e.printStackTrace()
        }
      }
    }
  }

  private def setOptionalParameters(args: Array[String]) {
    extractParameterValue(args, "-sh") match {
      case Some(s) => SampleHome = s
      case None =>
    }
    extractParameterValue(args, "-oh") match {
      case Some(s) => OutputDirectory = s
      case None =>
    }
  }

  //given the arguments' list, checks whether we have to run the menu mode or run the analysis
  private def extractMode(args: List[String]): String = {
    for (arg <- args) {
      if (arg.equals("-i")) return "-i"
      else if (arg.equals("-r")) return arg
      else if (arg.equals("-t")) return arg
    }
    ""
  }

  //given the arguments' list and a parameter, return the value of the given parameter
  private def extractParameterValue(args: Array[String], parameter: String): Option[String] = {
    var result: Option[String] = None
    try {
      for (i <- 0 to args.size - 1) {
        if (args(i).equals(parameter) && args.size >= i + 1) result = Some(args(i + 1))
      }
    }
    catch {
      case _: Throwable =>
    }
    result
  }


  //if we are in the mode to run a given testrun, it extracts the id of the testrun and the timeout
  private def extractIdTimeout(args: Array[String]): (Int, Int) = {
    var result = (-1, -1)
    try {
      for (i <- 0 to args.size - 1) {
        if (args(i).equals("-r") && args.size >= i + 2) result = (args(i + 1).toInt, args(i + 2).toInt)
      }
    }
    catch {
      case _: Throwable =>
    }
    if (result._1 == 0 - 1 || result._2 == 0 - 1) {
      println("The parameter for running the test run do not conform the standard '-r <id> <timeout>'")
      System.exit(1)
      (-1, -1)
    }
    else result
  }


  //display a menu and manage the input from the user
  private def menu(message: String, selection: Int => Boolean) = {
    var flag = true
    while (flag) {
      println(message)
      val input = readLine()
      var value = 0 - 1
      try {
        value = input.toInt
      }
      catch {
        case _: Throwable => println("Wrong input: " + input); readLine()
      }
      if (value != 0 - 1 && !selection(value))
        flag = false
    }
  }

  //display and manage a menu for a given test run
  private def testRunMenu(idTestRun: Int): Unit =
    menu(getTestRunMenu(idTestRun), {
      case 1 => println(getStatistics(idTestRun)); true
      case 2 =>

        var out = new PrintWriter(OutputDirectory + "RuntimeErrors.tsv")
        out.println(getRuntimeErrors(idTestRun))
        out.close()
        println("Runtime errors written in " + OutputDirectory + "RuntimeErrors.tsv")

        out = new PrintWriter(OutputDirectory + "CompilationErrors.tsv")
        out.println(getErrors("BrokenCompilations", idTestRun))
        out.close()
        println("Compilation errors written in " + OutputDirectory + "CompilationErrors.tsv")

        out = new PrintWriter(OutputDirectory + "AnalysisErrors.tsv")
        out.println(getErrors("BrokenAnalyses", idTestRun))
        out.close()
        println("Analysis errors written in " + OutputDirectory + "AnalysisErrors.tsv")

        out = new PrintWriter(OutputDirectory + "Statistics.tsv")
        out.println(getStatistics(idTestRun))
        out.close()
        println("Statistics written in " + OutputDirectory + "Statistics.tsv")

        out = new PrintWriter(OutputDirectory + "Warning.tsv")
        out.println(getOutput("WARNING:", idTestRun))
        out.close()
        println("Warnings written in " + OutputDirectory + "Warning.tsv")

        out = new PrintWriter(OutputDirectory + "Validated.tsv")
        out.println(getOutput("VALIDATED:", idTestRun))
        out.close()
        println("Validated properties written in " + OutputDirectory + "Validated.tsv")

        out = new PrintWriter(OutputDirectory + "Alloutputs.tsv")
        out.println(getOutput("", idTestRun))
        out.close()
        println("All outputs written in " + OutputDirectory + "Alloutputs.tsv")

        println("Press a key to go to the test run menu")
        readLine()

        true

      case 3 =>
        val previousTestRun = getExistingTestRun("compare", 10)

        var out = new PrintWriter(OutputDirectory + "DifferentCompilationErrors.tsv")
        out.println(getDifferentErrorMessages(idTestRun, previousTestRun, "BrokenCompilations"))
        out.close()
        println("Different compilation errors written in " + OutputDirectory + "DifferentCompilationErrors.tsv")

        out = new PrintWriter(OutputDirectory + "DifferentAnalysisErrors.tsv")
        out.println(getDifferentErrorMessages(idTestRun, previousTestRun, "BrokenAnalyses"))
        out.close()
        println("Different analysis errors written in " + OutputDirectory + "DifferentAnalysisErrors.tsv")

        out = new PrintWriter(OutputDirectory + "DifferentOutputs.tsv")
        out.println(getDifferentOutputs(idTestRun, previousTestRun))
        out.close()
        println("Different outputs written in " + OutputDirectory + "DifferentOutputs.tsv")

        out = new PrintWriter(OutputDirectory + "DifferentValidatedWarningOutputs.tsv")
        out.println(getDifferentValidatedWarningOutputs(idTestRun, previousTestRun))
        out.close()
        println("Different validated/warning outputs written in " + OutputDirectory + "DifferentValidatedWarningOutputs.tsv")

        out = new PrintWriter(OutputDirectory + "NewCompilationErrors.tsv")
        out.println(getNewErrorMessages(idTestRun, previousTestRun, "BrokenCompilations"))
        out.close()
        println("New compilation errors written in " + OutputDirectory + "NewCompilationErrors.tsv")

        out = new PrintWriter(OutputDirectory + "NewAnalysisErrors.tsv")
        out.println(getNewErrorMessages(idTestRun, previousTestRun, "BrokenAnalyses"))
        out.close()
        println("New analysis errors written in " + OutputDirectory + "NewAnalysisErrors.tsv")

        out = new PrintWriter(OutputDirectory + "NewOutputs.tsv")
        out.println(getNewOutputs(idTestRun, previousTestRun))
        out.close()
        println("New outputs written in " + OutputDirectory + "NewOutputs.tsv")

        out = new PrintWriter(OutputDirectory + "RemovedCompilationErrors.tsv")
        out.println(getNewErrorMessages(previousTestRun, idTestRun, "BrokenCompilations"))
        out.close()
        println("Removed compilation errors written in " + OutputDirectory + "RemovedCompilationErrors.tsv")

        out = new PrintWriter(OutputDirectory + "RemovedAnalysisErrors.tsv")
        out.println(getNewErrorMessages(previousTestRun, idTestRun, "BrokenAnalyses"))
        out.close()
        println("Removed analysis errors written in " + OutputDirectory + "RemovedAnalysisErrors.tsv")

        out = new PrintWriter(OutputDirectory + "RemovedOutputs.tsv")
        out.println(getNewOutputs(previousTestRun, idTestRun))
        out.close()
        println("Removed outputs written in " + OutputDirectory + "RemovedOutputs.tsv")

        println("Press a key to go to the test run menu")
        readLine()

        true
      case 4 =>
        println("Timeout? (sec)")
        val input = readLine()
        var timeout = -1
        while (timeout <= 0) {
          try {
            timeout = input.toInt
          }
          catch {
            case _ => println("Wrong value"); readLine()
          }
        }
        val cmds = new Array[String](3)
        cmds.update(0, "/bin/bash")
        cmds.update(1, "-c")
        cmds.update(2, SampleHome + "Test/runTestRun.sh " + idTestRun.toString + " " + timeout + " -sh " + SampleHome + " -oh " + OutputDirectory)
        val p = Runtime.getRuntime.exec(cmds)
        val output = new BufferedReader(new InputStreamReader(p.getInputStream))
        println(output.readLine)
        var s: String = ""
        while ( {
          s = output.readLine
          s
        } != null)
          println(s)
        true

      case 5 =>
        menu(getPopulateMenu(idTestRun), {
          case 1 => fromTable2ToBeAnalyzed(idTestRun, "RuntimeErrors", "TRUE"); true
          case 2 => fromTable2ToBeAnalyzed(idTestRun, "BrokenAnalyses", "TRUE"); true
          case 3 => fromTable2ToBeAnalyzed(idTestRun, "BrokenAnalyses", "Error='java.lang.ThreadDeath'"); true
          case 4 => fromTable2ToBeAnalyzed(idTestRun, "BrokenCompilations", "TRUE"); true
          case 9 => false
          case 0 => println("See you!"); sys.exit(0); false
        }
        )
        true
      case 9 =>
        false
      case 0 => println("See you!"); sys.exit(0); false
    }
    )


  //display and manage the main menu
  private def mainMenu(): Unit =
    menu(getInitialMenu, {
      case 1 => testRunMenu(createTestRun()); true
      case 2 => testRunMenu(copyTestRun()); true
      case 3 => testRunMenu(getExistingTestRun("select", 10)); true
      case 4 => deleteTestRun(); true
      case 0 => println("See you!"); sys.exit(0); false
    }
    )


  //text of the menus
  private def getPopulateMenu(id: Int) =
    "Test run #" + id + "\n" +
      "Please select one of the following options:\n" +
      "1) Move runtime errors to \"to be analyzed\" list\n" +
      "2) Move broken analyses to \"to be analyzed\" list\n" +
      "3) Move timeouted analyses to \"to be analyzed\" list\n" +
      "4) Move compilation errors to \"to be analyzed\" list\n" +
      "9) Go to the previous menu\n" +
      "0) Exit"

  private def getInitialMenu =
    "Welcome on the test run system of Sample\n" +
      "Please select one of the following options:\n" +
      "1) Create a new test run from scratch\n" +
      "2) Create a new test run by copying the parameterSystems of an existing analysis\n" +
      "3) Select an existing test run\n" +
      "4) Delete an existing test run\n" +
      "0) Exit"

  private def getTestRunMenu(id: Int) =
    "Test run #" + id + "\n" +
      "Please select one of the following options:\n" +
      "1) Show the statistics of the current test run\n" +
      "2) Write on tsv files the reports of the current test run\n" +
      "3) Compare the current test run with an existing one\n" +
      "4) Run the current test run\n" +
      "5) Add more programs to be analyzed\n" +
      "9) Go to the previous menu\n" +
      "0) Exit"

  //remove a test run from the database, removing (hopefully) all the records pointing to it in the (hopefully)
  //right order. it asks a further confirmation
  private def deleteTestRun() = {
    val id = getExistingTestRun("delete", 30)
    println("Digit 'y' if you want to delete the test run " + id)
    val input = readLine()
    if (input.equals("y")) {
      stmt.executeUpdate("DELETE FROM Analyses WHERE TestRun=" + id)
      stmt.executeUpdate("DELETE FROM BrokenAnalyses WHERE TestRun=" + id)
      stmt.executeUpdate("DELETE FROM BrokenCompilations WHERE TestRun=" + id)
      stmt.executeUpdate("DELETE FROM Output WHERE TestRun=" + id)
      stmt.executeUpdate("DELETE FROM RuntimeErrors WHERE TestRun=" + id)
      stmt.executeUpdate("DELETE FROM ToBeAnalyzed WHERE TestRun=" + id)
      stmt.executeUpdate("DELETE FROM TestRunAnalysisParameters WHERE TestRun=" + id)
      stmt.executeUpdate("DELETE FROM TestRunHeapAnalysisParameters WHERE TestRun=" + id)
      stmt.executeUpdate("DELETE FROM TestRun WHERE idTestRun=" + id)
      println("Test run " + id + " successfully deleted")
      readLine()
    }
  }

  //create a copy of an existing test run
  private def copyTestRun(): Int = createTestRunFrom(getExistingTestRun("copy", 5))

  //print the existing test runs. for now, I'm not printing the parameters, this would be too verbose
  private def printExistingTestRuns(max: Int) = {
    val rows = stmt.executeQuery("SELECT * FROM TestRun ORDER BY idTestRun DESC LIMIT 0, " + max)
    while (rows.next()) {
      println("Id: " + rows.getString("idTestRun"))
      println("Created: " + rows.getString("Date"))
      println("Value analysis: " + rows.getString("Analysis"))
      println("Heap analysis: " + rows.getString("HeapAnalysis"))
      println("Compiler: " + rows.getString("Compiler"))
      println("Iterator: " + rows.getString("Iterator"))
    }
    rows.close()
  }

  //ask the user to provide the id of an existing test run
  private def getExistingTestRun(action: String, max: Int): Int = {
    printExistingTestRuns(max)
    while (true) {
      println("Write the id of the test run you want to " + action + ":")
      val input = readLine()
      if (stmt.executeQuery("SELECT * FROM TestRun WHERE idTestRun=" + input.toInt).next())
        return input.toInt
      else {
        println("Wrong id, please retry")
        readLine()
      }
    }
    -1
  }

  //create a new test run by copying the parameters of an existing test run
  private def createTestRunFrom(id: Int): Int = {
    var rs = c.createStatement().executeQuery("SELECT * FROM TestRun WHERE idTestRun=" + id)
    if (!rs.next()) return -1
    val newId = recordTestRun(rs.getString("Compiler"), rs.getString("Analysis"), rs.getString("HeapAnalysis"), rs.getString("Property"), rs.getString("Iterator"))
    rs.close()
    rs = c.createStatement().executeQuery("SELECT * FROM TestRunAnalysisParameters WHERE TestRun=" + id)
    while (rs.next())
      recordParameter("TestRunAnalysisParameters", rs.getString("Name"), rs.getString("Value"), newId)
    rs.close()
    rs = c.createStatement().executeQuery("SELECT * FROM TestRunHeapAnalysisParameters WHERE TestRun=" + id)
    while (rs.next())
      recordParameter("TestRunHeapAnalysisParameters", rs.getString("Name"), rs.getString("Value"), newId)
    rs.close()
    return newId
  }


  //This method asks the user for all the parameters of a test run, and record them in the database
  private def createTestRun(): Int = {
    var config: Configuration = null
    var custom = false
    println("Existing configurations:\n" + Configurations.configs.map(c => c.label).mkString("\n"))
    while (config == null && !custom) {
      println("Name of the configuration or custom for custom configuration:")
      val input: String = readLine()
      if (input != "custom") {
        name2Object[Configuration](input, Configurations.configs.toArray, c => c.label) match {
          case Some(c) => config = c
          case None => println("Unknown configuration, please try again")
        }
      } else {
        custom = true
      }
    }

    if (!custom) {

      var iterator: IteratorOverPrograms = null
      println("Existing iterators:\n" + Iterators.iterators.map(c => c.label).mkString("\n"))
      while (iterator == null) {
        println("Name of the iterator:")
        val input: String = readLine()
        name2Object[IteratorOverPrograms](input, Iterators.iterators, p => p.label) match {
          case Some(i) => iterator = i
          case None => println("Unknown iterator, please try again")
        }
      }

      val id = recordTestRun(config.compiler, config.analysis, config.heapAnalysis, config.property, iterator.label)

      for ((a, b) <- config.parameters)
        recordParameter("TestRunAnalysisParameters", a, b, id)
      for ((a, b) <- config.heapParameters)
        recordParameter("TestRunHeapAnalysisParameters", a, b, id)

      populateToBeAnalyzed(iterator, name2Object[Compiler](config.compiler, InstalledPlugins.compilers, p => p.getLabel()).get, id)

      return id
    }

    var compiler: Compiler = null
    println("Existing compilers:\n" + InstalledPlugins.compilers.map(c => c.getLabel()).mkString("\n"))
    while (compiler == null) {
      println("Name of the compiler:")
      val input: String = readLine()
      name2Object[Compiler](input, InstalledPlugins.compilers, p => p.getLabel()) match {
        case Some(cs) => compiler = cs
        case None => println("Unknown compiler, please try again")
      }
    }

    var analysis: Analysis = null
    println("Existing value analyses:\n" + InstalledPlugins.analyses.map(c => c.getLabel()).mkString("\n"))
    while (analysis == null) {
      println("Name of the value analysis:")
      val input: String = readLine()
      name2Object[SemanticAnalysis[_]](input, InstalledPlugins.analyses, a => a.getLabel()) match {
        case Some(a) => analysis = a
        case None => println("Unknown value analysis, please try again")
      }
    }


    var parameters: Map[String, String] = Map.empty[String, String]
    println("Possible analysis's parameters:\n" + analysis.parameters().map(p => p._1).mkString("\n"))
    var input: String = " "
    while (!input.equals("")) {
      println("Name of the parameter (<enter> to stop):")
      input = readLine()
      if (!input.equals("")) {
        if (analysis.parameters().map(p => p._1).contains(input)) {
          println("Value:")
          val value = readLine()
          parameters = parameters + ((input, value))
        }
        else println("Unknown parameter, please try again")
      }
    }



    var heapanalysis: Analysis = null
    println("Existing heap analyses:\n" + InstalledPlugins.heapanalyses.map(c => c.getLabel()).mkString("\n"))
    while (heapanalysis == null) {
      println("Name of the heap analysis:")
      val input: String = readLine()
      name2Object[HeapDomain[_, _]](input, InstalledPlugins.heapanalyses, a => a.getLabel()) match {
        case Some(a) => heapanalysis = a
        case None => println("Unknown heap analysis, please try again")
      }
    }


    var heapparameters: Map[String, String] = Map.empty[String, String]
    println("Possible analysis's parameters:\n" + heapanalysis.parameters().map(p => p._1).mkString("\n"))
    input = " "
    while (!input.equals("")) {
      println("Name of the parameter (<enter> to stop):")
      input = readLine()
      if (!input.equals("")) {
        if (heapanalysis.parameters().map(p => p._1).contains(input)) {
          println("Value:")
          val value = readLine()
          heapparameters = heapparameters + ((input, value))
        }
        else println("Unknown parameter, please try again")
      }
    }

    var property: Property = null
    println("Existing properties:\n" + analysis.getProperties.map(c => c.getLabel()).mkString("\n"))
    while (property == null) {
      println("Name of the property:")
      val input: String = readLine()
      name2Object[Property](input, analysis.getProperties.toArray, p => p.getLabel()) match {
        case Some(p) => property = p
        case None => println("Unknown property, please try again")
      }
    }


    var iterator: IteratorOverPrograms = null
    println("Existing iterators:\n" + Iterators.iterators.map(c => c.label).mkString("\n"))
    while (iterator == null) {
      println("Name of the iterator:")
      val input: String = readLine()
      name2Object[IteratorOverPrograms](input, Iterators.iterators, p => p.label) match {
        case Some(i) => iterator = i
        case None => println("Unknown iterator, please try again")
      }
    }

    val id = recordTestRun(compiler.getLabel(), analysis.getLabel(), heapanalysis.getLabel(), property.getLabel(), iterator.label)

    for ((a, b) <- parameters)
      recordParameter("TestRunAnalysisParameters", a, b, id)
    for ((a, b) <- heapparameters)
      recordParameter("TestRunHeapAnalysisParameters", a, b, id)

    populateToBeAnalyzed(iterator, compiler, id)

    id
  }


  //create the test run
  private def recordTestRun(compiler: String, valueanalysis: String, heapanalysis: String, property: String, iterator: String): Int = {
    stmt.executeUpdate("INSERT INTO TestRun(Analysis, HeapAnalysis, Compiler, Property, Iterator, Date) VALUES ('" + valueanalysis + "', '" + heapanalysis + "', '" + compiler + "', '" + property + "', '" + iterator + "', '" + new Date().toString + "')", Statement.RETURN_GENERATED_KEYS)
    val rs: ResultSet = stmt.getGeneratedKeys
    rs.next()
    val id = rs.getInt(1)
    id
  }

  //add all the programs contained in the iterator to the to be analyzed list of the current test run
  def populateToBeAnalyzed(iterator: IteratorOverPrograms, compiler: Compiler, idTestRun: Int) {
    println("STARTING TO ADD PROGRAMS TO THE TODO LIST")
    while (iterator.hasNext) {
      var program: String = "<Program not compiled>"
      try {
        program = iterator.next().id
        if (!program.isEmpty) {
          println("Adding program " + program)
          val idprogram = add2Programs(program, compiler)
          println("Id: " + idprogram)
          if (add2ToBeAnalyzed(idprogram, idTestRun)) println("Program " + idprogram + " added to test run " + idTestRun)
          else println("Program " + idprogram + " already in the test run " + idTestRun)
        }
      }
      catch {
        case e: Throwable =>
          println(e)
          e.printStackTrace()
          println("Program " + program + " not added.\n" + e.getMessage)
      }
    }
    println("END OF ADDING PROGRAMS TO THE TODO LIST")
  }

  //record [heap] analysis' parameters in the corresponding table
  private def recordParameter(table: String, name: String, value: String, testRun: Int): Int =
    stmt.executeUpdate("INSERT INTO " + table + "(TestRun, Name, Value) VALUES (" + testRun.toString + ", '" + name + "', '" + value + "')")

  //given a name, an array of objects, and a function getName that given an object returns its name, this method
  //returns the object corresponding to its name, or None if such object does not exist
  private def name2Object[A](name: String, arr: Array[A], getName: A => String): Option[A] = {
    var analysis: Array[A] = arr.filter(p => getName(p).equals(name))
    if (analysis.size == 0) return None
    if (analysis.size > 1) throw new TestRunException("More than one " + arr.head.getClass.getName + " object with name " + name)
    else Some(analysis.head)
  }


  //return a string containing the runtime errors
  def getRuntimeErrors(idTestRun: Int) = {
    var output: String = ""
    val sql = "SELECT * FROM RuntimeErrors re, Programs p WHERE re.Program=p.ProgramID AND TestRun=" + idTestRun
    val rows = stmt.executeQuery(sql)
    output = output + "\nProgram\n"
    while (rows.next())
      output = output + "" + rows.getString("Name") + "\n"
    output
  }

  //return a string containing the warnings/validated properties/all outputs
  def getOutput(typ: String, idTestRun: Int) = {
    var output: String = ""
    val sql = "SELECT * FROM Output o, Programs p WHERE o.Program=p.ProgramID AND TestRun=" + idTestRun + " AND Message LIKE '" + typ + "%' ORDER BY Name"
    val rows = stmt.executeQuery(sql)
    output = output + "\nProgram\tProgram point\nMessage\n"
    while (rows.next())
      output = output + "" + rows.getString("Name") + "\t" + rows.getString("ProgramPoint") + "\t" + rows.getString("Message").substring(typ.length) + "\n"
    output
  }


  //return a string containing the error messages that were not in the previous test run
  def getNewErrorMessages(currentTestRun: Int, previousTestRun: Int, table: String) = {
    var output: String = ""
    val sql = "SELECT * FROM " + table + " ba1, Programs p WHERE ba1.Program=p.ProgramId AND ba1.TestRun=" + currentTestRun + " AND " +
      "NOT EXISTS ( " +
      "SELECT * FROM BrokenAnalyses ba2 " +
      "WHERE ba2.TestRun=" + previousTestRun + " AND ba2.Program=ba1.Program " +
      ");"

    val rows = stmt.executeQuery(sql)
    output = output + "Program\tNew error message\n"
    while (rows.next()) {
      output = output + rows.getString("Name") + "\t" + rows.getString("Error").replace("\n", ";").replace("\t", " ") + "\n"
    }
    output

  }

  //return a string containing the outputs that were not in the previous test run
  def getNewOutputs(currentTestRun: Int, previousTestRun: Int) = {
    var output: String = ""
    val sql = "SELECT * FROM Output bc1, Programs p WHERE bc1.Program=p.ProgramId AND bc1.TestRun=" + currentTestRun + " AND " +
      "NOT EXISTS ( " +
      "SELECT * FROM Output bc2 " +
      "WHERE bc2.TestRun=" + previousTestRun + " AND bc2.Program=bc1.Program AND bc1.ProgramPoint=bc2.ProgramPoint" +
      ");"


    val rows = stmt.executeQuery(sql)
    output = output + "Program\tProgramPoint\tNew output\n"
    while (rows.next()) {
      output = output + rows.getString("Name") + "\t" + rows.getString("ProgramPoint") + "\t" + rows.getString("Message").replace("\n", ";").replace("\t", " ") + "\n"
    }
    output

  }

  //return a string containing the error messages that are different among two test runs for a given table
  def getDifferentErrorMessages(currentTestRun: Int, previousTestRun: Int, table: String) = {
    var output: String = ""
    val sql = "SELECT *, bc1.Error AS CurrentMessage, bc2.Error AS PreviousMessage FROM " + table + " bc1, " + table + " bc2, Programs p WHERE p.ProgramId=bc1.Program AND bc1.TestRun=" + currentTestRun + " AND bc2.TestRun=" + previousTestRun + " AND bc1.Program=bc2.Program AND bc1.Error !=bc2.Error"

    val rows = stmt.executeQuery(sql)
    output = output + "Program\tCurrent message\tPrevious message\n"
    while (rows.next()) {
      output = output + rows.getString("Name") + "\t" + rows.getString("CurrentMessage").replace("\n", ";").replace("\t", " ") + "\t" + rows.getString("PreviousMessage").replace("\n", ";").replace("\t", " ") + "\n"
    }
    output

  }

  //return a string containing the outputs that are different among two test runs for a given table
  def getDifferentOutputs(currentTestRun: Int, previousTestRun: Int) = {
    var output: String = ""
    val sql = "SELECT *, bc1.Message AS CurrentMessage, bc2.Message AS PreviousMessage FROM Output bc1, Output bc2, Programs p WHERE p.ProgramId=bc1.Program AND bc1.TestRun=" + currentTestRun + " AND bc2.TestRun=" + previousTestRun + " AND bc1.Program=bc2.Program AND bc1.ProgramPoint=bc2.ProgramPointAND bc1.Message!=bc2.Message"

    val rows = stmt.executeQuery(sql)
    output = output + "Program\tProgramPoint\tCurrent message\tPrevious message\n"
    while (rows.next()) {
      output = output + rows.getString("Name") + "\t" + rows.getString("ProgramPoint") + "\t" + rows.getString("CurrentMessage").replace("\n", ";").replace("\t", " ") + "\t" + rows.getString("PreviousMessage").replace("\n", ";").replace("\t", " ") + "\n"
    }
    output

  }

  //return a string containing the outputs that are different among two test runs for a given table
  def getDifferentValidatedWarningOutputs(currentTestRun: Int, previousTestRun: Int) = {
    var output: String = ""
    val sql = "SELECT *, bc1.Message AS CurrentMessage, bc2.Message AS PreviousMessage FROM Output bc1, Output bc2, Programs p " +
      "WHERE p.ProgramId=bc1.Program AND bc1.TestRun=" + currentTestRun + " AND bc2.TestRun=" + previousTestRun + " AND bc1.Program=bc2.Program AND bc1.ProgramPoint=bc2.ProgramPoint AND bc1.Message!=bc2.Message " +
      "AND (" +
      "(bc1.Message LIKE 'WARNING%' AND bc2.Message LIKE 'VALIDATED%')" +
      "OR (bc1.Message LIKE 'VALIDATED%' AND bc2.Message LIKE 'WARNING%')" +
      ")"

    val rows = stmt.executeQuery(sql)
    output = output + "Program\tProgramPoint\tCurrent message\tPrevious message\n"
    while (rows.next()) {
      output = output + rows.getString("Name") + "\t" + rows.getString("ProgramPoint") + "\t" + rows.getString("CurrentMessage").replace("\n", ";").replace("\t", " ") + "\t" + rows.getString("PreviousMessage").replace("\n", ";").replace("\t", " ") + "\n"
    }
    output

  }

  //return a string containing the compilation/analysis errors and the list of scripts involved per each error's message
  def getErrors(table: String, idTestRun: Int) = {
    var output: String = ""

    var sql = "SELECT COUNT(*) AS Total FROM " + table + " WHERE TestRun=" + idTestRun
    var rows = stmt.executeQuery(sql)
    rows.next()
    output = output + "Failures:" + rows.getString("Total") + "\n"
    rows.close()

    sql = "SELECT COUNT(*) AS Number, Error FROM " + table + " WHERE TestRun=" + idTestRun + " GROUP BY Error ORDER BY Number DESC"
    rows = stmt.executeQuery(sql)
    output = output + "\nERROR\t#\tPrograms involved\n"
    while (rows.next()) {
      output = output + "\n"
      output = output + "" + rows.getString("Error").replace('\n', ';') + "\t" + rows.getString("Number") + "\t"

      sql = "SELECT Name FROM " + table + " t, Programs p WHERE t.Error='" + rows.getString("Error").replace("'", "''") + "' AND t.TestRun=" + idTestRun + " AND p.ProgramId=t.Program"
      val stmt2 = c.createStatement()
      val rows2 = stmt2.executeQuery(sql)
      while (rows2.next())
        output = output + rows2.getString("Name") + ", "
      rows2.close()
    }
    output

  }

  //return a string with the major statistics about the current test run
  def getStatistics(idTestRun: Int): String = {
    var output: String = ""

    var sql = "SELECT SUM(CompilerTime) AS SUMCT, SUM(AnalysisTime) AS SUMAT, SUM(PropertyTime) AS SUMPT, AVG(CompilerTime) AS AVGCT, AVG(AnalysisTime) AS AVGAT, AVG(PropertyTime) AS AVGPT FROM Analyses WHERE TestRun=" + idTestRun + ";"
    var rows = stmt.executeQuery(sql)
    rows.next()
    output = output + "----PERFORMANCES----\n"
    output = output + "\t Compilation \t Analysis \t Property \n"
    output = output + "Sum \t " + rows.getString("SUMCT") + " \t " + rows.getString("SUMAT") + " \t " + rows.getString("SUMPT") + " \n"
    output = output + "Average \t " + rows.getString("AVGCT") + " \t " + rows.getString("AVGAT") + " \t " + rows.getString("AVGPT") + " \n"
    rows.close()
    output = output + "\n\n"

    sql = "SELECT SUM(Warnings) AS SUMW, SUM(Validated) AS SUMV FROM Analyses WHERE TestRun=" + idTestRun + ";"
    rows = stmt.executeQuery(sql)
    rows.next()
    output = output + "----PRECISION----\n"
    output = output + "Computed \t Not computed \t % \n"
    val percentage: Double = rows.getInt("SUMV").toDouble / (rows.getInt("SUMV") + rows.getInt("SUMW")).toDouble
    output = output + rows.getString("SUMV") + " \t " + rows.getString("SUMW") + " \t " + percentage + " \n"
    rows.close()
    output = output + "\n\n"

    output = output + "----ERRORS----\n"
    sql = "SELECT COUNT(*) AS SUMA FROM Analyses WHERE TestRun=" + idTestRun + ";"
    rows = stmt.executeQuery(sql)
    rows.next()
    val suma: Double = rows.getInt("SUMA")
    rows.close()
    sql = "SELECT COUNT(*) AS SUMB FROM BrokenAnalyses WHERE TestRun=" + idTestRun + ";"
    rows = stmt.executeQuery(sql)
    rows.next()
    val sumb: Double = rows.getInt("SUMB")
    rows.close()
    sql = "SELECT COUNT(*) AS SUMR FROM RuntimeErrors WHERE TestRun=" + idTestRun + ";"
    rows = stmt.executeQuery(sql)
    rows.next()
    val sumr: Double = rows.getInt("SUMR")
    rows.close()
    sql = "SELECT COUNT(*) AS ToBe FROM ToBeAnalyzed WHERE TestRun=" + idTestRun + ";"
    rows = stmt.executeQuery(sql)
    rows.next()
    val sumToBe: Int = rows.getInt("ToBe")
    rows.close()
    sql = "SELECT COUNT(*) AS SUMC FROM BrokenCompilations WHERE TestRun=" + idTestRun + ";"
    rows = stmt.executeQuery(sql)
    rows.next()
    val sumc: Int = rows.getInt("SUMC")
    rows.close()

    val total = suma + sumb + sumr + sumc

    output = output + "Type \t # \t % \n"

    val perca: Double = suma / total
    val percb: Double = sumb / total
    val percr: Double = sumr / total
    val percc: Double = sumc / total

    output = output + "Successfully: \t " + suma + " \t " + perca + " \n"
    output = output + "Errors of the analysis: \t " + sumb + " \t " + percb + " \n"
    output = output + "Errors of the compiler: \t " + sumc + " \t " + percc + " \n"
    output = output + "Runtime errors: \t " + sumr + " \t " + percr + " \n"
    output = output + "\nStill to be analyzed: \t " + sumToBe + " \n"

    output = output + "\n\n"

    output


  }

  //move the scripts from RuntimeErrors OR BrokenAnalyses to ToBeAnalyzed
  def fromTable2ToBeAnalyzed(idTestRun: Int, table: String, condition: String) = {
    val added = stmt.executeUpdate("INSERT INTO ToBeAnalyzed(TestRun, Program) SELECT " + idTestRun + ", Program FROM " + table + " WHERE TestRun=" + idTestRun + " AND " + condition)
    val removed = stmt.executeUpdate("DELETE FROM " + table + " WHERE TestRun=" + idTestRun + " AND " + condition)
    if (added != removed)
      println("Something strange happened: " + added + " scripts to be analyzed where added, but " + removed + " runtime errors were removed")
    else println(added + " scripts have been moved from runtime errors to to be analyzed")
    readLine()
  }


  //Add a given program to the Programs table, or it returns its id if it is already in the table
  def add2Programs(path: String, compiler: Compiler): Int = {
    val rs = stmt.executeQuery("SELECT ProgramId FROM Programs WHERE Name='" + path + "'")
    if (rs.next()) rs.getInt("ProgramId")
    else {
      stmt.executeUpdate("INSERT INTO Programs(Name, LOC) VALUES ('" + path + "', " +
        compiler.getSourceCode("td://" + path).count {
          case '\n' => true
          case _ => false
        }
        + ")", Statement.RETURN_GENERATED_KEYS)
      val rs: ResultSet = stmt.getGeneratedKeys
      rs.next()
      rs.getInt(1)
    }
  }

  //Add a given program and test run to the ToBeAnalyzed table, or it returns false if it is already in the table
  def add2ToBeAnalyzed(idProgram: Int, testRun: Int): Boolean = {
    try {
      stmt.executeUpdate("INSERT INTO ToBeAnalyzed(TestRun, Program) VALUES (" + testRun + ", " + idProgram + ")")
      true
    }
    catch {
      case _: Throwable => return false
    }
  }


  def runAnalyses(idTestRun: Int, timeout: Int) = {
    val state = getParametersTestRun(idTestRun)
    val stmt = c.createStatement()
    val stmt2 = c.createStatement()
    val sql = "SELECT p.Name, p.ProgramId FROM ToBeAnalyzed tb, Programs p WHERE tb.Program=p.ProgramId AND TestRun=" + idTestRun
    val rows = stmt.executeQuery(sql)
    while (rows.next()) {
      val programId = rows.getInt("ProgramId")
      stmt2.executeUpdate("DELETE FROM ToBeAnalyzed WHERE Program=" + programId + " AND TestRun=" + idTestRun)
      stmt2.executeUpdate("INSERT INTO RuntimeErrors(Program, TestRun) VALUES (" + programId + ", " + idTestRun + ")")

      this.synchronized {
        val t = new AnalysisThread(rows.getString("Name"), programId, idTestRun, state._2, state._3, state._1, state._4, stmt)
        val initialTime = System.currentTimeMillis()
        t.start()
        while (t.isAlive && System.currentTimeMillis() - initialTime < timeout)
          this.wait(1000)
        while (t.isAlive) {
          for (i <- 0 to 100) t.stop()
          System.out.println("Trying to stop a thread")
          this.wait(1000)
        }
      }

      //analyzeOneProgram(rows.getString("Name"), programId, idTestRun, state._2, state._3, state._1, state._4)
      //stmt2.executeUpdate("DELETE FROM RuntimeErrors WHERE Program="+programId+" AND TestRun="+idTestRun)


    }

    rows.close()
  }

  //return all the parameters (compiler, analyses, and property) of the current test run
  def getParametersTestRun[S <: SemanticAnalysis[_], H <: HeapDomain[_, _]](idTestRun: Int): (Compiler, S, H, Property) = {
    var rs = stmt.executeQuery("SELECT * FROM TestRun WHERE idTestRun=" + idTestRun)
    rs.next()
    val analysis = name2Object[SemanticAnalysis[_]](rs.getString("Analysis"), InstalledPlugins.analyses, p => p.getLabel()).get.asInstanceOf[S]
    val compiler = name2Object[Compiler](rs.getString("Compiler"), InstalledPlugins.compilers, p => p.getLabel()).get
    val heapAnalysis = name2Object[HeapDomain[_, _]](rs.getString("HeapAnalysis"), InstalledPlugins.heapanalyses, p => p.getLabel()).get.asInstanceOf[H]
    val property = name2Object[Property](rs.getString("Property"), analysis.getProperties.toArray, p => p.getLabel()).get

    rs = stmt.executeQuery("SELECT * FROM TestRunAnalysisParameters WHERE TestRun=" + idTestRun)
    while (rs.next())
      analysis.setParameter(rs.getString("Name"), rs.getString("Value"))

    rs = stmt.executeQuery("SELECT * FROM TestRunHeapAnalysisParameters WHERE TestRun=" + idTestRun)
    while (rs.next())
      heapAnalysis.setParameter(rs.getString("Name"), rs.getString("Value"))

    (compiler, analysis, heapAnalysis, property)
  }

}


class TestRunException(exc: String) extends Exception(exc)
