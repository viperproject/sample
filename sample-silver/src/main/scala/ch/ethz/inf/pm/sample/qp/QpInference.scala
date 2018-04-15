/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.qp

import java.io.{File, PrintWriter}

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.domain.{MapDomain, StackDomain}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.inference.SilverExtender
import ch.ethz.inf.pm.sample.oorepresentation.{Compilable, DummyProgramPoint}
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import ch.ethz.inf.pm.sample.qp.QpParameters.PermissionState
import ch.ethz.inf.pm.sample.util.SampleExpressions._
import viper.silver.{ast => sil}

object QpParameters {

  val NUMBER_OF_WARMUP_RUNS = 50

  val NUMBER_OF_RUNS = 100

  val SMALLEST_SOLUTION = true

  val SLICE_ARRAYS = false

  val ADD_ALL_INVARIANTS = false

  val CONDITIONAL_INVARIANTS = false

  //type NumericalState = IntegerOctagonAnalysisState
  type NumericalState = ApronPolyhedraAnalysisState

  type PermissionState = QpState[QpSpecification]

  //val numericalEntry = IntegerOctagonAnalysisEntryState
  val numericalEntry = ApronPolyhedraAnalysisEntryState

  val permissionEntry = QpEntryState
}

object QpInference
  extends SilverExtender[PermissionState] {

  override val analysis = QpAnalysis()

  var sourceFilename = ""

  var timing = false

  var save = false

  var warmupRuns = QpParameters.NUMBER_OF_WARMUP_RUNS

  var normalRuns = QpParameters.NUMBER_OF_RUNS

  override def main(arguments: Array[String]): Unit = {
    require(arguments.nonEmpty, "No file or directory specified.")
    val filename = arguments(0)
    val file = new File(filename)
    require(file.exists, s"File or directory '$filename' does not exist.")

    processOptions(arguments.tail.toList)

    // get files
    val files =
      if (file.isFile) Array(file)
      else  file.listFiles
        .filter(_.isFile)
        .filter(_.getAbsolutePath.endsWith(".vpr"))

    for (current <- files) {
      println(current)
      val extended = extend(current)

      if (save) {
        val original = current.getAbsolutePath()
        val parts = original.split("\\.")
        val filename = parts.init.mkString(".") + ".out"
        val output = new PrintWriter(new File(filename))
        output.write(extended.toString)
        output.close()
      } else {
        printExtended(extended)
      }
    }
  }

  def processOptions(arguments: List[String]): Unit = arguments match {
    case "--time" :: rest =>
      timing = true
      processOptions(rest)
    case "--save" :: rest =>
      save = true
      processOptions(rest)
    case "-warmup" :: value :: rest =>
      warmupRuns = Integer.valueOf(value)
      processOptions(rest)
    case "-runs" :: value :: rest =>
      normalRuns = Integer.valueOf(value)
      processOptions(rest)
    case unknown :: rest =>
      println(s"Unknown option: $unknown")
      processOptions(rest)
    case Nil => // do nothing
  }

  override def compile(compilable: Compilable): sil.Program = {
    val program = super.compile(compilable)
    QpContext.setProgram(program)
    program
  }

  override def extend(program: sil.Program): sil.Program = {
    var result = program

    var totalTime = 0L
    var minTime = Long.MaxValue
    var maxTime = Long.MinValue

    if (!timing) {
      warmupRuns = 0
      normalRuns = 1
    }

    val totalRuns = warmupRuns + normalRuns

    if (timing && warmupRuns > 0) {
      println("--- warmup phase --- ")
    }

    for (i <- 0 until totalRuns) {

      val t0 = System.nanoTime()
      result = super.extend(program)
      val t1 = System.nanoTime()
      val time = t1 - t0

      if (timing) {
        if (i == warmupRuns) println("--- end of warmup ---")
        println(s"time: $time")

        if (i >= warmupRuns) {
          totalTime = totalTime + time
          minTime = math.min(minTime, time)
          maxTime = math.max(maxTime, time)
        }
      }
    }

    if (timing) {
      val avgTime = totalTime / normalRuns
      println("--- timing result ---")
      println(s"avg: ${avgTime / 1000000L}.${(avgTime / 1000L) % 1000L}ms")
      println(s"min: ${minTime / 1000000L}.${(minTime / 1000L) % 1000L}ms")
      println(s"max: ${maxTime / 1000000L}.${(maxTime / 1000L) % 1000L}ms")
    }

    result
  }

  override def extendProgram(program: sil.Program, results: ProgramResult[PermissionState]): sil.Program = {
    val extended = super.extendProgram(program, results)
    val functions = QpContext.getAuxiliaryFunctions
    extended.copy(functions = extended.functions ++ functions)(pos = extended.pos, info = extended.info, errT = extended.errT)
  }

  override def extendMethod(method: sil.Method, result: CfgResult[PermissionState]): sil.Method = {
    QpContext.setMethod(method.name)
    super.extendMethod(method, result)
  }

  override def inferParameters(method: sil.Method, result: CfgResult[PermissionState]): Seq[sil.LocalVarDecl] = {
    val inferred = Seq(QpContext.getReadParameter)
    method.formalArgs ++ inferred
  }

  override def extendStatement(statement: sil.Stmt, result: CfgResult[PermissionState]): sil.Stmt = statement match {
    case loop: sil.While =>
      val position = getLoopPosition(loop, result.cfg)
      // get loop pre- and postcondition
      val specifications = QpContext.getInvariant(position)
      val preconditions = QpConverter.generatePreconditions(specifications)
      val postconditions = QpConverter.generatePostconditions(specifications)
      // extend invariants with numerical invariants
      val invariants = {
        val state = QpContext.getNumerical.preStateAt(position)
        val domain = state.domain
        val constraints = domain.getConstraints(domain.ids.toSet)
        val inferred = constraints.map(DefaultSampleConverter.convert).toSeq
        inferred ++ loop.invs
      }
      // add loop pre- and postconditions as comments
      val info = {
        val pres = preconditions.map { condition => s"requires $condition" }
        val posts = postconditions.map { condition => s"ensures $condition" }
        sil.MakeInfoPair(loop.info, sil.SimpleInfo(pres ++ posts))
      }
      // extend body
      val body = extendBody(loop.body, result)
      // construct extended loop
      sil.While(loop.cond, invariants, body)(loop.pos, info)
    case _ => super.extendStatement(statement, result)
  }

  override def inferPreconditions(method: sil.Method, result: CfgResult[PermissionState]): Seq[sil.Exp] = {
    val position = firstPosition(result.cfg.entry)
    val state = result.preStateAt(position)
    val inferred = QpConverter.generatePreconditions(state.stack.get())
    val bounds = QpContext.getBounds
    bounds ++ inferred ++ method.pres
  }

  override def inferPostconditions(method: sil.Method, result: CfgResult[PermissionState]): Seq[sil.Exp] = {
    val position = firstPosition(result.cfg.entry)
    val state = result.preStateAt(position)
    val inferred = QpConverter.generatePostconditions(state.stack.get())
    inferred ++ method.posts
  }

  private object QpConverter
    extends DefaultSampleConverter {

    def generatePreconditions(domain: QpSpecification): Seq[sil.Exp] = domain.records match {
      case MapDomain.Top(_) => Seq.empty
      case MapDomain.Bottom(_) => Seq(sil.FalseLit()())
      case MapDomain.Inner(map, _) => generate(map.mapValues(_.precondition))
    }

    def generatePostconditions(domain: QpSpecification): Seq[sil.Exp] = domain.records match {
      case MapDomain.Top(_) => Seq.empty
      case MapDomain.Bottom(_) => Seq(sil.FalseLit()())
      case MapDomain.Inner(map, _) => generate(map.mapValues(_.postcondition))
    }

    def generate(map: Map[VariableIdentifier, Expression]): Seq[sil.Exp] = QpContext.getReceiver match {
      case None => Seq.empty
      case Some(function) =>
        val declarations = QpContext.getQuantified(function.name)
        val variables = declarations.map { declaration =>
          val typ = DefaultSilverConverter.convert(declaration.typ)
          VariableIdentifier(declaration.name)(typ)
        }

        map.toSeq.flatMap { case (field, permission) =>
          // slice specification
          val sliced = variables.foldRight(Set((List.empty[Expression], permission))) {
            case (variable, set) => set.flatMap { case (arguments, expression) =>
              // collect all values
              var values = Set.empty[Expression]
              var inequality = false
              expression.foreach {
                case Equal(`variable`, value) => values = values + value
                case Comparison(left, right, _) if left.contains(_ == variable) || right.contains(_ == variable) => inequality = true
                case _ => // do nothing
              }
              // slice if there is only one value or array slicing is enabled
              val slice = QpParameters.SLICE_ARRAYS && !variable.typ.isNumericalType && values.nonEmpty
              if ((values.size == 1 || slice) && !inequality) {
                for (value <- values) yield {
                  val transformed = expression.transform {
                    case Equal(`variable`, term) => Literal(term == value)
                    case other => other
                  }
                  (value :: arguments, transformed)
                }
              } else Seq((variable :: arguments, expression))
            }
          }

          for ((arguments, updated) <- sliced) yield {
            // simplify expression
            val simplified = QpMath.simplify(updated)
            // compute location of field access predicate
            val application = sil.FuncLikeApp(function, arguments.map(convert), Map.empty)
            val typ = convert(field.typ)
            val location = sil.FieldAccess(application, sil.Field(field.name, typ)())()
            // create field access predicate
            if (simplified == No) sil.TrueLit()()
            else {
              val converted = convert(simplified)
              val body = sil.FieldAccessPredicate(location, converted)()
              // create and return quantified expression
              val quantified = arguments.flatMap { argument => declarations.find { declaration => declaration.name == argument.toString }}
              if (quantified.isEmpty) body
              else sil.Forall(quantified, Seq.empty, body)()
            }
          }
        }
    }


    override def convert(expression: Expression): sil.Exp = expression match {
      case Min(left, right) => min(convert(left), convert(right))
      case Max(left, right) => max(convert(left), convert(right))
      case FunctionCallExpression(name, parameters, _) =>
        val function = QpContext.getFunction(name).get
        sil.FuncLikeApp(function, parameters.map(convert), Map.empty[sil.TypeVar, sil.Type])
      case other => super.convert(other)
    }

    def max(left: sil.Exp, right: sil.Exp): sil.Exp = {
      val function = QpContext.getMaxFunction
      val arguments = Seq(left, right)
      sil.FuncApp(function, arguments)()
    }

    def min(left: sil.Exp, right: sil.Exp): sil.Exp = {
      val function = QpContext.getMinFunction
      val arguments = Seq(left, right)
      sil.FuncApp(function, arguments)()
    }
  }

}

case class QpAnalysis()
  extends SilverAnalysis[PermissionState] {

  override def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[PermissionState] = {

    val name = method.name.toString
    QpContext.setMethod(name)

    val numerical = {
      val entry = QpParameters.numericalEntry.build(program, method)
      val interpreter = FinalResultForwardInterpreter(method.body, entry)
      interpreter.execute()
    }

    QpContext.setNumerical(name, numerical)

    val permission = {
      val entry = QpParameters.permissionEntry.build(program, method)
      val interpreter = QpInterpreter(method.body, entry)
      interpreter.execute()
    }

    permission
  }
}

object QpEntryState
  extends SilverEntryStateBuilder[PermissionState] {

  override def default: PermissionState = QpState(
    pp = DummyProgramPoint,
    expr = ExpressionSet(),
    stack = StackDomain.Bottom(QpSpecification())
  )

  override def build(program: SilverProgramDeclaration, method: SilverMethodDeclaration): PermissionState = {
    val fields = program.fields.map { field => field.variable.id }
    val record = QpSpecification().initialize(fields)
    super.build(program, method).copy(stack = StackDomain(record))
  }
}

