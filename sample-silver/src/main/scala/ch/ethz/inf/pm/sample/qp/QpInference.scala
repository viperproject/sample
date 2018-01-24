/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.qp

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.domain.{MapDomain, StackDomain}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.inference.SilverExtender
import ch.ethz.inf.pm.sample.oorepresentation.{Compilable, DummyProgramPoint}
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import ch.ethz.inf.pm.sample.qp.QpParameters.PermissionState
import ch.ethz.inf.pm.sample.util.SampleExpressions._
import viper.silver.ast.{Method, Program, While}
import viper.silver.{ast => sil}

object QpParameters {

  val NUMBER_OF_WARMUP_RUNS = 0

  val NUMBER_OF_RUNS = 1

  val SMALLEST_SOLUTION = true

  val SLICE_ARRAYS = true

  val ADD_ALL_INVARIANTS = false

  val CONDITIONAL_INVARIANTS = false

  type NumericalState = ApronPolyhedraAnalysisState

  type PermissionState = QpState[QpSpecification]

  val numericalEntry = ApronPolyhedraAnalysisEntryState

  val permissionEntry = QpEntryState
}

object QpInference
  extends SilverExtender[PermissionState] {

  override val analysis = QpAnalysis()

  override def compile(compilable: Compilable): sil.Program = {
    val program = super.compile(compilable)
    QpContext.setProgram(program)
    program
  }

  override def extend(program: Program): sil.Program = {

    val warmup = QpParameters.NUMBER_OF_WARMUP_RUNS
    val runs = QpParameters.NUMBER_OF_RUNS

    var result = program

    var total = 0L
    var min = Long.MaxValue
    var max = Long.MinValue

    for (i <- 0 until warmup + runs) {
      val t0 = System.nanoTime()
      result = super.extend(program)
      val t1 = System.nanoTime()

      if (i == warmup) println("--- end of warmup ---")

      val time = t1 - t0
      println(s"time: $time")

      if (i >= warmup) {
        total = total + time
        min = math.min(min, time)
        max = math.max(max, time)
      }
    }

    val avg = total / runs

    println(s"avg: ${avg / 1000000L}.${(avg / 1000L) % 1000L}ms")
    println(s"min: ${min / 1000000L}.${(min / 1000L) % 1000L}ms")
    println(s"max: ${max / 1000000L}.${(max / 1000L) % 1000L}ms")

    result
  }

  override def extendProgram(program: Program, results: ProgramResult[PermissionState]): sil.Program = {
    val extended = super.extendProgram(program, results)
    val functions = QpContext.getAuxiliaryFunctions
    extended.copy(functions = extended.functions ++ functions)(pos = extended.pos, info = extended.info, errT = extended.errT)
  }

  override def extendMethod(method: Method, result: CfgResult[PermissionState]): sil.Method = {
    QpContext.setMethod(method.name)
    super.extendMethod(method, result)
  }

  override def inferParameters(method: Method, result: CfgResult[PermissionState]): Seq[sil.LocalVarDecl] = {
    // TODO: Only add read parameter when it's actually needed.
    val inferred = Seq(QpContext.getReadParameter)
    method.formalArgs ++ inferred
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

  override def inferInvariants(loop: While, result: CfgResult[PermissionState]): Seq[sil.Exp] = {
    val position = getLoopPosition(loop, result.cfg)
    val invariant = QpContext.getInvariant(position)
    val preconditions: Seq[sil.Exp] = QpConverter.generatePreconditions(invariant).map { expression => sil.InhaleExhaleExp(expression, sil.TrueLit()())() }
    val postconditions: Seq[sil.Exp] = QpConverter.generatePostconditions(invariant).map { expression => sil.InhaleExhaleExp(sil.TrueLit()(), expression)() }

    val numerical = QpContext.getNumerical
    val domain = numerical.preStateAt(position).domain
    val constraints = domain.getConstraints(domain.ids.toSet)
    val converted = constraints.map(DefaultSampleConverter.convert).toSeq

    converted ++ preconditions ++ postconditions ++ loop.invs
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
              val slice = QpParameters.SLICE_ARRAYS && !variable.typ.isNumericalType
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

