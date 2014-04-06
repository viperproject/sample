package ch.ethz.inf.pm.td.analysis.backward

import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, State}
import ch.ethz.inf.pm.td.compiler.{TouchCollection, CFGGenerator, TouchType, TouchCompiler}
import ch.ethz.inf.pm.td.analysis.TouchApronRun
import solver.Solver
import solver.variables.{VariableFactory, IntVar}
import solver.variables.delta.IntDelta
import ch.ethz.inf.pm.td.analysis.interpreter._
import solver.search.strategy.IntStrategyFactory
import apron.{Scalar, Coeff, Lincons1}
import solver.constraints.IntConstraintFactory
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.td.analysis.interpreter.StringV
import ch.ethz.inf.pm.td.analysis.interpreter.ActualMethodParams
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.td.analysis.interpreter.RefV
import ch.ethz.inf.pm.td.analysis.interpreter.NumberV
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.FieldAndProgramPoint
import ch.ethz.inf.pm.sample.execution.AbstractErrorInfo
import ch.ethz.inf.pm.td.analysis.interpreter.BooleanV
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.NonRelationalHeapDomain

/** Helper trait that provides easy access to the TouchBoost abstract state components */
trait StateComponents[S <: State[S]] {
  def fullState: S

  def heap: NonRelationalHeapDomain[TouchApronRun.HeapId] = TouchApronRun.extractBasicHeap(fullState)

  def varEnv: VariableEnv[TouchApronRun.HeapId] = heap.variableEnv

  def heapEnv: HeapEnv[TouchApronRun.HeapId] = heap.heapEnv

  def allEnvVars: Set[VariableIdentifier] = varEnv.getVariables

  def allHeapIds: Set[TouchApronRun.HeapId] = heapEnv.ids

  def numericalInfo = TouchApronRun.extractNumericalState(fullState)

  def invalidInfo = TouchApronRun.extractInvalidValueInfo(fullState)

  /** Maps variable names used within Apron to Sample identifiers */
  def sampleIdForApronId(variable: String): Identifier = apronIdMap.get(variable) match {
    case Some(id) => id
    case None => sys.error(s"unknown variable $variable")
  }

  lazy val apronIdMap = numericalInfo.env.groupBy(_.getName).map { case (k,v) =>
    if (v.size > 1) sys.error(s"non-unique identifier name $k")
    else (k, v.toList(0))
  }
}

/** Generates test case inputs from a refined entry state of an abstract error investigation */
class PotentialCounterExampleGenerator[
    S <: State[S]](
    refinedEntry: RefinedEntry[S],
    errInfo: AbstractErrorInfo[S],
    compiler: TouchCompiler)
  extends StateComponents[S] {

  type ChocoIntVar = IntVar[_ <: IntDelta]

  val fullState = refinedEntry.postEnterState

  val entryMethod = refinedEntry.method

  val constraintSolver = new Solver()

  private var intSolverVars: Map[Identifier, ChocoIntVar] = Map.empty

  private var booleanSolverVars: Map[Identifier, ChocoIntVar] = Map.empty

  private var invalidnessSolverVars: Map[Identifier, ChocoIntVar] = Map.empty

  private var refSolverVars: Map[Identifier, RefSolverVar] = Map.empty


  /** First invocation of Choco solver is special */
  private var firstSolution = true

  private case class RefSolverVar(chocoVar: ChocoIntVar, choices: List[TouchApronRun.HeapId])

  /** Values in solution computed by the solver (all integers) */
  private case class SolverSolutionValues(
    intVarValues: Map[Identifier, Int],
    booleanVarValues: Map[Identifier, Int],
    invalidnessVarValues: Map[Identifier, Int],
    refVarValues: Map[Identifier, Int]
  )

  generateConstraints()

  /**
   * Computes the next solution using the constraint solver */
  def findNext(): Option[InterpreterTestInput] = {
    if (fullState.isBottom) return None

    nextConstraintSolution()
      .map(constructCounterExample)
  }


  private def constructCounterExample(solverSolutionValues: SolverSolutionValues): InterpreterTestInput = {
    // map caches the heap identifiers for which we already created a concrete object
    var heapObjLookup: Map[Identifier, RefV] = Map.empty
    val initialState = new ConcreteInterpreterState()

    def constructCollectionEntries(collectionId: Identifier): Map[TouchValue, TouchValue] = {
      val lengthIds = heap.getCollectionLength(collectionId)
      val NumberV(lengthVal) = constructValue(lengthIds.value.head)

      if (lengthVal <= 0) {
        // Create an empty collection
        Map.empty
      } else {
        // We don't actually use the length, simply create entries for tuples in
        // overapproximation.
        val overApproxIds = heap.getCollectionOverApproximation(collectionId)
        val tupleIds = heap.getCollectionTuples(overApproxIds.value.head)

        val entries =
          for (tuple <- tupleIds.value) yield {
            val key = heap.getCollectionKeyByTuple(tuple).asInstanceOf[TouchApronRun.HeapId]
            val value = heap.getCollectionValueByTuple(tuple).asInstanceOf[TouchApronRun.HeapId]
            constructValue(key) -> constructValue(value)
          }
        entries.toMap
      }
    }

    def constructRefValue(id: Identifier): RefV = {
      require(id.typ.isObject)

      val typ = id.typ.asInstanceOf[TouchType]

      val objRef =
        if (refSolverVars.contains(id)) {
          // Handle solver choices for references in environment like "v -> {PP1, PP2}"
          val refVar = refSolverVars(id)
          val heapIdChoice = solverSolutionValues.refVarValues(id)
          constructRefValue(refVar.choices(heapIdChoice))
        } else {
          heapObjLookup.getOrElse(id, {
            val possibleFields = typ.possibleTouchFields
            var fieldVals: Map[String, TouchValue] = Map.empty
            for (f <- possibleFields) {
              val fieldIdSet = heap.getFieldIdentifier(id, f.getName, f.typ, id.pp)._1
              if (!fieldIdSet.isTop && !fieldIdSet.isBottom) {
                // For now, simply pick first id in set.
                // Could also be added as a solver choices for heapenv
                val fieldId = fieldIdSet.value.head
                if (allHeapIds.contains(fieldId)) {
                  fieldVals += (f.getName -> constructValue(fieldId))
                }
              }
            }

            typ match {
              case collType: TouchCollection =>
                val collectionEntries = constructCollectionEntries(id)
                initialState.createCollection(collType, collectionEntries, fieldVals)
              case objType: TouchType if objType.isObject =>
                initialState.createObject(typ, fieldVals)
              case _ => sys.error("Only ref types instantiated here!")
            }
          })
        }

        heapObjLookup += (id -> objRef)
        objRef
    }

    def constructValue(id: Identifier): TouchValue = {
      val ValidSolverConst = 0
      val InvalidSolverConst = 1
      val determinedInvalidityVal = solverSolutionValues
        .invalidnessVarValues
        .getOrElse(id, ValidSolverConst)
      if (determinedInvalidityVal == InvalidSolverConst) {
        return InvalidV(id.typ)
      }

      id.typ.asInstanceOf[TouchType] match {
        case typ if typ.isBooleanType =>
          BooleanV(solverSolutionValues.booleanVarValues(id) != 0)
        case typ if typ.isNumericalType =>
          NumberV(solverSolutionValues.intVarValues(id))
        case typ if typ.isStringType =>
          // TODO: Use string domain to obtain value?
          StringV("")
        case typ =>
          constructRefValue(id)
      }
    }

    val methodParamIds = entryMethod
      .arguments(0)
      .map(paramDecl => paramDecl.variable.id)
    assert(methodParamIds.forall(id => allEnvVars.contains(id)),
        "Method parameter variable id not contained in environment")
    val startMethodParams = methodParamIds
      .map(id => id -> constructValue(id))
      .toMap

    val fixedNonDetInputs = allHeapIds
      .collect({ case id@FieldAndProgramPoint(NonDeterminismSourceHeapId(_, pp, _), "value", _, _) =>
        id
      })
      .map(id => id.pp -> constructValue(id))
      .toMap

   val singletonVars = compiler.singletonVars
      .map(id => id -> constructValue(id))
      .toMap
    singletonVars.foreach { case (id, value) => initialState.setGlobal(id, value) }

    val globalUserData = allEnvVars
      .filter(id => CFGGenerator.isGlobalReferenceIdent(id.name))
      .map(id => id -> constructValue(id))
      .toMap
    globalUserData.foreach { case (id, value) => initialState.setGlobal(id, value) }


    InterpreterTestInput(
      startMethod = refinedEntry.method,
      startMethodActualParams = ActualMethodParams(startMethodParams),
      initialState = initialState,
      globalUserData = globalUserData.keySet,
      nonDetInputs = fixedNonDetInputs,
      singletonVars = singletonVars.keySet
    )
  }

  private def nextConstraintSolution(): Option[SolverSolutionValues] = {
    val found = if (firstSolution) {
      constraintSolver.set(IntStrategyFactory.firstFail_InDomainMiddle(VariableFactory.castToIntVar(constraintSolver.getVars: _*)))
      constraintSolver.findSolution()
    } else constraintSolver.nextSolution()
    firstSolution = false
    if (found) {
      Some(SolverSolutionValues(
        intVarValues = intSolverVars.map({case (id, sv) => id -> sv.getValue}),
        booleanVarValues = booleanSolverVars.map({case (id, sv) => id -> sv.getValue}),
        invalidnessVarValues = invalidnessSolverVars.map({case (id, sv) => id -> sv.getValue}),
        refVarValues = refSolverVars.map({case (id, RefSolverVar(sv, _)) => id -> sv.getValue})
      ))
    } else {
      None
    }
  }

  private def generateConstraints() = {
    generateNumericalConstraints()
    generateInvalidnessConstraints()
    generateHeapConstraints()
  }

  private def generateNumericalConstraints() = {
    val sampleNumericalIds = apronIdMap.values
    val booleanIds = sampleNumericalIds.filter(id => id.typ.isBooleanType)
    val intIds = sampleNumericalIds filter { id =>
      val typ = id.typ
      typ.isNumericalType && !typ.isBooleanType
    }

    intSolverVars = intIds.map({ id: Identifier =>
      id -> VariableFactory.enumerated(id.getName, -100000, 100000, constraintSolver)
    }).toMap

    booleanSolverVars = booleanIds.map({ id: Identifier =>
      id -> VariableFactory.bool(id.getName, constraintSolver)
    }).toMap


    // Create numerical constraints for Apron representation
    if (numericalInfo.state.isDefined) {
      val linearConstraints = numericalInfo.state.get.toLincons(numericalInfo.domain)
      linearConstraints.foreach(processLinCons)
    }
  }

  private def generateInvalidnessConstraints()  = {
    invalidnessSolverVars = invalidInfo.keys.map({ id: Identifier =>
      val varName = id.getName + "_invalidness"
      val solverVar =
        invalidInfo.get(id) match {
          case Some(validityValue) =>
            if (validityValue.mustBeInvalid) {
              // one = true = invalid
              VariableFactory.eq(VariableFactory.one(constraintSolver))
            } else if (validityValue.mustBeValid) {
              // zero = false = valid
              VariableFactory.eq(VariableFactory.zero(constraintSolver))
            } else {
              VariableFactory.bool(varName, constraintSolver)
            }
          case _ =>
            VariableFactory.bool(varName, constraintSolver)
        }
      id -> solverVar
    }).toMap
  }

  private def generateHeapConstraints() = {
    val refIds = varEnv.map
      .filterKeys(id => id.typ.isObject)
      .filter { case (id, hidSet) =>
      !hidSet.isBottom && !hidSet.isTop && !hidSet.value.isEmpty
    }.mapValues(hidSet => hidSet.value.toList)

    refSolverVars = refIds.map({case (id, hids) =>
      id -> RefSolverVar(
        VariableFactory.enumerated(id.getName, 0, hids.size - 1, constraintSolver),
        hids)
    }).toMap
  }

  private def processLinCons(cons: Lincons1) = {
    val linterms = cons.getLinterms
    val termVars = linterms
      .map(_.getVariable)
      .map(sampleIdForApronId)
      .map(id => if (id.typ.isBooleanType) booleanSolverVars(id) else intSolverVars(id))
    val termCoeffs = linterms
      .map(_.getCoefficient)
      .map(apronCoeffToDouble)
      .map(_.toInt)

    val negApronCoeff = negateCoeff(cons.getCst)
    val rhsconstVal = apronCoeffToDouble(negApronCoeff).toInt
    val rhsConstVar = VariableFactory.fixed(rhsconstVal, constraintSolver)
    import scala.language.existentials // enables inference of existential type, prevents warning
    val scalarCons = IntConstraintFactory.scalar(termVars, termCoeffs, translateApronOp(cons.getKind), rhsConstVar)
    constraintSolver.post(scalarCons)
  }

  private def negateCoeff(c: Coeff): Coeff = {
    val negCoeff = c.copy
    negCoeff.neg()
    negCoeff
  }

  private def apronCoeffToDouble(c: Coeff): Double = {
    val doubleVal = Array[Double](1)
    // weird API... last parameter not documented
    c.asInstanceOf[Scalar].toDouble(doubleVal, 0)
    doubleVal(0)
  }

  private def translateApronOp(kind: Int): String = kind match {
    case Lincons1.DISEQ => "!="
    case Lincons1.SUP => ">"
    case Lincons1.EQ => "="
    case Lincons1.SUPEQ => ">="
  }
}
