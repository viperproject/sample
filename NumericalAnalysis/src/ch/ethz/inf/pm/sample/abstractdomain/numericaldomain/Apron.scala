package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import apron._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyNumericalType, Type, DummyBooleanType}

/**
 * @author Lucas Brutschy
 */
trait Apron[T <: Apron[T]]
  extends NumericalDomain.Relational[T]
  with SimplifiedSemanticDomain[T]
  with SimplifiedMergeDomain[T] {
  this:T =>

  def factory(apronState:apron.Abstract1,env:IdentifierSet):T

  def manager:apron.Manager

  def factory() = factory(new Abstract1(manager,makeEnvironment(Set.empty)),IdentifierSet.Bottom)

  /** Overwrite this to get a float domain */
  def isFloatDomain = false

  protected def makeEnvironment(ids:Set[Identifier]): Environment =
    if (!isFloatDomain)
      new Environment(ids.map(_.getName).toArray,new Array[String](0))
    else
      new Environment(new Array[String](0),ids.map(_.getName).toArray)

  protected def addEnvironment(env:Environment, ids:Set[Identifier]): Environment =
    if (!isFloatDomain)
      env.add(ids.map(_.getName).toArray,new Array[String](0))
    else
      env.add(new Array[String](0),ids.map(_.getName).toArray)

}

object Apron {

  trait Bottom[T <: Apron[T]] extends NumericalDomain.Relational.Bottom[T] with Apron[T] with SimplifiedMergeDomain.Bottom[T] {
    this:T =>

    override def add(ids:Set[Identifier]) =
      factory(new Abstract1(manager,makeEnvironment(ids)), IdentifierSet.Inner(ids))

    override def createVariable(variable: Identifier, typ: Type) = add(Set(variable))

    override def getConstraints(ids: Set[Identifier]) = Set(Constant("false",DummyBooleanType))

  }

  trait Top[T <: Apron[T]] extends NumericalDomain.Relational.Top[T] with Apron[T] with SimplifiedMergeDomain.Top[T] with BooleanExpressionSimplifier[T] {
    this:T =>

    override def assumeSimplified(expr: Expression) = this

    override def assign(variable: Identifier, expr: Expression) = this

  }

  trait Inner[T <: Apron[T],X <: Apron.Inner[T,X]]
    extends NumericalDomain.Relational.Inner[T,X]
    with Apron[T]
    with BooleanExpressionSimplifier[T] {
    this:T =>

    val apronState:apron.Abstract1
    val ids:IdentifierSet

    // COMBINATIONS

    override def glbSameEnvInner(that: X) =
      factory(apronState.meetCopy(manager,that.apronState),ids)

    override def lubSameEnvInner(that: X) =
      factory(apronState.joinCopy(manager,that.apronState),ids)

    override def wideningSameEnvInner(that: X) =
      factory(apronState.widening(manager,that.apronState),ids)

    override def unifyInner(that: X) =
      factory(apronState.unifyCopy(manager,that.apronState),this.ids ++ that.ids)

    override def lessEqualSameEnvInner(that: X) =
      apronState.isIncluded(manager,that.apronState)

    // ABSTRACT TRANSFORMERS

    override def assumeSimplified(expr: Expression) = {

      if (expr.ids.getNonTop exists (!numerical(_))) {
        this
      } else {
        expr match {

          // APRON fails to resolve !(a = b) and a != b. Instead, we have to specify a < b || a > b
          case BinaryArithmeticExpression(left, right, ArithmeticOperator.!=, typ) =>
            val newLeft = BinaryArithmeticExpression(left, right, ArithmeticOperator.>, typ)
            val newRight = BinaryArithmeticExpression(left, right, ArithmeticOperator.<, typ)
            assume(BinaryBooleanExpression(newLeft, newRight, BooleanOperator.||, typ))

          case _ =>

            val nonExisting = filterNonExisting(expr.ids.getNonTop)
            if (nonExisting.nonEmpty)
              add(nonExisting).assume(expr)
            else {
              val translator = new ApronInterfaceTranslator()(this)
              translator.toTcons1(expr, apronState.getEnvironment) match {

                case x :: xs =>
                  var result = apronState.meetCopy(manager, x)
                  for (xMore <- xs) {
                    result = result.joinCopy(manager, apronState.meetCopy(manager, xMore))
                  }
                  factory(result, ids)

                case Nil => throw new ApronException("empty set of constraints generated")

              }
            }


        }
      }
    }

    override def assign(variable: Identifier, expr: Expression) = {
      if (numerical(variable)) {
        if (expr.ids.getNonTop exists (!numerical(_))) {
          setToTop(variable)
        } else {
          val nonExisting = filterNonExisting(expr.ids.getNonTop)
          if (!exists(variable) || nonExisting.nonEmpty) {
            add(nonExisting + variable).assign(variable, expr)
          } else {
            val translator = new ApronInterfaceTranslator()(this)
            val exprIntern = translator.toTexpr1Intern(expr, apronState.getEnvironment)
            val assignedState =
              if (exprIntern.size > 1) {
                var curState = new Abstract1(manager, apronState.getEnvironment, true)
                for (e <- exprIntern) {
                  curState = curState.joinCopy(manager, apronState.assignCopy(manager, variable.getName, e, null))
                }
                curState
              } else if (exprIntern.size == 1) {
                apronState.assignCopy(manager, variable.getName, exprIntern.head, null)
              } else {
                throw new ApronException("Empty expression set created")
              }

            factory(assignedState, ids)
          }
        }
      } else this
    }

    override def setToTop(id: Identifier) = {
      if (numerical(id)) {
        if (exists(id)) factory(apronState.forgetCopy(manager, S(id), false), ids)
        else add(Set(id)).setToTop(id)
      } else this
    }

    // ADDING, REMOVING, EXPANDING AND FOLDING OF DIMENSIONS

    override def removeVariable(id: Identifier) = remove(Set(id))
    override def createVariable(id: Identifier, typ: Type) = add(Set(id))

    override def add(idsA: Set[Identifier]) = {
      val newIdsA = filterNonExisting(idsA) filter numerical
      if (newIdsA.nonEmpty) {
        val newEnv = addEnvironment(apronState.getEnvironment,newIdsA)
        factory(apronState.changeEnvironmentCopy(manager, newEnv, false), ids ++ newIdsA)
      } else this
    }

    override def fold(idsA: Set[Identifier], idB: Identifier) = {
      if (numerical(idB)) {
        val newIdsA = filterExisting(idsA) filter numerical
        if (!exists(idB) && newIdsA.nonEmpty) {
          val newEnv = addEnvironment(apronState.getEnvironment,Set(idB))
          factory(apronState.changeEnvironmentCopy(manager, newEnv, false).foldCopy(manager, SA(idB :: newIdsA.toList)), ids.fold(newIdsA, idB))
        } else remove(newIdsA).add(Set(idB))
      } else this
    }

    override def remove(idsA: Set[Identifier]) = {
      val newIdsA = filterExisting(idsA) filter numerical
      if (newIdsA.nonEmpty) {
        val newEnv = apronState.getEnvironment.remove(SA(newIdsA))
        factory(apronState.changeEnvironmentCopy(manager, newEnv, false), ids.remove(newIdsA))
      } else this
    }

    override def expand(idA: Identifier, idsB: Set[Identifier]) = {
      if (numerical(idA)) {
        val newIdsB = filterNonExisting(idsB) filter numerical
        if (exists(idA) && newIdsB.nonEmpty) {
          val newState = apronState.expandCopy(manager, S(idA), SA(newIdsB))
          val newEnv = newState.getEnvironment.remove(SA(idA))
          newState.changeEnvironment(manager, newEnv, false)
          factory(newState, ids.expand(idA, newIdsB))
        } else {
          remove(Set(idA)).add(newIdsB)
        }
      } else this
    }

    override def rename(idA: Identifier, idB: Identifier) = {
      if (numerical(idA) && numerical(idB)) {
        if (exists(idA) && !exists(idB))
          factory(apronState.renameCopy(manager,SA(idA),SA(idB)),ids.rename(idA,idB))
        else
          remove(Set(idA)).add(Set(idB))
      } else this
    }

    // QUERYING THE DOMAIN

    override def getConstraints(ids: Set[Identifier]) = {
      val translator = ApronInterfaceTranslator()(this)
      (for (id <- ids) yield {
        apronState.toLincons(manager).toList.filter(translator.constraintContains(_, id.getName))
      }).flatten.map(translator.translate).flatten
    }

    override def getStringOfId(id: Identifier): String = {
      val translator = ApronInterfaceTranslator()(this)
      val constraints = apronState.toLincons(manager).toList.filter(translator.constraintContains(_, id.getName))
      if (constraints.isEmpty) return "⊤"
      val expressions = constraints.map(translator.translate).flatten.map(ExpPrettyPrinter)
      expressions.sorted.mkString("\n")
    }

    override def toString:String = {
      val exps = ApronInterfaceTranslator()(this).translateAll() map ExpPrettyPrinter
      if (exps.isEmpty) return "⊤"
      "Environment: " + ids + "\n" + exps.toList.sorted.mkString("\n")
    }

    // HELPERS

    private def filterNonExisting(idsA:Set[Identifier]):Set[Identifier] = idsA diff ids.getNonTop
    private def filterExisting(idsA:Set[Identifier]):Set[Identifier] = ids.getNonTop intersect idsA
    private def exists(id:Identifier) = ids.contains(id)
    private def numerical(id:Identifier) = id.typ.isNumericalType
    private def S(id:Identifier):String = id.getName
    private def SA(id:Identifier):Array[String] = List(id.getName).toArray
    private def SA(id:List[Identifier]):Array[String] = id.map(_.getName).toArray
    private def SA(id:Set[Identifier]):Array[String] = id.map(_.getName).toArray

    override def getPossibleConstants(id: Identifier) = {
      val interval = apronState.getBound(manager,id.getName)
      if (interval.inf() == interval.sup()) {
        SetDomain.Default.Inner(Set(Constant(interval.inf.asInstanceOf[DoubleScalar].get().toString,DummyNumericalType)))
      } else {
        SetDomain.Default.Top[Constant]
      }
    }
  }

  trait Octagons extends Apron[Octagons] {
    override def manager = Octagons.manager
    override def bottom(): Octagons =
      Octagons.Bottom
    override def top(): Octagons = Octagons.Top
    override def factory(apronState: Abstract1, ids: IdentifierSet) =
      if (apronState.isBottom(manager)) Octagons.Bottom
      else if (ids.isTop && apronState.isTop(manager)) Octagons.Top
      else Octagons.Inner(apronState, ids)
  }

  object Octagons {
    def manager = new apron.Octagon
    object Bottom extends Octagons with Apron.Bottom[Octagons]
    object Top extends Octagons with Apron.Top[Octagons]
    case class Inner(apronState:apron.Abstract1, ids:IdentifierSet) extends Octagons with Apron.Inner[Octagons,Inner]
  }

  trait FloatOptOctagons extends Apron[FloatOptOctagons] {
    override def manager = FloatOptOctagons.manager
    override def bottom(): FloatOptOctagons = FloatOptOctagons.Bottom
    override def top(): FloatOptOctagons = FloatOptOctagons.Top
    override def factory(apronState: Abstract1, ids: IdentifierSet) =
      if (apronState.isBottom(manager)) FloatOptOctagons.Bottom
      else if (ids.isTop && apronState.isTop(manager)) FloatOptOctagons.Top
      else FloatOptOctagons.Inner(apronState, ids)
    override def isFloatDomain = true
  }

  object FloatOptOctagons {
    def manager = new apron.OptOctagon
    object Bottom extends FloatOptOctagons with Apron.Bottom[FloatOptOctagons]
    object Top extends FloatOptOctagons with Apron.Top[FloatOptOctagons]
    case class Inner(apronState:apron.Abstract1, ids:IdentifierSet) extends FloatOptOctagons with Apron.Inner[FloatOptOctagons,Inner]
  }


  trait OptOctagons extends Apron[OptOctagons] {
    override def manager = OptOctagons.manager
    override def bottom(): OptOctagons = OptOctagons.Bottom
    override def top(): OptOctagons = OptOctagons.Top
    override def factory(apronState: Abstract1, ids: IdentifierSet) =
      if (apronState.isBottom(manager)) OptOctagons.Bottom
      else if (ids.isTop && apronState.isTop(manager)) OptOctagons.Top
      else OptOctagons.Inner(apronState, ids)
  }

  object OptOctagons {
    def manager = new apron.OptOctagon
    object Bottom extends OptOctagons with Apron.Bottom[OptOctagons]
    object Top extends OptOctagons with Apron.Top[OptOctagons]
    case class Inner(apronState:apron.Abstract1, ids:IdentifierSet) extends OptOctagons with Apron.Inner[OptOctagons,Inner]
  }

  trait LinearEqualities extends Apron[LinearEqualities] {
    override def manager = LinearEqualities.manager
    override def bottom(): LinearEqualities = LinearEqualities.Bottom
    override def top(): LinearEqualities = LinearEqualities.Top
    override def factory(apronState: Abstract1, ids: IdentifierSet) =
      if (apronState.isBottom(manager)) LinearEqualities.Bottom
      else if (ids.isTop && apronState.isTop(manager)) LinearEqualities.Top
      else LinearEqualities.Inner(apronState, ids)
  }

  object LinearEqualities {
    def manager = new apron.PolkaEq
    object Bottom extends LinearEqualities with Apron.Bottom[LinearEqualities]
    object Top extends LinearEqualities with Apron.Top[LinearEqualities]
    case class Inner(apronState:apron.Abstract1, ids:IdentifierSet) extends LinearEqualities with Apron.Inner[LinearEqualities,Inner]
  }

  trait Box extends Apron[Box] {
    override def manager = Box.manager
    override def bottom(): Box = Box.Bottom
    override def top(): Box = Box.Top
    override def factory(apronState: Abstract1, ids: IdentifierSet) =
      if (apronState.isBottom(manager)) Box.Bottom
      else if (ids.isTop && apronState.isTop(manager)) Box.Top
      else Box.Inner(apronState, ids)
  }

  object Box {
    def manager = new apron.Box
    object Bottom extends Box with Apron.Bottom[Box]
    object Top extends Box with Apron.Top[Box]
    case class Inner(apronState:apron.Abstract1, ids:IdentifierSet) extends Box with Apron.Inner[Box,Inner]
  }
  
  trait StrictPolyhedra extends Apron[StrictPolyhedra] {
    override def manager = StrictPolyhedra.manager
    override def bottom(): StrictPolyhedra = StrictPolyhedra.Bottom
    override def top(): StrictPolyhedra = StrictPolyhedra.Top
    override def factory(apronState: Abstract1, ids: IdentifierSet) =
      if (apronState.isBottom(manager)) StrictPolyhedra.Bottom
      else if (ids.isTop && apronState.isTop(manager)) StrictPolyhedra.Top
      else StrictPolyhedra.Inner(apronState, ids)
  }

  object StrictPolyhedra {
    def manager = new apron.Polka(true)
    object Bottom extends StrictPolyhedra with Apron.Bottom[StrictPolyhedra]
    object Top extends StrictPolyhedra with Apron.Top[StrictPolyhedra]
    case class Inner(apronState:apron.Abstract1, ids:IdentifierSet) extends StrictPolyhedra with Apron.Inner[StrictPolyhedra,Inner]
  }
  
  trait Polyhedra extends Apron[Polyhedra] {
    override def manager = Polyhedra.manager
    override def bottom(): Polyhedra = Polyhedra.Bottom
    override def top(): Polyhedra = Polyhedra.Top
    override def factory(apronState: Abstract1, ids: IdentifierSet) =
      if (apronState.isBottom(manager)) Polyhedra.Bottom
      else if (ids.isTop && apronState.isTop(manager)) Polyhedra.Top
      else Polyhedra.Inner(apronState, ids)
  }

  object Polyhedra {
    def manager = new apron.Polka(false)
    object Bottom extends Polyhedra with Apron.Bottom[Polyhedra]
    object Top extends Polyhedra with Apron.Top[Polyhedra]
    case class Inner(apronState:apron.Abstract1, ids:IdentifierSet) extends Polyhedra with Apron.Inner[Polyhedra,Inner]
  }

}

class ApronException(s: String) extends Exception(s)
