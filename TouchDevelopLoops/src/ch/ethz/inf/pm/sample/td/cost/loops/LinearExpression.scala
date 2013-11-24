package ch.ethz.inf.pm.sample.td.cost.loops

import ch.ethz.inf.pm.sample.abstractdomain._
import collection.mutable
import apron.Lincons1


/*
    Offers static methods to create a LinearExpression.
 */
object LinearExpressionFactory {

  var constant = new Rational(0)
  var coefficients : mutable.HashMap[PubsVariable, Rational] = mutable.HashMap.empty     // key, value

  def fromLincon (l: Lincons1, vars: Array[String], iVar: String) : LinearExpression = {
    constant = new Rational(0)
    coefficients = mutable.HashMap.empty
    if (l.getKind == Lincons1.EQ)  {
      val iCoeff = getCoeff(l, iVar).toInt
      if (iCoeff == 0) null
      else {
        // find the constant
        var cst = getCst(l).toInt
        constant = new Rational(cst, -iCoeff)
        for (v <- vars if v != iVar) {
          var coeff = getCoeff(l, v).toInt
          val key = if (v == "old_" + iVar) iVar else v
          if (coeff != 0) coefficients.put(new PubsVariable(key, false), new Rational(coeff, -iCoeff))
        }
        new LinearExpression(constant, coefficients)
      }
    } else null
  }

  def fromLincon (l: Lincons1, vars: Array[String]) : LinearExpression = {
    constant = new Rational(0)
    coefficients = mutable.HashMap.empty
    if (l.isLinear)  {
      // find the constant
      val cst = getCst(l).toInt
      constant = new Rational(cst)
      // find the coefficients
      for (v <- vars) {
        val coeff = getCoeff(l, v).toInt
        val key = if (v.startsWith("old_")) v.substring(4) else v + "_prime"
        if (coeff != 0) coefficients.put(new PubsVariable(key, false), new Rational(coeff))
      }
      new LinearExpression(constant, coefficients)
    } else null
  }

  def fromConstant (c: Int) : LinearExpression = {
    constant = new Rational(c)
    coefficients = mutable.HashMap.empty
    new LinearExpression(constant, coefficients)
  }

  def fromVariable (v: PubsVariable) : LinearExpression = {
    constant = new Rational(0)
    coefficients = mutable.HashMap.empty
    coefficients.put(v, new Rational(1))
    new LinearExpression(constant, coefficients)
  }

  def fromExpression (e: Expression) : LinearExpression = {
    constant = new Rational(0)
    coefficients = mutable.HashMap.empty
    val result = fromExpression(e, false)
    if (result) new LinearExpression(constant, coefficients) else null
  }

  def fromExpression (e: Expression, negated: Boolean) : Boolean = {
    e match {
      case bae: BinaryArithmeticExpression => {
        if (bae.op == ArithmeticOperator.+) {
          // check recursively if both we have a linear expression on both sides of the +
          fromExpression(bae.left, negated) & fromExpression(bae.right, false)
        } else if (bae.op == ArithmeticOperator.-) {
          fromExpression(bae.left, negated) & fromExpression(bae.right, true)
        } else if (bae.op == ArithmeticOperator.*) {
          if (bae.left.isInstanceOf[VariableIdentifier] && bae.right.isInstanceOf[Constant]) {
            val n = Integer.parseInt(bae.right.asInstanceOf[Constant].constant)
            val r = if (negated) new Rational(-n) else new Rational(n)
            val v = bae.left.asInstanceOf[VariableIdentifier]
            coefficients.put(new PubsVariable(v.getName(), false), r)
            true
          } else if (bae.right.isInstanceOf[VariableIdentifier] && bae.left.isInstanceOf[Constant]) {
            val n = Integer.parseInt(bae.left.asInstanceOf[Constant].constant)
            val r = if (negated) new Rational(-n) else new Rational(n)
            val v = bae.right.asInstanceOf[VariableIdentifier]
            coefficients.put(new PubsVariable(v.getName(), false), r)
            true
          } else false
        } else false
      }
      case c: Constant => {
        try {
          constant = if (negated) new Rational(-Integer.parseInt(c.constant)) else new Rational(Integer.parseInt(c.constant))
          true
        }
        catch { case nfe : NumberFormatException => false }
      }
      case m: MaybeHeapIdSetDomain[Any] => {
        coefficients.put(new PubsVariable(m.toString(), false), if (negated) new Rational(-1) else new Rational(1))
        true
      }
      case v: VariableIdentifier => {
        coefficients.put(new PubsVariable(v.getName(), false), if (negated) new Rational(-1) else new Rational(1))
        true
      }
      case _ => false
    }
  }

  private def getCoeff(l: Lincons1, v: String) = {
    val temp = new Array[Double](1)
    if (l.getCoeff(v).isScalar) {
      l.getCoeff(v).sup().toDouble(temp, 0)
      temp.apply(0)
    } else 0.0
  }

  private def getCst(l: Lincons1) = {
    val temp = new Array[Double](1)
    if (l.getCst.isScalar) {
      l.getCst.sup().toDouble(temp, 0)
      temp.apply(0)
    } else 0.0
  }

}

/*
    A linear expression as it is used by the loop cost analysis.
 */
class LinearExpression(val constant : Rational, val coefficients : mutable.HashMap[PubsVariable, Rational]) {

  def variables = { coefficients.keySet }

  // does this LinearExpression have the form   a*v + b   where a >= 1 and b >= 0
  // we already know that all variables in 'others' increase
  def isIncUpdate (v: String, others: Set[String]) : Boolean = {
    var result = true
    for ((key, value) <- coefficients if !key.isConstant) {
      if (key.name != v && !value.isZero() && !others.contains(key.name)) result = false
      if (key.name == v && value.smaller(1)) result = false
    }
    if (constant.smaller(0)) result = false
    result || isConstUpdate(v)
  }

  // does this LinearExpression have the form   a*v + b   where 0 > a >= 1 and b <= 0
  // we already know that all variables in 'others' decrease
  def isDecUpdate (v: String, others: Set[String]) : Boolean = {
    var result = true
    for ((key, value) <- coefficients if !key.isConstant) {
      if (key.name != v && !value.isZero() && !others.contains(key.name)) result = false
      if (key.name == v && (value.larger(1) || value.smallerEqual(0))) result = false
    }
    if (constant.larger(0)) result = false
    result || isConstUpdate(v)
  }

  // does this LinearExpression have the form v
  def isConstUpdate (v: String) : Boolean = {
    var result = true
    for ((key, value) <- coefficients) {
      if (value != 0) result = false
    }
    result
  }

  // multiplier needed such that the constant and all coefficients become integer
  def multiplier(): Int = {
    var result = constant.denominator
    for ((key, value) <- coefficients) result = lcm(result, value.denominator)
    result
  }

  def multiple(n: Int) : LinearExpression = {
    val newConstant = constant.times(n)
    val newCoefficients : mutable.HashMap[PubsVariable, Rational] = mutable.HashMap.empty
    for ((key, value) <- coefficients) newCoefficients.put(key, value.times(n))
    new LinearExpression(newConstant, newCoefficients)
  }

  private def lcm(a: Int, b: Int) : Int = a*b/gcd(a,b)

  private def gcd(a: Int, b: Int) : Int = {
    if (b==0) a
    else gcd(b, a%b)
  }

  override def toString : String = {
    var temp = ""
    for ((key, value) <- coefficients) temp = temp + ("(" + value + "*" + key + ") + ")
    temp + "(" + constant + ")"
  }

  // for all variables for which we know a concrete value (i.e.  i = 0), insert it
  def toStringWithValues : String = {
    var temp = ""
    for ((key, value) <- coefficients) {
      if (key.initialValue != null) temp = temp + ("(" + value + "*" + key.initialValue + ") + ")
      else temp = temp + ("(" + value + "*" + key + ") + ")
    }
    temp + "(" + constant + ")"
  }

}