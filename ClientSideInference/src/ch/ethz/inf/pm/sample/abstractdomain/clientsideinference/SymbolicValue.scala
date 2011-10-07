package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.BinaryArithmeticExpression._
import ch.ethz.inf.pm.sample.oorepresentation.{VariableDeclaration, Type, MethodDeclaration}
import ch.ethz.inf.pm.sample.abstractdomain._

trait SymbolicValue[T <: SymbolicValue[T]] {
  override def equals(o : Any) : Boolean;
  def <=(a : T, b : T) : Boolean;
  def minimize() : Boolean;
}

object SymbolicSettings {
  def symbolicInt[T <: SymbolicInt[T, S], S <: SymbolicValue[S]] : T =
    new LinearSum(
      new Summation(Map.empty[S, Coefficient[T, S]], new IntervalsSymbolicValues(SystemParameters.typ.top(), "b", TypeOfContracts.precondition, new VariableIdentifier("v", SystemParameters.typ.top(), null), SymbolicContractTypes.min).asInstanceOf[S]),
      0,
      new IntervalsSymbolicValues(SystemParameters.typ.top(), "b", TypeOfContracts.precondition, new VariableIdentifier("v", SystemParameters.typ.top(), null), SymbolicContractTypes.min).asInstanceOf[S]).asInstanceOf[T];

  var precondition : TypeOfInference.Value = TypeOfInference.weakest;

  var postcondition : TypeOfInference.Value = TypeOfInference.strongest;


  /**
   * Given the name of a method, the expression over whom the method is called, and the list of expressions passed as
   * parameters to the method, this function returns a list of couples (exp, id) where id is the identifier contained in
   * the expression passed to the method, and exp is an arithmetic expression representing to what is equal in the
   * called method id (the expression contain the name of the argument of the method and this can be the only id in the
   * expression)
   * TODO: Write something decent here!
   */
  def rename(calledMethod : String, thisExpr : Expression, parameters : List[Expression], swap : Boolean) : (Type, List[(Expression, Identifier)]) = {

    val (methodDeclaration, classe) = SystemParameters.compiler.getMethod(calledMethod, thisExpr.getType(), exprsToTypes(parameters)) match {
      case Some(s) => s
      case None => throw new SymbolicDBMException("I don't know what I'm calling, so I cannot instantiate symbolic contracts")
    }
    if(methodDeclaration.arguments.size!=1) throw new SymbolicDBMException("Not yet supported");
    val methodNames = listVariableDeclToIds(methodDeclaration.arguments.apply(0));
    if(parameters.size != methodNames.size)
      throw new SymbolicDBMException("Not allowed");
    var result : List[(Expression, Identifier)] = Nil;
    for(i <- 0 to parameters.size-1)
      result = result ::: renameSingle (parameters.apply(i), methodNames.apply(i), swap) :: Nil;
      /*parameters.apply(i) match {
        case id : Identifier => result = result ::: ((methodNames.apply(i), id)) :: Nil;
        case BinaryArithmeticExpression(left : Identifier, right : Constant, ArithmeticOperator.+, returntyp) =>
          result = result ::: ((BinaryArithmeticExpression(methodNames.apply(i), right, if(swap) ArithmeticOperator.- else ArithmeticOperator.+, returntyp), left)) :: Nil;
        case BinaryArithmeticExpression(left : Identifier, right : Constant, ArithmeticOperator.-, returntyp) =>
          result = result ::: ((BinaryArithmeticExpression(methodNames.apply(i), right, if(swap) ArithmeticOperator.+ else ArithmeticOperator.-, returntyp), left)) :: Nil;
      } */
    return (classe, result);
  }

  def renameSingle(exp : Expression, methodId : Identifier, swap : Boolean) : (Expression, Identifier) =
    exp match {
        case id : Identifier => return ((methodId, id));
        case BinaryArithmeticExpression(left : Identifier, right : Constant, ArithmeticOperator.+, returntyp) =>
          return ((BinaryArithmeticExpression(methodId, right, if(swap) ArithmeticOperator.- else ArithmeticOperator.+, returntyp), left));
        case BinaryArithmeticExpression(left : Identifier, right : Constant, ArithmeticOperator.-, returntyp) =>
         return ((BinaryArithmeticExpression(methodId, right, if(swap) ArithmeticOperator.+ else ArithmeticOperator.-, returntyp), left));
    }

  private def exprsToTypes(expr : List[Expression]) : List[Type] = expr match {
    case x :: x1 => x.getType() :: exprsToTypes(x1);
    case Nil => Nil;
  }

  private def listVariableDeclToIds(pars : List[VariableDeclaration]) : List[Identifier] = pars match {
    case x :: x1 =>
      new VariableIdentifier(x.variable.getName(), x.typ, x.getPC()) :: listVariableDeclToIds(x1);
    case Nil => Nil;
  }

}

object TypeOfContracts extends Enumeration {
  val precondition = Value("Pre");
  val postcondition = Value("Post");
}

object TypeOfInference extends Enumeration {
  val weakest = Value("Weakest");
  val strongest = Value("Strongest");
}