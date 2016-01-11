package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type, NativeMethodSemantics}

object PermissionNativeMethodSemantics extends NativeMethodSemantics {

  /**
    * It defines the forward semantics of native method calls
    *
    * @param thisExpr the expression representing the object on whom the method is called
    * @param operator the string of the called method
    * @param parameters the parameters of the called method
    * @param typeparameters the list of type generics
    * @param returnedtype the type of the returned value
    * @param state the abstract state in which the method call is evaluated
    * @return the abstract state obtained after the forward evaluation of the native method call,
    *         None if the semantics of the method call is not defined
    */
  override def applyForwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet,
                                                          operator: String,
                                                          parameters: List[ExpressionSet],
                                                          typeparameters: List[Type],
                                                          returnedtype: Type,
                                                          programpoint: ProgramPoint,
                                                          state: S): Option[S] = ???

  /**
    * It defines the backward semantics of native method calls
    *
    * @param thisExpr the expression representing the object on whom the method is called
    * @param operator the string of the called method
    * @param parameters the parameters of the called method
    * @param typeparameters the list of type generics
    * @param returnedtype the type of the returned value
    * @param programpoint the program point of the method call
    * @param state the abstract state in which the method call is evaluated
    * @return the abstract state obtained after the backward evaluation of the native method call,
    *         None if the semantics of the method call is not defined
    */
  override def applyBackwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet,
                                                           operator: String,
                                                           parameters: List[ExpressionSet],
                                                           typeparameters: List[Type],
                                                           returnedtype: Type,
                                                           programpoint: ProgramPoint,
                                                           state: S,
                                                           oldPreState: S): Option[S] = ???
}