package ch.ethz.inf.pm.td.cloud

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Modifier}
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.compiler.{ApiMember, ApiMemberSemantics, CloudEnabledModifier}

/**
 * @author Lucas Brutschy
 */
trait CloudOperationSemantics extends ApiMemberSemantics {

  def typeModifiers:Set[Modifier]

  def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                     (implicit pp: ProgramPoint, state: S): S = {
    state
  }

}

trait CloudUpdateSemantics extends CloudOperationSemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                     (implicit pp: ProgramPoint, state: S): S = {

    val curState = super.forwardSemantics[S](this0,method,parameters)
    if (TouchAnalysisParameters.get.trackCloudTypes && typeModifiers.contains(CloudEnabledModifier)) {

      println(this0,method)

    }
    curState
  }

}

case class CloudUpdateWrapper(sem:ApiMemberSemantics, typeModifiers: Set[Modifier]) extends CloudUpdateSemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                              (implicit pp: ProgramPoint, state: S): S = {

    var curState = super.forwardSemantics[S](this0,method,parameters)
    curState = sem.forwardSemantics[S](this0,method,parameters)(pp,curState)
    curState

  }

}

trait CloudQuerySemantics extends CloudOperationSemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                     (implicit pp: ProgramPoint, state: S): S = {

    val curState = super.forwardSemantics[S](this0,method,parameters)
    if (TouchAnalysisParameters.get.trackCloudTypes && typeModifiers.contains(CloudEnabledModifier)) {

      println(this0,method)

    }
    curState

  }

}


case class CloudQueryWrapper(sem:ApiMemberSemantics, typeModifiers: Set[Modifier]) extends CloudQuerySemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                              (implicit pp: ProgramPoint, state: S): S = {

    var curState = super.forwardSemantics[S](this0,method,parameters)
    curState = sem.forwardSemantics[S](this0,method,parameters)(pp,curState)
    curState

  }

}


//object GlobalUpdateState {
//
//  def getUpdates(this0: ExpressionSet) = {
//
//  }
//
//  def recordUpdate[T <: TouchStateInterface[T]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet], curState: T): T = {
//
//  }
//
//}
//
//object CommutativitySpecs {
//
//  def commutative(updateOp:)
//
//
//}