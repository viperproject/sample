package semper.sample.multithreading

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{NonRelationalNumericalDomain, BoxedNonRelationalNumericalDomain}
import ch.ethz.inf.pm.sample.abstractdomain.{HeapIdSetDomain, Identifier, HeapIdentifier, Expression}
import ch.ethz.inf.pm.sample.SystemParameters

/**
 * Created with IntelliJ IDEA.
 * User: Pietro
 * Date: 06/11/12
 * Time: 16.41
 * To change this template use File | Settings | File Templates.
 */
/*class MultithreadedBoxedNonRelationalDomain[N <: NonRelationalNumericalDomain[N]](d: N) extends BoxedNonRelationalNumericalDomain[N](d) {
  override def factory() = new MultithreadedBoxedNonRelationalDomain[N](d.factory())

  override def eval(expr: Expression): N = super.eval(expr)/*expr match {
    case id: HeapIdentifier[_] =>
      var result = super.eval(expr);
      for(s <- ComputedInterference.value.value.keySet) {
        for(pc <- ComputedInterference.value.value.apply(s).value.keySet)
          if(s!=SystemParameters.currentMethod && ! SystemParameters.currentCFG.happensBefore(pc, expr.getProgramPoint()))
            if (ComputedInterference.value.value.apply(s).value.apply(pc)._1.value.contains(id))
              result=result.lub(result, ComputedInterference.value.value.apply(s).value.apply(pc)._2.asInstanceOf[MultithreadedBoxedNonRelationalDomain[N]].eval(expr));
      }
      return result;
    case xs: HeapIdSetDomain[_] =>
      var result = super.eval(expr);
      for(s <- ComputedInterference.value.value.keySet) {
        for(pc <- ComputedInterference.value.value.apply(s).value.keySet)
           if(s!=SystemParameters.currentMethod)
            for(id <- xs.value)
              if(! SystemParameters.currentCFG.happensBefore(pc, id.getProgramPoint()))
                if (ComputedInterference.value.value.apply(s).value.apply(pc)._1.value.contains(id))
                  result=result.lub(result, ComputedInterference.value.value.apply(s).value.apply(pc)._2.asInstanceOf[MultithreadedBoxedNonRelationalDomain[N]].get(id))
      }
      return result;
    case _ => return super.eval(expr);

  }*/
}*/