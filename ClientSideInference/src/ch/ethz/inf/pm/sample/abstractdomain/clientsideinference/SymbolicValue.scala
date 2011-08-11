package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

trait SymbolicValue[T <: SymbolicValue[T]] {
  override def equals(o : Any) : Boolean;
  def <=(a : T, b : T) : Boolean;
}