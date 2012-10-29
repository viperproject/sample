package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain._
import numericaldomain._

///**
//*
//* Lucas Brutschy
//* Date: 10/12/12
//* Time: 4:14 PM
//*
//*/
//
//trait EnvironmentDomain {
//
//  def
//
//
//}
//
//
//
///**
//* A domain that maps environment identifiers to some lattice
//*/
//trait EnvironmentDomain[E <: Enumeration, D <: Lattice[D], M <: EnvironmentDomain[E,D,M]]
//  extends FunctionalDomain[E,D,M] {
//
//}
//
///**
//* A domain that maps environment identifiers to the boolean domain
//*/
//class BooleanEnvironmentDomain[E <: Enumeration]
//  extends EnvironmentDomain[E,BooleanDomain,BooleanEnvironmentDomain[E]] {
//
//  override def factory() = new BooleanEnvironmentDomain[E]
//
//  def get(key : E) : BooleanDomain = this.value.get(key) match {
//    case None => new BooleanDomain()
//    case Some(x) => x
//  }
//
//}
//
///**
//* Combines numerical domain with an environment domain
//*/
//class NumericalAndEnvironmentDomain[N <: NumericalDomain[N], E <: Enumeration, D <: Lattice[D], M <: EnvironmentDomain[E,D,M],T <: NumericalAndEnvironmentDomain[N,E,D,M,T]]
//      (a1:N,a2:M) extends SemanticOneSidedProductDomain[N,M,NumericalAndEnvironmentDomain[N,E,D,M,T]] (a1,a2) {
//
//  override def factory() = new NumericalAndEnvironmentDomain[N,E,D,M,T](a1.factory(),a2.factory())
//
//  override def toString() = "Value domain:\n"+this._1.toString+"\nEnvironment Domain:\n"+this._2.toString
//
//}
