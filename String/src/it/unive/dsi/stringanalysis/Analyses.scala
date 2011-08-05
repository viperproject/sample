package it.unive.dsi.stringanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{SemanticDomain, SemanticAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation.NativeMethodSemantics
import ch.ethz.inf.pm.sample.property.Property
import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph

/**
 * Created by IntelliJ IDEA.
 * User: Pietro
 * Date: 04/08/11
 * Time: 17.00
 * To change this template use File | Settings | File Templates.
 */

abstract class StringAnalysis[S <: SemanticDomain[S]] extends SemanticAnalysis[S] {
  def getLabel() : String = "String analysis";
  def parameters() : List[(String, Any)] = Nil;
  def setParameter(label : String, value : Any) = throw new StringException("Parameters are not supported")
  override def reset() : Unit = Unit;
  def getProperties() : Set[Property] = Set(ShowGraph);
  def getNativeMethodsSemantics() : List[NativeMethodSemantics] = List(StringSemantics);
}

class BricksAnalysis extends StringAnalysis[Bricks] {
  override def getLabel() = super.getLabel()+": Bricks";
  def getInitialState() : Bricks = new Bricks();
}

class SurelyAndMaybeContainedCharactersAnalysis extends StringAnalysis[SurelyAndMaybeContainedCharacters] {
  override def getLabel() = super.getLabel()+": SurelyAndMaybeContainedCharacters";
  def getInitialState() : SurelyAndMaybeContainedCharacters = new SurelyAndMaybeContainedCharacters(new SurelyContainedCharacters(), new MaybeContainedCharacters());
}

class PrefixAndSuffixAnalysis extends StringAnalysis[PrefixAndSuffix] {
  override def getLabel() = super.getLabel()+": PrefixAndSuffix";
  def getInitialState() : PrefixAndSuffix = new PrefixAndSuffix(new Prefix(), new Suffix());
}