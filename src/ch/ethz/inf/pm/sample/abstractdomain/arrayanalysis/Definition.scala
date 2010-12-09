package ch.ethz.inf.pm.sample.abstractdomain.arrayanalysis

import ch.ethz.inf.pm.sample.abstractdomain._

trait ArrayAnalysis[D <: ArrayAnalysis[D]] {
	def updateArray(arr : Identifier, index : Expression, value : Expression) : D;
	def getArrayValue(arr : Identifier, index : Expression) : D;
	def getArrayLength(arr : Identifier) : D;
	def createArray(arr : Identifier, length : Expression) : D;
}