package ch.ethz.inf.pm.sample.wala

import java.io.IOException
import java.util

import com.ibm.wala.core.tests.callGraph.CallGraphTestUtil
import com.ibm.wala.dalvik.classLoader.DexIRFactory
import com.ibm.wala.dalvik.ipa.callgraph.impl.AndroidEntryPoint
import com.ibm.wala.dalvik.util.{AndroidEntryPointLocator, AndroidAnalysisScope}
import com.ibm.wala.dalvik.util.AndroidEntryPointLocator.LocatorFlags
import com.ibm.wala.ipa.callgraph.AnalysisOptions.ReflectionOptions
import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.ipa.callgraph.propagation.{PointerAnalysis, SSAPropagationCallGraphBuilder}
import com.ibm.wala.ipa.callgraph._
import com.ibm.wala.ipa.cha.{ClassHierarchy, IClassHierarchy, ClassHierarchyException}
import com.ibm.wala.util.CancelException

import scala.collection.JavaConverters._

/**
 * Implements the main analysis backed up with Wala
 *
 * @author Lucas Brutschy
 */
object WalaAnalysis  {

  def main(args:Array[String]) = {

    for (arg <- args) {
      WalaCompiler.compileFile(arg)
    }

  }

}
