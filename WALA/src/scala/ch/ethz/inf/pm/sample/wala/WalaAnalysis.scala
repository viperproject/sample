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
class WalaAnalysis  {


  def main(args:Array[String]) = {

    for (arg <- args) {
      ScanDroidEngine.makeFromJar(arg)
    }

  }

}

object ScanDroidEngine {

  var exclusions: String = getClass.getResource("/exclusions.txt").getPath
  var androidJar: String = getClass.getResource("/android.jar").getPath
  var regressions: String = getClass.getResource("/Java60RegressionExclusions.txt").getPath

  def makeFromJar(jar: String):ScanDroidEngine = {

    val time: Long = System.currentTimeMillis
    val path = jar
    val scope: AnalysisScope = AndroidAnalysisScope.setUpAndroidAnalysisScope(androidJar, jar, regressions)
    val cha: IClassHierarchy = ClassHierarchy.make(scope)
    val cache: AnalysisCache = new AnalysisCache(new DexIRFactory)
    val flags: Set[AndroidEntryPointLocator.LocatorFlags] = Set(
      LocatorFlags.INCLUDE_CALLBACKS,
      LocatorFlags.EP_HEURISTIC,
      LocatorFlags.WITH_CTOR,
      LocatorFlags.WITH_SUPER,
      LocatorFlags.CB_HEURISTIC
    )
    val eps: AndroidEntryPointLocator = new AndroidEntryPointLocator(flags.asJava)
    val es: util.List[AndroidEntryPoint] = eps.getEntryPoints(cha)
    val options: AnalysisOptions = new AnalysisOptions(scope, es)
    options.setReflectionOptions(ReflectionOptions.NONE)
    val cgb: SSAPropagationCallGraphBuilder = Util.makeZeroCFABuilder(options, cache, cha, scope)
    val cg = cgb.makeCallGraph(options)
    val ptr = cgb.getPointerAnalysis
    val walaTime = System.currentTimeMillis - time
    ScanDroidEngine(cg,ptr,path,walaTime)


  }

}

case class ScanDroidEngine(cg: CallGraph,ptr: PointerAnalysis[_],path: String,walaTime:Long)