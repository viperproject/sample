package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import org.apache.commons.io.IOUtils
import java.io._


/**
 * TVLARunner serves to invoke TVLA. It provides input to TVLA and parses its outputs
 */
object TVLARunner {

  //================================================================================
  // Settings
  //================================================================================

  /**
   * TVLA run method
   *
   * If true, launch a new JVM every time TVLA is called.
   * If false, call the main method in the same process and reset TVLA's state.
   *
   * NOTE: in the case of JVM there must be an environment variable TVLA_HOME set to
   *       the installation directory of TVLA (e.g  /home/rf/eth/bachelorthesis/TVSHeap/tvla3custom/)
   *       and PATH needs to contain $TVLA_HOME/bin
   */
  var launchJVM = false

  /**
   * Cache results obtained from TVLA.
   * Compensates for CFG inefficiences...
   */
  var cacheResults = true


  //================================================================================
  // Names of TVLA related files (input/output)
  //================================================================================

  val inputTVS = "in.tvs"
  val inputTVP = "program.tvp"
  val outputTVS = "out.tvs"
  val properties = "tvla.properties"
  val tvlaCommand = "tvla.bat"


  //================================================================================
  // Caching
  //================================================================================
  private var tvlaCache: Map[(String,List[TVPAction]), List[TVS[SimpleNode]]] = Map()

  def resetCache() {
    tvlaCache = Map()
  }


  //================================================================================
  // Statistics
  //================================================================================

  /**
   * Number of times TVLA was invoked
   */
  var runs: Int = 0

  var uniqueRuns: Int = 0


  /**
   * Total time spent executing TVLA
   */
  var totalMainTime = 0L

  /**
   * Total time spent resetting the state of TVLA
   */
  var totalResetTime = 0L

  /**
   * Reset all the statistics
   */
  def resetMethodStatistics()  {
    runs = 0
    uniqueRuns = 0
    totalMainTime = 0
    totalResetTime = 0
  }

  init()

  private def init() = {

    if (launchJVM) {
      System.setProperty("tvla.home", System.getenv("TVLA_HOME"))
    } else {
      System.setProperty("tvla.home",System.getProperty("user.dir"))
    }


    // copy the tvla properties file to the current working directory
    try {
      val TVLAProperties = getClass.getClassLoader.getResourceAsStream (properties)
      IOUtils.copy(TVLAProperties, new FileOutputStream(properties))
      (new File("user.properties")).createNewFile()
    } catch {
      case e => println("TVLA settings could not be copied to the working directory")
                println(e.toString)
                println(e.printStackTrace())
                exit(1)
    }
  }

  /**
   * Run TVLA
   *
   * @param tvs TVS file
   * @param tvp TVP file, the program
   *
   */
  def run(tvs: String , tvp: TVP): List[TVS[SimpleNode]] = {
    runs += 1

    if (cacheResults && tvlaCache.contains((tvs, tvp.actions))) {
      return tvlaCache((tvs, tvp.actions))
    }

    try {
      // delete old TVS output (TVLA just appends)
      val outfile = new File(outputTVS)
      if (outfile.exists())
        outfile.delete()

      // write inputs for TVLA to files
      var file = new BufferedWriter(new FileWriter(inputTVS))
      file.write(tvs)
      file.close()
      file = new BufferedWriter(new FileWriter(inputTVP))
      file.write(tvp.program)
      file.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }

    if (launchJVM)
      runProcess()
    else
      runMain()


    val parser = new TVSParser(tvp)
    val programpoints = parser.parseResultFile(outputTVS)

    // the end state is at the CFG node "exit"
    val result =
      programpoints.get("exit") match {
        case Some(tvs:List[TVS[SimpleNode]]) => tvs
        case None => throw new Exception("Executing TVLA failed")
      }

    if (cacheResults) {
      tvlaCache += (tvs,tvp.actions) -> result
      uniqueRuns += 1
    }

    result
  }


  /**
   * Run TVLA in the same process by invoking its main method
   */
  private def runMain() {
    println("Running TVLA")
    val t1 = System.currentTimeMillis()
    tvla.Runner.main(Array(inputTVP, inputTVS, "-props", properties))
    val t2 = System.currentTimeMillis()
    totalMainTime += t2 - t1
    tvla.Runner.reset()
    val t3 = System.currentTimeMillis()
    totalResetTime += t3 - t2
    println("End of TVLA run")
  }

  /**
   * Run TVLA in a separate Java VM
   */
  private def runProcess() {
    var builder = new ProcessBuilder
    builder.command(tvlaCommand, inputTVP, inputTVS)
    val t1 = System.currentTimeMillis()
    val process = builder.start()
    // wait until tvla terminated
    process.waitFor()
    val t2 = System.currentTimeMillis()
    totalMainTime += t2 - t1
  }

}