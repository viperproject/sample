package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import Kleene._
import java.io._
import org.apache.commons.io.FileUtils

/**
 * TVLALogger provides a logging facility for TVSHeap that is helpful for debugging
 *
 * NOTE: If enabled, this class requires an installation of Graphviz (dot command)
 */
object TVLALogger {

  /**
   * Turn logging on and off
   */
  var enabledLog: Boolean = false

  /**
   * All the logged events so far
   */
  var entries: List[LogEntry] = Nil

  /**
   * Logging directory (relative to cwd)
   */
  val dir = "logger/H%d/"

  /**
   * File name scheme for visualized heap states
   * (supported by graphviz: png,pdf)
   */
  val filename = "H%d_S%d_%s.%s"

  /**
   * Image format of visualized heap states
   * (supported by graphviz: png,pdf,...)
   */
  val fformat = "pdf"


  (FileUtils.deleteDirectory(new File("logger/")))


  /**
   * Add an entry to the log
   */
  def log(entry: LogEntry): Unit = {
    if (!enabledLog)
      return

    var id = entries.indexOf(entry)

    if (id < 0) {
      id = entries.size
      entries = entries :+ entry

      (new File(dir.format(id))).mkdirs()

      FileUtils.copyFile(new File("in.tvs"),new File(dir.format(id) + "in.tvs"))
      FileUtils.copyFile(new File("out.tvs"),new File(dir.format(id) + "out.tvs"))
      FileUtils.copyFile(new File("program.tvp"),new File(dir.format(id) + "program.tvp"))

      var n = 0
      for (s <- entry.before.structures) {
        DOTtoImage(TVStoDOT(s), dir.format(id) + filename.format(id,n,"before",fformat))
        n += 1
      }

      n = 0
      for (s <- entry.beforeEncoded) {
        DOTtoImage(TVStoDOT(s), dir.format(id) + filename.format(id,n,"beforeEncoded",fformat))
        n += 1
      }

      n = 0
      for (s <- entry.afterEncoded) {
        DOTtoImage(TVStoDOT(s), dir.format(id) + filename.format(id,n,"afterEncoded",fformat))
        n += 1
      }

      n = 0
      for (s <- entry.after.structures) {
        DOTtoImage(TVStoDOT(s), dir.format(id) + filename.format(id,n,"after",fformat))
        n += 1
      }
    }

    println("["+id+"] " + entry.actions.mkString("; "))
  }

  def reset() = {
    entries = Nil
    FileUtils.deleteDirectory(new File("logger/"))
  }

  /**
   * Use graphviz to convert a DOT file to an image
   */
  def DOTtoImage(dot: String, file: String) = {
    val builder = new ProcessBuilder
    builder.command("dot", "-T"+fformat, "-o", file)
    val process = builder.start()
    val dotStdin = new OutputStreamWriter(process.getOutputStream)
    dotStdin.write(dot)
    dotStdin.close()
    process.waitFor()
  }


  /**
   * Our custom quick-and-dirty conversion of TVS to graphviz DOT files because
   * the builtin support of TVLA is not configurable at all and cant output individual structures.
   */
  def TVStoDOT[N <: NodeName](tvs: TVS[N]): String = {
    def quote(s:String) = "\"" + s + "\" "

    val buf = new StringBuilder

    buf.append("digraph structure { \n")

    //buf.append("size="+quote("1.96,1.96!")+";\n")
    //buf.append("ratio=compress;\n")
    buf.append("ranksep=0.2;\n")
    buf.append("nodesep=0.2;\n")
    buf.append("edge [fontsize=10];\n")
    buf.append("node [fontsize=10];\n")

    for (n <- tvs.nodes) {
      val summarized = tvs.summarization.values.contains(n)
      var labels: List[String] = List(n.toString)

      for ((name,np) <- tvs.names; t <- np.values.get(n)) {
        t match {
          case True => labels ++= List(name.toString)
          case Unknown => labels ++= List(name.toString + " :1/2")
        }
      }

      buf.append(quote(n.toString))
      buf.append("[label=" + quote(labels.mkString("\\n")))
      if (summarized) {
        buf.append(", style=dashed, peripheries=1")
      }
      buf.append("];\n")
    }

    for ((k,v) <- tvs.programVariables; target <- v.value) {
      buf.append(quote(k) + "[shape=plaintext, style=bold, fontsize=16];\n")
      buf.append(quote(k) + " -> " + quote(target.toString) + ";\n")
    }

    for ((_,field) <- tvs.fields; (n1, (n2, t)) <- field.values) {
      buf.append(quote(n1.toString) + " -> " + quote(n2.toString))
      buf.append("[label=" + quote(field.name) + ", fontsize=16")
      if (t == Unknown) {
        buf.append(", style=dashed")
      }
      buf.append("];\n")
    }

    buf.append("}\n")

    buf.toString
  }

}

class LogEntry(val actions: List[TVPAction],
               val before: TVSHeap,
               val beforeEncoded: List[TVS[SimpleNode]],
               val after: TVSHeap,
               val afterEncoded: List[TVS[SimpleNode]]) {

  override def equals(that: Any) = that match {
    case that: LogEntry => this.actions == that.actions && this.before == that.before
    case _ => false
  }
}